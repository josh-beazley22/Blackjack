
library(R6)

Blackjack <- R6Class("Blackjack",
  public = list(
    ## Tracking hands
    player.hand = NULL, ## list() of num.player c()
    player.results = NULL, ## list() of num.player c()
    player.bet = NULL, ## c(Int)
    dealer.hand = NULL, ## c(Char)
    
    num.players = 1,
    player.chips = 200,
    
    deck.size = 8,
    seen.cards =  setNames(rep(0, 13), c('A','2','3','4','5','6','7','8','9','10','J','Q','K')),
    legal.moves = NULL,
    insurance.paid = NULL, # 1 = insurance lost, 21 = insurance won, 0 = no insurance.
    no.actions = NULL,
    hit.on.soft.17 = TRUE,
    
    card.names = c('A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K'),
    card.vals  = c( 1 ,  2 ,  3 ,  4 ,  5 ,  6 ,  7 ,  8 ,  9 ,  10 , 10 , 10 , 10),
    
    initialize = function(chips = NULL, num.players = NULL, deck.size = NULL, hit.on.soft.17 = NULL) {
      if (!is.null(chips)) self$chips <- chips
      if (!is.null(num.players)) {
        self$num.players <- num.players
        self$player.chips <- rep(self$player.chips, self$num.players)
      }
      if (!is.null(deck.size)) self$deck.size <- deck.size
      if (!is.null(hit.on.soft.17)) self$hit.on.soft.17 = hit.on.soft.17
    },
    
    print = function(...) {
      cat("Player hand:", paste(self$player.hand, collapse = ", "), "\n")
      cat("Player results:", paste(self$player.results, collapse = ", "), "\n")
      cat("Dealer face card:", paste(self$dealer.hand, collapse = ", "), "\n")
      cat("Bet:", paste(self$player.bet, collapse = ", "), "\n")
      cat("Chips:", paste(self$player.chips, collapse = ", "), "\n")
      cat("Number of Players:", self$num.players, "\n")
      cat("Deck size:", self$deck.size, "\n")
      cat("Seen cards:\n")
      print(self$seen.cards)
    }
  ),
)

floor.sum <- function(cards) {
  ## Treats Ace as 1 always.
  card.vals <- c(A = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6,
                 `7` = 7, `8` = 8, `9` = 9, `10` = 10, J = 10, Q = 10, K = 10)
  total <- sum(card.vals[cards])
  total
}

ceil.sum <- function(cards) {
  ## Treats Ace as 11 when the total is less than 21.
  card.vals <- c(A = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6,
                 `7` = 7, `8` = 8, `9` = 9, `10` = 10, J = 10, Q = 10, K = 10)
  total <- sum(card.vals[cards])
  if ("A" %in% cards && total <= 11) {
    total <- total + 10
  }
  total
}


dealer.must.hit <- function(dealer.hand, hit.on.soft.17) {
  sum = ceil.sum(dealer.hand)
  if (!hit.on.soft.17) {
    return(sum < 17)
  }
  
  if (sum > 17) {
    return(FALSE)
  } else if (sum < 17) {
    return(TRUE)
  } else {  ## upper.sum == 17
    if (floor.sum(dealer.hand) == 7) {
      ## soft 17 hand
      return(hit.on.soft.17)
    } else {
      return(FALSE)
    }
  }
}

draw.probabilities <- function(bird) {
  ## Calculates prob. of drawing each card individually.
  deck = rep(4*bird$deck.size, 13)
  names(deck) = bird$card.names
  deck = deck - bird$seen.cards
  deck['10'] = deck['10'] + deck['J'] + deck['Q'] + deck['K']
  deck = deck[1:10]
  deck / sum(deck)
}

draw.card <- function(bird) {
  
  ## Calculates prob. of drawing each card individually.
  deck = rep(4*bird$deck.size, 13)
  names(deck) = bird$card.names
  deck = deck - bird$seen.cards
  deck / sum(deck)
  
  ## Draw card from deck
  card = sample(bird$card.names, size = 1, prob = deck)
  ## Update bird$seen.cards to contain this newly drawn card
  bird$seen.cards[card] = bird$seen.cards[card] + 1
  card
}

game.reset <- function(bird) {
  
  ## Reset fields
  bird$player.hand = list()
  bird$player.results = list()
  bird$dealer.hand = c()
  bird$insurance.paid = 0
  bird$no.actions = rep(TRUE, bird$num.players)
  
  ## When half of the deck has been used, shuffle.
  if (sum(bird$seen.cards) > 56*bird$deck.size) {
    bird$seen.cards = setNames(rep(0, 13), bird$card.names)
  }
  
  ## Draw 1 card for each player
  for (i in 1:bird$num.players) {
    card = draw.card(bird)
    bird$player.hand[[i]] = c(card)
  }
  ## Dealer gets a face-up card
  bird$dealer.hand = draw.card(bird)
  
  ## Each player gets 1 more card.
  for (i in 1:bird$num.players) {
    card = draw.card(bird)
    bird$player.hand[[i]] = c(bird$player.hand[[i]], card)
  }
}

insurance.policy <- function(bird) {
 
  # DON'T CALL draw.card()-- instead generate random number & assign insurance.paid code
  
  ## EV = 
  ## + 0 * P(10-valued card)
  ## - bet/2 * P(other card)
  ## + bet * EV(insurance == 1)
  ## - bet * EV(insurance == 0)
  
  ## If a player chooses to call insurance, use insurance code
  # bird$player.results[[player.no]] = c(77, 0.5 * bird$player.bet[[player.no]])
}

policy <- function(bird, player.no) {
  ## Let's make flags to control for legal moves.
  bird$legal.moves = c("hit", "stand")
  hand <- bird$player.hand[[player.no]]
  
  # 1. split only possible when player.hand has two identical cards
  if (length(hand) == 2 && hand[1] == hand[2]) {
    bird$legal.moves = c(bird$legal.moves, "split")
  }
  # 2. double down only possible when player.hand has two cards
  if (length(hand) == 2) {
    bird$legal.moves = c(bird$legal.moves, "double down")
  }
  # 3. surrender only possible immediately on dealt
  if (bird$no.actions[[player.no]]) {
    bird$legal.moves = c(bird$legal.moves, "surrender")
  }
  ## Return random action from legal.moves
  sample(bird$legal.moves, 1)
}

player.turn <- function(bird, player.no) {
  
  ## Reset player results
  bird$player.results[[player.no]] = c()
  ## Player has taken an action, so surrendering is not allowed
  bird$no.actions[player.no] = FALSE
  ## Find best move from policy
  move = policy(bird, player.no)
  ## Define local variables
  hand = bird$player.hand[[player.no]]
  bet = bird$player.bet[player.no]
  
  if (move == "split") {
    ## Hand 1
    bird$player.hand[[player.no]] = c(hand[1], draw.card(bird))
    player.turn(bird, player.no)
    hand1.result = bird$player.results[[player.no]]
    ## Hand 2
    bird$player.hand[[player.no]] = c(hand[1], draw.card(bird))
    player.turn(bird, player.no)
    ## Combine Results
    bird$player.results[[player.no]] = c(bird$player.results[[player.no]], hand1.result)
  }
  else if (move == "double down") {
    ## draw card
    bird$player.hand[[player.no]] = c(hand, draw.card(bird))
    ## calc sum
    sum = ceil.sum(bird$player.hand[[player.no]])
    ## store result
    bird$player.results[[player.no]] = c(sum, 2*bet)
  }
  else if (move == "surrender") {
    ## store loss code and bet size
    bird$player.results[[player.no]] = c(22, 0.5 * bet)
  }
  else if (move == "hit") {
    bird$player.hand[[player.no]] = c(hand, draw.card(bird))
    sum = ceil.sum(bird$player.hand[[player.no]])
    if (sum > 21) {
      ## Bust
      bird$player.results[[player.no]] = c(sum, bet)
    } else {
      ## Proceed to next action
      player.turn(bird, player.no)
    }
  }
  else if (move == "stand") {
    bird$player.results[[player.no]] = c(ceil.sum(hand), bet)
  }
}

dealer.turn <- function(bird) {
  
  ## When insurance is paid, the face-down card cannot be 10-valued.
  if (length(bird$dealer.hand) == 1 && bird$insurance.paid == 1) {
    draw.probs = draw.probabilities(bird)
    ## Remove all 10 draws
    draw.probs = draw.probs[1:9] / sum(draw.probs[1:9])
    ## sample from draw.probs to draw a card
    card = names(sample(draw.probs, 1))
    bird$dealer.hand = c(bird$dealer.hand, card)
    ## mark down as a seen card
    bird$seen.cards[card] = bird$seen.cards[card] + 1
  }
  ## Dealer draws until hand total is 17+
  while(dealer.must.hit(bird$dealer.hand, bird$hit.on.soft.17)) {
    bird$dealer.hand = c(bird$dealer.hand, draw.card(bird))
  }
  dealer.sum = ceil.sum(bird$dealer.hand)
  ## Payout all players
  for (player.no in 1:bird$num.players) {
    result = bird$player.results[[player.no]]
    for (i in seq(1, length(result), by=2)) {
      if (result[i] == 21 && length(bird$player.hand[i]) == 2) {
        ## Player wins -- Blackjack pays out 1.5x bet
        bird$player.chips[[player.no]] = bird$player.chips[[player.no]] + 1.5 * result[i+1]
      } else if (result[i] > 21) {
        ## Player losses -- total > 21
        bird$player.chips[[player.no]] = bird$player.chips[[player.no]] - result[i+1]
      } else if (dealer.sum > 21) {
        ## Player win -- dealer total > 21
        bird$player.chips[[player.no]] = bird$player.chips[[player.no]] + result[i+1]
      } else if (result[i] < dealer.sum) {
        ## Player losses -- dealer > player
        bird$player.chips[[player.no]] = bird$player.chips[[player.no]] - result[i+1]
      } else if (result[i] > dealer.sum) {
        ## Player wins -- total > dealer.sum
        bird$player.chips[[player.no]] = bird$player.chips[[player.no]] + result[i+1]
      } else {
        ## Push -- dealer & player tie. No change in chips
      }
      ## Set bet to zero
      bird$player.bet[player.no] = 0
    }
  }
}

main <- function() {
  
  bird <- Blackjack$new(num.players = 3)
  
  ## play 100 hands
  for (game.no in 1:1) {
    
    ## TODO dynamically change bet based on a strategy
    bird$player.bet = rep(10, bird$num.players)
    ## Deal cards to every player
    game.reset(bird)
    ## Ask all players to pay insurance when applicable
    if (bird$dealer.hand[1] == 'A') {
      ## Each player chooses if they would like to buy insurance
      insurance.policy(bird)
      if (bird$insurance.paid == 21) {
        ## Dealer has blackjack. Payout insurance.
        for (player.no in 1:bird$num.players) {
          result = bird$player.results[[player.no]]
          if (result[1] != 77) {
            ## player did not call for insurance. player loses bet.
            bird$player.chips[[player.no]] = bird$player.chips[[player.no]] - bird$player.bet[[player.no]]
          }
        }
        ## Dealer has blackjack. Move to next game.
        next
      }
    }
    ## Each player takes a turn in order
    for (player.no in 1:bird$num.players) {
      player.turn(bird, player.no)
    }
    ## Dealer draws and pays winners
    dealer.turn(bird)
  }
}





