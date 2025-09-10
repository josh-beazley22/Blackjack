
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
    min.bet = 10,
    loss.count = c(0),
    
    deck.size = 8,
    seen.cards =  setNames(rep(0, 13), c('A','2','3','4','5','6','7','8','9','10','J','Q','K')),
    insurance.paid = 0, # 1 = insurance lost, 21 = insurance won, 0 = no insurance.
    no.actions = TRUE,
    hit.on.soft.17 = TRUE,
    
    card.names = c('A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K'),
    card.vals  = setNames(c(1,2,3,4,5,6,7,8,9,10,10,10,10), c('A','2','3','4','5','6','7','8','9','10','J','Q','K')),
    
    initialize = function(chips = NULL, num.players = NULL, deck.size = NULL, hit.on.soft.17 = NULL) {
      if (!is.null(chips)) self$player.chips <- chips
      if (!is.null(num.players)) {
        self$num.players <- num.players
        self$no.actions <- rep(TRUE, self$num.players)
        self$player.chips <- rep(self$player.chips, self$num.players)
        self$loss.count <- rep(0, self$num.players)
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


find.legal.moves <- function(bird, player.no) {
  
  hand <- bird$player.hand[[player.no]]
  if (floor.sum(hand) > 21) {
    return(c("stand"))
  }
  
  # Hit & Stand - always legal
  legal.moves = c("hit", "stand")
  # Split -- only possible when player.hand has two identical cards
  if (length(hand) == 2 && hand[1] == hand[2]) {
    legal.moves = c(legal.moves, "split")
  }
  # Double Down -- only possible when player.hand has two cards
  if (length(hand) == 2) {
    legal.moves = c(legal.moves, "double down")
  }
  # Surrendering -- only possible immediately when dealt
  if (bird$no.actions[[player.no]]) {
    legal.moves = c(legal.moves, "surrender")
  }
  legal.moves
}


dealer.must.hit <- function(dealer.hand, hit.on.soft.17) {
  ## Returns Boolean true if the dealer must hit
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
  
  ## When a player has 0 or fewer chips, they are disqualified
  # if (any(bird$player.chips <= 0)) {
  #   bird$num.players = bird$num.players - sum(bird$player.chips <= 0)
  #   bird$loss.count = bird$loss.count[bird$player.chips > 0]
  #   bird$bet.strategy = bird$bet.strategy[bird$player.chips > 0]
  #   bird$player.chips = bird$player.chips[bird$player.chips > 0]
  # }
  
  ## Reset fields
  bird$player.hand = list()
  bird$player.results = list()
  bird$dealer.hand = c()
  bird$insurance.paid = 0
  bird$no.actions = rep(TRUE, bird$num.players)
  for (i in 1:bird$num.players) {
    bird$player.results[[i]] = c()
  }
  
  ## When half of the deck has been used, shuffle.
  if (sum(bird$seen.cards) > 0.5*52*bird$deck.size) {
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


insurance.action <- function(bird, insurance.decision) {
  
  if (any(insurance.decision == TRUE)) {
    draw.probs = draw.probabilities(bird)
    if (names(sample(draw.probs, 1)) == '10') {
      ## Dealer drew blackjack. Payout insurance.
      for (player.no in 1:bird$num.players) {
        if (insurance.decision[player.no] == FALSE) {
          ## player did not call for insurance. player loses bet.
          bird$player.chips[[player.no]] = bird$player.chips[[player.no]] - bird$player.bet[[player.no]]
        }
      }
      ## Hand is over
      bird$insurance.paid = 22
    } else {
      ## Dealer did not draw blackjack. Continue knowing that the dealer cannot draw a 10.
      bird$insurance.paid = 1
    }
  }
}


player.turn <- function(bird, player.no, policy) {
  
  if (policy == "optimal") {
    action.table = optimal.player.policy(bird, player.no)
    action = action.table[which.max(action.table[[2]]), 1]
    
  } else if (policy == "random") {
    legal.moves = find.legal.moves(bird, player.no)
    action = sample(legal.moves, 1)
    
  } else if (policy == "dealer") {
    action = ifelse(
      dealer.must.hit(bird$player.hand[[player.no]], bird$hit.on.soft.17),
      "hit",
      "stand"
    )
    ## always split when possible
    hand = bird$player.hand[[player.no]]
    if (length(hand) == 2 && hand[1] == hand[2]) {
      action = "split"
    }
  } else if (policy == "wikipedia") {
    ## look up action from Wikipedia's table
  }
  # TODO: script more player policies
  
  ## Player has taken an action, so surrendering is not allowed anymore
  bird$no.actions[player.no] = FALSE
  
  ## Define local variables
  hand = bird$player.hand[[player.no]]
  bet = bird$player.bet[player.no]
  
  if (action == "split") {
    ## Hand 1
    bird$player.hand[[player.no]] = c(hand[1], draw.card(bird))
    player.turn(bird, player.no, policy)
    hand1.result = bird$player.results[[player.no]]
    ## Hand 2
    bird$player.hand[[player.no]] = c(hand[1], draw.card(bird))
    player.turn(bird, player.no, policy)
    ## Combine Results
    bird$player.results[[player.no]] = c(bird$player.results[[player.no]], hand1.result)
  } else if (action == "double down") {
    ## draw card
    bird$player.hand[[player.no]] = c(hand, draw.card(bird))
    ## calc sum
    sum = ceil.sum(bird$player.hand[[player.no]])
    ## store result
    bird$player.results[[player.no]] = c(sum, 2*bet)
  } else if (action == "surrender") {
    ## store loss code and bet size
    bird$player.results[[player.no]] = c(67, 0.5 * bet)
  } else if (action == "hit") {
    bird$player.hand[[player.no]] = c(hand, draw.card(bird))
    sum = ceil.sum(bird$player.hand[[player.no]])
    if (sum > 21) {
      ## Bust
      bird$player.results[[player.no]] = c(sum, bet)
    } else {
      ## Proceed to next action
      player.turn(bird, player.no, policy)
    }
  } else if (action == "stand") {
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
        bird$loss.count[[player.no]] = 0
      } else if (result[i] > 21) {
        ## Player losses -- total > 21
        bird$player.chips[[player.no]] = bird$player.chips[[player.no]] - result[i+1]
        bird$loss.count[[player.no]] = bird$loss.count[[player.no]] + 1
      } else if (dealer.sum > 21) {
        ## Player win -- dealer total > 21
        bird$player.chips[[player.no]] = bird$player.chips[[player.no]] + result[i+1]
        bird$loss.count[[player.no]] = 0
      } else if (result[i] < dealer.sum) {
        ## Player losses -- dealer > player
        bird$player.chips[[player.no]] = bird$player.chips[[player.no]] - result[i+1]
        bird$loss.count[[player.no]] = bird$loss.count[[player.no]] + 1
      } else if (result[i] > dealer.sum) {
        ## Player wins -- total > dealer.sum
        bird$player.chips[[player.no]] = bird$player.chips[[player.no]] + result[i+1]
        bird$loss.count[[player.no]] = 0
      } else {
        ## Push -- dealer & player tie. No change in chips
      }
      ## Set bet to zero
      bird$player.bet[player.no] = 0
    }
  }
}







