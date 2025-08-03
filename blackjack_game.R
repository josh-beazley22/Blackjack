
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
    insurance.paid = NULL,
    no.actions = NULL,
    
    card.names = c('A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K'),
    card.vals  = c( 1 ,  2 ,  3 ,  4 ,  5 ,  6 ,  7 ,  8 ,  9 ,  10 , 10 , 10 , 10),
    
    initialize = function(chips = NULL, num.players = NULL, deck.size = NULL) {
      if (!is.null(chips)) self$chips <- chips
      if (!is.null(num.players)) {
        self$num.players <- num.players
        self$player.chips <- rep(self$player.chips, self$num.players)
      }
      if (!is.null(deck.size)) self$deck.size <- deck.size
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
  indices <- match(cards, card.names)
  if (any(is.na(indices))) {
    stop("Invalid card(s) detected")
  }
  sum(card.vals[indices])
}

ceil.sum <- function(cards) {
  ## Treats Ace as 11 when the total is less than 21.
  card.names = c('A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K')
  card.vals  = c( 1 ,  2 ,  3 ,  4 ,  5 ,  6 ,  7 ,  8 ,  9 ,  10 , 10 , 10 , 10)
  indices <- match(cards, card.names)
  if (any(is.na(indices))) {
    stop("Invalid card(s) detected")
  }
  total = sum(card.vals[indices])
  if (any(cards == 'A') && total <= 11) {
    total = total + 10
  }
  total
}

draw.probs <- function(bird) {
  ## Calculates prob. of drawing each card individually.
  deck = rep(4*bird$deck.size, 13)
  names(deck) = bird$card.names
  deck = deck - bird$seen.cards
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
  ## Special method.
  ## Compute EV of: win insurance bet + improvement in EV by losing insurance
  
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
  ## TODO: calculate EV from legal moves
  ## Return random action from legal.moves
  move = (sample(bird$legal.moves, 1))
  print(move)
  return(move)
}

player.turn <- function(bird, player.no) {
  if (is.null(player.no)) stop("player.no is NULL")
  
  ## Reset player results
  bird$player.results[[player.no]] = c()
  ## Player has taken an action, so surrendering is not allowed
  bird$no.actions[player.no] = FALSE
  ## Find best move from policy
  move = policy(bird, player.no)
  ## Define local variables
  hand = bird$player.hand[[player.no]]
  bet = bird$player.bet[[player.no]]
  
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
  ## Dealer draws until hand total is 17+
  while(ceil.sum(bird$dealer.hand) < 17) {
    bird$dealer.hand = c(bird$dealer.hand, draw.card(bird))
  }
  dealer.sum = ceil.sum(bird$dealer.hand)
  ## Payout all players
  for (player.no in 1:bird$num.players) {
    result = bird$player.results[[player.no]]
    for (i in seq(1, length(result), by=2)) {
      if (result[i] > 21 || result[i] < dealer.sum) {
        ## Player losses -- total > 21 or total < dealer
        bird$player.chips[[player.no]] = bird$player.chips[[player.no]] - result[i+1]
      } else if (result[i] > dealer.sum) {
        ## Player wins -- total > dealer
        bird$player.chips[[player.no]] = bird$player.chips[[player.no]] + result[i+1]
      } else {
        ## Push -- dealer & player tie. No change in chips
      }
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
    if (length(bird$dealer.hand) == 1 && bird$dealer.hand[1] == 'A') {
      insurance.policy(bird)
    }
    ## Each player takes a turn in order
    for (player.no in 1:bird$num.players) {
      player.turn(bird, player.no)
    }
    ## Dealer draws and pays winners
    dealer.turn(bird)
  }
}





