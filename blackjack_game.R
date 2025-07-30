
library(R6)

Blackjack <- R6Class("Blackjack",
  public = list(
    ## Tracking hands
    player.hand = NULL, ## index-able container of c()
    dealer.hand = NULL, ## c()
    
    num.players = 1,
    chips = 200,
    bet = NULL,
    
    deck.size = 8,
    seen.cards =  setNames(rep(0, 13), c('A','2','3','4','5','6','7','8','9','10','J','Q','K')),
    legal.moves = NULL,
    insurance = NULL,
    no.actions = NULL,
    
    card.names = c('A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K'),
    card.vals  = c( 1 ,  2 ,  3 ,  4 ,  5 ,  6 ,  7 ,  8 ,  9 ,  10 , 10 , 10 , 10),
    
    initialize = function(chips = NULL, num.players = NULL, deck.size = NULL) {
      if (!is.null(chips)) self$chips <- chips
      if (!is.null(num.players)) self$num.players <- num.players
      if (!is.null(deck.size)) self$deck.size <- deck.size
    },
    
    print = function(...) {
      cat("Player hand:", paste(self$player.hand, collapse = ", "), "\n")
      cat("Dealer face card:", self$dealer.hand, "\n")
      cat("Bet:", self$bet, "\n")
      cat("Chips:", self$chips, "\n")
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

draw.probs <- function(crow) {
  ## Calculates prob. of drawing each card individually.
  deck = rep(4*crow$deck.size, 13)
  names(deck) = crow$card.names
  deck = deck - crow$seen.cards
  deck / sum(deck)
}

draw.card <- function(crow) {
  
  ## Calculates prob. of drawing each card individually.
  deck = rep(4*crow$deck.size, 13)
  names(deck) = crow$card.names
  deck = deck - crow$seen.cards
  deck / sum(deck)
  
  ## Draw card from deck
  card = sample(crow$card.names, size = 1, prob = deck)
  ## Update crow$seen.cards to contain this newly drawn card
  crow$seen.cards[card] = crow$seen.cards[card] + 1
  card
}

game.reset <- function(crow) {
  ## TODO: empty index-able c() container
  crow$player.hand = NULL
  crow$dealer.hand = c()
  crow$insurance = 0
  crow$no.actions = TRUE
  
  ## When half of the deck has been used, shuffle.
  if (sum(crow$seen.cards) > 56*crow$deck.size) {
    crow$seen.cards = setNames(rep(0, 13), crow$card.names)
  }
  
  ## Draw 1 card for each player
  for (i in 1:crow$num.players) {
    card = draw.card(crow)
    ## The first card is mine.
    if (i == 1) {
      crow$player.hand = c(card)
    }
  }
  ## Dealer gets a face-up card
  crow$dealer.hand = draw.card(crow)
  
  ## Each player gets 1 more card.
  for (i in 1:crow$num.players) {
    card = draw.card(crow)
    ## The first card is mine.
    if (i == 1) {
      crow$player.hand = c(crow$player.hand, card)
    }
  }
}

player.turn <- function(crow) {
  ## Return-- player.sum, bet
  
  move = policy(crow)
  print(move)
  crow$no.actions = FALSE
  if (move == "insurance") {
    crow$dealer.hand = c(crow$dealer.hand, draw.card(crow))
    if (crow$dealer.hand == 21) {
      crow$insurance = 1
      ## win code, insurance pays 2:1
      return(c(0, crow$bet))
    }
    crow$insurance = 0
    # lose code, insurance bet is half pot
    return(c(22, 0.5 * crow$bet))
  }
  else if (move == "split") {
    ## Hand 1
    crow$player.hand = c(crow$player.hand[1], draw.card(crow))
    hand1 = player.turn(crow)
    ## Hand 2
    crow$player.hand = c(crow$player.hand[1], draw.card(crow))
    hand2 = player.turn(crow)
    return(c(hand1, hand2))
  }
  else if (move == "double down") {
    crow$player.hand = c(crow$player.hand, draw.card(crow))
    sum = ceil.sum(crow$player.hand)
    return(c(sum, 2*crow$bet))
  }
  else if (move == "surrender") {
    return(c(22, 0.5 * crow$bet))
  }
  else if (move == "hit") {
    crow$player.hand = c(crow$player.hand, draw.card(crow))
    if (ceil.sum(crow$player.hand) > 21) {
      ## Bust
      return(c(ceil.sum(crow$player.hand), crow$bet))
    }
    ## Proceed to next action
    return(player.turn(crow))
  }
  else if (move == "stand") {
    return(c(ceil.sum(crow$player.hand), crow$bet))
  }
}

main <- function() {
  
  crow <- Blackjack$new()
  
  ## play 100 games
  for (game.no in 1:1) {
    
    crow$bet = 10 ## TODO dynamically change bet based on a strategy
    game.reset(crow)
    print(player.turn(crow))
    crow$print()
    if (crow$insurance == 1) {
      ## Insurance paid, dealer has 21.
    }
    else if (crow$insurance == -1) {
      ## Insurance paid, dealer does not have 21.
    }
    else {
      ## No info on dealer's hole card.
    }
  }
}

policy <- function(crow) {
  ## Let's make flags to control for legal moves.
  crow$legal.moves = c("hit", "stand")
  
  # 1. split only possible when player.hand has two identical cards
  if (length(crow$player.hand) == 2 && crow$player.hand[1] == crow$player.hand[2]) {
    crow$legal.moves = c(crow$legal.moves, "split")
  }
  # 2. double down only possible when player.hand has two cards
  if (length(crow$player.hand) == 2) {
    crow$legal.moves = c(crow$legal.moves, "double down")
  }
  # 3. surrender only possible immediately on dealt
  if (crow$no.actions) {
    crow$legal.moves = c(crow$legal.moves, "surrender")
  }
  # 4. insurance only possible before all other actions
  if (crow$no.actions && all(crow$dealer.hand == 'A')) {
    crow$legal.moves = c(crow$legal.moves, "insurance")
  }
  crow$legal.moves = c("split", "stand")
  ## Return random action from legal.moves
  return(sample(crow$legal.moves, 1))
}



