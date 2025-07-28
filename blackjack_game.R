
library(R6)

BoardState <- R6Class("BoardState",
  public = list(
    player.hand = NULL,
    dealer.face = NULL,
    bet = NULL,
    chips = NULL,
    num.players = NULL,
    deck.size = NULL,
    seen.cards = NULL,
    card.names = c('A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K'),
    card.vals  = c( 1 ,  2 ,  3 ,  4 ,  5 ,  6 ,  7 ,  8 ,  9 ,  10 , 10 , 10 , 10),
    moves = c('hit', 'stand', 'split', 'double down', 'surrender'),
    
    initialize = function(player.hand = character(),
                          dealer.face = "", bet = 0, chips = 0, num.players = 1,
                          deck.size = 52, seen.cards = NULL) {
      
      self$player.hand <- player.hand
      self$dealer.face <- dealer.face
      self$bet <- bet
      self$chips <- chips
      self$num.players <- num.players
      self$deck.size <- deck.size
      
      if (is.null(seen.cards)) {
        self$seen.cards <- setNames(rep(0, 13), c('A','2','3','4','5','6','7','8','9','10','J','Q','K'))
      } else {
        self$seen.cards <- seen.cards
      }
      
      # Validity check (can raise error early)
      private$validate()
    },
    
    print = function(...) {
      cat("Player hand:", paste(self$player.hand, collapse = ", "), "\n")
      cat("Dealer face card:", self$dealer.face, "\n")
      cat("Bet:", self$bet, "\n")
      cat("Chips:", self$chips, "\n")
      cat("Number of Players:", self$num.players, "\n")
      cat("Deck size:", self$deck.size, "\n")
      cat("Seen cards:\n")
      print(self$seen.cards)
    }
  ),
  
  private = list(
    validate = function() {
      if (any(!nzchar(self$player.hand))) {
        stop("All cards in 'player.hand' must be non-empty strings.")
      }
      if (!is.null(self$seen.cards)) {
        if (any(!names(self$seen.cards) %in% c('A','2','3','4','5','6','7','8','9','10','J','Q','K'))) {
          stop("Invalid card names in 'seen.cards'.")
        }
      }
    }
  )
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

gen.card.draw.probs <- function(bird) {
  ## Calculates prob. of drawing each card individually.
  deck = rep(4*bird$deck.size, 13)
  names(deck) = bird$card.names
  deck = deck - bird$seen.cards
  deck / sum(deck)
}

draw.card <- function(bird) {
  
  ## Create probability distribution based on seen cards
  deck = gen.card.draw.probs(bird)
  ## Draw card from deck
  card = sample(bird$card.names, size = 1, prob = deck)
  ## Update bird$seen.cards to contain this newly drawn card
  bird$seen.cards[card] = bird$seen.cards[card] + 1
  card
}

game.reset <- function(bird) {
  bird$insurance = 0
  bird$no.actions = TRUE
  
  ## When half of the deck has been used, shuffle.
  if (sum(bird$seen.cards) > 56*bird$deck.size) {
    bird$seen.cards = setNames(rep(0, 13), bird$card.names)
  }
  
  ## Draw 1 card for each player
  for (i in 1:bird$num.players) {
    card = draw.card(bird)
    ## The first card is mine.
    if (i == 1) {
      bird$player.hand = c(card)
    }
  }
  ## Dealer gets a face-up card
  bird$dealer.face = draw.card(bird)
  
  ## Each player gets 1 more card.
  for (i in 1:bird$num.players) {
    card = draw.card(bird)
    ## The first card is mine.
    if (i == 1) {
      bird$player.hand = c(bird$player.hand, card)
    }
  }
}

player.turn <- function(bird) {
  ## Return-- player.sum, bet
  
  move = policy(bird)
  bird$no.actions = FALSE
  if (move == "insurance") {
    bird$dealer.face = c(bird$dealer.face, draw.card(bird))
    if (bird$dealer.face == 21) {
      bird$insurance = 1
      ## win code, insurance pays 2:1
      return(0, bird$bet)
    }
    bird$insurance = 0
    # lose code, insurance bet is half pot
    return(22, 0.5 * bird$bet)
  }
  else if (move == "split") {
    ## Hand 1
    bird$player.hand = c(bird$player.hand[1], draw.card(bird))
    hand1 = player.turn(bird)
    ## Hand 2
    bird$player.hand = c(bird$player.hand[1], draw.card(bird))
    hand2 = player.turn(bird)
    return(c(hand1, hand2))
  }
  else if (move == "double down") {
    bird$player.hand = c(bird$player.hand, draw.card(bird))
    sum = ceil.sum(bird$player.hand)
    return(c(sum, 2*bird$bet))
  }
  else if (move == "surrender") {
    return(22, 0.5 * bird$bet)
  }
  else if (move == "hit") {
    bird$player.hand = c(bird$player.hand, draw.card(bird))
    if (ceil.sum(bird$player.hand) > 21) {
      ## Bust
      return(ceil.sum(bird$player.hand), bird$bet)
    }
    ## Proceed to next action
    return(player.turn(bird))
  }
  else if (move == "stand") {
    return(ceil.sum(bird$player.hand), bird$bet)
  }
}

main <- function() {
  
  penguin <- BoardState$new(deck.size = 8, chips = 200, num.player = 1)
  
  ## play 100 games
  for (game.no in 1:100) {
    
    penguin$bet = 10 ## TODO dynamically change bet based on a strategy
    game.reset(penguin)
    player.turn(penguin)
    if (penguin$insurance == 1) {
      ## Insurance paid, dealer has 21.
    }
    else if (penguin$insurance == -1) {
      ## Insurance paid, dealer does not have 21.
    }
    else {
      ## No info on dealer's hole card.
    }
  }
}

policy <- function(bird) {
  ## Let's make flags to control for legal moves.
  bird$legal.moves = c("hit", "stand")
  
  # 1. split only possible when player.hand has two identical cards
  if (length(bird$player.hand) == 2 && bird$player.hand[1] == bird$player.hand[2]) {
    bird$legal.moves = c(bird$legal.moves, "split")
  }
  # 2. double down only possible when player.hand has two cards
  if (length(bird$player.hand) == 2) {
    bird$legal.moves = c(bird$legal.moves, "double down")
  }
  # 3. surrender only possible immediately on dealt
  if (bird$no.actions) {
    bird$legal.moves = c(bird$legal.moves, "surrender")
  }
  # 4. insurance only possible before all other actions
  if (bird$no.actions && all(bird$dealer.face == 'A')) {
    bird$legal.moves = c(bird$legal.moves, "insurance")
  }
}



