

setClass(
  "board.state",
  slots = c(player.hand = "character", ace = "logical",
      dealer.face = "character", bet = "numeric", 
      chips = "numeric", deck.size = "numeric", seen.cards = "character"),
  validity = function(object) {
    # Optional check: ensure 'player.hand' is all non-empty character strings
    if (any(!nzchar(object@player.hand))) {
      return("All cards must be non-empty character strings.")
    }
    # Optional check: ensure 'seen.cards' is all non-empty character strings
    if (any(!nzchar(object@seen.cards))) {
      return("All cards must be non-empty character strings.")
    }
    TRUE
  }
)
dealer.library <- read.csv("dealer_blackjack_probs.csv", row.names=1, 
                           check.names = F)

p1 <- new("board.state", player.hand = c("5", "A"), ace = TRUE,
            dealer.face = "Q")

card.names <- c('A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K')
card.vals  <- c( 1 ,  2 ,  3 ,  4 ,  5 ,  6 ,  7 ,  8 ,  9 ,  10 , 10 , 10 , 10)
card.probs <- rep(1/13, 13)
 
card.sum <- function(cards) {
  indices <- match(cards, card.names)   # get positions
  if (any(is.na(indices))) {
    stop("Invalid card(s) detected")
  }
  sum(card.vals[indices])               # sum the corresponding values
}

## Stand EV is the same even if player holds an Ace
stand.EV <- function(player.total, dealer.face) {
  
  col.names = as.numeric(colnames(dealer.library))
  dealer.results <- dealer.library[dealer.face, ]
  win.prob <- sum(dealer.results[, col.names < player.total])
  loss.prob <- sum(dealer.results[, col.names > player.total])
  return(win.prob - loss.prob)
}

hit.EV <- function(player.total, dealer.face) {
  
  dealer.results <- dealer.library[dealer.face, ]
  all.stand.EV <- numeric(21)
  for (i in 1:21) {
    all.stand.EV[i] <- stand.EV(i, dealer.face)
  }
  
  all.hit.EV <- numeric(21)
  all.hit.EV[21] = -1
  
  # compute hit EV for player totals 20 down to 1
  for (i in 20:1) {
    cumm.EV <- 0
    bust.prob <- 0
    
    for (j in seq_along(card.vals)) {
      card <- card.vals[j]
      prob <- card.probs[j]
      
      new.total <- i + card
      
      if (card == 1 && i < 11) {
        # Ace as 11
        ev1 <- max(all.stand.EV[i + 1], all.hit.EV[i + 1])
        ev2 <- max(all.stand.EV[i + 11], all.hit.EV[i + 11])
        best.ev <- max(ev1, ev2)
      } else if (new.total <= 21) {
        best.ev <- max(all.stand.EV[new.total], all.hit.EV[new.total])
      } else {
        bust.prob <- bust.prob + prob
        next
      }
      
      cumm.EV <- cumm.EV + prob * best.ev
    }
    all.hit.EV[i] <- cumm.EV - bust.prob
  }
  return(all.hit.EV[player.total])
}

double.down.EV <- function(p1) {
  ## Assumes that I hit once then stand.
  ## Useful for computing EV in double down scenarios
  
  house = dealer.library[dealer.face, ]
  
  if (p1@ace) {
    ## Logic for player holding an ace
    hands <- card.sum(p1@player.hand) + card.vals
    hands[hands <= 11] <- hands[hands <= 11] + 10
  } else {
    hands <- card.sum(p1@player.hand) + card.vals
    if (card.sum(p1@player.hand) < 11) {
      hands[1] <- hands[1] + 10
    }
  }
  ## Calculate probability of each hand total.
  probs = c(sum(hands <  17), 
            sum(hands == 17), 
            sum(hands == 18),
            sum(hands == 19), 
            sum(hands == 20), 
            sum(hands == 21), 
            sum(hands >  21)) / 13
  
  win.EV = probs[1]*house['0'] +
           probs[2]*house['0'] +
           probs[3]*sum(house[, c('0', '17')]) + 
           probs[4]*sum(house[, c('0', '17', '18')]) +
           probs[5]*sum(house[, c('0', '17', '18', '19')]) +
           probs[6]*sum(house[, c('0', '17', '18', '19', '20')])
  
  loss.EV = probs[1]*sum(house[, c('17', '18', '19', '20', '21')]) + 
           probs[2]*sum(house[, c('18', '19', '20', '21')]) + 
           probs[3]*sum(house[, c('19', '20', '21')]) + 
           probs[4]*sum(house[, c('20', '21')]) +
           probs[5]*sum(house[, c('21')]) +
           probs[6]*0 +
           probs[7]*1
  
  return(2*(win.EV - loss.EV))
}

surrender.EV <- function(player.total, dealer.face) {
  
  return(-.5)
}

compute.EV <- function(player.total, dealer.face) {
  
  EV <- c(hit.EV(player.total, dealer.face),
          double.down.EV(player.total, dealer.face),
          stand.EV(player.total, dealer.face),
          surrender.EV(player.total, dealer.face))
  
  names(EV) <- c("hit", "double down", "stand", "surrender")
  
  cat(
    sprintf("EV hit = %.2f\n", EV["hit"]),
    sprintf("EV double down = %.2f\n", EV["double down"]),
    sprintf("EV stand = %.2f\n", EV["stand"]),
    sprintf("EV surrender = %.2f\n", EV["surrender"]),
    sprintf("Recommendation = %s\n", names(EV)[which.max(EV)])
  )
}

## TODO: when the dealer is showing an ace, I know it's not a blackjack
## when I am deciding to split, double down, etc.
  

  

