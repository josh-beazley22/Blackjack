

## Stand EV is the same even if player holds an Ace
stand.EV <- function(penguin) {
  
  col.names = as.numeric(colnames(dealer.library))
  dealer.sim <- dealer.library[penguin@dealer.face, ]
  
  ## When dealer shows Ace, the hole card is checked when insurance is payed out.
  if (penguin@dealer.face == 'A') {
    dealer.sim['21'] = 0
    dealer.sim = dealer.sim / sum(dealer.sim)
  }
  ## Vector of possible player hands
  player.sum = floor.sum(penguin@player.hand)
  if (penguin@ace & player.sum <= 11) {
    player.sum = player.sum + 10
  }
  ## Compute EV
  win.prob <- sum(dealer.sim[, col.names < player.sum])
  loss.prob <- sum(dealer.sim[, col.names > player.sum])
  win.prob - loss.prob
}

hit.EV <- function(penguin) {
  
  dealer.sim <- dealer.library[dealer.face, ]
  
  ## Iterate for every card player could draw
  for (i in 1:13) {
    kiwi = new("board.state", 
               player.hand = c(penguin@player.hand, card.names[i]),
               ace = (penguin@ace && card.names[i] == 'A'),
               dealer.face = penguin@dealer.face,
               deck.size = penguin@deck.size, 
               seen.cards = c(penguin@seen.cards, card.names[i]))
    ## TODO: look up EV of kiwi.
    ## summation: prob(card) * EV
  }
}
  
hit.EV.legacy <- function(penguin) {
  
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
  all.hit.EV[player.total]
}

double.down.EV <- function(penguin) {
  ## Assumes that I hit once then stand.
  ## Useful for computing EV in double down scenarios
  
  house = dealer.library[dealer.face, ]
  
  if (penguin@ace) {
    ## Logic for player holding an ace
    hands <- floor.sum(penguin@player.hand) + card.vals
    hands[hands <= 11] <- hands[hands <= 11] + 10
  } else {
    hands <- floor.sum(penguin@player.hand) + card.vals
    if (floor.sum(penguin@player.hand) < 11) {
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
  
  2*(win.EV - loss.EV)
}

surrender.EV <- function(penguin) {
  ## Player always loses half pot on a surrender
  -0.5
}

compute.EV <- function(penguin) {
  
  EV <- c(hit.EV(player.total, dealer.face),
          double.down.EV(penguin),
          stand.EV(penguin),
          surrender.EV(penguin))
  
  names(EV) <- c("hit", "double down", "stand", "surrender")
  
  cat(
    sprintf("EV hit = %.2f\n", EV["hit"]),
    sprintf("EV double down = %.2f\n", EV["double down"]),
    sprintf("EV stand = %.2f\n", EV["stand"]),
    sprintf("EV surrender = %.2f\n", EV["surrender"]),
    sprintf("Recommendation = %s\n", names(EV)[which.max(EV)])
  )
}


  

