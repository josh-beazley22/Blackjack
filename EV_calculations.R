

simulate.dealer <- function(bird) {
  ## Initialize
  dealer.bins <<- c("17"=0, "18"=0, "19"=0, "20"=0, "21"=0, "22+"=0)
  cards.left = rep(4*bird$deck.size, 13) - bird$seen.cards
  
  ## Collapse 10, J, Q, K into one bin.
  cards.left['10'] = cards.left['J'] + cards.left['Q'] + cards.left['K']
  cards.left = cards.left[1:10]
  
  ## If insurance has been lost, then dealer cannot hold a 10-value card
  if (bird$insurance.paid == 1) {
    cards.left = cards.left[1:9]
  }
  
  ## Run simulation (actually an exact computation)
  simulate.dealer.helper(
    bird$dealer.hand,
    cards.left,
    prob = 1,
    soft.17 = bird$hit.on.soft.17
  )
  ## Sanity check
  if (abs(sum(dealer.bins) - 1) > 1e-8) {
    stop("Dealer simulation probabilities do not sum to 1. Invalid simulation.")
  }
  dealer.bins
}

simulate.dealer.helper <- function(dealer.hand, deck, prob, soft.17) {
  total <- ceil.sum(dealer.hand)
  
  # Dealer busts
  if (total > 21) {
    dealer.bins["22+"] <<- dealer.bins["22+"] + prob
    return()
  }
  # Dealer stands
  else if (!dealer.must.hit(dealer.hand, soft.17)) {
    dealer.bins[as.character(total)] <<- dealer.bins[as.character(total)] + prob
    return()
  }
  # Dealer hits
  total.cards <- sum(deck)
  for (card in names(deck)) {
    if (deck[card] > 0) {
      new.deck <- deck
      new.deck[card] <- new.deck[card] - 1
      new.hand <- c(dealer.hand, card)
      p <- prob * (deck[card] / total.cards)
      simulate.dealer.helper(new.hand, new.deck, p, soft.17)
    }
  }
}

compute.EV <- function(bird) {
  
  dealer.sim <- simulate.dealer(bird)
  dealer.bust = dealer.sim['22+']
  dealer.sim = dealer.sim[1:5]
  
  ## Compute sum for each player
  player.sum = c()
  for (hand in bird$player.hand) {
    player.sum = c(player.sum, ceil.sum(hand))
  }
  
  ## Compute stand EV
  stand.EV = numeric(bird$num.players)
  for (i in 1:bird$num.players) {
    if (player.sum[i] > 21) {
      stand.EV[i] = -1
      next
    }
    win.prob = dealer.bust + sum(dealer.sim[as.numeric(names(dealer.sim)) < player.sum[i]])
    lose.prob = sum(dealer.sim[as.numeric(names(dealer.sim)) > player.sum[i]])
    stand.EV[i] = win.prob - lose.prob
  }
  
  ## Optimal hitting algorithm
}


double.down.EV <- function(bird, dealer.sim) {
  ## Assumes that I hit once then stand.
  ## Useful for computing EV in double down scenarios
  
  EV = numeric(bird$num.players)
  for (i in 1:bird$num.players) {
    
    draw.probs = draw.probabilities(bird)
    cards = Map(c, rep(bird$player.hand[i], 10), bird$card.names[1:10])
    card.sums = numeric(10)
    for (j in 1:10) {
      card.sums[j] = ceil.sum(cards[[j]])
    }

    ## Calculate probability of each hand total.
    probs = c(sum(draw.probs[card.sums < 17]), 
              sum(draw.probs[card.sums == 17]), 
              sum(draw.probs[card.sums == 18]),
              sum(draw.probs[card.sums == 19]), 
              sum(draw.probs[card.sums == 20]), 
              sum(draw.probs[card.sums == 21]), 
              sum(draw.probs[card.sums > 21]))
    
    ## Compare probability of player sum reaching a total to the dealer reaching that total
    win.EV = probs[1]*dealer.sim['22+'] +
      probs[2]*dealer.sim['22+'] +
      probs[3]*sum(dealer.sim[c('22+', '17')]) + 
      probs[4]*sum(dealer.sim[c('22+', '17', '18')]) +
      probs[5]*sum(dealer.sim[c('22+', '17', '18', '19')]) +
      probs[6]*sum(dealer.sim[c('22+', '17', '18', '19', '20')])
    
    loss.EV = probs[1]*sum(dealer.sim[c('17', '18', '19', '20', '21')]) + 
      probs[2]*sum(dealer.sim[c('18', '19', '20', '21')]) + 
      probs[3]*sum(dealer.sim[c('19', '20', '21')]) + 
      probs[4]*sum(dealer.sim[c('20', '21')]) +
      probs[5]*sum(dealer.sim[c('21')]) +
      probs[6]*0 +
      probs[7]*1
    
    EV[i] = 2*(win.EV - loss.EV)
  }
}




