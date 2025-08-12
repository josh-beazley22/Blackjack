

simulate.dealer <- function(bird) {
  ## Initialize
  dealer.bins <<- c("17"=0, "18"=0, "19"=0, "20"=0, "21"=0, "22+"=0)
  cards.left = rep(4*bird$deck.size, 13) - bird$seen.cards
  
  ## Collapse 10, J, Q, K into one bin.
  cards.left['10'] = cards.left['J'] + cards.left['Q'] + cards.left['K']
  cards.left = cards.left[1:10]
  
  ## Run simulation (actually an exact computation)
  simulate.dealer.helper(
    bird$dealer.hand,
    cards.left,
    prob = 1,
    soft.17 = bird$hit.on.soft.17,
    insurance = bird$insurance.paid
  )
  ## Must re-weight dealer.bins when insurance was paid
  if (bird$insurance.paid == 1) {
    dealer.bins = dealer.bins / sum(dealer.bins)
  }
  ## Sanity check
  if (abs(sum(dealer.bins) - 1) > 1e-8) {
    stop("Dealer simulation probabilities do not sum to 1. Invalid simulation.")
  }
  dealer.bins
}

simulate.dealer.helper <- function(dealer.hand, deck, prob, soft.17, insurance) {
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
    if (insurance == 1 && card == '10') {
      ## Insurance requires that dealer's face-down card must not be 10-valued.
      next
    }
    if (deck[card] > 0) {
      ## card is draw from the deck with probability p.
      new.deck <- deck
      new.deck[card] <- new.deck[card] - 1
      new.hand <- c(dealer.hand, card)
      p <- prob * (deck[card] / total.cards)
      simulate.dealer.helper(new.hand, new.deck, p, soft.17, insurance = 0)
    }
  }
}

double.down.EV <- function(bird, dealer.sim) {
  ## Follows player policy of hitting once then stand.
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

compute.EV <- function(bird) {
  ## Optimal hit vs. stand algorithm
  
  dealer.simulation <- simulate.dealer(bird)
  dealer.bust = dealer.simulation['22+']
  dealer.sim = dealer.simulation[1:5]
  draw.probs = draw.probabilities(bird)
  
  ## Compute EV for standing at every possible total from 1 to 21
  stand.EV = numeric(21)
  for (i in 1:21) {
    win.prob = dealer.bust + sum(dealer.sim[as.numeric(names(dealer.sim)) < i])
    lose.prob = sum(dealer.sim[as.numeric(names(dealer.sim)) > i])
    stand.EV[i] = win.prob - lose.prob
  }
  
  ## Compute hit EV in scenarios where Ace is always 1. Player total >= 11
  hit.EV = numeric(21)
  hit.EV[21] = -1
  for (i in 20:11) {
    curr.EV = 0
    for (card in bird$card.names[1:10]) {
      ## Iterate through each card.
      total = i + bird$card.vals[card]
      if (total > 21) {
        ## Bust
        curr.EV = curr.EV - draw.probs[card]
        next
      }
      ## Must decide to hit or stand at this total
      best.choice.EV = max(c(hit.EV[total], stand.EV[total]))
      curr.EV = curr.EV + best.choice.EV * draw.probs[card]
    }
    hit.EV[i] = curr.EV
  }
  
  ## Handling Aces. Cases where player total < 11
  ## EV is different when player is holding an Ace that can be either 11 or 1.
  sto.ace = numeric(21)
  sto.ace[11:21] = hit.EV[11:21]
  sto.no.ace = numeric(21)
  sto.no.ace[11:21] = hit.EV[11:21]
  
  for (i in 10:1) {
    
    ceil.total = c(i+11, (i+2) : (i+10))
    floor.total = (i+1) : (i+10)
    ## look up EV of hitting when current hand has NO ACE.
    hit.EV.no.ace = pmax(
      sto.no.ace[ceil.total[1:10]],
      c(sto.ace[floor.total[1]], sto.no.ace[floor.total[2:10]])
    )
    ## look up EV of standing when current hand has NO ACE.
    stand.EV.no.ace = pmax(
      stand.EV[ceil.total[1:10]],
      stand.EV[floor.total[1:10]]
    )
    ## Compute final EV
    sto.no.ace[i] = sum(draw.probs * pmax(
      stand.EV.no.ace,
      hit.EV.no.ace
    ))
    
    ceil.total = (i+11) : (i+20)
    ceil.total[ceil.total > 21] = ceil.total[ceil.total > 21] - 10
    floor.total = (i+1) : (i+10)
    ## look up EV of hitting when current hand has ACE.
    hit.EV.ace = pmax(
      sto.ace[ceil.total[1:10]],
      sto.ace[floor.total[1:10]]
    ) 
    ## look up EV of standing when current hand has ACE.
    stand.EV.ace = pmax(
      stand.EV[ceil.total[1:10]],
      stand.EV[floor.total[1:10]]
    )
    sto.ace[i] = sum(draw.probs * pmax(
      stand.EV.ace,
      hit.EV.ace
    ))
  }
  return(list(sto.ace, sto.no.ace, stand.EV))
}


policy <- function(bird, player.no) {
  ## Let's make flags to control for legal moves.
  legal.moves = c("hit", "stand")
  hand <- bird$player.hand[[player.no]]
  
  # 1. split only possible when player.hand has two identical cards
  if (length(hand) == 2 && hand[1] == hand[2]) {
    legal.moves = c(legal.moves, "split")
  }
  # 2. double down only possible when player.hand has two cards
  if (length(hand) == 2) {
    legal.moves = c(legal.moves, "double down")
  }
  # 3. surrender only possible immediately on dealt
  if (bird$no.actions[[player.no]]) {
    legal.moves = c(legal.moves, "surrender")
  }
  ## TODO: for simulations, handle all players at once using one call of compute.EV
  ## Build list of EV for each action, then select action with highest EV
  optimal.action = list()
  penguin = bird$clone(deep = TRUE)
  hit.or.stand = compute.EV(bird, player.no)
  optimal.action = c(optimal.action, list(hit.or.stand))
  
  if (any(legal.moves == "split")) {
    penguin$player.hand = penguin$player.hand[1]
    split = compute.EV(penguin)
    split[2] = 2*split[2]  # split doubles bet
  }
  if (any(legal.moves == "double down")) {
    double.down = double.down.EV(penguin, player.no)
    double.down[2] = 2*double.down[2]  # double down doubles bet
  }
  if (any(legal.moves == "surrender")) {
    
  }
}

insurance.policy <- function(bird) {
  
  # DON'T CALL draw.card()-- instead generate random number & assign insurance.paid code
  
  ## EV(insurance) = 
  ## + 0 * P(10-valued card)
  ## - bet/2 * P(other card)
  ## + bet * EV(insurance == 1)
  ## - bet * EV(insurance == 0)
  
  ## EV(no insurance) =
  ## - bet * P(10-valued card)
  ## + bet * P(other card) * EV(insurance == 1)
  ## == EV(insurance == 0)
  
  ## If a player chooses to call insurance, use insurance code
  # bird$player.results[[player.no]] = c(77, 0.5 * bird$player.bet[[player.no]])
  
  ## Call functions for applicable EV and probabilities
  draw.probs = draw.probabilities(bird)
  bird$insurance.paid = 1
  yes.insurance = compute.EV(bird)
  bird$insurance.paid = 0
  no.insurance = compute.EV(bird)
  
  hand = bird$player.hand[[1]]
  foo = -draw.probs[[10]] + sum(draw.probs[1:9]) * lookup.EV(yes.insurance, hand)[[2]]
  
  insurance.EV = numeric(bird$num.players)
  for (p in 1:bird$num.players) {
    ## Compute insurance EV for each player
    hand = bird$player.hand[[p]]
    insurance.EV[p] = -0.5 * sum(draw.probs[1:9]) + 
      lookup.EV(yes.insurance, hand)[[2]] - lookup.EV(no.insurance, hand)[[2]]
  }
  insurance.EV
  ## TODO: there is no buy insurance option in player.turn()
  ## TODO: does only 1 player need to buy insurance
}

lookup.EV <- function(EV, hand) {
  ## Finds the EV from the table for the specific hand.
  ## EV -- list(hit.ace, hit.no.ace, stand)
  
  if (any(hand == 'A')) {
    result = max(
      EV[[1]][floor.sum(hand)],
      EV[[3]][ceil.sum(hand)]
    )
    action = ifelse(
      EV[[1]][floor.sum(hand)] < EV[[3]][ceil.sum(hand)],
      "stand",
      "hit"
    )
    return(list(action, result))
    
  } else {
    result = max(
      EV[[2]][floor.sum(hand)],
      EV[[3]][ceil.sum(hand)]
    )
    action = ifelse(
      EV[[2]][floor.sum(hand)] < EV[[3]][ceil.sum(hand)],
      "stand",
      "hit"
    )
    return(list(action, result))
  }
}




