

dealer.sim <- function(bird, M = 1e4) {
  ## Runs a simulation of M hands using card counting to get probs of 
  ## each dealer total.
  
  ## Store variables from bird object locally
  dealer.results <- numeric(M)
  dealer.hand <- bird$dealer.hand
  deck <- rep(4*bird$deck.size, 13) - bird$seen.cards
  hit.on.soft.17 <- bird$hit.on.soft.17
  
  for (epoch in 1:M) {
    curr.hand <- dealer.hand
    ## shuffle a deck then iterate an index for each drawn card
    shuffled.deck = sample(rep(names(deck), times = deck))
    i = 1
    while (dealer.must.hit(curr.hand, hit.on.soft.17)) {
      curr.hand = c(curr.hand, shuffled.deck[i])
      i = i + 1
    }
    dealer.results[epoch] = ceil.sum(curr.hand)
  }
  # Split results so dealer busts are grouped as one category
  tbl = table(dealer.results)
  tbl_main <- tbl[names(tbl) %in% as.character(17:21)]
  tbl_22plus <- sum(tbl[as.numeric(names(tbl)) >= 22])
  
  # Add 22+ bin
  tbl_cleaned <- c(tbl_main, `22+` = tbl_22plus)
  return(tbl_cleaned / M)
}

fast.sim <- function(bird, M = 1e4) {
  ## Runs a simulation of M hands using card counting to get probs of 
  ## each dealer total.
  
  ## Store variables from bird object locally
  dealer.results <- numeric(M)
  dealer.hand <- bird$dealer.hand
  deck <- rep(4*bird$deck.size, 13) - bird$seen.cards
  hit.on.soft.17 <- bird$hit.on.soft.17
  
  ## create M stacks of uniquely shuffled decks
  shuffled.matrix <- t(replicate(M, sample(rep(names(deck), times = deck))))
  ## set first column(s) to dealer.hand
  shuffled.matrix[, seq_along(dealer.hand)] <- rep(dealer.hand, each = nrow(shuffled.matrix))
  
  card.vals <- c(A = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6,
                 `7` = 7, `8` = 8, `9` = 9, `10` = 10, J = 10, Q = 10, K = 10)
  value.matrix <- matrix(card.vals[shuffled.matrix], nrow = M)
  
  # Track running sums and soft aces count
  row.sums <- rep(0, M)
  ace.hand <- rep(FALSE, M)
  finished <- rep(FALSE, M)
  
  for (col in 1:ncol(value.matrix)) {
    active <- !finished
    
    # Add current card values to active hands
    row.sums[active] <- row.sums[active] + value.matrix[active, col]
    ace.hand[shuffled.matrix[active, col] == "A"] <- TRUE
    # Count Ace as 11 when applicable
    soft.ace.hand = ace.hand & row.sums <= 11
    temp.sums = row.sums
    temp.sums[soft.ace.hand] = temp.sums[soft.ace.hand] + 10
    
    # Mark appropriate hands as finished
    if (hit.on.soft.17) {
      finished[temp.sums > 17] = TRUE
      finished[temp.sums == 17 & !soft.ace.hand] = TRUE
    } else {
      finished[temp.sums >= 17] = TRUE
    }
    if (all(finished)) break
  }
  ## convert all soft-ace hands
  row.sums[soft.ace.hand] = row.sums[soft.ace.hand] + 10
  # Split results so dealer busts are grouped as one category
  tbl = table(row.sums)
  tbl_main <- tbl[names(tbl) %in% as.character(17:21)]
  tbl_22plus <- sum(tbl[as.numeric(names(tbl)) >= 22])
  
  # Add 22+ bin
  tbl_cleaned <- c(tbl_main, `22+` = tbl_22plus)
  return(tbl_cleaned / M)
}

dealer.bins <- c("17"=0, "18"=0, "19"=0, "20"=0, "21"=0, "22+"=0)
simulate.dealer(bird, bird$dealer.hand,rep(4*bird$deck.size, 13) - bird$seen.cards, 1)
simulate.dealer <- function(bird, dealer.hand, deck, prob) {
  # Recursive function to simulate dealer outcomes
  total <- ceil.sum(dealer.hand)
  
  if (total > 21) {
    dealer.bins["22+"] <<- dealer.bins["22+"] + prob
    return()
  }
  
  else if (!dealer.must.hit(dealer.hand, bird$hit.on.soft.17)) {
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
      simulate.dealer(bird, new.hand, new.deck, p)
    }
  }
}







