## Simulated hands of Blackjack
## Exact probabilities calculated for dealer result given face-up card

# Draw a card from a standard playing card deck with equal probability
draw.card <- function() {
  card.names <- c('A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K')
  draw <- runif(1, 1, 14)
  return(card.names[draw])
}

# Calc total of hand using standard Blackjack rules
# Input-- c() vector of chars
hand.sum <- function(hand) {
  values <- sapply(hand, function(card) {
    if (card %in% c('J', 'Q', 'K')) {
      return(10)
    } else if (card == 'A') {
      return(11)  # count Aces as 11 for now
    } else {
      return(as.numeric(card))
    }
  })
  
  total <- sum(values)
  num.aces <- sum(hand == 'A')
  
  # Downgrade Aces from 11 to 1 if total > 21
  while (total > 21 && num.aces > 0) {
    total <- total - 10  # reduce one Ace from 11 to 1
    num.aces <- num.aces - 1
  }
  
  return(total)
}


## Compute the distribution of dealer totals given a face card.
## Input-- char
## Return-- table of probs
dealer.sim <- function(M, face) {
  dealer.results <- numeric(M)
  
  ## 10,000 simulated hands
  for (i in 1:M) {
    hand <- c(face)
    total <- hand.sum(hand)
    
    # draw until 17, dealer rules
    while(total < 17) {
      hand <- c(hand, draw.card())
      total <- hand.sum(hand)
    }
    dealer.results[i] <- total
  }
  
  # Split results so dealer busts are grouped as one category
  tbl = table(dealer.results)
  tbl_main <- tbl[names(tbl) %in% as.character(17:21)]
  tbl_22plus <- sum(tbl[as.numeric(names(tbl)) >= 22])
  
  # Add 22+ bin
  tbl_cleaned <- c(tbl_main, `22+` = tbl_22plus)
  return(tbl_cleaned / M)
}

## Store simulation results for all dealer face cards
dealer.library <- function(M) {
  card.names <- c('A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K')
  library <- c()
  for (i in 1:13) {
    library <- c(library, dealer.sim(M, card.names[i]))
  }
  mat <- matrix(library, ncol = 6, byrow = TRUE)
  
  # Set row and column names
  colnames(mat) <- c('17', '18', '19', '20', '21', '0')
  rownames(mat) <- card.names[1:13]
  return(mat)
}
library = dealer.library(1e4)
write.csv(library, file = "dealer_blackjack_probs.csv", row.names = TRUE)


