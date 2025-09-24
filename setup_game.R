

## Setup a gamestate then find the optimal player action

bird <- Blackjack$new(
  chips = 2000,
  deck.size = 8,
  num.players = 3,
  hit.on.soft.17 = TRUE
)


# Example usage
policy <- PolicyList$new()
policy$add(optimal.oscar)
policy$add(random.reddington)
policy$add(stale.dale)



game.loop <- function(bird, policy) {
  ## Perform one complete Blackjack hand using the given gamestate and policy
  game.reset(bird)
  bet.policy(bird, policy)
  ins.decision = insurance.policy(bird, policy)
  insurance.action(bird, ins.decision)
  if (bird$insurance.paid == 22) {
    # Dealer drew blackjack. Reset.
    return()
  }
  for (player.no in 1:bird$num.players) {
    policy.name = policy$get(player.no)$player.policy
    player.turn(bird, player.no, policy.name)
  }
  dealer.turn(bird)
}

all.of.the.tests() {
  
  episode.length = 40
  trials = 50
  sto = matrix(nrow=0, ncol=trials)
  colnames(sto) = 1:trials
  
  num.players.list = 1:8
  deck.size.list = seq(2, 8, by=2)
  
  # Test table sizes 1-8
  for (num.players in num.players.list) {
    for (deck.size in deck.size.list) {
      
      data = numeric(trials)
      for (trial in 1:trials) {
        
        ## setup game and policy
        bird <- Blackjack$new(num.players = num.players, chips = 0, deck.size = 2)
        policy <- PolicyList$new()
        policy$add(optimal.oscar)
        
        i = 1
        while (num.players > i) {
          policy$add(random.reddington)
          i = i + 1
        }
      
        # play episode.length number of Blackjack hands
        for (epoch in 1:episode.length) {
          game.loop(bird, policy)
        }
        # record current chips of player 1 (optimal.oscar)
        data[trial] = bird$player.chips[1]
      }
      ## save data to multi-dimensional matrix
      new.row = matrix(data, nrow=1)
      rownames(new.row) = paste0("num.players=", num.players, ", deck.size=", deck.size)
      sto = rbind(sto, new.row)
      
      write.csv(sto, file = "test_numplayers_&_decksize.txt", row.names = FALSE)
    }
  }
  
}

## Define game & policy

policy <- PolicyList$new()
policy$add(optimal.oscar)
policy$add(stale.dale)


game.loop(bird, policy)




