

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




sim.study <- function() {
  
  ## make list of player policies
  
  
  ## iterate main game loop using policies
  ## track game statistics
}