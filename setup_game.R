

## Setup a gamestate then find optimal the optimal player action

bird <- Blackjack$new(
  chips = 2000,
  deck.size = 8,
  num.players = 1,
  hit.on.soft.17 = TRUE
)

bird$dealer.hand = "3"
bird$player.hand = list(c("J", "4"))
if (bird$dealer.hand[1] == 'A') {
  EV = insurance.policy(bird)
  cat("Buy Insurance:", EV > 0, "\n")
  cat("Insurance EV =", EV, "\n")
}
action = optimal.policy(bird, 1) 
cat("Best action =", action, "\n")
