

## Loadable policy module


library(R6)
Policy <- R6Class("Policy",
   public = list(
     
     bet.policy = "minimum",
     player.policy = "optimal", # random
     insurance.policy = "always", # never, optimal
   
   
   initialize = function(bet.strat = NULL, player.strat = NULL, insurance.strat = NULL) {
     if (!is.null(bet.strat)) self$bet.policy <- bet.strat
     if (!is.null(player.strat)) self$player.policy <- player.strat
     if (!is.null(insurance.strat)) self$insurance.policy <- insurance.strat
   },
   
   print = function(...) {
     cat("Bet Strategy:", self$bet.policy, "\n")
     cat("Player Strategy:", self$player.policy, "\n")
     cat("Insurance Strategy:", self$insurance.policy, "\n")
   }
   ),
)

## optimal policy according to my EV calculations
optimal.oscar <- Policy$new(bet.strat = "minimum", player.strat = "optimal", insurance.strat = "optimal")

## random player policy which never buys insurance -- therefore decision  
## lies purely with intellectual player(s)
random.reddington <- Policy$new(bet.strat = "minimum", player.strat = "random", insurance.strat = "never")


PolicyList <- R6Class("PolicyList",
  public = list(
    policies = list(),
    
    add = function(policy) {
      self$policies <- c(self$policies, list(policy))
    },
    
    get = function(i) {
      self$policies[[i]]
    },
    
    # Pull all bet policies
    get_bet_policies = function() {
      sapply(self$policies, function(p) p$bet.policy)
    },
    
    # Pull all player policies
    get_player_policies = function() {
      sapply(self$policies, function(p) p$player.policy)
    },
    
    # Pull all insurance policies
    get_insurance_policies = function() {
      sapply(self$policies, function(p) p$insurance.policy)
    },
    
    print = function(...) {
      for (i in seq_along(self$policies)) {
        cat("Policy", i, ":\n")
        self$policies[[i]]$print()
        cat("\n")
      }
    }
  )
)

# Example usage
policy <- PolicyList$new()
policy$add(optimal.oscar)
policy$add(random.reddington)




sim.study <- function() {
  
  ## make list of player policies
  
  
  ## iterate main game loop using policies
  ## track game statistics
}