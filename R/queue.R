R6_queue <- R6::R6Class(
  "R6_queue",
  public = list(
    nodes = NULL,
    initialize = function(nodes){
      self$nodes <- nodes
    },
    push = function(x){
      self$nodes <- c(self$nodes, x)
    },
    pop = function(n = 1){
      n <- min(length(self$nodes), n)
      index <- seq_len(n)
      out <- self$nodes[index]
      self$nodes <- self$nodes[-index]
      out
    },
    n = function(){
      length(self$nodes)
    }
  )
)
