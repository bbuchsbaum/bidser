bids_file <- function(x) {
  class(x) <- c("bids_file", "bids_node")
  x
}

bids_node <- function(node) {
  class(node) <- c("bids_node")
  node
}




