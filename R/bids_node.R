
bids_node <- function(name, ..., class=NULL) {
  structure(name, ..., class=c(class, "bids_node"))  
}

bids_folder <- function(name, ...) {
  bids_node(name, ..., class="bids_folder")
}

bids_file <-  function(name, ...) {
  bids_node(name, ..., class="bids_file")
}

match_attr.bids_node <- function(x, ...) {
  ll <- list(...)
  
  all(sapply(names(ll), function(key) {
    stringr::str_detect(attr(x, key), as.character(ll[[key]]))
  }))
}