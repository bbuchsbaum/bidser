

read_example <- function(project) {
  projurl <- paste0("https://raw.githubusercontent.com/bids-standard/bids-examples/master/", project)
  part_df <- rio::import(paste0(projurl, "/participants.tsv"))

}

## match key-value
str_match_all("sub-1001_task-hello_run-01", "([A-Za-z0-9]+-[A-Za-z0-9]+)_*")

devtools::install_github("SWotherspoon/Combin8R")
library(Combin8R)
p=pSeq("bids_file",
    pSeq("subject", pLiteral("sub"), pLiteral("-"), pRegex("id", "[A-Za-z0-9]+")),
    pMany("has_session",
          pSeq("session", pLiteral("_"), pLiteral("ses"), pLiteral("-"), pRegex("id", "[A-Za-z0-9]+"))
    ))


bids_project <- function(projname, path=".") {
  part_df <- rio::import(paste0(path, "/participants.tsv"))
  dsetinfo <- rio::import(paste0(path, "/dataset_description.json"))

}

scan_subject <- function(path, id) {

}

list_files_github <- function(user, repo, subdir="") {
  gurl <- paste0("https://api.github.com/repos/", user, "/", repo, "/git/trees/master?recursive=1")
  req <- httr::GET(gurl)
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
  if (! subdir == "") {
    grep(paste0(subdir, "/"), filelist, value = TRUE, fixed = TRUE)
  } else {
    filelist
  }
}
