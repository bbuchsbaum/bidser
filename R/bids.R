
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

read_example <- function(project) {
  projurl <- paste0("https://raw.githubusercontent.com/bids-standard/bids-examples/master/", project)
  part_df <- rio::import(paste0(projurl, "/participants.tsv"))
}



get_sessions <- function(path, sid) {
  dnames <- basename(fs::dir_ls(paste0(path, "/", sid)))
  ret <- str_detect(dnames, "ses-.*")
  if (any(ret)) {
    dnames[ret]
  } else {
    list()
  }
}

descend <- function(node, path, ftype, parser) {
  dnames <- basename(fs::dir_ls(paste0(path)))
  ret <- str_detect(dnames, ftype)
  node <- node$AddChild(ftype)
  if (any(ret)) {
    fnames <- basename(fs::dir_ls(paste0(path, "/", ftype)))

    for (fname in fnames) {
      mat <- parse(parser, fname)
      if (!is.null(mat)) {
        n <- node$AddChild(fname)
        for (key in names(mat$result)) {
          
          #if (length(mat$task[[key]]) > 1) {
          #  browser()
          #}
          if (length(mat$result[[key]]) > 0) {
            #message("key = ", key)
            #message("val = ", mat$result[[key]])
            n[[key]] <- mat$result[[key]]
          }
        }
      }
    }
  }
}


#' load a BIDS project
#' 
#' @param path the file path of the project
#' @importFrom data.tree Node
#' 
#' @examples 
#' 
#' p <- system.file("inst/extdata/ds001", package="bidser")
#' #path <- "~/code/bidser/inst/extdata/ds005"
bids_project <- function(path=".") {
  aparser <- anat_parser()
  fparser <- func_parser()

  path <- normalizePath(path)

  if (!file.exists(paste0(path, "/participants.tsv"))) {
    stop("participants.tsv is missing")
  }

  if (!file.exists(paste0(path, "/dataset_description.json"))) {
    stop("dataset_description.json is missing")
  }

  part_df <- read.table(paste0(path, "/participants.tsv"), header=TRUE, stringsAsFactors=FALSE)
  desc <- jsonlite::read_json(paste0(path, "/dataset_description.json"))

  project_name <- basename(path)
  bids <- Node$new(project_name)

  sdirs <- as.character(part_df$participant_id)
  
  if (!all(str_detect("^sub-", sdirs))) {
    ind <- which(!str_detect("^sub-", sdirs))
    sdirs[ind] <- paste0("sub-", sdirs[ind])
  }
  
 
  has_sessions <- FALSE

  for (sdir in sdirs) {
    if (file.exists(paste0(path, "/", sdir))) {
      node <- bids$AddChild(sdir)
      node$subid <- sdir
    } else {
      next
    }

    sessions <- get_sessions(path, sdir)

    if (length(sessions) > 0) {
      has_sessions <- TRUE
      for (sess in sessions) {
        snode <- node$AddChild(sess)
        snode$session <- gsub("ses-", "", sess)
        descend(snode, paste0(path, "/", sdir, "/", sess), "anat", aparser)
        descend(snode, paste0(path, "/", sdir, "/", sess), "func", fparser)
      }
    } else {
      descend(node, paste0(path, "/", sdir), "anat", aparser)
      descend(node, paste0(path, "/", sdir), "func", fparser)
    }
  }



  tbl <- ToDataFrameTypeCol(bids, 'subid', 'session', 'task', 'type')

  ret <- list(bids_tree = bids,
              tbl = tbl,
              path=path,
              has_sessions=has_sessions)

  class(ret) <- "bids_project"
  ret
}


#' @export
sessions.bids_project <- function(x) {
  if (x$has_session) {
    unique(p$bids_tree$Get("session", filterFun = function(x) !is.null(x$session)))
  } else {
    NULL
  }
}

#' @export
tasks.bids_project <- function(x) {
  unique(x$bids_tree$Get("task", filterFun = function(x) !is.null(x$task)))
}


#' @export
participants.bids_project <- function (x, ...) {
  #ret <- x$bids_tree$Get("subid", filterFun = function(x) !is.null(x$subid))
  unique(x$tbl$subid)
}


#' @export
#' @examples 
#' 
#' p <- system.file("inst/extdata/ds001", package="bidser")
#' fs <- func_scans(bids_project(p), subid="sub-0[123]", run="0[123]")
func_scans.bids_project <- function (x, subid="^sub-.*", task=".*", run = ".*", modality="bold", ...) {
  ret <- x$bids_tree$Get("type", filterFun = function(z) {
    if (!is.null(z$type) && z$modality == modality && str_detect(z$name, subid)  && str_detect(z$name, task) && str_detect(z$run, run) && str_detect(z$suffix, ".nii(.gz)?$")) {
      browser()
      TRUE
    } else {
      FALSE
    }
  })
  
  ret <- names(ret)
  paths <- sapply(stringr::str_split(ret, "_"), function(sp) {
    paste0(sp[[1]], "/", sp[[2]], "/func")
  })
  paste0(paths, "/", ret)
}



#' @export
event_files.bids_project <- function (x, subid="^sub-.*", task=".*", run="0[123]", ...) {
  ret <- x$bids_tree$Get("type", filterFun = function(z) {
    if (!is.null(z$type) && z$modality == "events" && str_detect(z$name, subid)  && str_detect(z$name, task)) {
      TRUE
    } else {
      FALSE
    }
  })

  ret <- names(ret)
  paths <- sapply(stringr::str_split(ret, "_"), function(sp) {
    paste0(sp[[1]], "/", sp[[2]], "/func")
  })
  paste0(paths, "/", ret)
}



