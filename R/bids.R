
## TODO
## heatmap plot
## x axis is subject
## y axis is task by run
## squares colored by file-size
## files could be confounds, scans, preproc, etc.

#' @export
encode.character <- function(fname) {
  p <- bids_parser()
  ret <- parse(p, fname)
  if (!is.null(ret)) {
    v <- ret$result$value
    v[!sapply(v, is.null)]
  } else {
    NULL
  }
}

#' @keywords internal
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

#' @keywords internal
read_example <- function(project) {
  projurl <- paste0("https://raw.githubusercontent.com/bids-standard/bids-examples/master/", project)
  part_df <- rio::import(paste0(projurl, "/participants.tsv"))
}


#' @keywords internal
get_sessions <- function(path, sid) {
  dnames <- basename(fs::dir_ls(paste0(path, "/", sid)))
  ret <- str_detect(dnames, "ses-.*")
  if (any(ret)) {
    dnames[ret]
  } else {
    list()
  }
}


#' @keywords internal
descend <- function(node, path, ftype, parser) {
  dnames <- basename(fs::dir_ls(paste0(path)))
  ret <- str_detect(dnames, ftype)
  
  node <- add_node(node, ftype, folder=ftype)
  #node <- node$AddChild(ftype)
  #node$folder=ftype
  
  if (any(ret)) {
    fnames <- basename(fs::dir_ls(paste0(path, "/", ftype)))

    for (fname in fnames) {
      mat <- parse(parser, fname)
      if (!is.null(mat)) {
        keep <- sapply(mat$result, function(x) !is.null(x) && length(x) > 0)
        res <- mat$result[keep]
    
        pf <- purrr::partial(Node$new, fname)
        n <- Node$new(fname)
        n <- do.call(pf, res)
        
        node$AddChildNode(n)
      }
    }
  }
}



add_node <- function(bids, name, ...) {
  bids$AddChild(name, ...)
}

add_file <- function(bids, name,...) {
  bids$AddChild(name, ...)
}

## TODO add ability to load one subject only
## TODO create a "bids_object" to represent files and folders?

#' Load a BIDS project at a file path
#' 
#' bids project
#' 
#' @param path the file path of the project
#' @importFrom data.tree Node
#' @import stringr
#' @importFrom progress progress_bar 
#' @importFrom future availableCores
#' @export
#' @examples 
#' 
#' # p <- system.file("inst/extdata/7t_trt", package="bidser")
#' # path <- "~/code/bidser/inst/extdata/7t_trt"
#' # pp <- bids_project(p)
#' 
#' # pp2 <- bids_project(system.file("inst/extdata/megalocalizer", package="bidser"), fmriprep=TRUE)
bids_project <- function(path=".", fmriprep=FALSE, prep_dir = "derivatives/fmriprep") {
  aparser <- anat_parser()
  fparser <- func_parser()
  
  path <- normalizePath(path)

  if (!file.exists(paste0(path, "/participants.tsv"))) {
    stop("participants.tsv is missing")
  }

  if (!file.exists(paste0(path, "/dataset_description.json"))) {
    warning("dataset_description.json is missing")
    desc <- list()
  } else {
    desc <- jsonlite::read_json(paste0(path, "/dataset_description.json"))
  }

  part_df <- read.table(paste0(path, "/participants.tsv"), header=TRUE, stringsAsFactors=FALSE)
  project_name <- basename(path)

  bids <- Node$new(project_name)
  bids_raw <- add_node(bids, "raw")
  
  if (fmriprep) {
    #bids_prep <- bids$AddChild("derivatives/fmriprep")
    bids_prep <- add_node(bids, prep_dir)
    prep_func_parser <- fmriprep_func_parser()
    prep_anat_parser <- fmriprep_anat_parser() 
  } 
    
  sdirs <- as.character(part_df$participant_id)
  
  if (!all(stringr::str_detect(sdirs, "^sub"))) {
    ind <- which(!str_detect(sdirs, "^sub"))
    sdirs[ind] <- paste0("sub-", sdirs[ind])
  }
  
  has_sessions <- FALSE

  pb <- progress::progress_bar$new(total = length(sdirs))

  for (sdir in sdirs) {
   
    if (file.exists(paste0(path, "/", sdir))) {
      #node <- bids_raw$AddChild(sdir)
      node <- add_node(bids_raw, sdir)
      if (fmriprep && file.exists(paste0(path, "/", prep_dir, "/", sdir))) {
        #prepnode <- bids_prep$AddChild(sdir)
        prepnode <- add_node(bids_prep, sdir)
      }
    } else {
      next
    }

    sessions <- get_sessions(path, sdir)

    if (length(sessions) > 0) {
      has_sessions <- TRUE
      for (sess in sessions) {
        #snode <- node$AddChild(sess)
        snode <- add_node(node, sess, session=gsub("ses-", "", sess))
        #snode$session <- gsub("ses-", "", sess)
      
        descend(snode, paste0(path, "/", sdir, "/", sess), "anat", aparser)
        descend(snode, paste0(path, "/", sdir, "/", sess), "func", fparser)
        
        if (fmriprep) {
          #snode_prepped <- prepnode$AddChild(sess)
          #snode_prepped$session <- gsub("ses-", "", sess)
          snode_prepped <- add_node(prepnode, sess, session=gsub("ses-", "", sess))
          
          descend(snode_prepped, paste0(path, "/", prep_dir, "/", sdir, "/", sess), "anat", prep_anat_parser)
          descend(snode_prepped, paste0(path, "/", prep_dir, "/", sdir, "/", sess), "func", prep_func_parser)
          
        }
      }
    } else {
      descend(node, paste0(path, "/", sdir), "anat", aparser)
      descend(node, paste0(path, "/", sdir), "func", fparser)
      
      if (fmriprep) {
        descend(prepnode, paste0(path, "/", prep_dir, "/", sdir), "func", prep_anat_parser)
        descend(prepnode, paste0(path, "/", prep_dir, "/", sdir), "func", prep_func_parser)
      }
    }
    
    pb$tick()
  }
  
  tbl <- tibble::as_tibble(data.tree::ToDataFrameTypeCol(bids, 'name', 'type', 'subid', 'session', 'task', 'run', 'modality', 'suffix'))
  tbl <- tbl %>% select(-starts_with("level_"))
  
  ret <- list(name=project_name, 
              part_df=part_df,
              bids_tree = bids,
              tbl = tbl,
              path=path,
              has_fmriprep=fmriprep,
              prep_dir=if (fmriprep) prep_dir else "",
              has_sessions=has_sessions)

  class(ret) <- "bids_project"
  ret
}

#' @export
flat_list.bids_project <- function(x, full_path=TRUE) {
  if (full_path) {
    data.tree::ToDataFrameTable(x$bids_tree, "pathString", "name") %>% filter(stringr::str_detect(name, "^sub-")) %>%
    rename(path=pathString) %>% select(path)
  } else {
    data.tree::ToDataFrameTable(x$bids_tree, "pathString", "name") %>% filter(stringr::str_detect(name, "^sub-")) %>%
      rename(path=pathString) %>% select(name)
  }
}

#' @export
print.bids_project <- function(x) {
  cat("project: ", x$name, "\n")
  cat("participants (n):", nrow(x$part_df), "\n")
  cat("tasks: ", tasks(x), "\n")
  if (x$has_sessions) {
    cat("sessions: ", sessions(x), "\n")
  }
  if (x$has_fmriprep) {
    cat("fmriprep: ", x$prep_dir, "\n")
  }
  cat("image types: ", unique(x$tbl$type[!is.na(x$tbl$type)]), "\n")
  cat("modalities: ", paste(unique(x$tbl$modality[!is.na(x$tbl$modality)]), collapse=", "), "\n")
  cat("keys: ", paste(unique(x$bids_tree$attributesAll), collapse=", "), "\n")
  
}


#' @export
sessions.bids_project <- function(x) {
  if (x$has_session) {
    unique(unlist(x$bids_tree$Get("session", filterFun = function(x) !is.null(x$session))))
  } else {
    NULL
  }
}

#' @export
tasks.bids_project <- function(x) {
  sort(unique(x$bids_tree$Get("task", filterFun = function(x) {!is.na(x$task) && !is.null(x$task) } )))
  ##unique(x$bids_tree$Get("task", filterFun = function(x) !is.null(x$task) & !is.na(x$task)))
}


#' @export
participants.bids_project <- function (x, ...) {
  #ret <- x$bids_tree$Get("subid", filterFun = function(x) !is.null(x$subid))
  unique(x$tbl$subid[!is.na(x$tbl$subid)])
}

#' @describeIn func_scans 
#' @examples 
#' 
#' p <- system.file("inst/extdata/ds001", package="bidser")
#' fs <- func_scans(bids_project(p), subid="sub-0[123]", run="0[123]")
#' @export
func_scans.bids_project <- function (x, subid=".*", task=".*", run = ".*", session=".*", 
                                     modality="bold", full_path=TRUE, ...) {
  
  f <- function(node) {
    paste0(node$path[3:length(node$path)], collapse="/")
  }
  
  ret <- x$bids_tree$children$raw$Get(f, filterFun = function(z) {
    if (z$isLeaf && !is.null(z$task) &&  !is.null(z$type) && str_detect_null(z$modality,modality)
        && str_detect_null(z$subid, subid)  && str_detect_null(z$task, task) 
        && str_detect_null(z$session, session, default=TRUE) 
        && str_detect_null(z$run, run, TRUE) && str_detect_null(z$suffix, "nii(.gz)?$")) {
      TRUE
    } else {
      FALSE
    }
  })
  
  #ret <- names(ret)
  #paths <- sapply(stringr::str_split(ret, "_"), function(sp) {
  #  paste0(sp[[1]], "/", sp[[2]], "/func")
  #})
  #paste0(paths, "/", ret)
  if (full_path && !is.null(ret)) {
    ret <- paste0(x$path, "/", ret)
  }
  
  unname(ret)
}


#' @keywords internal
str_detect_null <- function(x, pat, default=FALSE) {
  if (is.null(x) || is.na(x)) default else str_detect(x,pat)
}

#' @describeIn preproc_scans 
#' @examples 
#' #proj <- bids_project(system.file("inst/extdata/megalocalizer", package="bidser"), fmriprep=TRUE)
#' #preproc_scans(proj)
#' @export
preproc_scans.bids_project <- function (x, subid=".*", task=".*", run = ".*", 
                                        variant="a^", space=".*", session=".*", 
                                        modality="bold", full_path=FALSE, ...) {
  f <- function(node) {
    paste0(node$path[2:length(node$path)], collapse="/")
  }
  
  ## fixme...
  pdir <- x$prep_dir
  ret <- x$bids_tree$children[[pdir]]$Get(f, filterFun = function(z) {
    if (is.null(variant) && !is.null(z$variant)) {
      return(FALSE)
    }
    
    ##if (is.null(z$deriv)) {
    ##  return(FALSE)
    ##}
    
    if (is.null(variant)) {
      variant <- ".*"
    }
    
    if (z$isLeaf && (str_detect_null(z$deriv , "preproc") || str_detect_null(z$desc , "preproc")) && !is.null(z$type) && 
        str_detect_null(z$modality,modality) && 
        str_detect_null(z$name, subid)  && str_detect_null(z$name, task) && 
        str_detect_null(z$variant, variant, TRUE) && str_detect_null(z$space, space, TRUE) && 
        str_detect_null(z$run, run, TRUE) && 
        str_detect_null(z$session, session, TRUE) && 
        str_detect_null(z$suffix, "nii(.gz)?$")) {
      TRUE
    } else {
      FALSE
    }
  })
  
  if (full_path && !is.null(ret)) {
    paste0(x$path, "/", ret)
  } else {
    ret
  }
}

key_match <- function(default=FALSE, ...) {
  keyvals <- list(...)
  if (length(keyvals) == 0) {
    return(function(x) TRUE)
  }
  keys <- names(keyvals)
  function(x) {
    all(sapply(keys, function(k) {
      if (is.null(keyvals[[k]]) && !is.null(x[[k]])) {
        FALSE
      } else if (is.null(keyvals[[k]]) && is.null(x[[k]])) {
        TRUE
      } else {
        if (keyvals[[k]] == ".*") {
          TRUE
        } else {
          str_detect_null(x[[k]], keyvals[[k]], default)
        }
      }
    }))
  }
}


#' @param regex a regular expression to match files
#' @param full_path return full_path of files
#' @param strict if `TRUE` require that a queried key must exist in match files. 
#'     Otherwise, allow matches for files missing queried key.
#' @param ... additional keys to match on (e.g. subid = "01", task="wm")
#' @export
#' @rdname search_files 
#' @importFrom stringr str_detect
#' @examples
#'  proj <- bids_project(system.file("extdata/ds001", package="bidser"), fmriprep=FALSE)
#'  x = search_files(proj, regex="events")
search_files.bids_project <- function(x, regex=".*", full_path=FALSE, strict=TRUE, new=FALSE, ...) {
  f <- function(node) {
    ## TODO deal with this
    if (node$path[2] == "derivatives/fmriprep") {
      paste0(node$path[2:length(node$path)], collapse="/")
    } else {
      paste0(node$path[3:length(node$path)], collapse="/")
    }
  }
  
  new=FALSE
  if (new) {
    
    nodes <- data.tree::Traverse(x$bids_tree, filterFun = function(x) x$isLeaf && str_detect(x$name, regex))
    fnames <- sapply(nodes, "[[", "name")
    keys <- list(...)
    if (length(keys) > 0) {
      reg2 <- sapply(names(keys), function(k) paste0(".*(", k, "-", keys[[k]], ")"))
      reg2 <- paste0(unlist(reg2), collapse="")
      keep <- str_detect(fnames, reg2)
      ret <- fnames[keep]
    } else {
      ret <- fnames
    }
    # keys <- list(...)
    # nodes <- data.tree::Traverse(x$bids_tree, filterFun =function(x) x$isLeaf && str_detect(x$name, regex))
    # fnames <- sapply(nodes, "[[", "name")
    # keep <- stringr::str_detect(fnames, regex)
    # nodes <- nodes[keep]
    # fnames <- fnames[keep]
    # 
    # gdf <- lapply(names(keys), function(k) {
    #   Get(nodes, k)
    # }) %>% setNames(names(keys)) %>% bind_cols()
    # 
    # mval <- Reduce("+", lapply(seq_along(keys), function(i) {
    #   k <- names(keys)[[i]]
    #   v <- keys[[i]]
    #   str_detect(gdf[[k]], v)
    # }))
    # 
    # keep2 <- which(mval == length(keys))
    # ret <- fnames[keep2]
    # ret
  } else {

    matcher <- key_match(default=!strict, ...)
    ret <- x$bids_tree$Get(f, filterFun = function(z) {
      z$isLeaf && str_detect(z$name, regex) && matcher(z)
    }, simplify=FALSE)
  }
  
  if (length(ret) == 0) {
    return(NULL)
  }
  
  ret <- if (full_path && !is.null(ret)) {
    paste0(x$path, "/", ret)
  } else {
    ret
  }
  
  as.vector(unlist(ret))
}


match_attribute <- function(x, ...) {
  ll <- list(...)
  
  all(sapply(names(ll), function(key) {
    stringr::str_detect(attr(x, key), as.character(ll[[key]]))
  }))
}






