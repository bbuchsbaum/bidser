#!/usr/bin/env Rscript

arg_value <- function(args, name, default = NULL) {
  hit <- match(name, args)
  if (is.na(hit) || hit == length(args)) {
    return(default)
  }
  args[[hit + 1L]]
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

has_flag <- function(args, name) {
  name %in% args
}

load_checkout <- function() {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(".", quiet = TRUE)
  } else if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    library(bidser)
  }
}

median_ms <- function(x) {
  stats::median(x, na.rm = TRUE)
}

bench_reps <- function(engine, operation, reps, fun, count_fun = length,
                       version = NA_character_, dataset = NA_character_) {
  rows <- vector("list", reps)
  for (i in seq_len(reps)) {
    gc()
    value <- NULL
    elapsed <- system.time({
      value <- fun()
    })[["elapsed"]]
    rows[[i]] <- data.frame(
      engine = engine,
      operation = operation,
      rep = i,
      elapsed_ms = elapsed * 1000,
      n = count_fun(value),
      version = version,
      dataset = dataset,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

run_bidser <- function(dataset, reps) {
  load_checkout()

  dataset <- normalizePath(dataset, winslash = "/", mustWork = TRUE)
  dataset_name <- basename(dataset)
  version <- as.character(utils::packageVersion("bidser"))
  tmp_index_dir <- tempfile("bidser-bench-index-")
  dir.create(tmp_index_dir)
  on.exit(unlink(tmp_index_dir, recursive = TRUE, force = TRUE), add = TRUE)

  construct_no_index <- bench_reps(
    engine = "bidser",
    operation = "construct_no_index",
    reps = reps,
    version = version,
    dataset = dataset_name,
    fun = function() {
      bids_project(dataset, derivatives = "none", index = "none")
    },
    count_fun = function(proj) {
      nrow(flat_list(proj, full_path = FALSE))
    }
  )

  construct_index_build <- bench_reps(
    engine = "bidser",
    operation = "construct_index_build",
    reps = reps,
    version = version,
    dataset = dataset_name,
    fun = function() {
      index_path <- file.path(tmp_index_dir, paste0("index-", Sys.getpid(), "-", sample.int(.Machine$integer.max, 1), ".rds"))
      bids_project(dataset, derivatives = "none", index = "auto", index_path = index_path)
    },
    count_fun = function(proj) {
      nrow(proj$index %||% data.frame())
    }
  )

  cached_index_path <- file.path(tmp_index_dir, "cached-index.rds")
  invisible(bids_project(dataset, derivatives = "none", index = "auto", index_path = cached_index_path))
  construct_index_cached <- bench_reps(
    engine = "bidser",
    operation = "construct_index_cached",
    reps = reps,
    version = version,
    dataset = dataset_name,
    fun = function() {
      bids_project(dataset, derivatives = "none", index = "auto", index_path = cached_index_path)
    },
    count_fun = function(proj) {
      nrow(proj$index %||% data.frame())
    }
  )

  proj <- bids_project(dataset, derivatives = "none", index = "auto",
                       index_path = file.path(tmp_index_dir, "query-index.rds"))
  query_bold01 <- bench_reps(
    engine = "bidser",
    operation = "query_bold_sub01",
    reps = reps,
    version = version,
    dataset = dataset_name,
    fun = function() {
      query_files(
        proj,
        regex = "bold\\.nii\\.gz$",
        subid = "01",
        task = "mixedgamblestask",
        scope = "raw"
      )
    }
  )
  query_dwi <- bench_reps(
    engine = "bidser",
    operation = "query_dwi",
    reps = reps,
    version = version,
    dataset = dataset_name,
    fun = function() {
      query_files(proj, datatype = "dwi", kind = "dwi", scope = "raw")
    }
  )

  all_files <- query_files(proj, regex = ".*", scope = "raw")
  file_count <- data.frame(
    engine = "bidser",
    operation = "file_count_all",
    rep = 0L,
    elapsed_ms = NA_real_,
    n = length(all_files),
    version = version,
    dataset = dataset_name,
    stringsAsFactors = FALSE
  )

  do.call(rbind, list(
    construct_no_index,
    construct_index_build,
    construct_index_cached,
    query_bold01,
    query_dwi,
    file_count
  ))
}

run_pybids <- function(dataset, reps, pybids_spec) {
  dataset <- normalizePath(dataset, winslash = "/", mustWork = TRUE)
  py_code <- tempfile("pybids-bench-", fileext = ".py")
  err_file <- tempfile("pybids-bench-", fileext = ".err")
  on.exit(unlink(c(py_code, err_file), force = TRUE), add = TRUE)

  writeLines(c(
    "import csv, statistics, sys, time",
    "from bids import BIDSLayout",
    "import bids",
    "dataset = sys.argv[1]",
    "reps = int(sys.argv[2])",
    "dataset_name = dataset.rstrip('/').split('/')[-1]",
    "version = getattr(bids, '__version__', 'unknown')",
    "rows = []",
    "def add(operation, rep, elapsed_ms, n):",
    "    rows.append({'engine': 'pybids', 'operation': operation, 'rep': rep, 'elapsed_ms': elapsed_ms, 'n': n, 'version': version, 'dataset': dataset_name})",
    "last_layout = None",
    "for i in range(1, reps + 1):",
    "    t0 = time.perf_counter()",
    "    last_layout = BIDSLayout(dataset, validate=False, database_path=None)",
    "    elapsed = (time.perf_counter() - t0) * 1000.0",
    "    add('construct_no_index', i, elapsed, len(last_layout.get(return_type='filename')))",
    "layout = last_layout or BIDSLayout(dataset, validate=False, database_path=None)",
    "for i in range(1, reps + 1):",
    "    t0 = time.perf_counter()",
    "    files = layout.get(subject='01', suffix='bold', extension='.nii.gz', return_type='filename')",
    "    elapsed = (time.perf_counter() - t0) * 1000.0",
    "    add('query_bold_sub01', i, elapsed, len(files))",
    "for i in range(1, reps + 1):",
    "    t0 = time.perf_counter()",
    "    files = layout.get(datatype='dwi', suffix='dwi', return_type='filename')",
    "    elapsed = (time.perf_counter() - t0) * 1000.0",
    "    add('query_dwi', i, elapsed, len(files))",
    "add('file_count_all', 0, '', len(layout.get(return_type='filename')))",
    "writer = csv.DictWriter(sys.stdout, fieldnames=['engine', 'operation', 'rep', 'elapsed_ms', 'n', 'version', 'dataset'])",
    "writer.writeheader()",
    "writer.writerows(rows)"
  ), py_code)

  out <- tryCatch(
    system2(
      "uv",
      c("run", "--with", pybids_spec, "python", py_code, dataset, as.character(reps)),
      stdout = TRUE,
      stderr = err_file
    ),
    warning = function(w) structure(character(0), status = 1L, warning = conditionMessage(w)),
    error = function(e) structure(character(0), status = 1L, error = conditionMessage(e))
  )
  status <- attr(out, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) {
    err <- if (file.exists(err_file)) paste(readLines(err_file, warn = FALSE), collapse = "\n") else ""
    warning("pybids benchmark failed; continuing with bidser rows only.\n", err, call. = FALSE)
    return(data.frame())
  }

  utils::read.csv(text = paste(out, collapse = "\n"), stringsAsFactors = FALSE)
}

print_summary <- function(results) {
  timing <- results[!is.na(results$elapsed_ms), , drop = FALSE]
  if (nrow(timing) > 0) {
    summary <- aggregate(
      elapsed_ms ~ engine + operation,
      data = timing,
      FUN = median_ms
    )
    names(summary)[names(summary) == "elapsed_ms"] <- "median_ms"
    count_summary <- aggregate(n ~ engine + operation, data = timing, FUN = stats::median)
    summary <- merge(summary, count_summary, by = c("engine", "operation"), all.x = TRUE)
    summary <- summary[order(summary$operation, summary$engine), ]
    print(summary, row.names = FALSE)
  }

  counts <- results[results$operation == "file_count_all", c("engine", "n", "version", "dataset"), drop = FALSE]
  if (nrow(counts) > 0) {
    cat("\nFile-count parity surface:\n")
    print(counts, row.names = FALSE)
  }
}

args <- commandArgs(trailingOnly = TRUE)
dataset <- arg_value(args, "--dataset", file.path("pybids", "tests", "data", "ds005"))
reps <- as.integer(arg_value(args, "--reps", "7"))
out_path <- arg_value(args, "--out", file.path(
  "tools", "bench-results",
  paste0("pybids-parity-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
))
pybids_spec <- arg_value(args, "--pybids", "./pybids")
skip_pybids <- has_flag(args, "--skip-pybids")

if (is.na(reps) || reps < 1L) {
  stop("`--reps` must be a positive integer.", call. = FALSE)
}

bidser_rows <- run_bidser(dataset, reps)
pybids_rows <- if (skip_pybids) data.frame() else run_pybids(dataset, reps, pybids_spec)
results <- rbind(bidser_rows, pybids_rows)

print_summary(results)

if (!identical(out_path, "none")) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(results, out_path, row.names = FALSE)
  cat("\nWrote benchmark rows to ", out_path, "\n", sep = "")
}
