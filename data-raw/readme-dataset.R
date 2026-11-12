# Writes the small demonstration dataset used by README.Rmd.
#
# A two-session study (10 subjects, ses-pre / ses-post) with anat, func
# (task-nback x 2 runs, task-rest), fmap and dwi, plus fMRIPrep derivatives
# with confound tables. Image files contain a real NIfTI-1 header (so volume
# counts can be read) padded to a realistic size. A few problems are planted
# so the QC figures have something to show:
#   - sub-06 never returned for ses-post
#   - sub-03 has no ses-pre dwi
#   - sub-07 ses-post nback run-2 stopped early (130 of 240 volumes)
#   - sub-05 ses-pre nback run-2 has no events.tsv
#   - fMRIPrep produced nothing for sub-09 ses-post nback run-2
#   - sub-04 moved a lot during ses-post rest

make_readme_dataset <- function(root = tempfile("bidser-readme-")) {
  set.seed(1)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  write_text <- function(rel, lines) {
    f <- file.path(root, rel)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, f)
  }
  write_json <- function(rel, x) {
    f <- file.path(root, rel)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(x, f, auto_unbox = TRUE, pretty = TRUE)
  }
  # NIfTI-1 header (gzipped) with the given volume count, padded to `bytes`
  write_nifti <- function(rel, nvol = 1, bytes = 1e6) {
    f <- file.path(root, rel)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    con <- gzfile(f, "wb")
    writeBin(348L, con, size = 4, endian = "little")
    writeBin(raw(36), con)
    writeBin(as.integer(c(if (nvol > 1) 4 else 3, 64, 64, 36, nvol, 1, 1, 1)),
             con, size = 2, endian = "little")
    writeBin(raw(348 - 56), con)
    close(con)
    con <- file(f, "r+b")
    seek(con, round(bytes) - 1, rw = "write")
    writeBin(as.raw(0), con)
    close(con)
  }
  confounds <- function(n, high_motion = FALSE) {
    fd <- abs(stats::rnorm(n, if (high_motion) 0.42 else 0.12, if (high_motion) 0.25 else 0.05))
    fd[sample(n, 3)] <- stats::runif(3, 0.6, 1.2)
    fd[1] <- NA
    gs <- cumsum(stats::rnorm(n, 0, 0.3)) + 100
    data.frame(
      global_signal = gs,
      csf = stats::rnorm(n) + 0.6 * as.numeric(scale(gs)),
      white_matter = stats::rnorm(n),
      framewise_displacement = fd,
      trans_x = cumsum(stats::rnorm(n, 0, 0.02)), trans_y = cumsum(stats::rnorm(n, 0, 0.02)),
      trans_z = cumsum(stats::rnorm(n, 0, 0.03)), rot_x = cumsum(stats::rnorm(n, 0, 5e-4)),
      rot_y = cumsum(stats::rnorm(n, 0, 4e-4)), rot_z = cumsum(stats::rnorm(n, 0, 4e-4))
    )
  }

  write_json("dataset_description.json",
             list(Name = "Working Memory Training", BIDSVersion = "1.8.0"))
  write_text("participants.tsv",
             c("participant_id\tage\tgroup",
               sprintf("sub-%02d\t%d\t%s", 1:10, sample(19:34, 10, TRUE),
                       rep(c("training", "control"), 5))))
  write_text("README", "Demonstration dataset written by data-raw/readme-dataset.R")
  write_json("task-nback_bold.json",
             list(TaskName = "nback", RepetitionTime = 2, EchoTime = 0.03))
  write_json("task-rest_bold.json", list(TaskName = "rest", RepetitionTime = 2))

  fp <- "derivatives/fmriprep"
  write_json(file.path(fp, "dataset_description.json"),
             list(Name = "fMRIPrep", BIDSVersion = "1.8.0", DatasetType = "derivative",
                  GeneratedBy = list(list(Name = "fMRIPrep", Version = "23.2.0"))))

  for (s in sprintf("%02d", 1:10)) for (ses in c("pre", "post")) {
    if (s == "06" && ses == "post") next
    sub <- paste0("sub-", s)
    dir <- file.path(sub, paste0("ses-", ses))
    stem <- paste0(sub, "_ses-", ses)

    if (ses == "pre") write_nifti(sprintf("%s/anat/%s_T1w.nii.gz", dir, stem), 1, 1.4e7)
    for (task in c("nback", "rest")) for (run in if (task == "nback") 1:2 else 1) {
      bold <- sprintf("%s/func/%s_task-%s_run-%d_bold.nii.gz", dir, stem, task, run)
      nvol <- if (task == "rest") 180 else 240
      if (s == "07" && ses == "post" && task == "nback" && run == 2) nvol <- 130
      write_nifti(bold, nvol, nvol * 1.75e5)
      if (task == "nback" && !(s == "05" && ses == "pre" && run == 2)) {
        onsets <- seq(10, 460, by = 30)
        write_text(sprintf("%s/func/%s_task-nback_run-%d_events.tsv", dir, stem, run),
                   c("onset\tduration\ttrial_type\tresponse_time",
                     sprintf("%d\t20\t%s\t%.3f", onsets,
                             rep(c("0back", "2back"), length.out = length(onsets)),
                             stats::rnorm(length(onsets), 0.75, 0.12))))
      }
      if (!(s == "09" && ses == "post" && task == "nback" && run == 2)) {
        base <- sprintf("%s/%s/func/%s_task-%s_run-%d", fp, dir, stem, task, run)
        write_nifti(paste0(base, "_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz"),
                    nvol, nvol * 5.5e5)
        conf <- confounds(nvol, high_motion = s == "04" && ses == "post" && task == "rest")
        f <- file.path(root, paste0(base, "_desc-confounds_timeseries.tsv"))
        utils::write.table(conf, f, sep = "\t", quote = FALSE, row.names = FALSE, na = "n/a")
      }
    }
    write_nifti(sprintf("%s/fmap/%s_phasediff.nii.gz", dir, stem), 1, 1.1e6)
    write_nifti(sprintf("%s/fmap/%s_magnitude1.nii.gz", dir, stem), 1, 1.0e6)
    if (!(s == "03" && ses == "pre")) {
      write_nifti(sprintf("%s/dwi/%s_dir-AP_dwi.nii.gz", dir, stem), 65, 9.5e7)
      write_text(sprintf("%s/dwi/%s_dir-AP_dwi.bval", dir, stem), "0 1000")
      write_text(sprintf("%s/dwi/%s_dir-AP_dwi.bvec", dir, stem), c("0 1", "0 0", "0 0"))
    }
  }
  invisible(root)
}
