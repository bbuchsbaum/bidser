#' @importFrom Combin8R pSeq pLiteral pRegex pAlt
NULL

sub_matcher <- pSeq(function(value) {value[[3]]$value }, pLiteral("sub"), pLiteral("-"), pRegex("id", "[A-Za-z0-9]+"))

session_matcher <- pSeq(function(value) { value[[4]]$value }, pLiteral("_"), pLiteral("ses"), pLiteral("-"), pRegex("id", "[A-Za-z0-9]+"))

func_mod_matcher <- pSeq(function(value) { value[[2]][[1]] },
                         pLiteral("_"), pAlt("modality", pSeq(function(x) list(type=x[[1]][[1]], suffix=x[[2]][[1]]), pLiteral("bold"), pLiteral(".nii.gz")),
                                                         pSeq(pLiteral("bold"), pLiteral(".json")),
                                                         pSeq(pLiteral("events"), pLiteral(".tsv")),
                                                         pSeq(pLiteral("sbref"), pLiteral(".nii.gz")),
                                                         pSeq(pLiteral("physio"), pLiteral(".tsv"))))

fmap_matcher <- pSeq(function(value) { value[[2]][[1]][[1]] },
                         pLiteral("_"), pAlt("fmap", pRegex("magnitude[12]*.nii.gz"), pRegex("phasediff.nii.gz"),
                                             pRegex("magnitude[12]*.nii.gz"), pLiteral("phase[12]*.nii.gz")))


optional_key <- function(label, regex="[A-Za-z0-9]+") {
  pMany(paste0("has_", label),
        pSeq(function(value) { value[[4]]$value}, pLiteral("_"), pLiteral(label), pLiteral("-"), pRegex("id", regex)))
}

mandatory_key <- function(label, regex="[A-Za-z0-9]+") {
  pSeq(function(value) { value[[4]]$value}, pLiteral("_"), pLiteral(label), pLiteral("-"), pRegex("id", regex))
}






#space_matcher <- pSeq(function(value) { value[[4]]$value }, pLiteral("_"), pLiteral("space"), pLiteral("-"), pRegex("id", "[A-Za-z0-9]+"))
#label_matcher <- pSeq(function(value) { value[[4]]$value }, pLiteral("_"), pLiteral("label"), pLiteral("-"), pRegex("id", "[A-Za-z0-9]+"))
#variant_matcher <- pSeq(function(value) { value[[4]]$value }, pLiteral("_"), pLiteral("variant"), pLiteral("-"), pRegex("id", "[A-Za-z0-9]+"))



structural_matcher <- pSeq(function(value) { value[[2]][[1]][[1]] }, pLiteral("_"), pAlt("modality",
                                                                                         pLiteral("defacemask"),
                                                                                         pLiteral("T1w"),
                                                                                         pLiteral("T2w"),
                                                                                         pLiteral("T1rho"),
                                                                                         pLiteral("T1map"),
                                                                                         pLiteral("T2map"),
                                                                                         pLiteral("T2star"),
                                                                                         pLiteral("FLAIR"),
                                                                                         pLiteral("FLASH"),
                                                                                         pLiteral("PDmap"),
                                                                                         pLiteral("PD"),
                                                                                         pLiteral("PDT2"),
                                                                                         pLiteral("inplaneT1"),
                                                                                         pLiteral("inpaneT2"),
                                                                                         pLiteral("angio")), pAlt(pLiteral(".nii.gz"), pLiteral(".json")))



#' construct a parser for functional data
#' @examples
#' fp <- func_parser()
#' parse(fp, "sub-01_ses-1_task-rest_acq-fullbrain_run-1_bold.nii.gz")
func_parser <- function() {
  builder <- function(x) {
      list(type="func",
           subid=x[[1]],
           session=x[[2]]$value,
           task=x[[3]],
           acquisition=x[[4]]$value,
           contrast=x[[5]]$value,
           dir=x[[6]]$value,
           reconstruction=x[[7]]$value,
           run=x[[8]]$value,
           mod=x[[9]]$value,
           modality=x[[10]]$type,
           suffix=x[[10]]$suffix)
  }

  parser <- pSeq(builder,
                        sub_matcher,
                        optional_key("ses"),
                        mandatory_key("task"),
                        optional_key("acq"),
                        optional_key("ce"),
                        optional_key("dir"),
                        optional_key("rec"),
                        optional_key("run", "[0-9]+"),
                        optional_key("echo", "[0-9]+"),
                        func_mod_matcher
  )

  ret <- list(parser=parser)
  class(ret) <- c("func_parser", "parser")
  ret

}

#' construct a parser for anatomical data
#' @examples
#' ap <- anat_parser()
#' parse(ap, "sub-01_ses-1_T1map.nii.gz")
#' parse(ap, "sub-01_ses-1_T1w.nii.gz")
#' parse(ap, "sub-01_ses-retest_T1w.nii.gz")
anat_parser <- function() {
  builder <- function(x) {
    list(type="anat",
         subid=x[[1]],
         session=x[[2]]$value,
         acquisition=x[[3]]$value,
         contrast=x[[4]]$value,
         dir=x[[5]]$value,
         reconstruction=x[[6]]$value,
         run=x[[7]]$value,
         mod=x[[8]]$value,
         modality=x[[9]])
  }

  parser <- pSeq(builder,
                     sub_matcher,
                     optional_key("ses"),
                     optional_key("acq"),
                     optional_key("ce"),
                     optional_key("dir"),
                     optional_key("rec"),
                     optional_key("run", "[0-9]+"),
                     optional_key("mod"),
                     structural_matcher)

  ret <- list(parser=parser)
  class(ret) <- c("anat_parser", "parser")
  ret

}


#' construct a parser for field map types
#' @examples
#' p <- fmap_parser()
#' parse(p, "sub-01_ses-1_run-1_magnitude1.nii.gz")
#' parse(p, "sub-01_ses-1_run-1_phasediff.nii.gz")
#' parse(p, "sub-01_ses-1_run-2_magnitude2.nii.gz")
fmap_parser <- function() {
  builder <- function(x) {
    list(type="fmap",
         subid=x[[1]],
         session=x[[2]]$value,
         acquisition=x[[3]]$value,
         contrast=x[[4]]$value,
         dir=x[[5]]$value,
         reconstruction=x[[6]]$value,
         run=x[[7]]$value,
         mod=x[[8]]$value,
         modality=x[[9]])
  }

  parser <- pSeq(builder,
                 sub_matcher,
                 optional_key("ses"),
                 optional_key("acq"),
                 optional_key("ce"),
                 optional_key("dir"),
                 optional_key("rec"),
                 optional_key("run", "[0-9]+"),
                 optional_key("mod"),
                 structural_matcher)

  ret <- list(parser=parser)
  class(ret) <- c("anat_parser", "parser")
  ret

}

parse.parser <- function(x, fname) {
  x$parser(fname)
}

funcpreptypes_matcher <- pSeq(function(value) { value[[2]][[1]][[1]] },
                              pLiteral("_"), pAlt("preptype", pLiteral("roi.nii.gz"), pLiteral("preproc.nii.gz"), pLiteral("brainmask.nii.gz"),
                                                  pLiteral("confounds.tsv"), pLiteral("AROMAnoiseICs.csv")))


func_prep_builder <- function(x) {
  list(subid=x[[1]],
       session=x[[2]][[1]],
       task=x[[3]],
       acquisition=x[[4]][[1]],
       reconstruction=x[[5]][[1]],
       run=x[[6]][[1]],
       modality=x[[7]],
       space=x[[8]][[1]],
       label=x[[9]][[1]],
       variant=x[[10]][[1]],
       desc=x[[11]])

  func_prep_matcher  <- pSeq(function(x) func_prep_builder(x),
                             sub_matcher,
                             pMany("has_session",session_matcher),
                             task_matcher,
                             pMany("has_acquisition",acq_matcher),
                             pMany("has_reconstruction", recon_matcher),
                             pMany("has_run", run_matcher),
                             pSeq(function(value) { value[[2]][[1]][[1]] }, pLiteral("_"), pLiteral("bold")),
                             pMany("has_space", space_matcher),
                             pMany("has_label", label_matcher),
                             pMany("has_variant", variant_matcher),
                             funcpreptypes_matcher)
}









#parsed <- func_matcher("sub-01_task-balloonanalogrisktask_run-01_bold.nii.gz")
#parsed_anat <- anat_matcher("sub-01_ses-mri_acq-mprage_T1w.nii.gz")

