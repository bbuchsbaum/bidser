#' @importFrom Combin8R pSeq pLiteral pRegex pAlt pMany
NULL

sub_matcher <- pSeq(function(value) {value[[3]]$value }, pLiteral("sub"), pLiteral("-"), pRegex("id", "[A-Za-z0-9]+"))

session_matcher <- pSeq(function(value) { value[[4]]$value }, pLiteral("_"), pLiteral("ses"), pLiteral("-"), pRegex("id", "[A-Za-z0-9]+"))

extractor <- function(x) { list(type=x[[1]][[1]], suffix=x[[2]][[1]]) }

func_mod_matcher <- pSeq(function(x) { x[[2]] },
                         pLiteral("_"), pAlt(function(x) x,
                                                         pSeq(extractor, pLiteral("bold"), pLiteral(".nii.gz")),
                                                         pSeq(extractor, pLiteral("bold"), pLiteral(".json")),
                                                         pSeq(extractor, pLiteral("events"), pLiteral(".tsv")),
                                                         pSeq(extractor, pLiteral("sbref"), pLiteral(".nii.gz")),
                                                         pSeq(extractor, pLiteral("physio"), pLiteral(".tsv"))))

fmap_matcher <- pSeq(function(value) { value[[2]][[1]][[1]] },
                         pLiteral("_"), pAlt("fmap", pRegex("magnitude[12]*.nii.gz"), pRegex("phasediff.nii.gz"),
                                             pRegex("magnitude[12]*.nii.gz"), pLiteral("phase[12]*.nii.gz")))


optional_key <- function(label, regex="[A-Za-z0-9]+") {
  pMany(paste0("has_", label),
        pSeq(function(value) { value[[4]]$value}, pLiteral("_"), pLiteral(label), pLiteral("-"), pRegex("id", regex)))
}

optional_literal <- function(lit, label) {
  pMany(paste0("has_", label),
        pSeq(function(value) { value[[2]][[1]][[1]] }, pLiteral("_"), pLiteral(lit))
  )
}

mandatory_key <- function(label, regex="[A-Za-z0-9]+") {
  pSeq(function(value) { value[[4]]$value}, pLiteral("_"), pLiteral(label), pLiteral("-"), pRegex("id", regex))
}

one_of <- function(labels) {
  lits <- lapply(labels, pLiteral)
  pSeq(function(value) { value[[2]][[1]] }, pLiteral("_"), do.call(pAlt, c(lits, tag=function(x) { x})))
}

zero_or_one_of <- function(labels, label) {
  lits <- lapply(labels, pLiteral)
  pMany(paste0("has_", label),
        pSeq(function(value) { value[[2]][[1]] }, pLiteral("_"), do.call(pAlt, c(lits, tag=function(x) { x})))
  )
}


#space_matcher <- pSeq(function(value) { value[[4]]$value }, pLiteral("_"), pLiteral("space"), pLiteral("-"), pRegex("id", "[A-Za-z0-9]+"))
#label_matcher <- pSeq(function(value) { value[[4]]$value }, pLiteral("_"), pLiteral("label"), pLiteral("-"), pRegex("id", "[A-Za-z0-9]+"))
#variant_matcher <- pSeq(function(value) { value[[4]]$value }, pLiteral("_"), pLiteral("variant"), pLiteral("-"), pRegex("id", "[A-Za-z0-9]+"))



gen_lit <- function(type, suffix, extractor) {
  pSeq(extractor, pLiteral(type), pLiteral(suffix))
}

gen_lits <- function(types, suffix, extractor) {
  lapply(types, function(t) gen_lit(t,suffix,extractor))
  
}

anat_types <- c("defacemask","T1w", "T2w","T1map", "T2map", "T2star","FLAIR", "FLASH", "PDmap","PD","PDT2",
                "inplaneT1", "inplaneT2", "angio")
anat_nii <- gen_lits(anat_types, ".nii.gz", extractor)
anat_json <- gen_lits(anat_types, ".json", extractor)

anat_mod_matcher <- pSeq(function(x) { x[[2]] }, 
                           pLiteral("_"), do.call(pAlt, c(anat_nii, anat_json, tag=function(x) x)))
                                              
                                              
                                                                                  

#' construct a parser for functional data
#' @examples
#' fp <- func_parser()
#' parse(fp, "sub-01_ses-1_task-rest_acq-fullbrain_run-1_bold.nii.gz")
#' parse(fp, "sub-01_ses-1_task-nback_acq-fullbrain_run-1_events.tsv")
func_parser <- function() {
  builder <- function(x) {

      list(type="func",
           subid=x[[1]],
           session=unlist(x[[2]]$value),
           task=x[[3]],
           acquisition=unlist(x[[4]]$value),
           contrast=unlist(x[[5]]$value),
           dir=unlist(x[[6]]$value),
           reconstruction=unlist(x[[7]]$value),
           run=unlist(x[[8]]$value),
           mod=unlist(x[[9]]$value),
           modality=x[[10]]$type,
           suffix=substr(x[[10]]$suffix, 2, nchar(x[[10]]$suffix)))
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
#'
anat_parser <- function() {
  builder <- function(x) {
    #browser()
    list(type="anat",
         subid=x[[1]],
         session=x[[2]]$value,
         acquisition=x[[3]]$value,
         contrast=x[[4]]$value,
         dir=x[[5]]$value,
         reconstruction=x[[6]]$value,
         run=x[[7]]$value,
         mod=x[[8]]$value,
         modality=x[[9]]$type,
         suffix=substr(x[[9]]$suffix, 2, nchar(x[[9]]$suffix)))
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
                     anat_mod_matcher)

  ret <- list(parser=parser)
  class(ret) <- c("anat_parser", "parser")
  ret

}


#' construct a parser for field map types
#' 
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
  class(ret) <- c("fmap_parser", "parser")
  ret

}


parse.parser <- function(x, fname) {
  x$parser(fname)
}



func_prep_types <- c("roi", "preproc", "brainmask", "confounds", "AROMAnoiseICs", "bold", "regressors")

funcpreptypes_matcher <- pSeq(function(x) { x[[2]] },
                              pLiteral("_"), pAlt(function(x) x,
                                                  gen_lit("roi", ".nii.gz", extractor), 
                                                  gen_lit("regressors", ".tsv", extractor),
                                                  gen_lit("preproc",".nii.gz", extractor), 
                                                  gen_lit("bold",".nii.gz", extractor), 
                                                  gen_lit("brainmask", ".nii.gz", extractor),
                                                  gen_lit("confounds", ".tsv", extractor), 
                                                  gen_lit("MELODICmix", ".tsv", extractor), 
                                                  gen_lit("AROMAnoiseICs", ".csv", extractor)))

#' construct a parser for fmriprep func types
#' 
#' @examples
#' p <- fmriprep_func_parser()
#' parse(p, "sub-2002_task-mega_run-09_bold_space-MNI152NLin2009cAsym_preproc.nii.gz")
#' parse(p, "sub-2002_task-mega_run-08_bold_space-MNI152NLin2009cAsym_brainmask.nii.gz")
#  parse(p, "sub-2002_task-mega_run-08_bold_space-MNI152NLin2009cAsym_variant-smoothAROMAnonaggr_preproc.nii.gz")
#  parse(p, "sub-2002_task-mega_run-08_bold_space-T1w_label-aparcaseg_roi.nii.gz")
#  parse(p, "sub-2002_task-mega_run-08_bold_space-T1w_label-aseg_roi.nii.gz")
#  parse(p, "sub-2002_task-mega_run-09_bold_AROMAnoiseICs.csv")
#  parse(p, "sub-2002_task-mega_run-09_bold_confounds.tsv")
#  parse(p, "sub-2002_task-mega_run-09_bold_MELODICmix.tsv")
#  parse(p, "sub-301_task-repetition_run-4_space-MNI152Lin_res-native_desc-preproc_bold.nii.gz")
#  parse(p, "sub-301_task-localizer_run-3_desc-confounds_regressors.tsv")
fmriprep_func_parser <- function() {

  builder <- function(x) {
    ret <- list(type="func",
         subid=x[[1]],
         session=unlist(x[[2]]$value),
         task=x[[3]],
         acquisition=unlist(x[[4]]$value),
         reconstruction=unlist(x[[5]]$value),
         run=unlist(x[[6]]$value),
         modality=unlist(x[[7]]$value),
         space=unlist(x[[8]]$value),
         res=unlist(x[[9]]$value),
         desc=unlist(x[[10]]$value),
         label=unlist(x[[11]]$value),
         variant=unlist(x[[12]]$value),
         #desc=unlist(x[[11]]$value),
         deriv=unlist(x[[13]]$type),
         suffix=substr(x[[13]]$suffix, 2, nchar(x[[13]]$suffix)))
    
    if (is.null(ret$modality) && !is.null(ret$deriv)) {
      ret$modality <- ret$deriv
    }
    
    ret
    
    
  }
  

  parser  <- pSeq(function(x) builder(x),
                             sub_matcher,
                  
                             optional_key("ses"),
                             mandatory_key("task"),
                             optional_key("acq"),
                             optional_key("rec"),
                             optional_key("run", "[0-9]+"),
                             optional_literal("bold", "modality"),
                             #pSeq(function(value) { value[[2]][[1]][[1]] }, pLiteral("_"), pLiteral("bold")),
                             optional_key("space"),
                             optional_key("res"),
                             optional_key("desc"),
                             optional_key("label"),
                             optional_key("variant"),
                             funcpreptypes_matcher)
    
    ret <- list(parser=parser)
    class(ret) <- c("fmriprep_func_parser", "parser")
    ret
}


anat_prep_types <- c("preproc", "brainmask", "probtissue", "mask", "probseg", "T1w", "dtissue", "warp",
                     "inflated.L.surf","inflated.R.surf","pial.L.surf","pial.R.surf", "affine")
                     
                     
anatpreptypes_matcher <- pSeq(function(x) { x[[2]] },
                              pLiteral("_"), pAlt(function(x) x,
                                                  gen_lit("preproc", ".nii.gz", extractor), 
                                                  gen_lit("brainmask",".nii.gz", extractor), 
                                                  gen_lit("probtissue", ".nii.gz", extractor),
                                                  gen_lit("mask", ".nii.gz", extractor),
                                                  gen_lit("T1w", ".nii.gz", extractor),
                                                  gen_lit("probseg", ".nii.gz", extractor),
                                                  gen_lit("dtissue", ".nii.gz", extractor), 
                                                  gen_lit("warp", ".h5", extractor),
                                                  gen_lit("inflated.L.surf", ".gii", extractor),
                                                  gen_lit("inflated.R.surf", ".gii", extractor),
                                                  gen_lit("pial.L.surf", ".gii", extractor),
                                                  gen_lit("pial.R.surf", ".gii", extractor),
                                                  gen_lit("affine", ".txt", extractor)))



#' construct a parser for anatomical data
#' @examples
#' ap <- fmriprep_anat_parser()
#' parse(ap, "sub-2001_T1w_inflated.L.surf.gii")
#' parse(ap, "sub-2001_T1w_space-MNI152NLin2009cAsym_class-GM_probtissue.nii.gz")
#' parse(ap, "sub-301_space-MNI152Lin_desc-brain_mask.nii.gz")
#' parse(ap, "sub-301_space-MNI152NLin2009cAsym_label-GM_probseg.nii.gz")
fmriprep_anat_parser <- function() {
  builder <- function(x) {
    
    list(type="anat",
         subid=x[[1]],
         session=unlist(x[[2]]$value),
         acquisition=unlist(x[[3]]$value),
         contrast=unlist(x[[4]]$value),
         dir=unlist(x[[5]]$value),
         reconstruction=unlist(x[[6]]$value),
         run=unlist(x[[7]]$value),
         modality=unlist(x[[8]]),
         label=unlist(x[[9]]$value),
         space=unlist(x[[10]]$value),
         desc=unlist(x[[11]]$value),
         target=unlist(x[[12]]$value),
         class=unlist(x[[13]]$value),
         mod=unlist(x[[14]]$value),
         deriv=x[[15]]$type,
         suffix=substr(x[[15]]$suffix, 2, nchar(x[[15]]$suffix)))
  }
  
  parser <- pSeq(builder,
                 sub_matcher,
                 optional_key("ses"),
                 optional_key("acq"),
                 optional_key("ce"),
                 optional_key("dir"),
                 optional_key("rec"),
                 optional_key("run", "[0-9]+"),
                 zero_or_one_of(anat_types, "anat_type"),
                 optional_key("space"),
                 optional_key("label"),
                 optional_key("desc"),
                 optional_key("target"),
                 optional_key("class"),
                 optional_key("mod"),
                 anatpreptypes_matcher)
  
  ret <- list(parser=parser)
  class(ret) <- c("fmriprep_anat_parser", "parser")
  ret
  
}




# sub-2001_T1w_brainmask.nii.gz						sub-2001_T1w_smoothwm.R.surf.gii
# sub-2001_T1w_class-CSF_probtissue.nii.gz				sub-2001_T1w_space-MNI152NLin2009cAsym_brainmask.nii.gz
# sub-2001_T1w_class-GM_probtissue.nii.gz					sub-2001_T1w_space-MNI152NLin2009cAsym_class-CSF_probtissue.nii.gz
# sub-2001_T1w_class-WM_probtissue.nii.gz					sub-2001_T1w_space-MNI152NLin2009cAsym_class-GM_probtissue.nii.gz
# sub-2001_T1w_dtissue.nii.gz						sub-2001_T1w_space-MNI152NLin2009cAsym_class-GM_probtissue_small.nii.gz
# sub-2001_T1w_inflated.L.surf.gii					sub-2001_T1w_space-MNI152NLin2009cAsym_class-WM_probtissue.nii.gz
# sub-2001_T1w_inflated.R.surf.gii					sub-2001_T1w_space-MNI152NLin2009cAsym_dtissue.nii.gz
# sub-2001_T1w_label-aparcaseg_roi.nii.gz					sub-2001_T1w_space-MNI152NLin2009cAsym_label-aparcaseg_roi.nii.gz
# sub-2001_T1w_label-aseg_roi.nii.gz					sub-2001_T1w_space-MNI152NLin2009cAsym_preproc.nii.gz
# sub-2001_T1w_midthickness.L.surf.gii					sub-2001_T1w_space-MNI152NLin2009cAsym_target-T1w_warp.h5
# sub-2001_T1w_midthickness.R.surf.gii					sub-2001_T1w_space-orig_target-T1w_affine.txt
# sub-2001_T1w_pial.L.surf.gii						sub-2001_T1w_target-MNI152NLin2009cAsym_warp.h5
# sub-2001_T1w_pial.R.surf.gii						sub-2001_T1w_target-fsnative_affine.txt
# sub-2001_T1w_preproc.nii.gz

