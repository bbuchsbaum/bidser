context("parser")
test_that("can parse various file types", {
  expect_type(encode("sub-2001_T1w_brainmask.nii.gz"), "list")
  expect_type(encode("sub-2001_T1w_class-CSF_probtissue.nii.gz"), "list")
  expect_type(encode("sub-2001_T1w_class-GM_probtissue.nii.gz"), "list")
  expect_type(encode("sub-2001_T1w_class-WM_probtissue.nii.gz"), "list")
  expect_type(encode("sub-2001_T1w_dtissue.nii.gz"), "list")
  expect_type(encode("sub-2001_T1w_inflated.L.surf.gii"), "list")
  expect_type(encode("sub-2001_T1w_inflated.R.surf.gii"), "list")
  expect_type(encode("sub-2001_T1w_label-aparcaseg_roi.nii.gz"), "list")		
  expect_type(encode("sub-2001_T1w_label-aseg_roi.nii.gz"), "list")		
  expect_type(encode("sub-2001_T1w_midthickness.L.surf.gii"), "list")	
  expect_type(encode("sub-2001_T1w_midthickness.R.surf.gii"), "list")	
  expect_type(encode("sub-2001_T1w_pial.L.surf.gii"), "list")	
  expect_type(encode("sub-2001_T1w_pial.R.surf.gii"), "list")
  
  expect_type(encode("sub-2001_T1w_preproc.nii.gz"), "list")
  expect_type(encode("sub-2001_T1w_smoothwm.R.surf.gii"), "list")
  expect_type(encode("sub-2001_T1w_space-MNI152NLin2009cAsym_brainmask.nii.gz"), "list")
  expect_type(encode("sub-2001_T1w_space-MNI152NLin2009cAsym_class-CSF_probtissue.nii.gz"), "list")
  expect_type(encode("sub-2001_T1w_space-MNI152NLin2009cAsym_class-GM_probtissue.nii.gz"), "list")
  #expect_type(encode("sub-2001_T1w_space-MNI152NLin2009cAsym_class-GM_probtissue_small.nii.gz"), "list")
  expect_type(encode("sub-2001_T1w_space-MNI152NLin2009cAsym_class-WM_probtissue.nii.gz"), "list")
  expect_type(encode("sub-2001_T1w_preproc.nii.gz"), "list")
  
  expect_type(encode("sub-2001_T1w_target-fsnative_affine.txt"), "list")
  expect_type(encode("sub-2001_T1w_target-MNI152NLin2009cAsym_warp.h5"), "list")
  expect_type(encode("sub-2001_T1w_space-MNI152NLin2009cAsym_target-T1w_warp.h5"), "list")
  expect_type(encode("sub-2001_T1w_space-orig_target-T1w_affine.txt"), "list")
  expect_type(encode("sub-2001_T1w_space-MNI152NLin2009cAsym_preproc.nii.gz"), "list")
  expect_type(encode("sub-2001_T1w_space-MNI152NLin2009cAsym_dtissue.nii.gz"), "list")
  expect_type(encode("sub-1006_task-phoneme_run-all_bold_space-MNI152NLin2009cAsym_latent.lv.h5"), "list")
  expect_type(encode("sub-1006_task-phoneme_run-all_bold_space-MNI152NLin2009cAsym_desc-junk_latent.lv.h5"), "list")
  expect_type(encode("sub-28_task-citizenfour_run-04_space-MNI152Lin_res-native_desc-brain_mask.nii.gz"), "list")
  expect_type(encode("sub-2001_task-test_confounds.tsv"), "list")
  expect_type(encode("sub-2001_ses-01_task-test_run-01_bold_space-MNI152NLin2009cAsym_preproc.nii.gz"), "list")
  expect_type(encode("sub-2001_ses-01_task-test_run-01_bold_space-MNI152NLin2009cAsym_variant-smoothAROMAnonaggr_preproc.nii.gz"), "list")
  expect_type(encode("sub-2001_task-test_run-01_bold_MELODICmix.tsv"), "list")
  expect_type(encode("sub-2001_task-test_run-01_bold_AROMAnoiseICs.tsv"), "list")
  expect_type(encode("sub-2001_task-alice_echo-2_bold.nii.gz"), "list")
  
  #expect_type(encode("sub-2001_task-test_run-01_bold_AROMAnoiseICs.csv"), "list")
  
  #BIDS 1.6.0 conventions
  expect_type(encode("sub-01_task-test_desc-confounds_timeseries.tsv"), "list")
  expect_type(encode("sub-2001_task-test_run-01_desc-MELODIC_mixing.tsv"), "list")
  
  expect_type(encode("sub-01_ses-01_task-test_run-01_echo-01_bold.nii.gz"), "list")
  expect_type(encode("sub-01_task-test_desc-preproc_bold.nii.gz"), "list")
  expect_type(encode("sub-01_task-test_run-01_desc-preproc_bold.nii.gz"), "list")
  expect_type(encode("sub-01_ses-01_task-test_run-01_desc-preproc_bold.nii.gz"), "list")
  expect_type(encode("sub-01_ses-01_task-test_run-01_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz"), "list")
  expect_type(encode("sub-sid000009_task-movie_run-03_space-MNI152Lin_res-native_desc-preproc_bold.nii.gz"), "list")
})


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
