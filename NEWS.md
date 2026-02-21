# bidser 0.3.0

* Fix `load_all_events()` and `read_events()` to use tab delimiter for BIDS TSV
  event files instead of space delimiter.
* Fix `read_sidecar()` to handle JSON sidecars containing vector-valued fields
  (e.g. `SliceTiming`) by storing them as list-columns instead of failing with
  a "differing number of rows" error.

# bidser 0.2.0

* TBD.

# bidser 0.1.0

* Initial CRAN release.
* Read, validate, and query BIDS-compliant neuroimaging projects.
* Support for fMRIPrep derivatives including confound tables, preprocessed
  scans, and brain masks.
* Mock BIDS project creation for testing (`create_mock_bids()`).
* Confound processing with PCA reduction and canonical variable aliasing.
* Visualization of PCA confound scores and loadings (`plot.bids_confounds()`).
* Search and filter BIDS files by subject, session, task, run, space,
  and other entities.
