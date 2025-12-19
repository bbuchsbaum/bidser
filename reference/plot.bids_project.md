# Plot a BIDS project as a dendrogram

This method visualises the hierarchical file structure of a BIDS
project. The tree is converted to a dendrogram and drawn using base
graphics. Large projects can be trimmed by setting a maximum depth.

## Usage

``` r
# S3 method for class 'bids_project'
plot(x, max_depth = Inf, ...)

# S3 method for class 'mock_bids_project'
plot(x, max_depth = Inf, ...)
```

## Arguments

- x:

  A `bids_project` object.

- max_depth:

  Maximum depth of the tree to display. Defaults to `Inf` so the full
  hierarchy is shown.

- ...:

  Additional arguments passed to
  [`graphics::plot`](https://rdrr.io/r/graphics/plot.default.html).

## Value

The input object `x` is returned invisibly.

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  plot(proj)
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#>                                                                levelName
#> 1   bids_example_ds001                                                  
#> 2    °--raw                                                             
#> 3        ¦--sub-01                                                      
#> 4        ¦   ¦--anat                                                    
#> 5        ¦   ¦   ¦--sub-01_T1w.nii.gz                                   
#> 6        ¦   ¦   °--sub-01_inplaneT2.nii.gz                             
#> 7        ¦   °--func                                                    
#> 8        ¦       ¦--sub-01_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 9        ¦       ¦--sub-01_task-balloonanalogrisktask_run-01_events.tsv 
#> 10       ¦       ¦--sub-01_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 11       ¦       ¦--sub-01_task-balloonanalogrisktask_run-02_events.tsv 
#> 12       ¦       ¦--sub-01_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 13       ¦       °--sub-01_task-balloonanalogrisktask_run-03_events.tsv 
#> 14       ¦--sub-02                                                      
#> 15       ¦   ¦--anat                                                    
#> 16       ¦   ¦   ¦--sub-02_T1w.nii.gz                                   
#> 17       ¦   ¦   °--sub-02_inplaneT2.nii.gz                             
#> 18       ¦   °--func                                                    
#> 19       ¦       ¦--sub-02_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 20       ¦       ¦--sub-02_task-balloonanalogrisktask_run-01_events.tsv 
#> 21       ¦       ¦--sub-02_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 22       ¦       ¦--sub-02_task-balloonanalogrisktask_run-02_events.tsv 
#> 23       ¦       ¦--sub-02_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 24       ¦       °--sub-02_task-balloonanalogrisktask_run-03_events.tsv 
#> 25       ¦--sub-03                                                      
#> 26       ¦   ¦--anat                                                    
#> 27       ¦   ¦   ¦--sub-03_T1w.nii.gz                                   
#> 28       ¦   ¦   °--sub-03_inplaneT2.nii.gz                             
#> 29       ¦   °--func                                                    
#> 30       ¦       ¦--sub-03_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 31       ¦       ¦--sub-03_task-balloonanalogrisktask_run-01_events.tsv 
#> 32       ¦       ¦--sub-03_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 33       ¦       ¦--sub-03_task-balloonanalogrisktask_run-02_events.tsv 
#> 34       ¦       ¦--sub-03_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 35       ¦       °--sub-03_task-balloonanalogrisktask_run-03_events.tsv 
#> 36       ¦--sub-04                                                      
#> 37       ¦   ¦--anat                                                    
#> 38       ¦   ¦   ¦--sub-04_T1w.nii.gz                                   
#> 39       ¦   ¦   °--sub-04_inplaneT2.nii.gz                             
#> 40       ¦   °--func                                                    
#> 41       ¦       ¦--sub-04_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 42       ¦       ¦--sub-04_task-balloonanalogrisktask_run-01_events.tsv 
#> 43       ¦       ¦--sub-04_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 44       ¦       ¦--sub-04_task-balloonanalogrisktask_run-02_events.tsv 
#> 45       ¦       ¦--sub-04_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 46       ¦       °--sub-04_task-balloonanalogrisktask_run-03_events.tsv 
#> 47       ¦--sub-05                                                      
#> 48       ¦   ¦--anat                                                    
#> 49       ¦   ¦   ¦--sub-05_T1w.nii.gz                                   
#> 50       ¦   ¦   °--sub-05_inplaneT2.nii.gz                             
#> 51       ¦   °--func                                                    
#> 52       ¦       ¦--sub-05_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 53       ¦       ¦--sub-05_task-balloonanalogrisktask_run-01_events.tsv 
#> 54       ¦       ¦--sub-05_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 55       ¦       ¦--sub-05_task-balloonanalogrisktask_run-02_events.tsv 
#> 56       ¦       ¦--sub-05_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 57       ¦       °--sub-05_task-balloonanalogrisktask_run-03_events.tsv 
#> 58       ¦--sub-06                                                      
#> 59       ¦   ¦--anat                                                    
#> 60       ¦   ¦   ¦--sub-06_T1w.nii.gz                                   
#> 61       ¦   ¦   °--sub-06_inplaneT2.nii.gz                             
#> 62       ¦   °--func                                                    
#> 63       ¦       ¦--sub-06_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 64       ¦       ¦--sub-06_task-balloonanalogrisktask_run-01_events.tsv 
#> 65       ¦       ¦--sub-06_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 66       ¦       ¦--sub-06_task-balloonanalogrisktask_run-02_events.tsv 
#> 67       ¦       ¦--sub-06_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 68       ¦       °--sub-06_task-balloonanalogrisktask_run-03_events.tsv 
#> 69       ¦--sub-07                                                      
#> 70       ¦   ¦--anat                                                    
#> 71       ¦   ¦   ¦--sub-07_T1w.nii.gz                                   
#> 72       ¦   ¦   °--sub-07_inplaneT2.nii.gz                             
#> 73       ¦   °--func                                                    
#> 74       ¦       ¦--sub-07_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 75       ¦       ¦--sub-07_task-balloonanalogrisktask_run-01_events.tsv 
#> 76       ¦       ¦--sub-07_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 77       ¦       ¦--sub-07_task-balloonanalogrisktask_run-02_events.tsv 
#> 78       ¦       ¦--sub-07_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 79       ¦       °--sub-07_task-balloonanalogrisktask_run-03_events.tsv 
#> 80       ¦--sub-08                                                      
#> 81       ¦   ¦--anat                                                    
#> 82       ¦   ¦   ¦--sub-08_T1w.nii.gz                                   
#> 83       ¦   ¦   °--sub-08_inplaneT2.nii.gz                             
#> 84       ¦   °--func                                                    
#> 85       ¦       ¦--sub-08_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 86       ¦       ¦--sub-08_task-balloonanalogrisktask_run-01_events.tsv 
#> 87       ¦       ¦--sub-08_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 88       ¦       ¦--sub-08_task-balloonanalogrisktask_run-02_events.tsv 
#> 89       ¦       ¦--sub-08_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 90       ¦       °--sub-08_task-balloonanalogrisktask_run-03_events.tsv 
#> 91       ¦--sub-09                                                      
#> 92       ¦   ¦--anat                                                    
#> 93       ¦   ¦   ¦--sub-09_T1w.nii.gz                                   
#> 94       ¦   ¦   °--sub-09_inplaneT2.nii.gz                             
#> 95       ¦   °--func                                                    
#> 96       ¦       ¦--sub-09_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 97       ¦       ¦--sub-09_task-balloonanalogrisktask_run-01_events.tsv 
#> 98       ¦       ¦--sub-09_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 99       ¦       ¦--sub-09_task-balloonanalogrisktask_run-02_events.tsv 
#> 100      ¦       °--... 2 nodes w/ 0 sub                                
#> 101      °--... 7 nodes w/ 72 sub                                       
# }
```
