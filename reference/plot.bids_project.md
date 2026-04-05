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
#> 4        ¦   ¦--func                                                    
#> 5        ¦   ¦   ¦--sub-01_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 6        ¦   ¦   ¦--sub-01_task-balloonanalogrisktask_run-01_events.tsv 
#> 7        ¦   ¦   ¦--sub-01_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 8        ¦   ¦   ¦--sub-01_task-balloonanalogrisktask_run-02_events.tsv 
#> 9        ¦   ¦   ¦--sub-01_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 10       ¦   ¦   °--sub-01_task-balloonanalogrisktask_run-03_events.tsv 
#> 11       ¦   °--anat                                                    
#> 12       ¦       ¦--sub-01_T1w.nii.gz                                   
#> 13       ¦       °--sub-01_inplaneT2.nii.gz                             
#> 14       ¦--sub-02                                                      
#> 15       ¦   ¦--func                                                    
#> 16       ¦   ¦   ¦--sub-02_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 17       ¦   ¦   ¦--sub-02_task-balloonanalogrisktask_run-01_events.tsv 
#> 18       ¦   ¦   ¦--sub-02_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 19       ¦   ¦   ¦--sub-02_task-balloonanalogrisktask_run-02_events.tsv 
#> 20       ¦   ¦   ¦--sub-02_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 21       ¦   ¦   °--sub-02_task-balloonanalogrisktask_run-03_events.tsv 
#> 22       ¦   °--anat                                                    
#> 23       ¦       ¦--sub-02_T1w.nii.gz                                   
#> 24       ¦       °--sub-02_inplaneT2.nii.gz                             
#> 25       ¦--sub-03                                                      
#> 26       ¦   ¦--func                                                    
#> 27       ¦   ¦   ¦--sub-03_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 28       ¦   ¦   ¦--sub-03_task-balloonanalogrisktask_run-01_events.tsv 
#> 29       ¦   ¦   ¦--sub-03_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 30       ¦   ¦   ¦--sub-03_task-balloonanalogrisktask_run-02_events.tsv 
#> 31       ¦   ¦   ¦--sub-03_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 32       ¦   ¦   °--sub-03_task-balloonanalogrisktask_run-03_events.tsv 
#> 33       ¦   °--anat                                                    
#> 34       ¦       ¦--sub-03_T1w.nii.gz                                   
#> 35       ¦       °--sub-03_inplaneT2.nii.gz                             
#> 36       ¦--sub-04                                                      
#> 37       ¦   ¦--func                                                    
#> 38       ¦   ¦   ¦--sub-04_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 39       ¦   ¦   ¦--sub-04_task-balloonanalogrisktask_run-01_events.tsv 
#> 40       ¦   ¦   ¦--sub-04_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 41       ¦   ¦   ¦--sub-04_task-balloonanalogrisktask_run-02_events.tsv 
#> 42       ¦   ¦   ¦--sub-04_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 43       ¦   ¦   °--sub-04_task-balloonanalogrisktask_run-03_events.tsv 
#> 44       ¦   °--anat                                                    
#> 45       ¦       ¦--sub-04_T1w.nii.gz                                   
#> 46       ¦       °--sub-04_inplaneT2.nii.gz                             
#> 47       ¦--sub-05                                                      
#> 48       ¦   ¦--func                                                    
#> 49       ¦   ¦   ¦--sub-05_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 50       ¦   ¦   ¦--sub-05_task-balloonanalogrisktask_run-01_events.tsv 
#> 51       ¦   ¦   ¦--sub-05_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 52       ¦   ¦   ¦--sub-05_task-balloonanalogrisktask_run-02_events.tsv 
#> 53       ¦   ¦   ¦--sub-05_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 54       ¦   ¦   °--sub-05_task-balloonanalogrisktask_run-03_events.tsv 
#> 55       ¦   °--anat                                                    
#> 56       ¦       ¦--sub-05_T1w.nii.gz                                   
#> 57       ¦       °--sub-05_inplaneT2.nii.gz                             
#> 58       ¦--sub-06                                                      
#> 59       ¦   ¦--func                                                    
#> 60       ¦   ¦   ¦--sub-06_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 61       ¦   ¦   ¦--sub-06_task-balloonanalogrisktask_run-01_events.tsv 
#> 62       ¦   ¦   ¦--sub-06_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 63       ¦   ¦   ¦--sub-06_task-balloonanalogrisktask_run-02_events.tsv 
#> 64       ¦   ¦   ¦--sub-06_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 65       ¦   ¦   °--sub-06_task-balloonanalogrisktask_run-03_events.tsv 
#> 66       ¦   °--anat                                                    
#> 67       ¦       ¦--sub-06_T1w.nii.gz                                   
#> 68       ¦       °--sub-06_inplaneT2.nii.gz                             
#> 69       ¦--sub-07                                                      
#> 70       ¦   ¦--func                                                    
#> 71       ¦   ¦   ¦--sub-07_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 72       ¦   ¦   ¦--sub-07_task-balloonanalogrisktask_run-01_events.tsv 
#> 73       ¦   ¦   ¦--sub-07_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 74       ¦   ¦   ¦--sub-07_task-balloonanalogrisktask_run-02_events.tsv 
#> 75       ¦   ¦   ¦--sub-07_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 76       ¦   ¦   °--sub-07_task-balloonanalogrisktask_run-03_events.tsv 
#> 77       ¦   °--anat                                                    
#> 78       ¦       ¦--sub-07_T1w.nii.gz                                   
#> 79       ¦       °--sub-07_inplaneT2.nii.gz                             
#> 80       ¦--sub-08                                                      
#> 81       ¦   ¦--func                                                    
#> 82       ¦   ¦   ¦--sub-08_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 83       ¦   ¦   ¦--sub-08_task-balloonanalogrisktask_run-01_events.tsv 
#> 84       ¦   ¦   ¦--sub-08_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 85       ¦   ¦   ¦--sub-08_task-balloonanalogrisktask_run-02_events.tsv 
#> 86       ¦   ¦   ¦--sub-08_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 87       ¦   ¦   °--sub-08_task-balloonanalogrisktask_run-03_events.tsv 
#> 88       ¦   °--anat                                                    
#> 89       ¦       ¦--sub-08_T1w.nii.gz                                   
#> 90       ¦       °--sub-08_inplaneT2.nii.gz                             
#> 91       ¦--sub-09                                                      
#> 92       ¦   ¦--func                                                    
#> 93       ¦   ¦   ¦--sub-09_task-balloonanalogrisktask_run-01_bold.nii.gz
#> 94       ¦   ¦   ¦--sub-09_task-balloonanalogrisktask_run-01_events.tsv 
#> 95       ¦   ¦   ¦--sub-09_task-balloonanalogrisktask_run-02_bold.nii.gz
#> 96       ¦   ¦   ¦--sub-09_task-balloonanalogrisktask_run-02_events.tsv 
#> 97       ¦   ¦   ¦--sub-09_task-balloonanalogrisktask_run-03_bold.nii.gz
#> 98       ¦   ¦   °--sub-09_task-balloonanalogrisktask_run-03_events.tsv 
#> 99       ¦   °--anat                                                    
#> 100      ¦       °--... 2 nodes w/ 0 sub                                
#> 101      °--... 7 nodes w/ 72 sub                                       
# }
```
