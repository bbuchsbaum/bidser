# Convenience confound sets for fMRIPrep

Provides predefined, version-robust groups of confound variable names as
described in the fMRIPrep documentation. These sets abstract over naming
changes between fMRIPrep releases via the internal alias resolver used
by
[`read_confounds()`](https://bbuchsbaum.github.io/bidser/reference/read_confounds.md).

## Usage

``` r
confound_set(name, n = NULL)
```

## Arguments

- name:

  Character. The name of the convenience set (see list above).

- n:

  Optional integer used by CompCor sets to limit the number of
  components (e.g., first 5 or 6). Ignored for other sets.

## Value

A character vector of confound variable names and/or wildcard tokens
that can be passed to `read_confounds(..., cvars = confound_set(...))`.

## Details

In addition to exact names, wildcard patterns are supported by the
confound resolver:

- `prefix*` selects all columns that start with `prefix` (e.g.,
  `cosine_*`, `motion_outlier_*`, `a_comp_cor_*`).

- `prefix*[N]` selects the first `N` matches (e.g., `a_comp_cor_*[6]`).

- Suffix combinations such as `_derivative1`, `_power2`, and
  `_derivative1_power2` are resolved across both old and new base names
  (e.g., `trans_x_derivative1` also finds `X_derivative1` if present).

Available sets (case-insensitive):

- `"motion6"`: 6 rigid-body motion parameters: `trans_x`, `trans_y`,
  `trans_z`, `rot_x`, `rot_y`, `rot_z`.

- `"motion12"`: `motion6` + first temporal derivatives (adds
  `*_derivative1`).

- `"motion24"`: `motion12` + quadratic terms of base and derivatives
  (adds `*_power2` and `*_derivative1_power2`).

- `"global3"`: global signals: `csf`, `white_matter`, `global_signal`.

- `"9p"`: `motion6` + `global3` (9 parameters total).

- `"36p"`: `motion24` + `global3` plus their derivatives and quadratics
  (i.e., the canonical 36-parameter set).

- `"acompcor"`: anatomical CompCor components (`a_comp_cor_*`). Use `n`
  to cap the number of components retained, e.g., `n = 6` -\>
  `a_comp_cor_*[6]`.

- `"tcompcor"`: temporal CompCor components (`t_comp_cor_*`). Supports
  `n` as above.

- `"compcor"`: both anatomical and temporal CompCor (applies `n` to each
  family if provided).

- `"cosine"`: discrete cosine-basis regressors (matches both `cosine_*`
  and `cosine*`).

- `"outliers"`: outlier/censoring covariates including
  `framewise_displacement`, `rmsd` (if present), `motion_outlier_*`, and
  `non_steady_state_outlier*`.

- `"dvars"`: DVARS family: `dvars`, `std_dvars`, `non_std_dvars`,
  `vx_wisestd_dvars` (resolved to whichever names exist in your
  dataset).

- `"fd"`: framewise displacement only (`framewise_displacement`).

## Examples

``` r
# Common usage: 24-parameter motion set
confound_set("motion24")
#>  [1] "trans_x"                    "trans_y"                   
#>  [3] "trans_z"                    "rot_x"                     
#>  [5] "rot_y"                      "rot_z"                     
#>  [7] "trans_x_derivative1"        "trans_y_derivative1"       
#>  [9] "trans_z_derivative1"        "rot_x_derivative1"         
#> [11] "rot_y_derivative1"          "rot_z_derivative1"         
#> [13] "trans_x_power2"             "trans_y_power2"            
#> [15] "trans_z_power2"             "rot_x_power2"              
#> [17] "rot_y_power2"               "rot_z_power2"              
#> [19] "trans_x_derivative1_power2" "trans_y_derivative1_power2"
#> [21] "trans_z_derivative1_power2" "rot_x_derivative1_power2"  
#> [23] "rot_y_derivative1_power2"   "rot_z_derivative1_power2"  

# 36-parameter model (Satterthwaite/Friston-style)
confound_set("36p")
#>  [1] "trans_x"                          "trans_y"                         
#>  [3] "trans_z"                          "rot_x"                           
#>  [5] "rot_y"                            "rot_z"                           
#>  [7] "trans_x_derivative1"              "trans_y_derivative1"             
#>  [9] "trans_z_derivative1"              "rot_x_derivative1"               
#> [11] "rot_y_derivative1"                "rot_z_derivative1"               
#> [13] "trans_x_power2"                   "trans_y_power2"                  
#> [15] "trans_z_power2"                   "rot_x_power2"                    
#> [17] "rot_y_power2"                     "rot_z_power2"                    
#> [19] "trans_x_derivative1_power2"       "trans_y_derivative1_power2"      
#> [21] "trans_z_derivative1_power2"       "rot_x_derivative1_power2"        
#> [23] "rot_y_derivative1_power2"         "rot_z_derivative1_power2"        
#> [25] "csf"                              "white_matter"                    
#> [27] "global_signal"                    "csf_derivative1"                 
#> [29] "white_matter_derivative1"         "global_signal_derivative1"       
#> [31] "csf_power2"                       "white_matter_power2"             
#> [33] "global_signal_power2"             "csf_derivative1_power2"          
#> [35] "white_matter_derivative1_power2"  "global_signal_derivative1_power2"

# First 6 anatomical CompCor components
confound_set("acompcor", n = 6)
#> [1] "a_comp_cor_*[6]"

# All cosine regressors and outlier indicators
confound_set("cosine")
#> [1] "cosine_*" "cosine*" 
confound_set("outliers")
#> [1] "framewise_displacement"    "rmsd"                     
#> [3] "motion_outlier_*"          "non_steady_state_outlier*"
```
