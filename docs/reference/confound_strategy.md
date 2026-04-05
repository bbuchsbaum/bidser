# Confound denoising strategies

Creates a structured confound strategy object that specifies which
variables to reduce via PCA and which to keep as-is. Pass the result
directly to `read_confounds(..., cvars = confound_strategy(...))`.

## Usage

``` r
confound_strategy(
  name = NULL,
  pca_vars = NULL,
  raw_vars = NULL,
  perc_var = -1,
  npcs = -1
)
```

## Arguments

- name:

  Character. Name of a predefined strategy (see above), or `NULL` for a
  custom strategy.

- pca_vars:

  Character vector of confound names/wildcards to include in PCA
  reduction. Ignored when `name` is specified.

- raw_vars:

  Character vector of confound names/wildcards to keep without
  reduction. Ignored when `name` is specified.

- perc_var:

  Numeric. Percentage of variance to retain from PCA (default -1,
  meaning use `npcs` instead).

- npcs:

  Integer. Number of PCs to retain (default -1, meaning use `perc_var`
  instead).

## Value

A `confound_strategy` object (S3 class) that can be passed as the
`cvars` argument to
[`read_confounds()`](https://bbuchsbaum.github.io/bidser/reference/read_confounds.md).

## Details

When a strategy is passed to `read_confounds`, the function:

1.  Selects the `pca_vars` columns and reduces them via PCA (retaining
    `perc_var`\\

2.  Selects the `raw_vars` columns and keeps them unchanged.

3.  Column-binds the PCA scores with the raw columns.

Available named strategies:

- `"pcabasic80"`:

  PCA over motion24 + aCompCor + tCompCor + CSF + white matter,
  retaining 80\\ Discrete cosine regressors are appended un-reduced.

## Examples

``` r
# Named strategy
confound_strategy("pcabasic80")
#> $name
#> [1] "pcabasic80"
#> 
#> $pca_vars
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
#> [25] "csf"                        "white_matter"              
#> [27] "a_comp_cor_*"               "t_comp_cor_*"              
#> 
#> $raw_vars
#> [1] "cosine_*" "cosine*" 
#> 
#> $perc_var
#> [1] 80
#> 
#> $npcs
#> [1] -1
#> 
#> attr(,"class")
#> [1] "confound_strategy"

# Custom strategy: PCA motion + compcor to 5 PCs, keep cosine regressors
confound_strategy(
  pca_vars = c(confound_set("motion24"), confound_set("compcor")),
  raw_vars = confound_set("cosine"),
  npcs = 5
)
#> $name
#> [1] "custom"
#> 
#> $pca_vars
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
#> [25] "a_comp_cor_*"               "t_comp_cor_*"              
#> 
#> $raw_vars
#> [1] "cosine_*" "cosine*" 
#> 
#> $perc_var
#> [1] -1
#> 
#> $npcs
#> [1] 5
#> 
#> attr(,"class")
#> [1] "confound_strategy"
```
