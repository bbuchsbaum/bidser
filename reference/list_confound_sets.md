# List available confound sets

Returns the names and short descriptions of the predefined confound sets
usable with
[`confound_set()`](https://bbuchsbaum.github.io/bidser/reference/confound_set.md).

## Usage

``` r
list_confound_sets()
```

## Value

A data.frame with columns `set` and `description`.

## Examples

``` r
list_confound_sets()
#>               set
#> 1         motion6
#> 2        motion12
#> 3        motion24
#> 4         global3
#> 5              9p
#> 6             36p
#> 7        acompcor
#> 8        tcompcor
#> 9         compcor
#> 10         cosine
#> 11       outliers
#> 12          dvars
#> 13             fd
#> 14 legacy_default
#>                                                                     description
#> 1                                                  Rigid-body motion (6 params)
#> 2                                               Motion + first derivatives (12)
#> 3                                        Friston 24-parameter motion model (24)
#> 4                                               CSF, WM, and global signals (3)
#> 5                      9-parameter model: motion6 + CSF + WM + GlobalSignal (9)
#> 6            36-parameter model: motion24 + globals with derivs/quadratics (36)
#> 7                                Anatomical CompCor components (use n to limit)
#> 8                                  Temporal CompCor components (use n to limit)
#> 9                         Both anatomical and temporal CompCor (use n to limit)
#> 10                                             Discrete cosine basis regressors
#> 11               FD/RMSD, motion spike regressors, and nonsteady-state outliers
#> 12             DVARS family (dvars, std_dvars, non_std_dvars, vx_wisestd_dvars)
#> 13                                                  Framewise displacement only
#> 14 Legacy read_confounds() default = former DEFAULT_CVARS2 (26 canonical names)
```
