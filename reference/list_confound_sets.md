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
#>         set                                                        description
#> 1   motion6                                       Rigid-body motion (6 params)
#> 2  motion12                                    Motion + first derivatives (12)
#> 3  motion24                             Friston 24-parameter motion model (24)
#> 4   global3                                    CSF, WM, and global signals (3)
#> 5        9p           9-parameter model: motion6 + CSF + WM + GlobalSignal (9)
#> 6       36p 36-parameter model: motion24 + globals with derivs/quadratics (36)
#> 7  acompcor                     Anatomical CompCor components (use n to limit)
#> 8  tcompcor                       Temporal CompCor components (use n to limit)
#> 9   compcor              Both anatomical and temporal CompCor (use n to limit)
#> 10   cosine                                   Discrete cosine basis regressors
#> 11 outliers     FD/RMSD, motion spike regressors, and nonsteady-state outliers
#> 12    dvars   DVARS family (dvars, std_dvars, non_std_dvars, vx_wisestd_dvars)
#> 13       fd                                        Framewise displacement only
```
