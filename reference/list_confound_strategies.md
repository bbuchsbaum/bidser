# List available confound strategies

Returns the names and short descriptions of the predefined confound
strategies usable with
[`confound_strategy()`](https://bbuchsbaum.github.io/bidser/reference/confound_strategy.md).

## Usage

``` r
list_confound_strategies()
```

## Value

A data.frame with columns `strategy` and `description`.

## Examples

``` r
list_confound_strategies()
#>     strategy                                                      description
#> 1 pcabasic80 PCA(motion24 + aCompCor + tCompCor + CSF + WM, 80% var) + cosine
```
