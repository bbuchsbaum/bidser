# Check and clean confound tables

`check_confounds()` reports nuisance columns that are unsuitable for
model matrices, such as zero-variance columns within a run.
`clean_confounds()` drops the flagged columns and stores the diagnostics
on the returned object.

## Usage

``` r
check_confounds(x, checks = c("zero_variance", "rank"), group_vars = NULL)

clean_confounds(
  x,
  clean = c("zero_variance", "rank"),
  group_vars = NULL,
  inform = TRUE
)
```

## Arguments

- x:

  A confound table, typically a `bids_confounds` object.

- checks:

  Character vector of checks to run. Supported values are
  `"zero_variance"` and `"rank"`.

- group_vars:

  Optional character vector of columns defining run-level groups for
  flat tables.

- clean:

  Character vector of cleaning operations to apply. Supported values are
  `"none"`, `"zero_variance"`, and `"rank"`.

- inform:

  Logical. If `TRUE`, report dropped columns with run labels.

## Value

`check_confounds()` returns a tibble of diagnostics. `clean_confounds()`
returns `x` with flagged columns removed and a `confound_diagnostics`
attribute containing the same diagnostic rows.

## Details

These helpers understand both nested `bids_confounds` objects returned
by `read_confounds(..., nest = TRUE)` and flat confound tables. For flat
tables, checks are run within the identifier columns present in the data
(`participant_id`, `task`, `session`, and `run` by default).

## Examples

``` r
df <- tibble::tibble(
  participant_id = "01",
  task = "rest",
  run = "01",
  cosine00 = c(1, 0, -1),
  cosine01 = c(0, 0, 0)
)
check_confounds(df)
#> # A tibble: 1 × 11
#>   participant_id task  session run   source role     column  reason action    sd
#>   <chr>          <chr> <chr>   <chr> <chr>  <chr>    <chr>   <chr>  <chr>  <dbl>
#> 1 01             rest  NA      01    NA     confound cosine… zero_… flag       0
#> # ℹ 1 more variable: rank <int>
clean_confounds(df)
#> Dropped zero-variance confounds:
#> # A tibble: 3 × 4
#>   participant_id task  run   cosine00
#>   <chr>          <chr> <chr>    <dbl>
#> 1 01             rest  01           1
#> 2 01             rest  01           0
#> 3 01             rest  01          -1
```
