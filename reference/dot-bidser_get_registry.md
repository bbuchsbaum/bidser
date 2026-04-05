# Datatype registry for bidser

The registry stores named entries for each BIDS datatype known to the
package (built-ins) plus any user-registered custom datatypes. It is
initialised during `.onLoad` via `.register_builtin_datatypes()`.

## Usage

``` r
.bidser_get_registry()
```

## Details

The registry lives on the package-level environment `bidser_pkg_env`
which is defined in `R/bidser_env.R`.
