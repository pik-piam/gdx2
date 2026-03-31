# calcScaling

This function creates a GAMS file with scaling of variables. The scaling
is calculated based on a gdx file containing all variables of a run.

## Usage

``` r
calcScaling(gdx, file = NULL, magnitude = 2)
```

## Arguments

- gdx:

  path to a gdx file

- file:

  A file name the scaling GAMS code should be written to. If NULL the
  code is returned by the function

- magnitude:

  The order of magnitude for which variables should be scaled. All
  variables with average absolute values which are either below
  10^(-magnitude) or above 10^(magnitude) will be scaled.

## Value

A vector with the scaling GAMS code if file=NULL, otherwise nothing is
returned.

## See also

[`readGDX`](readGDX.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
calcScaling("fulldata.gdx")
} # }
```
