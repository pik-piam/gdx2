# Interface package for GDX files in R

R package **gdx2**, version **0.1.4**

[![CRAN status](https://www.r-pkg.org/badges/version/gdx2)](https://cran.r-project.org/package=gdx2)  [![R build status](https://github.com/pik-piam/gdx2/workflows/check/badge.svg)](https://github.com/pik-piam/gdx2/actions) [![codecov](https://codecov.io/gh/pik-piam/gdx2/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/gdx2) 

## Purpose and Functionality

A wrapper package for the gamstransfer package extending its functionality
    and allowing to read GDX files directly in R. It is emulating the basic features of the readGDX function in the gdx package
    but now based on gamstransfer instead of gdxrrw which served as a basis for gdx.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("gdx2")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **gdx2** in publications use:

Dietrich J (2024). _gdx2: Interface package for GDX files in R_. R package version 0.1.4, <URL: https://github.com/pik-piam/gdx2>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {gdx2: Interface package for GDX files in R},
  author = {Jan Philipp Dietrich},
  year = {2024},
  note = {R package version 0.1.4},
  url = {https://github.com/pik-piam/gdx2},
}
```
