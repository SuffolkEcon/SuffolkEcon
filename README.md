# `SuffolkEcon`

`SuffolkEcon` is an experimental R package for the Department of Economics at Suffolk University to help teach introductory concepts in statistics, econometrics and optimization.  

## Installation

Currently the package is only available on GitHub. To install it, first install the `remotes` package:

```r
install.packages('remotes')
```

Then you can install `SuffolkEcon`:

```r
remotes::install_github('SuffolkEcon/SuffolkEcon')
```

and load it in an R script or an R notebook in the usual way:

```r
library(SuffolkEcon)
```


## Why this package? 


The package is not meant to substitute for other packages in the R ecosystem that students will learn and use inside and outside of introductory courses. 

Instead, `SuffolkEcon` aims to make a more marginal contribution to teaching basic concepts and reducing handoff frictions between classes by providing some one-line routines to do things like:

* run "what-if" experiments using real data (e.g., bootstrapping sample statistics or linear models)
* plotting distributions and probabilities, and
* plotting user-defined functions and their derivatives, roots and optima. 

In the spirit of fitting into the greater package network, `SuffolkEcon` is built with [tidyverse](https://www.tidyverse.org/) principles and is meant to be loaded alongside the `tidyverse` (`SuffolkEcon` imports `dplyr`, `ggplot`, `stringr` and `rlang`). All `SuffolkEcon` functions return either `data.frame` or `ggplot` objects.

## Contributing

This package is very much a work in progress, so contributions and bug reports are welcome. Drop us a line at ldegeest [at] suffolk [dot] edu, or report a bug on [GitHub](https://github.com/SuffolkEcon/SuffolkEcon/issues).

## Acknowledgements

This package was funded by a grant from the [Center for Teaching and Scholarly Excellence](https://www.suffolk.edu/academics/research-at-suffolk/center-for-teaching-scholarly-excellence) at Suffolk University.

## License

The source code of this package is licensed under the MIT License.
