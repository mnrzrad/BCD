# BCD <img src="man/figures/logo.png" align="right" width="25%"/>

[![R-CMD-check](https://github.com/mnrzrad/BCD/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mnrzrad/BCD/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/BCD)](https://cran.r-project.org/package=BCD)
[![Downloads per
month](https://cranlogs.r-pkg.org/badges/BCD)](https://cran.r-project.org/package=BCD)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/BCD)](https://cran.r-project.org/package=BCD)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

The R package *BCD* provides core functions for the implementation of bivariate binomial, geometric, and Poisson distributions based on conditional specifications. The package also includes tools for data generation and goodness-of-fit testing for these three distribution families. See [Ghosh et al. (2025)](https://www.tandfonline.com/doi/full/10.1080/03610926.2024.2315294), [Ghosh et al. (2023)](https://www.tandfonline.com/doi/full/10.1080/03610918.2021.2004419), and [Ghosh et al. (2021)](https://www.tandfonline.com/doi/full/10.1080/02664763.2020.1793307).

This work is funded by national funds through the FCT - Fundação para a
Ciência e a Tecnologia, I.P., under the scope of the projects
UIDB/00297/2020 and UIDP/00297/2020 (Center for Mathematics and
Applications)".

## Installation

You can install the **stable** version from
[CRAN](https://cran.r-project.org/package=BCD).

``` s
install.packages('BCD', dependencies = TRUE)
```

You can install the **development** version from
[Github](https://github.com/mnrzrad/BCD)

``` s
# install.packages("remotes")
remotes::install_github("mnrzrad/BCD")
```

## To cite package `BCD` in publications use:

Norouzirad, M., Marques, F. J., Ghosh, I. (2025) *BCD: Bivariate Distributions via Conditional Specification*. R package version 0.1.1,
<https://cran.r-project.org/package=BCD>.

A BibTeX entry for LaTeX users is

@Manual{BCD, 
title = {BCD: Bivariate Distributions via Conditional Specification},
author = {Mina Norouzirad and Filipe J. Marques and Indranil Shosh},
year = {2025}, 
note = {R package version 0.1.1},
url = {<https://cran.r-project.org/package=BCD>} }

## License

This package is free and open source software, licensed under GPL-3.
