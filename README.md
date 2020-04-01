

## Overview

The `TBDfun` package is a miscellaneous collection of utility functions, reference datasets, and other tools developed by [TBD Solutions'](https://www.tbdsolutions.com/) knowledge services team and collaborators.

## Installation

This package is not on CRAN.  To install the working version, use `devtools`:

```
devtools::install_github("j-hagedorn/TBDfun", ref = "master")
```

## Getting Started

Load the package using:

```
library(TBDfun)
```

In order to allow for easy auto-completion using the IDE, functions in this package have the prefix `tbd_`.  Wherever possible, the first argument passed to the function is a dataframe, which allows for use in piped chains (i.e. using `%>%`).
