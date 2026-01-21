# BETtR <img src="https://raw.githubusercontent.com/ThatObiGuy/BETtR/refs/heads/main/tempLogo.png" align="right" width=120 height=139 alt="" />

BETtR. An R package for structuring, visualising, and exploring betting market odds data.

## Overview

BETtR is an R package for analysing sports betting odds data over time. It allows users to organise odds data, visualise how markets evolve before an event, and explore time-series–based forecasting methods for odds movements.

## Installation

BETtR is under active development and is not available on CRAN. It can be installed from GitHub.

To install the package **with the vignette built**, run:

```r
install.packages("devtools")
devtools::install_github("ThatObiGuy/BETtR", build_vignettes = TRUE)
```

After successful installation you can proceed with loading the package as such:

```r
library(bettr)
```

For a more comprehensive guide other than `?bettr`, run the following command to get full introduction, including worked examples, in the package vignette:

```r
vignette("BETtR", package="bettr")
```

## Basic Usage

### Convert raw odds data into a bettr_data object
```r
bettr_obj <- make_bettr(raw_odds_data, make_tsibble = TRUE)
```

### Visualise odds movement
```r
plot(bettr_obj)
```

### Explore predictive models
```r
pred <- predict(bettr_obj, odds = "home_odds", h = 24, model = "skellam")
```

<img src="https://raw.githubusercontent.com/ThatObiGuy/BETtR/refs/heads/main/plan.jpg"/>
