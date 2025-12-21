# BETtR <img src="https://raw.githubusercontent.com/ThatObiGuy/BETtR/refs/heads/main/tempLogo.png" align="right" width=120 height=139 alt="" />

BETtR. An R package for structuring, visualising, and exploring betting market odds data.

## Overview

BETtR provides tools for working with betting odds data observed over time. It introduces a custom S3 class, `bettr_data`, built on top of `tsibble`, and includes methods for visualisation and exploratory prediction using multiple time-series models.

## Installation

BETtR is under active development and can be installed from GitHub:

```r
install.packages("devtools")
devtools::install_github("ThatObiGuy/BETtR")
```

After successful installation you can proceed with loading the package as such:

```r
library(bettr)
```

For a more comprehensive guide other than `?bettr`, run the following command to get a detailed introduction and examples are available in the package vignette:

```r
vignette("BETtR", package="BETtR")
```

## Basic Usage

### Convert raw odds data into a bettr_data object
```r
bettr_obj <- make_bettr(raw_odds_data)
```

### Visualise odds movement
```r
plot(bettr_obj)
```

### Explore predictive models
```r
pred <- predict(bettr_obj, h = 67)
```

<img src="https://raw.githubusercontent.com/ThatObiGuy/BETtR/refs/heads/main/plan.jpg"/>
