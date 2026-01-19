# bettr 0.1.0

*Release date: 2026-01-21*

## Initial Release

This is the initial release of **BETtR**, submitted for ST403 coursework.
The package provides tools for structuring, visualising, and forecasting betting market odds data.

#### Core Functions

* `make_bettr()`: Convert raw betting data into structured `bettr_data` objects
  - Validates required columns (`event_id`, `logged_time`, `home_odds`, `away_odds`, `draw_odds`)
  - Handles missing values with optional removal via `drop_NA_values` argument
  - Optional conversion to `tsibble` format for time series analysis
  - Support for custom column names through flexible parameter mapping

* `plot.bettr_data()`: Interactive visualisation of odds movements
  - Plots percentage changes in opening or closing favourite odds over time
  - Interactive tooltips using `ggiraph` with fallback to static `ggplot2`
  - Support for custom fixture labels via `fixture` argument

* `predict.bettr_data()`: Time-series forecasting of betting odds
  - Three modelling approaches: ARIMA, ETS (exponential smoothing), and Skellam-based models
  - Automatic gap detection and interpolation for time series data
  - Flexible forecast horizons (default 36 hours)
  - Option to fit individual models or all models simultaneously

#### Data

* `football`: Built-in dataset featuring 1,573 observations from 10 English Premier League 2025 matches
  - Includes home, away, and draw odds

### Dependencies

* Core dependencies: `dplyr`, `ggplot2`, `tsibble`, `fabletools`, `fable`, `forecast`, `skellam`, `ggiraph`, `lubridate`, `magrittr`
* Zesty dependency: `genzplyr` (via GitHub remote)

### Documentation

* Comprehensive Roxygen2 documentation for all exported functions
* Package-level documentation accessible via `?bettr`
* Vignette demonstrating package workflow
* Test suite using `testthat` (edition 3)

---

## Future Development Plans

The following features are planned for future versions:

### Version 0.2.0 (Pending approval)

* **Enhanced prediction models**: Investigate additional forecasting approaches
  - Machine learning models (e.g., random forests, gradient boosting)
  - Ensemble methods combining multiple model predictions

* **Extended plotting functionality**: Add `odd` argument options
  - `"winner"`: Track odds for the ultimate match winner
  - `"all"`: Display all three outcomes (home/draw/away) simultaneously
  - Custom odds column selection for user-defined metrics

* **Performance improvements**: Optimise internal functions for larger datasets

Bug reports and feature requests can be submitted at:
<https://github.com/ThatObiGuy/bettr/issues>
