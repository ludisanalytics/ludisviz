# ludisviz Package

**ludisviz** is an R package that enables users to create basic data visualizations of match-level and season-level stats, developed for use on the [Ludis Analytics](https://www.ludisanalytics.com/) platform.

The package provides two types of functions:
1) Functions that manipulate dataframes of stats to format them for data visualization (see [**format_data.R**](/R/format_data.R)).
2) Functions that create ggplot object visualizations using the formatted data frames (see [**visualize_data.R**](/R/visualize_data.R)). 
These visualizations are optimized to be viewed using the [**plotly::ggplotly()**](https://www.rdocumentation.org/packages/plotly/versions/4.9.3/topics/ggplotly) function, which converts them to interactive plotly objects.

## Installation
Latest stable version:
``` R
devtools::install_github("ludisanalytics/ludisviz")
```

- If you do not have the [devtools](https://github.com/hadley/devtools) package, you will need to install it before attempting the package installation:
``` R
install.packages("devtools")
```
