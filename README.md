
# casecountapp

<!-- badges: start -->
[![R build status](https://github.com/WorldHealthOrganization/casecountapp/workflows/R-CMD-check/badge.svg)](https://github.com/WorldHealthOrganization/casecountapp/actions)
<!-- badges: end -->

This R package contains functions to create, view, and deploy interactive applications of multi-resolution geographical case count time series data.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("WorldHealthOrganization/casecountapp")
```

## Example

The code below creates a case counts app for the United States (using a set of data that comes with the package) at the country and state level.

```r
library(casecountapp)

# specify data sources
country_sources <- source_list(
  source_entry(source_id = "JHU", admin_level = 0,
    file = system.file("example-data/admin0/JHU.csv", package = "casecountapp")),
  source_entry(source_id = "NYT", admin_level = 0,
    file = system.file("example-data/admin0/NYT.csv", package = "casecountapp")),
  source_entry(source_id = "FACTS", admin_level = 0,
    file = system.file("example-data/admin0/FACTS.csv", package = "casecountapp")))

state_sources <- source_list(
  source_entry(source_id = "JHU", admin_level = 1,
    file = system.file("example-data/admin1/JHU.csv", package = "casecountapp")),
  source_entry(source_id = "NYT", admin_level = 1,
    file = system.file("example-data/admin1/NYT.csv", package = "casecountapp")),
  source_entry(source_id = "FACTS", admin_level = 1,
    file = system.file("example-data/admin1/FACTS.csv", package = "casecountapp")))

# register app
app <- register_app("US-Covid19", path = tempdir())

# build country display
country_display <- build_casecount_display(
  app,
  sources = country_sources,
  ref_source = "NYT",
  name = "United States",
  desc = "Covid-19 cases and deaths in the US",
  geo_link = geo_link_href(display = "States", ref_level = "states"),
  min_date = as.Date("2020-03-01"),
  order = 1,
  nrow = 1,
  ncol = 1,
  case_fatality_max = 12,
  thumb = system.file("thumbs/US/country.png", package = "casecountapp")
)

# build state display
state_display <- build_casecount_display(
  app,
  sources = state_sources,
  ref_source = "NYT",
  name = "States",
  desc = "Covid-19 cases and deaths in the US by state",
  views = default_views(
    ref_source = "NYT", comp_sources = c("JHU", "FACTS"), entity_pl = "states"),
  state = list(
    sort = list(trelliscopejs::sort_spec("cur_case_nyt", dir = "desc")),
    labels = list(), sidebar = 4),
  min_date = as.Date("2020-03-01"),
  order = 2,
  case_fatality_max = 12,
  thumb = system.file("thumbs/US/states.png", package = "casecountapp")
)

# view the app
view_app(app)
```
