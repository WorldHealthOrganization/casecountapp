# file.copy(sources_us[[1]]$file, "inst/example-data/admin0/JHU.csv")
# file.copy(sources_us[[2]]$file, "inst/example-data/admin0/NYT.csv")
# file.copy(sources_us[[3]]$file, "inst/example-data/admin0/FACTS.csv")
# file.copy(sources_us[[4]]$file, "inst/example-data/admin1/JHU.csv")
# file.copy(sources_us[[5]]$file, "inst/example-data/admin1/NYT.csv")
# file.copy(sources_us[[6]]$file, "inst/example-data/admin1/FACTS.csv") 

country_sources <- list(
  list(source_id = "JHU", admin_level = 0,
    file = system.file("example-data/admin0/JHU.csv", package = "casecountapp")),
  list(source_id = "NYT", admin_level = 0,
    file = system.file("example-data/admin0/NYT.csv", package = "casecountapp")),
  list(source_id = "FACTS", admin_level = 0,
    file = system.file("example-data/admin0/FACTS.csv", package = "casecountapp")))

state_sources <- list(
  list(source_id = "JHU", admin_level = 1,
    file = system.file("example-data/admin1/JHU.csv", package = "casecountapp")),
  list(source_id = "NYT", admin_level = 1,
    file = system.file("example-data/admin1/NYT.csv", package = "casecountapp")),
  list(source_id = "FACTS", admin_level = 1,
    file = system.file("example-data/admin1/FACTS.csv", package = "casecountapp")))

test_that("app creation works", {

  app <- register_app("US-Covid19")

  country_display <- build_casecount_display(
    app,
    sources = country_sources,
    ref_source = "NYT",
    name = "United States",
    desc = "Covid-19 cases and deaths in the US",
    geo_links = list(list(
      display = "States",
      variable = "admin0_name",
      cur_level = "country",
      ref_level = "states",
      type = "href"
    )),
    order = 1,
    nrow = 1,
    ncol = 1,
    case_fatality_max = 12,
    thumb = system.file("thumbs/US/country.png", package = "casecountapp")
  )

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
    order = 2,
    case_fatality_max = 12,
    thumb = system.file("thumbs/US/states.png", package = "casecountapp")
  )
})
