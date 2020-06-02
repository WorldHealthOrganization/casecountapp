#' @import geoutils
geo_data <- list(
  admin0 = geoutils::admin0,
  admin1 = geoutils::admin1,
  admin2 = geoutils::admin2,
  continents = geoutils::continents,
  who_regions = geoutils::who_regions
)

# internal (maybe expose later)
get_geo_level <- function(sources) {
  geo_level <- sources[[1]]$admin_level
  if (geo_level %in% c(0:2))
    geo_level <- paste0("admin", geo_level)
  geo_level
}

get_geo_higher_level <- function(sources) {
  geo_level <- sources[[1]]$admin_level
  if (geo_level %in% c(1:2)) {
    geo_level <- paste0("admin", geo_level - 1)
  } else {
    message("Higher geo level set to 'NA'")
    geo_level <- NA
  }
  geo_level
}

#' @importFrom tidyr nest
# internal
get_casecount_data <- function(sources, ref_source) {
  lvl <- unique(unlist(lapply(sources, function(x) x$admin_level)))
  if (length(lvl) > 1)
    stop("Cannot assemble data that comes from multiple admin levels.",
      call. = FALSE)

  ids <- unlist(lapply(sources, function(x) x$source_id))
  if (length(unique(ids)) != length(sources))
    stop("Each source must have a unique source_id.\nFound: ",
      paste(ids, collapse = ","), " but there are ",
      length(sources), " sources.",
      call. = FALSE)

  d <- lapply(sources, function(x) {
    suppressMessages(readr::read_csv(x$file, na = "")) %>%
      dplyr::mutate(source = x$source_id)
  }) %>%
  dplyr::bind_rows()

  if (!ref_source %in% ids)
    stop("ref_source: ", ref_source, " is not in the list of sources.",
      call. = FALSE)
  ids <- c(ref_source, setdiff(ids, ref_source))
  d$source <- factor(d$source, levels = ids)

  geo_obj <- get_geo_level(sources)

  if (geo_obj == "global")
    d$global_code <- "GL"

  # nest
  code_vars <- names(d)[grepl("_code$", names(d))]
  if (length(code_vars) == 0)
    stop("Could not find any geo codes in the data.")
  d <- d %>%
    dplyr::group_by_at(code_vars) %>%
    tidyr::nest() %>%
    dplyr::ungroup()

  # merge geo data
  gu_data_nms <- a <- utils::data(package = "geoutils")$results[, "Item"]
  if (!geo_obj %in% gu_data_nms)
    stop("Could not find data in geoutils package matching admin_level: ", lvl,
      call. = FALSE)
  geo_dat <- get(utils::data(list = geo_obj, package = "geoutils"))

  d2 <- dplyr::inner_join(d, geo_dat, by = code_vars)

  if (nrow(d2) < nrow(d)) {
    a <- dplyr::anti_join(
      dplyr::select(d, dplyr::one_of(code_vars)),
      dplyr::select(geo_dat, dplyr::one_of(code_vars)),
      by = code_vars
    )
    message("Data removed for the following geographic regions because ",
      "matching geo data in '", geo_obj, "' was not found.")
    message(paste(format(a)[-3], collapse = "\n"))
  }

  d2
}
