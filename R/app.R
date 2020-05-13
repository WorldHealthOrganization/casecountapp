#' Register an app
#' @param name App name.
#' @param path Directory in which app will be deployed.
#'   By default, this is a temporary directory, and then the app can be
#'   deployed to possibly several locations using [deploy_app()].
#' @export
register_app <- function(name, path = tempfile()) {
  if (!dir.exists(path))
    dir.create(path)

  structure(list(
    name = name,
    path = path
  ), class = c("registered_app", "list"))
}

#' Add a case count display using 'geo cards' to an app
#' @export
build_casecount_display <- function(
  app,
  sources,
  name,
  desc,
  ref_source = NULL,
  append_higher_admin_name = FALSE,
  geo_links = NULL,
  nrow = 2,
  ncol = 3,
  thumb = FALSE,
  state = NULL,
  views = NULL,
  id = NULL,
  order = 1,
  case_fatality_max = 20
  # md_desc = md
) {
  if (!inherits(app, "registered_app"))
    stop("'app' not a valid object. See register_app().", call. = FALSE)

  if (is.null(ref_source))
    ref_source <- sources[[1]]$source

  geo_higher_level <- NULL
  if (append_higher_admin_name)
    geo_higher_level <- get_geo_higher_level(sources)

  # assemble data
  d <- get_casecount_data(sources, ref_source)
  # d$data[[1]] %>% group_by(source) %>% summarise(max_date = max(date))

  # add cognostics
  pb <- progress::progress_bar$new(
    format = "Computing cognostics [:bar] :percent :current/:total eta::eta",
    total = nrow(d))
  get_cogs2 <- function(x, pop) {
    pb$tick()
    get_cogs(x, pop)
  }
  dc <- d %>%
    mutate(cogs = map2_cog(data, population, get_cogs2))

  if (!is.null(geo_links)) {
    for (ll in geo_links) {
      nm <- paste0("view_", ll$ref_level)
      dc[[nm]] <- cog_disp_filter(
        ll$display,
        var = ll$variable,
        desc = paste0("View plots for ", ll$ref_level,
          " in this ", ll$cur_level),
        val = dc[[ll$variable]], default_label = TRUE,
        type = ll$type
      )
    }
  }

  # get limits
  lims <- get_lims(d)

  # add geocard panels
  pb <- progress::progress_bar$new(
    format = "Creating panels [:bar] :percent :current/:total eta::eta",
    total = nrow(dc))
  dc$panel <- lapply(seq_len(nrow(dc)), function(i) {
    pb$tick()
    geocard(dc[i, ], ref_source = ref_source,
      geo_level = get_geo_level(sources),
      geo_higher_level = geo_higher_level,
      y_log_domain = lims,
      case_fatality_max = case_fatality_max)
  })
  class(dc$panel) <- c("trelliscope_panels", "list")

  cnms <- setdiff(names(dc), c("flag_url", "map_url"))
  dc2 <- dc %>% ungroup() %>% select(one_of(cnms))

  # make plot
  p <- trelliscope(dc2,
    name = name,
    desc = desc,
    # md_desc = md_desc,
    path = app$path,
    state = state,
    width = 500,
    height = 416,
    thumb = thumb,
    views = views,
    id = id,
    order = order,
    nrow = nrow,
    ncol = ncol)

  print(p)
}

#' Copies app to a location
#' @export
view_app <- function(app) {
  if (!inherits(app, "registered_app"))
    stop("Not a valid object.", call. = FALSE)
  browseURL(file.path(app$path, "index.html"))
}

#' Copies app to a location
#' @export
deploy_app <- function(app, dest_dir, require_token = FALSE) {
  if (!inherits(app, "registered_app"))
    stop("Not a valid object.", call. = FALSE)
}

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
    suppressMessages(readr::read_csv(x$file)) %>%
      dplyr::mutate(source = x$source_id)
  }) %>%
  dplyr::bind_rows()

  if (!ref_source %in% ids)
    stop("ref_source: ", ref_source, " is not in the list of sources.",
      call. = FALSE)
  ids <- c(ref_source, setdiff(ids, ref_source))
  d$source <- factor(d$source, levels = ids)

  # nest
  code_vars <- names(d)[grepl("_code$", names(d))]
  d <- d %>%
    dplyr::group_by_at(code_vars) %>%
    tidyr::nest() %>%
    dplyr::ungroup()

  # merge geo data
  geo_obj <- get_geo_level(sources)

  geo_dat <- try(get(geo_obj, as.environment("package:geoutils")))
  if (inherits(geo_dat, "try-error"))
    stop("Could not find data in geoutils package matching admin_level: ", lvl,
      call. = FALSE)

  d2 <- dplyr::inner_join(d, geo_dat, by = code_vars)

  if (nrow(d2) < nrow(d)) {
    a <- dplyr::anti_join(
      dplyr::select(d, tidyselect::one_of(code_vars)),
      dplyr::select(geo_dat, tidyselect::one_of(code_vars)),
      by = code_vars
    )
    message("Data removed for the following geographic regions because ",
      "matching geo data in '", geo_obj, "' was not found.")
    message(paste(format(a)[-3], collapse = "\n"))
  }

  class(d2) <- c(class(d2), "casecounts")
  d2
}
