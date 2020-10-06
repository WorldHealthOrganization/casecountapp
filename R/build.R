#' Add a case count display using 'geo cards' to an app
#'
#' @param app An app object created with [register_app()].
#' @param sources A list of sources created with [source_list()].
#' @param name A name for the display, which is displayed in the header of the app when this display is selected.
#' @param desc A description that will be shown under the name.
#' @param ref_source The name of the reference source, which should match one of the source_ids provided in [source_entry()] when building the [source_list()].
#' @param append_higher_admin_name If \code{TRUE}, will append the higher-level admin name to the geocard headers. For example, if currently creating a display for admin level 2 (e.g. US counties), setting to TRUE will append the state name to the geocard header. So if viewing for Benton County in the state of Washington, the header, instead of being simply being "Benton", will now be "Benton, Washington".
#' @param max_date Optional maximum date to plot. If not specified, will be set to latest date.
#' @param min_date Optional minimum date to plot. If not specified, will be set to 8 weeks prior to max date.
#' @param geo_links Optional object or list of objects created by [geo_link_href()] or [geo_link_filter()] that will provide links to other displays (e.g. for a given continent, provide a link in its card that when clicked will open up the display for all countries in that continent).
#' @param nrow Default number of rows to show in the display.
#' @param ncol Default number of columns to show in the display.
#' @param thumb Optional path to an image file (png or jpeg) to use as the thumbnail for the display.
#' @param state Optional initial state (see the \code{state} argument of [trelliscopejs::trelliscope] for more details).
#' @param views Optional pre-specified views to allow the user to choose between. See [default_views()] for more information.
#' @param id Optional ID passed to [trelliscopejs::trelliscope].
#' @param order Integer indicating what order this display should appear in in the display list shown when the app opens.
#' @param case_fatality_max The maximum y-axis limit to allow for case fatality percentage. Passed to [geocard::geocard()].
#' @param md_desc Optional longer-form description of the display, provided as a markdown string.
#' @importFrom geocard get_cogs geocard
#' @importFrom dplyr %>% mutate filter select ungroup one_of
#' @importFrom purrr map map_lgl
#' @importFrom trelliscopejs map2_cog trelliscope cog_disp_filter
#' @importFrom progress progress_bar
#' @importFrom rlang .data
#' @export
build_casecount_display <- function(
  app,
  sources,
  name,
  desc,
  ref_source = NULL,
  append_higher_admin_name = FALSE,
  max_date = NULL,
  min_date = NULL,
  geo_links = NULL,
  nrow = 2,
  ncol = 3,
  thumb = FALSE,
  state = NULL,
  views = NULL,
  id = NULL,
  order = 1,
  case_fatality_max = 20,
  md_desc = ""
) {
  if (!inherits(app, "registered_app"))
    stop("'app' not a valid object. See register_app().", call. = FALSE)

  if (!inherits(sources, "casecount_source_list"))
    stop("sources must be specified by source_list()", call. = FALSE)

  if (is.null(ref_source))
    ref_source <- sources[[1]]$source

  if (inherits(geo_links, "geo_link"))
    geo_links <- list(geo_links)
  lapply(geo_links, function(lnk) {
    if (!inherits(lnk, "geo_link"))
      stop("geo_links must be a list of objects created with ",
        "geo_link_href() or geo_link_filter().", call. = FALSE)
  })

  geo_higher_level <- NULL
  if (append_higher_admin_name)
    geo_higher_level <- get_geo_higher_level(sources)

  # assemble data
  d <- get_casecount_data(sources, ref_source)
  # d$data[[1]] %>% group_by(source) %>% summarise(max_date = max(date))

  # TODO: allow parameter to this function to specify how far back to plot
  if (is.null(max_date))
    max_date <- max(do.call(c, lapply(d$data, function(x) max(x$date))))
  if (is.null(min_date))
    min_date <- max_date - (8 * 7)

  # prune data to only those within date range and have proper cases
  # (sometimes a case is reported early on and then reverted to zero)
  d <- d %>%
    dplyr::mutate(
      data = purrr::map(.data$data, function(a) {
        dplyr::filter(a, .data$date >= min_date & .data$date <= max_date)
      }),
      keep = purrr::map_lgl(.data$data, function(a) !all(a$cases == 0))) %>%
    dplyr::filter(.data$keep) %>%
    dplyr::select(-dplyr::one_of("keep"))

  # add cognostics
  pb <- progress::progress_bar$new(
    format = "Computing cognostics [:bar] :percent :current/:total eta::eta",
    total = nrow(d))
  get_cogs2 <- function(x, pop) {
    pb$tick()
    geocard::get_cogs(x, pop)
  }
  dc <- d %>%
    dplyr::mutate(cogs = trelliscopejs::map2_cog(.data$data,
      .data$population, get_cogs2))

  if (!is.null(geo_links)) {
    for (ll in geo_links) {
      nm <- paste0("view_", ll$ref_level)
      descr <- ifelse(is.null(ll$desc), paste0("View ", ll$ref_level), ll$desc)
      if (ll$cog_type == "cog_disp_filter") {
        dc[[nm]] <- trelliscopejs::cog_disp_filter(
          ll$display, var = ll$variable, desc = descr,
          val = dc[[ll$variable]], default_label = TRUE, type = ll$type)
      } else if (ll$cog_type == "cog_href") {
        dc[[nm]] <- trelliscopejs::cog_href(
          paste0("#display=", ll$display), type = ll$type, desc = descr)
      }
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

    card_name <- dc[i,][[paste0(get_geo_level(sources), "_name")]]
    if (is.character(geo_higher_level)) {
      card_name <- paste0(card_name, ", ",
        dc[i, ][[paste0(geo_higher_level, "_name")]])
    }

    img_url <- NULL
    if ("map_url" %in% names(dc))
      img_url <- dc[i, ]$map_url

    geocard::geocard(
      data = dc[i, ]$data[[1]],
      card_name = card_name,
      cog = dc[i, ]$cogs[[1]],
      population = dc[i, ]$population,
      ref_source = ref_source,
      y_log_domain = lims,
      min_date = min_date,
      max_date = max_date,
      img_url = img_url,
      case_fatality_max = case_fatality_max)
  })
  class(dc$panel) <- c("trelliscope_panels", "list")

  cnms <- setdiff(names(dc), c("flag_url", "map_url"))
  dc2 <- dc %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(cnms))

  # make plot
  p <- trelliscopejs::trelliscope(dc2,
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
    ncol = ncol,
    md_desc = md_desc,
    disclaimer = app$disclaimer)

  print(p)
}