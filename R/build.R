#' Add a case count display using 'geo cards' to an app
#'
#' @param app An app object created with [register_app()].
#' @param sources TODO
#' @param name TODO
#' @param desc TODO
#' @param ref_source TODO
#' @param append_higher_admin_name TODO
#' @param max_date TODO
#' @param min_date TODO
#' @param geo_links TODO
#' @param nrow TODO
#' @param ncol TODO
#' @param disclaimer TODO
#' @param thumb TODO
#' @param state TODO
#' @param views TODO
#' @param id TODO
#' @param order TODO
#' @param case_fatality_max TODO
#' @param md_desc TODO
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
  disclaimer = FALSE,
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

  if (is.null(ref_source))
    ref_source <- sources[[1]]$source

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
    disclaimer = disclaimer)

  print(p)
}