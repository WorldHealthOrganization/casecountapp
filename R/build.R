#' Add a case count display using 'geo cards' to an app
#'
#' @param app TODO
#' @param sources TODO
#' @param name TODO
#' @param desc TODO
#' @param ref_source TODO
#' @param append_higher_admin_name TODO
#' @param geo_links TODO
#' @param nrow TODO
#' @param ncol TODO
#' @param thumb TODO
#' @param state TODO
#' @param views TODO
#' @param id TODO
#' @param order TODO
#' @param case_fatality_max TODO
#' @importFrom geocard get_cogs geocard get_lims
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

  # TODO: allow parameter to this function to specify how far back to plot
  max_date <- max(do.call(c, lapply(d$data, function(x) max(x$date))))
  min_date <- max_date - (8 * 7)

  # prune data to only those within date range and have proper cases
  # (sometimes a case is reported early on and then reverted to zero)
  d <- d %>%
    dplyr::mutate(
      data = purrr::map(.data$data, function(a) {
        dplyr::filter(a, .data$date >= min_date)
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
      dc[[nm]] <- trelliscopejs::cog_disp_filter(
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
  lims <- geocard::get_lims(d)

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
    ncol = ncol)

  print(p)
}