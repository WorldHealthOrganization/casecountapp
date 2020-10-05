#' Get limits
#'
#' @param d TODO
#' @importFrom dplyr one_of
#' @importFrom utils tail
#' @export
get_lims <- function(d) {
  # TODO: check that the object is valid
  # if (!inherits(d, "casecounts"))
  #   stop("Data must be of class 'casecounts'.")

  tmp <- dplyr::bind_rows(lapply(seq_len(nrow(d)), function(i) {
    d$data[[i]]$idx <- i
    d$data[[i]]
  }))

  pdat <- tmp %>%
    dplyr::group_by(.data$source, .data$idx) %>%
    dplyr::mutate(
      new_cases = c(.data$cases[1], diff(.data$cases)),
      new_deaths = c(.data$deaths[1], diff(.data$deaths)),
      new_cases = ifelse(.data$new_cases < 0, 0, .data$new_cases),
      new_deaths = ifelse(.data$new_deaths < 0, 0, .data$new_deaths),
      case_fatality_pct = ifelse(.data$cases == 0, 0, 100 * .data$deaths / .data$cases)
    ) %>%
    dplyr::filter(.data$date >= min(.data$date[.data$cases > 0]))

  nms <- c("cases", "deaths", "new_cases", "new_deaths", "case_fatality_pct")

  lims <- list(daily = list(), weekly = list())

  lims$daily$min <- pdat %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(nms)) %>%
    dplyr::summarise_all(function(x) max(c(min(x), 1))) %>%
    as.list()

  lims$daily$max <- pdat %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(nms)) %>%
    dplyr::summarise_all(max) %>%
    as.list()

  wpdat <- pdat %>%
    dplyr::group_by(.data$source, .data$idx) %>%
    dplyr::mutate(
      ind = utils::tail(c(rep(1:(ceiling(dplyr::n() / 7)), each = 7), 0), dplyr::n())) %>%
    dplyr::group_by(.data$source, .data$ind, .data$idx) %>%
    dplyr::summarise(
      date = utils::tail(.data$date, 1),
      cases = utils::tail(.data$cases, 1),
      deaths = utils::tail(.data$deaths, 1),
      new_cases = sum(.data$new_cases),
      new_deaths = sum(.data$new_deaths),
      case_fatality_pct = ifelse(.data$cases == 0, 0, 100 * .data$deaths / .data$cases),
      n = dplyr::n()
    ) %>%
    dplyr::filter(.data$n == 7) %>%
    dplyr::select(-dplyr::one_of("n"))

  lims$weekly$min <- wpdat %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(nms)) %>%
    dplyr::summarise_all(function(x) max(c(min(x), 1))) %>%
    as.list()

  lims$weekly$max <- wpdat %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(nms)) %>%
    dplyr::summarise_all(max) %>%
    as.list()

  lims
}
