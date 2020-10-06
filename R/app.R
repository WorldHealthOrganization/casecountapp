#' Register an app
#'
#' @param name App name.
#' @param path Directory in which app will be deployed.
#'   By default, this is a temporary directory, and then the app can be
#'   deployed to possibly several locations using [deploy_cp()] or other
#'   deployment mechanisms.
#' @param disclaimer Optional string of disclaimer text to display when the app is loaded.
#' @export
#' @importFrom dplyr filter select mutate bind_rows group_by_at ungroup inner_join anti_join
#' @importFrom purrr map map_lgl
#' @importFrom progress progress_bar
#' @importFrom readr read_csv
register_app <- function(
  name, path = tempfile(), disclaimer = FALSE
) {
  if (!dir.exists(path))
    dir.create(path)

  path <- normalizePath(path)

  structure(list(
    name = name,
    path = path,
    disclaimer = disclaimer
  ), class = c("registered_app", "list"))
}

#' Copies app to a location
#'
#' @param app An app object created with [register_app()].
#' @importFrom utils browseURL
#' @export
view_app <- function(app) {
  if (!inherits(app, "registered_app"))
    stop("Not a valid app object.", call. = FALSE)
  utils::browseURL(file.path(app$path, "index.html"))
}
