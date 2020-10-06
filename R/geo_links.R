#' Specify a cognostic that links to another display of a higher level of geographic resolution associated with the given geographic entity.
#'
#' @param display The name of the display to link to.
#' @param variable The variable to use to specify which subset to 
#' @param ref_level String that will be used in constructing the cognostic name (e.g. if ref_level = "who_regions", the cognostic will be called "view_who_regions").
#' @param desc An optional description for the cognostic.
#' @param type One of either "href" or "href_hash". "href" will open the link in a new page. "href_hash" will update the page's hash and reload the page (useful when changing state inside an iframe).
#' @export
geo_link_filter <- function(display, variable, ref_level, desc = NULL, type = "href") {
  type <- match.arg(type, c("href", "href_hash"))
  if (is.null(desc))
    desc <- paste0("View ", ref_level)

  structure(list(
    display = display,
    variable = variable,
    ref_level = ref_level,
    type = type,
    cog_type = "cog_disp_filter"
  ), class = c("geo_link", "list"))
}

#' Specify a cognostic that links to another display of a higher level of geographic resolution.
#'
#' @param display The name of the display to link to.
#' @param ref_level String that will be used in constructing the cognostic name (e.g. if ref_level = "who_regions", the cognostic will be called "view_who_regions").
#' @param desc An optional description for the cognostic.
#' @param type One of either "href" or "href_hash". "href" will open the link in a new page. "href_hash" will update the page's hash and reload the page (useful when changing state inside an iframe).
#' @export
geo_link_href <- function(display, ref_level, desc = NULL, type = "href") {
  type <- match.arg(type, c("href", "href_hash"))
  if (is.null(desc))
    desc <- paste0("View ", ref_level)

  structure(list(
    display = display,
    ref_level = ref_level,
    type = type,
    desc = desc,
    cog_type = "cog_href"
  ), class = c("geo_link", "list"))
}
