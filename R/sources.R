#' Specify a source list
#' @param \ldots objects created from [source_entry()]
#' @export
source_list <- function(...) {
  lapply(list(...), function(x) {
    if (!inherits(x, "casecounts_source"))
      stop("all arguments to source_list() must be of class ",
        "'casecounts_source'",
        call. = FALSE)
  })

  structure(list(...), class = c("casecount_source_list", "list"))
}

#' Specify a source entry
#' @param source_id A unique ID for the source.
#' @param admin_level One of "0", "1", "2", "global", "continent", "who_region".
#' @param file A URL or local file path to the data file for this source.
#' @export
source_entry <- function(source_id, admin_level, file) {
  admlvls <- c("0", "1", "2", "global", "continent", "who_region")
  if (!as.character(admin_level) %in% admlvls)
    stop("admin_level must be one of: ", paste(admlvls, collapse = ", "),
      call. = FALSE)
  structure(
    list(source_id = source_id, admin_level = admin_level, file = file),
    class = c("casecounts_source", "list")
  )
}
