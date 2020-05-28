#' Deploy an app to Netlify
#'
#' @param app An app object created with [register_app()].
#' @param netlify_app_id TODO
#' @param require_app_token TODO
#' @export
deploy_netlify <- function(app, netlify_app_id, require_app_token = FALSE) {
  if (!inherits(app, "registered_app"))
    stop("Not a valid app object.", call. = FALSE)

  a <- system("which netlify")
  if (a == 1)
    stop("Could not find netlify CLI. See here: https://cli.netlify.com to install.")

  token <- Sys.getenv("NETLIFY_AUTH_TOKEN")
  if (token == "")
    stop("Need to set environment variable NETLIFY_AUTH_TOKEN.")

  if (!file.exists(file.path(app$path, "index.html")))
    stop("Must be an index.html file in the app directory: \n  ",
      app$path)

  cmd <- paste0("netlify deploy --prod --dir ", app$path,
    " --auth $NETLIFY_AUTH_TOKEN --site ", netlify_app_id)

  res <- system(cmd)
  if (res == 1)
    stop("There was an issue deploying to netlify.")
  
  res == 0
}

#' Copy app to a local location
#' 
#' @param app An app object created with [register_app()].
#' @param dest_dir TODO
#' @param require_app_token TODO
#' @export
deploy_cp <- function(app, dest_dir, require_app_token = FALSE) {
  if (!inherits(app, "registered_app"))
    stop("Not a valid object.", call. = FALSE)

  message("Not yet implemented")
}

#' Copy app to a remote location over SCP
#'
#' @param app An app object created with [register_app()].
#' @param ip TODO
#' @param scp_token TODO
#' @param require_app_token TODO
#' @export
deploy_scp <- function(app, ip, scp_token = NULL, require_app_token = FALSE) {
  if (!inherits(app, "registered_app"))
    stop("Not a valid object.", call. = FALSE)

  message("Not yet implemented")
}
