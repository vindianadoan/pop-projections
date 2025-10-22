#' Population Projections Dashboard
#'
#' Main function to run the Shiny dashboard
#'
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "popprojections")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `popprojections`.", call. = FALSE)
  }
  
  shiny::runApp(app_dir, display.mode = "normal")
}
