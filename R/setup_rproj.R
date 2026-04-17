#' Ensure working directory setup
#'
#' Checks if the directories ("Data", "Output", "Output/Data",
#' "Output/Plots", "Script") exist in the current working directory. If any of
#' these directories are missing, it creates them to ensure that the project
#' structure is properly set up.
#'
#' @returns No return value
#' @export
#'
setup_rproj <- function() {
  cat("Ensuring project directory structure...\n")
  if (!dir.exists("Data")) dir.create("Data")
  if (!dir.exists("Output")) dir.create("Output")
  if (!dir.exists("Output/Data")) dir.create("Output/Data")
  if (!dir.exists("Output/Plots")) dir.create("Output/Plots")
  if (!dir.exists("Script")) dir.create("Script")
}
