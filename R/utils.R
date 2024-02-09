#' Install useeior (via devtools).
install_useeior <- function() {
  installed_pkg <- installed.packages()
  if (!"devtools"%in%installed_pkg[, "Package"]) {
    install.packages("devtools")
  }
  if (!"useeior" %in% installed_pkg[, "Package"]) {
    cli::cli_alert_info("Installing useeior v{useeior_ver} (tag @{useeior_tag}) from GitHub...")
    devtools::install_github(paste0("USEPA/useeior@v1.4.0"))
  }
}
