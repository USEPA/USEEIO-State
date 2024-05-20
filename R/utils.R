#' Install useeior (via remotes).
install_useeior <- function() {
  installed_pkg <- installed.packages()
  if (!"remotes"%in%installed_pkg[, "Package"]) {
    install.packages("remotes")
  }
  if (!"useeior" %in% installed_pkg[, "Package"]) {
    cli::cli_alert_info("Installing useeior v{useeior_ver} (tag @{useeior_tag}) from GitHub...")
    devtools::install_github(paste0("USEPA/useeior@v1.5.1"))
  }
}
