useeior_ver = "1.6.0"

#' Install useeior (via remotes).
install_useeior <- function() {
  installed_pkg <- installed.packages()
  if (!"remotes"%in%installed_pkg[, "Package"]) {
    install.packages("remotes")
  }
  if (!"useeior" %in% installed_pkg[, "Package"]) {
    cli::cli_alert_info("Installing useeior v{useeior_ver} from GitHub...")
    remotes::install_github(paste0("USEPA/useeior@v", useeior_ver))
  }
}


#' Download model for EPA Data Commons.
download_model_RDS <- function(modelname) {
  file_name <- paste0(modelname,".rds")
  file_url <- paste0("https://dmap-data-commons-ord.s3.amazonaws.com/USEEIO-State/",file_name)
  directory <- "../models/"
  file_path <- paste0(directory, file_name)
  year <- substr(modelname, nchar(modelname)-1, nchar(modelname))
  if(!file.exists(file_path)) {
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
    # Download file
    utils::download.file(file_url,file_path,
                         mode = "wb", quiet = TRUE)
    print(paste0("Downloading model for ", substr(modelname,0,2), " in 20", year, " to ", file_path, "."))
  } else {
    print(paste("File already exists at", file_path))
  }
  return(file_path)
}
