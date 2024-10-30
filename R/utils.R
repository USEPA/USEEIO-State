# useeior_ver = "v1.6.1"
useeior_ver = "develop"

#' Install useeior (via pak).
install_useeior <- function() {
  installed_pkg <- installed.packages()
  if (!"pak"%in%installed_pkg[, "Package"]) {
    install.packages("pak", repos = "http://cran.r-project.org") # see issue #22
  }
  if (!"useeior" %in% installed_pkg[, "Package"]) {
    cli::cli_alert_info("Installing useeior {useeior_ver} from GitHub...")
    pak::pkg_install(paste0("USEPA/useeior@", useeior_ver))
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

#' Load state population table.
load_state_population <- function(path=NULL) {
  if (is.null(path)) {
    path <- file.path("../data/state_population.csv")
  } else {
    path <- file.path(path, "state_population.csv")
  }
  pop <- read.csv(path, header=TRUE)
  pop <- pop[,c("FlowAmount","Abbreviation","Year")]
  colnames(pop) <- c("Population","State","Year")
  return(pop)
}
