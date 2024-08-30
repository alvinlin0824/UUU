#' Read EDC Data
#'
#' @param directory Directory of EDC
#' @param name_of_edc Name of EDC data
#'
#' @return A tibble
#' @export

read_edc <- function(directory, name_of_edc){

  # Check packages
  packages <- c("tidyverse","haven","rlang")
  for (package_name in packages) {
    if (!require(package_name, character.only = TRUE)) {
      install <- readline(prompt = paste("Package", package_name, "is not installed. Do you want to install it? (Yes/No): "))
      # If say yes
      if (tolower(install) == "yes") {
        install.packages(package_name)
        library(package_name, character.only = TRUE)
        message(paste("Package", package_name, "has been installed and loaded."))
      }
       # If say no
       else {
        message(paste("Package", package_name, "was not installed."))
      }
    }
      else {
      message(paste("Package", package_name, "is already installed and loaded."))
    }
  }

  args <- rlang::ensyms(name_of_edc)
  name_of_edc <- paste(purrr::map(args, rlang::as_string), collapse = " ")

  return(haven::read_sas(stringr::str_c(directory, "/", name_of_edc, ".sas7bdat")))
}
