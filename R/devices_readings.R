#' Import devices.csv and readings.csv upload data
#'
#' @param devices_path File path of devices.csv.
#' @param readings_path   File path of readings.csv.
#' @param index  Index to get an individual or specific upload. Default is NULL.
#'
#' @return A tibble
#' @export

devices_readings <- function(devices_path, readings_path, index = NULL){

  # Check packages
  packages <- c("tidyverse","vroom","rlang")
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
    #   else {
    #   message(paste("Package", package_name, "is already installed and loaded."))
    # }
  }



  if (length(devices_path) != length(readings_path)) {rlang::abort("Number of devices.csv is not equal to number of readings.csv!")}

  if (!is.numeric(index) && !is.null(index)) {rlang::abort("Index must be numeric!")}

  if(is.numeric(index)) {
    if (index > length(devices_path) || index > length(readings_path)) {rlang::abort("Index beyond the range!")}

    else {
      map2(
        # Devices.csv
        devices_path[index] |>
          purrr::set_names() |>
          map(possibly(\(path) vroom::vroom(path, delim = ",", col_types = c(SiteID = "c", SubjectID = "c", activationTime= "c"), show_col_types = F, col_select = c(SiteID,SubjectID, ConditionID, sensorSN, activationTime)), tibble::tibble()), .progress = T) |>
          map(\(df) df |> rename(timeLocal = activationTime)),
        # readings.csv
        readings_path[index] |>
          purrr::set_names() |>
          map(possibly(\(path) vroom::vroom(path, delim = ",", col_types = c(SiteID = "c", SubjectID = "c", lifecount = "c", timeLocal = "c", reading = "d"), show_col_types = F,
                                            col_select = c(SiteID, SubjectID, ConditionID, lifecount, timeLocal, reading, analyte, uom, DataType)), tibble::tibble()), .progress = T), bind_rows) |>
        # Fill sensor serial number
        map(\(df) df |> fill(sensorSN), .progress = T) |>
        list_rbind(names_to = "Path") |>
        transmute(Path = Path,
                  Site = SiteID,
                  `Subject ID` = SubjectID,
                  `Condition ID` = ConditionID,
                  `Sensor Serial Number` = sensorSN,
                  Lifecount = lifecount,
                  `Date Time` = ymd_hms(str_remove(timeLocal,"-[0-9][0-9]:00")),
                  Analyte = analyte,
                  uom = uom,
                  Type = case_when(DataType == "raw" ~ "906",
                                   DataType == "historical" ~ "905",
                                   .default = "SENSOR_STARTED (58)"),
                  Reading = reading) |>
        suppressWarnings()
    }
  }
  else if (is.null(index)) {
    map2(
      # Devices.csv
      devices_path |>
        purrr::set_names() |>
        map(possibly(\(path) vroom::vroom(path, delim = ",", col_types = c(SiteID = "c", SubjectID = "c", activationTime= "c"), show_col_types = F, col_select = c(SiteID,SubjectID, ConditionID, sensorSN, activationTime)), tibble::tibble()), .progress = T) |>
        map(\(df) df |> rename(timeLocal = activationTime)),
      # readings.csv
      readings_path |>
        purrr::set_names() |>
        map(possibly(\(path) vroom::vroom(path, delim = ",", col_types = c(SiteID = "c", SubjectID = "c", lifecount = "c", timeLocal = "c", reading = "d"), show_col_types = F,
                                          col_select = c(SiteID, SubjectID, ConditionID, lifecount, timeLocal, reading, analyte, uom, DataType)), tibble::tibble()), .progress = T), bind_rows) |>
      # Fill sensor serial number
      map(\(df) df |> fill(sensorSN), .progress = T) |>
      list_rbind(names_to = "Path") |>
      transmute(Path = Path,
                Site = SiteID,
                `Subject ID` = SubjectID,
                `Condition ID` = ConditionID,
                `Sensor Serial Number` = sensorSN,
                Lifecount = lifecount,
                `Date Time` = ymd_hms(str_remove(timeLocal,"-[0-9][0-9]:00")),
                Analyte = analyte,
                uom = uom,
                Type = case_when(DataType == "raw" ~ "906",
                                 DataType == "historical" ~ "905",
                                 .default = "SENSOR_STARTED (58)"),
                Reading = reading) |>
      suppressWarnings()
  }
}
