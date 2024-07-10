#' Import devices.csv and readings.csv upload data
#'
#' @param devices_path File path of devices.csv.
#' @param readings_path   File path of readings.csv.
#' @param index  Index to get an individual or specific upload. Default is NULL.
#'
#' @return A tibble
#' @export

devices_readings <- function(devices_path, readings_path, index = NULL){

  if (length(devices_path) != length(readings_path)) {rlang::abort("Number of devices.csv is not equal to number of readings.csv!")}

  if (!is.numeric(index) && !is.null(index)) {rlang::abort("Index must be numeric!")}

  if(is.numeric(index)) {
    if (index > length(devices_path) || index > length(readings_path)) {rlang::abort("Index beyond the range!")}

    else {
      map2(
        # Devices.csv
        devices_path[index] |>
          set_names() |>
          map(possibly(\(path) vroom::vroom(path, delim = ",", col_types = c(SubjectID = "c", activationTimeUTC = "T"), show_col_types = F, col_select = c(SubjectID, ConditionID, sensorSN, activationTimeUTC)), tibble::tibble()), .progress = T) |>
          map(\(df) df |> rename(timeUTC = activationTimeUTC)),
        # readings.csv
        readings_path[index] |>
          set_names() |>
          map(possibly(\(path) vroom::vroom(path, delim = ",", col_types = c(SubjectID = "c", timeUTC = "T", reading = "d"), show_col_types = F, col_select = c(SubjectID, ConditionID, timeUTC, reading, analyte, DataType)), tibble::tibble()), .progress = T), bind_rows) |>
        # Fill sensor serial number
        map(\(df) df |> fill(sensorSN), .progress = T) |>
        list_rbind(names_to = "Path") |>
        transmute(Path = Path,
                  `Subject ID` = SubjectID,
                  `Condition ID` = ConditionID,
                  `Sensor Serial Number` = sensorSN,
                  `Date Time` = force_tz(with_tz(timeUTC, tzone = "US/Pacific"), tzone = "UTC"),
                  Type = case_when(DataType == "raw" ~ "906",
                                   DataType == "historical" ~ "905",
                                   .default = "SENSOR_STARTED (58)"),
                  Reading = reading,
                  Analyte = analyte) |>
        suppressWarnings()
    }
  }
  else if (is.null(index)) {
    map2(
      # Devices.csv
      devices_path |>
        set_names() |>
        map(\(path) vroom::vroom(path, delim = ",", show_col_types = F, col_types = c(SubjectID = "c", activationTimeUTC = "T"), col_select = c(SubjectID, ConditionID, sensorSN, activationTimeUTC)), .progress = T) |>
        map(\(df) df |> rename(timeUTC = activationTimeUTC)),
      # readings.csv
      readings_path |>
        set_names() |>
        map(\(path) vroom::vroom(path, delim = ",", show_col_types = F, , col_types = c(SubjectID = "c", timeUTC = "T", reading = "d"), col_select = c(SubjectID, ConditionID, timeUTC, reading, analyte, DataType)), .progress = T), bind_rows) |>
      # Fill sensor serial number
      map(\(df) df |> fill(sensorSN), .progress = T) |>
      list_rbind(names_to = "Path") |>
      transmute(Path = Path,
                `Subject ID` = SubjectID,
                `Condition ID` = ConditionID,
                `Sensor Serial Number` = sensorSN,
                `Date Time` = force_tz(with_tz(timeUTC, tzone = "US/Pacific"), tzone = "UTC"),
                Type = case_when(DataType == "raw" ~ "906",
                                 DataType == "historical" ~ "905",
                                 .default = "SENSOR_STARTED (58)"),
                Reading = reading,
                Analyte = analyte) |>
      suppressWarnings()
  }
}
