#' Import oneMinutes csv upload data
#'
#' @param events File path of events.csv.
#' @param one_minutes   File path of oneMinutes.csv.
#' @param index  Index to get an individual or specific upload. Default is NULL.
#'
#' @return A tibble
#' @export

oneminutes <- function(events, one_minutes, index = NULL) {

  # Individual File
  if (is.numeric(index)) {
    purrr::map2(
      # First List
      # Import Events
      events[index] |>
        purrr::set_names() |>
        # Consider empty events.csv
        purrr::map(purrr::possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_types = c(Time = "c", Type = "c",`Col 9` = "c"),col_select = c(Date,Time,Type,`Col 9`)),tibble::tibble()),.progress = TRUE) |>
        purrr::map(\(df) df |> dplyr::filter(Type == "SENSOR_STARTED (58)"),.progress = TRUE) |>
        purrr::map(\(df) df |> dplyr::transmute(
          `Date Time` = lubridate::ymd_hms(stringr::str_c(Date,Time,sep = " ")),
          Type = Type,
          `Sensor Serial Number` = `Col 9`),.progress = TRUE),

      purrr::map2(
        # Import oneminutes.csv
        one_minutes[index] |>
          purrr::map(purrr::possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_types = c(Type = "c"),col_select = c(`Unique Record ID`,Date,Time,Type,TpSk),n_max = 2),tibble::tibble()),.progress = TRUE),
        one_minutes[index] |>
          purrr::map(purrr::possibly(\(path) data.table::fread(path,select = c(1:4,7),skip = 3,col.names = c("Unique Record ID","Date","Time","Type","TpSk"),colClasses = c("V2" = "Date","V4" = "character")),tibble::tibble()),.progress = TRUE),
        dplyr::bind_rows,.progress = TRUE) |>
        purrr::map(\(df) df |> dplyr::transmute(`Subject ID` =
                                                  dplyr::case_when(
                                                    # Site ID == ADC
                                                    stringr::str_to_upper(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{3}",ignore_case = T))) == "ADC" ~ stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = )[:digit:]{4}",ignore_case = T)),
                                                    # Site ID == 009
                                                    stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{2}",ignore_case = T)) == "00" ~
                                                      stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = 00).{1}",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                                                    # Site ID starts with 1
                                                    stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{1}",ignore_case = T)) == "1" ~
                                                      stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{3}",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                                                    # Site ID == 081 or 057
                                                    stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{1}",ignore_case = T)) == "0" ~ stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = 0).{2}",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = )[:digit:]+",ignore_case = T))),
                                                    # Site ID mislabeled
                                                    .default = stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = )[:alnum:]+",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = )[:digit:]+",ignore_case = T)))
                                                  ),
                                                `Condition ID` = stringr::str_to_upper(stringr::str_extract(df[1,1],stringr::regex("(?<=Condition ID = ).{3}",ignore_case = T))),
                                                `Date Time` = lubridate::ymd_hms(stringr::str_c(Date,Time,sep = " ")),
                                                Type = Type,
                                                TpSk = TpSk),.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::slice(3:n())),dplyr::bind_rows,.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::arrange(`Date Time`),.progress = TRUE) |>
      purrr::map(\(df) df |> tidyr::fill(c(`Subject ID`,`Condition ID`),.direction = "up"),.progress = TRUE) |>
      purrr::map(\(df) df |> tidyr::fill(`Sensor Serial Number`,.direction = "down"),.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::filter(!(!is.na(`Sensor Serial Number`) & is.na(`Date Time`))),.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::relocate(`Subject ID`,`Condition ID`,`Sensor Serial Number`,
                                             `Date Time`,Type,TpSk),.progress = TRUE) |>
      purrr::list_rbind(names_to = "Path")

  } else {
    # All upload data
    # First List
    # Import Events
    purrr::map2(
      events |>
        purrr::set_names() |>
        # Consider empty events.csv
        purrr::map(purrr::possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_types = c(Time = "c", Type = "c",`Col 9` = "c"),col_select = c(Date,Time,Type,`Col 9`)),tibble::tibble()),.progress = TRUE) |>
        purrr::map(\(df) df |> dplyr::filter(Type == "SENSOR_STARTED (58)"),.progress = TRUE) |>
        purrr::map(\(df) df |> dplyr::transmute(
          `Date Time` = lubridate::ymd_hms(stringr::str_c(Date,Time,sep = " ")),
          Type = Type,
          `Sensor Serial Number` = `Col 9`),.progress = TRUE),

      purrr::map2(
        # Import oneminutes.csv
        one_minutes |>
          purrr::map(purrr::possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_types = c(Type = "c"),col_select = c(`Unique Record ID`,Date,Time,Type,TpSk),n_max = 2),tibble::tibble()),.progress = TRUE),
        one_minutes |>
          purrr::map(purrr::possibly(\(path) data.table::fread(path,select = c(1:4,7),skip = 3,col.names = c("Unique Record ID","Date","Time","Type","TpSk"),colClasses = c("V2" = "Date","V4" = "character")),tibble::tibble()),.progress = TRUE),
        dplyr::bind_rows,.progress = TRUE) |>
        purrr::map(\(df) df |> dplyr::transmute(`Subject ID` =
                                                  dplyr::case_when(
                                                    # Site ID == ADC
                                                    stringr::str_to_upper(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{3}",ignore_case = T))) == "ADC" ~ stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = )[:digit:]{4}",ignore_case = T)),
                                                    # Site ID == 009
                                                    stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{2}",ignore_case = T)) == "00" ~
                                                      stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = 00).{1}",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                                                    # Site ID starts with 1
                                                    stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{1}",ignore_case = T)) == "1" ~
                                                      stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{3}",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                                                    # Site ID == 081 or 057
                                                    stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{1}",ignore_case = T)) == "0" ~ stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = 0).{2}",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = )[:digit:]+",ignore_case = T))),
                                                    # Site ID mislabeled
                                                    .default = stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = )[:alnum:]+",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = )[:digit:]+",ignore_case = T)))
                                                  ),
                                                `Condition ID` = stringr::str_to_upper(stringr::str_extract(df[1,1],stringr::regex("(?<=Condition ID = ).{3}",ignore_case = T))),
                                                `Date Time` = lubridate::ymd_hms(stringr::str_c(Date,Time,sep = " ")),
                                                Type = Type,
                                                TpSk = TpSk),.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::slice(3:n())),dplyr::bind_rows,.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::arrange(`Date Time`),.progress = TRUE) |>
      purrr::map(\(df) df |> tidyr::fill(c(`Subject ID`,`Condition ID`),.direction = "up"),.progress = TRUE) |>
      purrr::map(\(df) df |> tidyr::fill(`Sensor Serial Number`,.direction = "down"),.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::filter(!(!is.na(`Sensor Serial Number`) & is.na(`Date Time`))),.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::relocate(`Subject ID`,`Condition ID`,`Sensor Serial Number`,
                                             `Date Time`,Type,TpSk),.progress = TRUE) |>
      purrr::list_rbind(names_to = "Path") |>
      # Remove Duplicated Uploads
      # dplyr::distinct() |>
      dplyr::arrange(Path,`Subject ID`,`Condition ID`,`Date Time`)
  }
}
