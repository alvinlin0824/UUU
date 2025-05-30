#' Import anaPlus.csv or ana.csv upload data
#'
#' @param events File path of events.csv.
#' @param ana    File path of anaPlus.csv or ana.csv.
#' @param index  Index to get an individual or specific upload. Default is NULL.
#'
#' @return A tibble
#' @export

mobi_anaPlus <- function(events, ana, index = NULL) {

  # Check packages
  packages <- c("tidyverse","vroom","rlang","data.table")
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


  if (length(events) != length(ana)) {rlang::abort("Number of events.csv is not equal to number of anaPlus.csv!")}

  if (!is.numeric(index) && !is.null(index)) {rlang::abort("Index must be numeric!")}

  # Individual File
  if (is.numeric(index)) {

    if (index > length(events) || index > length(ana)) {rlang::abort("Index beyond the range!")}

    else {
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
        # Second List
        # Import anaPlus.csv or ana.csv
        ana[index] |>
          purrr::map(purrr::possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_types = c(Date = "c", Time = "c", Type = "c"),col_select = c(`Unique Record ID`,Date,Time,Type,ANA,Rate,Tr),n_max = 2),tibble::tibble()),.progress = TRUE),
        ana[index] |>
          purrr::map(purrr::possibly(\(path) data.table::fread(path,select = c(1:7),skip = 3,col.names = c("Unique Record ID","Date","Time","Type","ANA","Rate","Tr"),colClasses = c("V2" = "character","V3" = "character","V4" = "character","V5" = "numeric","V6" = "numeric","V7" = "numeric")),tibble::tibble()),.progress = TRUE),
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
                                                ANA = ANA,
                                                Rate = Rate,
                                                Tr = Tr),.progress = TRUE) |>
        purrr::map(\(df) df |> dplyr::slice(2:n())),dplyr::bind_rows,.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::arrange(`Date Time`),.progress = TRUE) |>
      purrr::map(\(df) df |> tidyr::fill(c(`Subject ID`,`Condition ID`),.direction = "up"),.progress = TRUE) |>
      purrr::map(\(df) df |> tidyr::fill(`Sensor Serial Number`,.direction = "down"),.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::filter(!(!is.na(`Sensor Serial Number`) & is.na(`Date Time`))),.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::relocate(`Subject ID`,`Condition ID`,`Sensor Serial Number`,
                                             `Date Time`,Type,ANA,Rate,Tr),.progress = TRUE) |>
      purrr::list_rbind(names_to = "Path") |>
      suppressWarnings()
   }
  }
    else if (is.null(index)) {

    # All Upload Data
    purrr::map2(
      # First List
      # Import Events
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
        # Second List
        # Import anaPlus.csv or ana.csv
        ana |>
          purrr::map(purrr::possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_types = c(Date = "c", Time = "c", Type = "c"),col_select = c(`Unique Record ID`,Date,Time,Type,ANA,Rate,Tr),n_max = 2),tibble::tibble()),.progress = TRUE),
        ana |>
          purrr::map(purrr::possibly(\(path) data.table::fread(path,select = c(1:7),skip = 3,col.names = c("Unique Record ID","Date","Time","Type","ANA","Rate","Tr"),colClasses = c("V2" = "character","V3" = "character","V4" = "character","V5" = "numeric","V6" = "numeric","V7" = "numeric")),tibble::tibble()),.progress = TRUE),
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
                                                ANA = ANA,
                                                Rate = Rate,
                                                Tr = Tr),.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::slice(2:n())),dplyr::bind_rows,.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::arrange(`Date Time`),.progress = TRUE) |>
      purrr::map(\(df) df |> tidyr::fill(c(`Subject ID`,`Condition ID`),.direction = "up"),.progress = TRUE) |>
      purrr::map(\(df) df |> tidyr::fill(`Sensor Serial Number`,.direction = "down"),.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::filter(!(!is.na(`Sensor Serial Number`) & is.na(`Date Time`))),.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::relocate(`Subject ID`,`Condition ID`,`Sensor Serial Number`,
                                             `Date Time`,Type,ANA,Rate,Tr),.progress = TRUE) |>
      purrr::list_rbind(names_to = "Path") |>
      # Remove Duplicated Uploads
      # dplyr::distinct() |>
      dplyr::arrange(Path,`Subject ID`,`Condition ID`,`Sensor Serial Number`) |>
      suppressWarnings()
  }
}
