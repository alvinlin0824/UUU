#' Import Ketone csv upload data
#'
#' @param ketone_path File path of ketone.csv.
#' @param index  Index to get an individual or specific upload. Default is NULL.
#'
#' @return A tibble
#' @export

ketone <- function(ketone_path, index = NULL){
  # # Individual File
  if (is.numeric(index)) {
    ketone_path[index] |>
      purrr::map(purrr::possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_select = c(`Unique Record ID`,Date,Time,`Ketone Reading`,Status)),tibble::tibble()),.progress = TRUE) |>
      purrr::map(\(df) df |>
                   dplyr::transmute(`Subject ID` =
                                      dplyr::case_when(
                                        # Site ID == ADC
                                        stringr::str_to_upper(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{3}",ignore_case = T))) == "ADC" ~ stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = ).{4}",ignore_case = T)),
                                        # Site ID == 009
                                        stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{2}",ignore_case = T)) == "00" ~
                                          stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = 00).{1}",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                                        # Site ID starts with 1
                                        stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{1}",ignore_case = T)) == "1" ~
                                          stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{3}",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                                        # Site ID == 081
                                        stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{1}",ignore_case = T)) == "0" ~ stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = 0).{2}",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = )[:digit:]+",ignore_case = T))),
                                        # Site ID mislabeled
                                        .default = stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = )[:alnum:]+",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = )[:digit:]+",ignore_case = T)))
                                      ),
                                    Visit = stringr::str_extract(df[1,1],stringr::regex("(?<=Condition ID = ).{3}",ignore_case = T)),
                                    `Ketone Date Time` = lubridate::ymd_hms(stringr::str_c(Date,Time,sep = " ")),
                                    Reference = `Ketone Reading`,
                                    Status = Status
                   ),.progress = TRUE) |>
      purrr::map(\(df) df |> filter(!is.na(`Ketone Date Time`)),.progress = TRUE) |>
      purrr::list_rbind(names_to = "Path") |>
      # Filter Status == 0
      dplyr::filter(Status == 0) |>
      # Calculate Average ketone reading if time stamp are same.
      dplyr::mutate(Reference = mean(Reference),
                    .by = c(`Subject ID`,`Ketone Date Time`)) |>
      # Remove Duplicated
      dplyr::distinct() |>
      # select Useful Columns
      dplyr::select(!Status) |>
      dplyr::arrange(`Subject ID`,Visit,`Ketone Date Time`)
  } else {
    # All Upload Data
    ketone_path |>
      purrr::map(purrr::possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_select = c(`Unique Record ID`,Date,Time,`Ketone Reading`,Status)),tibble::tibble()),.progress = TRUE) |>
      purrr::map(\(df) df |>
                   dplyr::transmute(`Subject ID` =
                                      dplyr::case_when(
                                        # Site ID == ADC
                                        stringr::str_to_upper(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{3}",ignore_case = T))) == "ADC" ~ stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = ).{4}",ignore_case = T)),
                                        # Site ID == 009
                                        stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{2}",ignore_case = T)) == "00" ~
                                          stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = 00).{1}",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                                        # Site ID starts with 1
                                        stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{1}",ignore_case = T)) == "1" ~
                                          stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{3}",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                                        # Site ID == 081
                                        stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = ).{1}",ignore_case = T)) == "0" ~ stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = 0).{2}",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = )[:digit:]+",ignore_case = T))),
                                        # Site ID mislabeled
                                        .default = stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = )[:alnum:]+",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = )[:digit:]+",ignore_case = T)))
                                      ),
                                    Visit = stringr::str_extract(df[1,1],stringr::regex("(?<=Condition ID = ).{3}",ignore_case = T)),
                                    `Ketone Date Time` = lubridate::ymd_hms(stringr::str_c(Date,Time,sep = " ")),
                                    Reference = `Ketone Reading`,
                                    Status = Status
                   ),.progress = TRUE) |>
      purrr::map(\(df) df |> filter(!is.na(`Ketone Date Time`)),.progress = TRUE) |>
      purrr::list_rbind(names_to = "Path") |>
      # Filter Status == 0
      dplyr::filter(Status == 0) |>
      # Calculate Average ketone reading if time stamp are same.
      dplyr::mutate(Reference = mean(Reference),
                    .by = c(`Subject ID`,`Ketone Date Time`)) |>
      # Remove Duplicated
      dplyr::distinct() |>
      # select Useful Columns
      dplyr::select(!Status) |>
      dplyr::arrange(`Subject ID`, Visit,`Ketone Date Time`)
  }
}
