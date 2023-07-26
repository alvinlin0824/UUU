#' Import Freestyle csv upload data
#'
#' @param freestyle_path File path of freestyle.csv.
#' @param index  Index to get an individual or specific upload. Default is NULL.
#'
#' @return A tibble
#' @export


freestyle <- function(freestyle_path, index = NULL) {

  # Individual File
  if (is.numeric(index)) {
       freestyle_path[index] |>
      purrr::map(purrr::possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_select = c(`Unique Record ID`,Date,Time,`FreeStyle Reading`,Status)),tibble::tibble()),.progress = TRUE) |>
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
                        .default = stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = )[:alpha:]+",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = )[:digit:]+",ignore_case = T)))
                      ),
        `BG Date Time` = lubridate::ymd_hms(stringr::str_c(Date,Time,sep = " ")),
         Reference = `FreeStyle Reading`,
         Status = Status
        ),.progress = TRUE) |>
      purrr::map(\(df) df |> filter(!is.na(`BG Date Time`)),.progress = TRUE) |>
      purrr::list_rbind() |>
      # Filter Status == 0
      dplyr::filter(Status == 0) |>
      # Calculate Average freeStyle reading if time stamp are same.
      dplyr::mutate(Reference = mean(Reference),
             .by = c(`Subject ID`,`BG Date Time`)) |>
      # Remove Duplicated
      dplyr::distinct() |>
      # select Useful Columns
      dplyr::select(!Status) |>
      dplyr::arrange(`Subject ID`,`BG Date Time`)

  } else {
    # All Upload Data
    freestyle_path |>
      purrr::map(purrr::possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_select = c(`Unique Record ID`,Date,Time,`FreeStyle Reading`,Status)),tibble::tibble()),.progress = TRUE) |>
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
                          .default = stringr::str_c(stringr::str_extract(df[1,1],stringr::regex("(?<=Site ID = )[:alpha:]+",ignore_case = T)),stringr::str_extract(df[1,1],stringr::regex("(?<=Subject ID = )[:digit:]+",ignore_case = T)))
                        ),
                      `BG Date Time` = lubridate::ymd_hms(stringr::str_c(Date,Time,sep = " ")),
                      Reference = `FreeStyle Reading`,
                      Status = Status
            ),.progress = TRUE) |>
      purrr::map(\(df) df |> dplyr::filter(!is.na(`BG Date Time`)),.progress = TRUE) |>
      purrr::list_rbind() |>
      # Filter Status == 0
      dplyr::filter(Status == 0) |>
      # Calculate Average freeStyle reading if time stamp are same.
      dplyr::mutate(Reference = mean(Reference),
             .by = c(`Subject ID`,`BG Date Time`)) |>
      # Remove Duplicated
      dplyr::distinct() |>
      # select Useful Columns
      dplyr::select(!Status) |>
      dplyr::arrange(`Subject ID`,`BG Date Time`)
 }
}
