#' Import Events csv upload data
#'
#' @param events_path File path of events.csv.
#' @param index Index to get an individual or specific upload. Default is NULL.
#'
#' @return A tibble
#' @export

events <- function(events_path, index = NULL){
  if (is.numeric(index)) {
    events_path[index] |>
      purrr::map(purrr::possibly(\(path) vroom::vroom(path,delim = ",",
                                 col_names = T,
                                 show_col_types = F,
                                 col_types = c(`Col 5` = "c",`Col 9` = "c"),
                                 col_select = c(`Unique Record ID`,Date,Time,Type,`Col 5`,`Col 9`)),
                                 tibble::tibble()),.progress = TRUE) |>
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
                      `Condition ID` = stringr::str_extract(df[1,1],stringr::regex("(?<=Condition ID = ).{3}",ignore_case = T)),
                      `Reader ID` = stringr::str_extract(df[2,1],stringr::regex("(?<=\\s).{13}",ignore_case = T)),
                      `Sensor Serial Number` = `Col 5`,
                      `Col 9` = `Col 9`,
                      `Event Date Time` = lubridate::ymd_hms(stringr::str_c(Date,Time,sep = " ")),
                       Type = Type),.progress = TRUE) |>
      purrr::list_rbind() |>
      # Remove Duplicated
      dplyr::distinct() |>
      # Remove Type is NA
      dplyr::filter(!is.na(Type)) |>
      dplyr::mutate(`Col 9` = dplyr::case_when(
                  stringr::str_detect(`Col 9`,"[:alpha:]") ~ `Col 9`,
                                 .default = NA_character_),
               `Sensor Serial Number` = dplyr::case_when(
                 stringr::str_detect(`Sensor Serial Number`,"[:alpha:]") ~ `Sensor Serial Number`,
                          .default = `Col 9`)) |>
      tidyr::fill(`Sensor Serial Number`,.direction = "updown") |>
      dplyr::select(!`Col 9`) |>
      dplyr::arrange(`Subject ID`,`Condition ID`)
    } else {
      events_path |>
        purrr::map(purrr::possibly(\(path) vroom::vroom(path,delim = ",",
                                   col_names = T,
                                   show_col_types = F,
                                   col_types = c(`Col 5` = "c",`Col 9` = "c"),
                                   col_select = c(`Unique Record ID`,Date,Time,Type,`Col 5`,`Col 9`)),
                                   tibble::tibble()),.progress = TRUE) |>
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
                        `Condition ID` = stringr::str_extract(df[1,1],stringr::regex("(?<=Condition ID = ).{3}",ignore_case = T)),
                        `Reader ID` = stringr::str_extract(df[2,1],stringr::regex("(?<=\\s).{13}",ignore_case = T)),
                        `Sensor Serial Number` = `Col 5`,
                        `Col 9` = `Col 9`,
                        `Event Date Time` = lubridate::ymd_hms(stringr::str_c(Date,Time,sep = " ")),
                         Type = Type),.progress = TRUE) |>
        # Remove Type is NA
        purrr::map(\(df) df |> dplyr::filter(!is.na(Type)),.progress = TRUE) |>
        purrr::map(\(df) df |> dplyr::mutate(`Col 9` = dplyr::case_when(str_detect(`Col 9`,"[:alpha:]") ~ `Col 9`,
                                                   .default = NA_character_),
                               `Sensor Serial Number` = dplyr::case_when(
                                 stringr::str_detect(`Sensor Serial Number`,"[:alpha:]")
                                 ~ `Sensor Serial Number`,
                                 .default = `Col 9`)),.progress = TRUE) |>
        purrr::map(\(df) df |> tidyr::fill(`Sensor Serial Number`,.direction = "updown"),.progress = TRUE) |>
        purrr::list_rbind() |>
        dplyr::select(!`Col 9`) |>
        # Remove Duplicates
        dplyr::distinct() |>
        dplyr::arrange(`Subject ID`,`Condition ID`)
 }
}
