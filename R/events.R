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
      map(possibly(\(path) vroom(path,delim = ",",
                                 col_names = T,
                                 show_col_types = F,
                                 col_types = c(`Col 5` = "c"),
                                 col_select = c(`Unique Record ID`,Date,Time,Type,`Col 5`)),
          tibble()),.progress = TRUE) |>
      map(\(df) df |>
            transmute(`Subject ID` =
                        case_when(
                          # Site ID == ADC
                          str_to_upper(str_extract(df[1,1],regex("(?<=Site ID = ).{3}",ignore_case = T))) == "ADC" ~ str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T)),
                          # Site ID == 009
                          str_extract(df[1,1],regex("(?<=Site ID = ).{2}",ignore_case = T)) == "00" ~
                            str_c(str_extract(df[1,1],regex("(?<=Site ID = 00).{1}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                          # Site ID starts with 1
                          str_extract(df[1,1],regex("(?<=Site ID = ).{1}",ignore_case = T)) == "1" ~
                            str_c(str_extract(df[1,1],regex("(?<=Site ID = ).{3}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                          # Site ID == 081
                          .default = str_c(str_extract(df[1,1],regex("(?<=Site ID = 0).{2}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T)))
                        ),
                      `Condition ID` = str_extract(df[1,1],regex("(?<=Condition ID = ).{3}",ignore_case = T)),
                      `Reader ID` = str_extract(df[2,1],regex("(?<=\\s).{13}",ignore_case = T)),
                      `Sensor Serial Number` = `Col 5`,
                      `Date Time` = ymd_hms(str_c(Date,Time,sep = " ")),
                       Type = Type)) |>
        list_rbind() |>
      # Remove Duplicated
        distinct() |>
      # Remove Type is NA
        filter(!is.na(Type)) |>
        mutate(`Sensor Serial Number` = case_when(
                    Type == "LOG_CLEAR (13)" ~ "2",
                    !str_length(`Sensor Serial Number`) %in% c(9,11) ~ NA,
                    .default = `Sensor Serial Number`)) |>
        fill(`Sensor Serial Number`,.direction = "down") |>
        mutate(`Sensor Serial Number` = case_when(
                   `Sensor Serial Number` == "2" ~ NA,
                    .default = `Sensor Serial Number`)) |>
        fill(`Sensor Serial Number`,.direction = "up") |>
        arrange(`Subject ID`,`Condition ID`,`Sensor Serial Number`,`Date Time`)
    } else {
      events_path |>
        map(possibly(\(path) vroom(path,delim = ",",
                                   col_names = T,
                                   show_col_types = F,
                                   col_types = c(`Col 5` = "c"),
                                   col_select = c(`Unique Record ID`,Date,Time,Type,`Col 5`)),
                     tibble()),.progress = TRUE) |>
        map(\(df) df |>
              transmute(`Subject ID` =
                          case_when(
                            # Site ID == ADC
                            str_to_upper(str_extract(df[1,1],regex("(?<=Site ID = ).{3}",ignore_case = T))) == "ADC" ~ str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T)),
                            # Site ID == 009
                            str_extract(df[1,1],regex("(?<=Site ID = ).{2}",ignore_case = T)) == "00" ~
                              str_c(str_extract(df[1,1],regex("(?<=Site ID = 00).{1}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                            # Site ID starts with 1
                            str_extract(df[1,1],regex("(?<=Site ID = ).{1}",ignore_case = T)) == "1" ~
                              str_c(str_extract(df[1,1],regex("(?<=Site ID = ).{3}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                            # Site ID == 081
                            .default = str_c(str_extract(df[1,1],regex("(?<=Site ID = 0).{2}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T)))
                          ),
                        `Condition ID` = str_extract(df[1,1],regex("(?<=Condition ID = ).{3}",ignore_case = T)),
                        `Reader ID` = str_extract(df[2,1],regex("(?<=\\s).{13}",ignore_case = T)),
                        `Sensor Serial Number` = `Col 5`,
                        `Date Time` = ymd_hms(str_c(Date,Time,sep = " ")),
                        Type = Type)) |>
        list_rbind() |>
        # Remove Duplicated
        distinct() |>
        # Remove Type is NA
        filter(!is.na(Type)) |>
        mutate(`Sensor Serial Number` = case_when(
          Type == "LOG_CLEAR (13)" ~ "2",
          !str_length(`Sensor Serial Number`) %in% c(9,11) ~ NA,
          .default = `Sensor Serial Number`)) |>
        fill(`Sensor Serial Number`,.direction = "down") |>
        mutate(`Sensor Serial Number` = case_when(
          `Sensor Serial Number` == "2" ~ NA,
          .default = `Sensor Serial Number`)) |>
        fill(`Sensor Serial Number`,.direction = "up") |>
        arrange(`Subject ID`,`Condition ID`,`Sensor Serial Number`,`Date Time`)
 }
}
