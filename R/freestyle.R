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
       map(possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_select = c(`Unique Record ID`,Date,Time,`FreeStyle Reading`,Status)),tibble()),.progress = TRUE) |>
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
                        str_extract(df[1,1],regex("(?<=Site ID = ).{1}",ignore_case = T)) == "0" ~ str_c(str_extract(df[1,1],regex("(?<=Site ID = 0).{2}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                        # Site ID mislabeled
                        .default = str_c(str_extract(df[1,1],regex("(?<=Site ID = )[:alpha:]+",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = )[:digit:]+",ignore_case = T)))
                      ),
        `BG Date Time` = ymd_hms(str_c(Date,Time,sep = " ")),
         Reference = `FreeStyle Reading`,
         Status = Status
        )) |>
      map(\(df) df |> filter(!is.na(`BG Date Time`))) |>
      list_rbind() |>
      # Filter Status == 0
      filter(Status == 0) |>
      # Calculate Average freeStyle reading if time stamp are same.
      mutate(Reference = mean(Reference),
             .by = c(`Subject ID`,`BG Date Time`)) |>
      # Remove Duplicated
      distinct() |>
      # select Useful Columns
      select(!Status) |>
      arrange(`Subject ID`,`BG Date Time`)

  } else {
    # All Upload Data
    freestyle_path |>
      map(possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_select = c(`Unique Record ID`,Date,Time,`FreeStyle Reading`,Status)),tibble()),.progress = TRUE) |>
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
                          str_extract(df[1,1],regex("(?<=Site ID = ).{1}",ignore_case = T)) == "0" ~ str_c(str_extract(df[1,1],regex("(?<=Site ID = 0).{2}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                          # Site ID mislabeled
                          .default = str_c(str_extract(df[1,1],regex("(?<=Site ID = )[:alpha:]+",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = )[:digit:]+",ignore_case = T)))
                        ),
                      `BG Date Time` = ymd_hms(str_c(Date,Time,sep = " ")),
                      Reference = `FreeStyle Reading`,
                      Status = Status
            )) |>
      map(\(df) df |> filter(!is.na(`BG Date Time`))) |>
      list_rbind() |>
      # Filter Status == 0
      filter(Status == 0) |>
      # Calculate Average freeStyle reading if time stamp are same.
      mutate(Reference = mean(Reference),
             .by = c(`Subject ID`,`BG Date Time`)) |>
      # Remove Duplicated
      distinct() |>
      # select Useful Columns
      select(!Status) |>
      arrange(`Subject ID`,`BG Date Time`)
 }
}
