#' Import Apol csv upload data
#'
#' @param events File path of events.csv.
#' @param gluc   File path of gluc.csv.
#' @param index  Index to get an individual or specific upload. Default is NULL.
#'
#' @return A tibble
#' @export


apol <- function(events, gluc, index = NULL) {

  # Individual File
  if (is.numeric(index)) {
    map2(
      # First List
      # Import Events
      events[index] |>
        set_names() |>
        # Consider empty events.csv
        map(possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_types = c(`Col 9` = "c"),col_select = c(Date,Time,Type,`Col 9`)),tibble()),.progress = TRUE) |>
        map(\(df) df |> filter(Type == "SENSOR_STARTED (58)"),.progress = TRUE) |>
        map(\(df) df |> transmute(
          `Date Time` = ymd_hms(str_c(Date,Time,sep = " ")),
           Type = Type,
          `Sensor Serial Number` = `Col 9`),.progress = TRUE),
        # Replaced Sensors Only
        # map(\(df) df |> slice_max(`Date Time`,n = 1)),

      map2(
        # Second List
        # Import gluc.csv
        gluc[index] |>
          map(possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_types = c(Type = "c"),col_select = c(`Unique Record ID`,Date,Time,Type,Gl,St,Tr),n_max = 2),tibble()),.progress = TRUE),
        gluc[index] |>
          map(possibly(\(path) data.table::fread(path,select = c(1:7),skip = 3,col.names = c("Unique Record ID","Date","Time","Type","Gl","St","Tr"),colClasses = c("V2" = "Date","V4" = "character")),tibble()),.progress = TRUE),
        bind_rows,.progress = TRUE) |>
        map(\(df) df |> transmute(`Subject ID` =
                                    case_when(
                                      # Site ID == ADC
                                      str_to_upper(str_extract(df[1,1],regex("(?<=Site ID = ).{3}",ignore_case = T))) == "ADC" ~ str_extract(df[1,1],regex("(?<=Subject ID = )[:digit:]{4}",ignore_case = T)),
                                      # Site ID == 009
                                      str_extract(df[1,1],regex("(?<=Site ID = ).{2}",ignore_case = T)) == "00" ~
                                        str_c(str_extract(df[1,1],regex("(?<=Site ID = 00).{1}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                                      # Site ID starts with 1
                                      str_extract(df[1,1],regex("(?<=Site ID = ).{1}",ignore_case = T)) == "1" ~
                                        str_c(str_extract(df[1,1],regex("(?<=Site ID = ).{3}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                                      # Site ID == 081 or 057
                                      str_extract(df[1,1],regex("(?<=Site ID = ).{1}",ignore_case = T)) == "0" ~ str_c(str_extract(df[1,1],regex("(?<=Site ID = 0).{2}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = )[:digit:]+",ignore_case = T))),
                                      # Site ID mislabeled
                                      .default = str_c(str_extract(df[1,1],regex("(?<=Site ID = )[:alpha:]+",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = )[:digit:]+",ignore_case = T)))
                                    ),
                                  `Condition ID` = str_extract(df[1,1],regex("(?<=Condition ID = ).{3}",ignore_case = T)),
                                  `Reader ID` = str_extract(df[2,1],regex("(?<=\\s).{13}",ignore_case = T)),
                                  `Date Time` = ymd_hms(str_c(Date,Time,sep = " ")),
                                   Type = Type,
                                   Gl = Gl,
                                   St = St,
                                   Tr = Tr),.progress = TRUE) |>
        map(\(df) df |> slice(3:n()),.progress = TRUE),bind_rows,.progress = TRUE) |>
      map(\(df) df |>  arrange(`Date Time`),.progress = TRUE) |>
      map(\(df) df |> fill(c(`Subject ID`,`Condition ID`,`Reader ID`),.direction = "up"),.progress = TRUE) |>
      map(\(df) df |> fill(`Sensor Serial Number`,.direction = "down"),.progress = TRUE) |>
      map(\(df) df |> relocate(`Subject ID`,`Condition ID`,`Sensor Serial Number`,
                               `Reader ID`,`Date Time`,Type,Gl,St,Tr),.progress = TRUE) |>
      list_rbind(names_to = "Path")

  } else {

    # All Upload Data
    map2(
      # First List
      # Import Events
      events |>
        set_names() |>
        # Consider empty events.csv
        map(possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_types = c(`Col 9` = "c"),col_select = c(Date,Time,Type,`Col 9`)),tibble()),.progress = TRUE) |>
        map(\(df) df |> filter(Type == "SENSOR_STARTED (58)"),.progress = TRUE) |>
        map(\(df) df |> transmute(
          `Date Time` = ymd_hms(str_c(Date,Time,sep = " ")),
           Type = Type,
          `Sensor Serial Number` = `Col 9`),.progress = TRUE),
        # Replaced Sensors Only
        # map(\(df) df |> slice_max(`Date Time`,n = 1)),

      map2(
        # Second List
        # Import gluc.csv
        gluc |>
          map(possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,col_types = c(Type = "c"),col_select = c(`Unique Record ID`,Date,Time,Type,Gl,St,Tr),n_max = 2),tibble()),.progress = TRUE),
        gluc |>
          map(possibly(\(path) data.table::fread(path,select = c(1:7),skip = 3,col.names = c("Unique Record ID","Date","Time","Type","Gl","St","Tr"),colClasses = c("V2" = "Date","V4" = "character")),tibble()),.progress = TRUE),
        bind_rows,.progress = TRUE) |>
        map(\(df) df |> transmute(`Subject ID` =
                                    case_when(
                                      # Site ID == ADC
                                      str_to_upper(str_extract(df[1,1],regex("(?<=Site ID = ).{3}",ignore_case = T))) == "ADC" ~ str_extract(df[1,1],regex("(?<=Subject ID = )[:digit:]{4}",ignore_case = T)),
                                      # Site ID == 009
                                      str_extract(df[1,1],regex("(?<=Site ID = ).{2}",ignore_case = T)) == "00" ~
                                        str_c(str_extract(df[1,1],regex("(?<=Site ID = 00).{1}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                                      # Site ID starts with 1
                                      str_extract(df[1,1],regex("(?<=Site ID = ).{1}",ignore_case = T)) == "1" ~
                                        str_c(str_extract(df[1,1],regex("(?<=Site ID = ).{3}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                                      # Site ID == 081 or 057
                                      str_extract(df[1,1],regex("(?<=Site ID = ).{1}",ignore_case = T)) == "0" ~ str_c(str_extract(df[1,1],regex("(?<=Site ID = 0).{2}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = )[:digit:]+",ignore_case = T))),
                                      # Site ID mislabeled
                                      .default = str_c(str_extract(df[1,1],regex("(?<=Site ID = )[:alpha:]+",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = )[:digit:]+",ignore_case = T)))
                                    ),
                                  `Condition ID` = str_extract(df[1,1],regex("(?<=Condition ID = ).{3}",ignore_case = T)),
                                  `Reader ID` = str_extract(df[2,1],regex("(?<=\\s).{13}",ignore_case = T)),
                                  `Date Time` = ymd_hms(str_c(Date,Time,sep = " ")),
                                   Type = Type,
                                   Gl = Gl,
                                   St = St,
                                   Tr = Tr),.progress = TRUE) |>
        map(\(df) df |> slice(3:n()),.progress = TRUE),bind_rows,.progress = TRUE) |>
      map(\(df) df |> arrange(`Date Time`),.progress = TRUE) |>
      map(\(df) df |> fill(c(`Subject ID`,`Condition ID`,`Reader ID`),.direction = "up"),.progress = TRUE) |>
      map(\(df) df |> fill(`Sensor Serial Number`,.direction = "down"),.progress = TRUE) |>
      map(\(df) df |> relocate(`Subject ID`,`Condition ID`,`Sensor Serial Number`,
                               `Reader ID`,`Date Time`,Type,Gl,St,Tr),.progress = TRUE) |>
      list_rbind(names_to = "Path") |>
      # Remove Duplicated Uploads
      distinct() |>
      arrange(Path,`Subject ID`,`Condition ID`,`Sensor Serial Number`)
  }
}
