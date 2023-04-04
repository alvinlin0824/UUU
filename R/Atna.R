library(tidyverse)
library(fs)
library(vroom)

# AUU path \\wf00168p.oneabbott.com\data1\CDM\ADC-US-RES-21217\SE02\UploadData\AUU\083-IDR
# \\oneabbott.com\dept\ADC\Technical_OPS\Clinical_Affairs\Clinical Study Files\Sensor Systems\ADC-US-RES-16157_InHouse Sensor\CDM\RX Data\Sub-Study 076

# 217 SE02
# file_list <- dir_ls(gsub("\\\\", "/", r"(\\wf00168p.oneabbott.com\data1\CDM\ADC-US-RES-21217\SE02\UploadData\AUU\083-IDR)"),recurse = T,glob = "*events.csv|*gluc.csv|*freestyle.csv")
# events_path <- file_list[str_detect(file_list,"events") & str_detect(file_list,"FV") & !str_detect(file_list,regex("Archive",ignore_case = T)) &  !str_detect(file_list,regex("Transfers",ignore_case = T))]
# gluc_path <- file_list[str_detect(file_list,"gluc") & str_detect(file_list,"FV") & !str_detect(file_list,regex("Archive",ignore_case = T)) &  !str_detect(file_list,regex("Transfers",ignore_case = T))]

# InHouse 076
# file_list <- dir_ls(gsub("\\\\", "/", r"(\\oneabbott.com\dept\ADC\Technical_OPS\Clinical_Affairs\Clinical Study Files\Sensor Systems\ADC-US-RES-16157_InHouse Sensor\CDM\RX Data\Sub-Study 076)"),recurse = T,glob = "*events.csv|*gluc.csv|*freestyle.csv")
# events_path <- file_list[str_detect(file_list,"events") & !str_detect(file_list,regex("BGM",ignore_case = T)) &  !str_detect(file_list,regex("Interim",ignore_case = T))]
# gluc_path <- file_list[str_detect(file_list,"gluc") & !str_detect(file_list,regex("BGM",ignore_case = T)) &  !str_detect(file_list,regex("Interim",ignore_case = T))]
# free_path <- file_list[str_detect(file_list,"freestyle") & str_detect(file_list,regex("BGM",ignore_case = T)) &  !str_detect(file_list,regex("Interim",ignore_case = T))]

atna <- function(events, gluc, index = NULL, ...) {

  # Individual File
  if (is.numeric(index)) {
    map2(
      # First List
      # Import Events
      events[index] |>
        # Consider empty events.csv
        map(possibly(\(path) vroom(path,delim = ",",col_names = T,show_col_types = F,col_types = c(`Col 9` = "c"),col_select = c(Date,Time,Type,`Col 9`)),tibble()),.progress = TRUE) |>
        map(\(df) df |> filter(Type == "SENSOR_STARTED (58)")) |>
        map(\(df) df |> transmute(
          `Date Time` = ymd_hms(str_c(Date,Time,sep = " ")),
          Type = Type,
          `Sensor Serial Number` = `Col 9`
        )) |>
        # Replaced Sensors Only
        map(\(df) df |> slice_max(`Date Time`,n = 1)),

      map2(
        # Second List
        # Import gluc.csv
        gluc[index] |>
          map(possibly(\(path) vroom(path,delim = ",",col_names = T,show_col_types = F,col_types = c(Type = "c"),col_select = c(`Unique Record ID`,Date,Time,Type,Gl),n_max = 2),tibble())),
        gluc[index] |>
          map(possibly(\(path) data.table::fread(path,select = c(1:5),skip = 3,col.names = c("Unique Record ID","Date","Time","Type","Gl"),colClasses = c("V2" = "Date","V4" = "character")),tibble())),
        bind_rows,.progress = TRUE) |>
        map(\(df) df |> transmute(`Subject ID` =
                                    case_when(
                                      # Site ID == ADC
                                      str_extract(df[1,1],regex("(?<=Site ID = ).{3}",ignore_case = T)) == "ADC" ~ str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T)),
                                      # Site ID == 009
                                      str_extract(df[1,1],regex("(?<=Site ID = ).{2}",ignore_case = T)) == "00" ~
                                        str_c(str_extract(df[1,1],regex("(?<=Site ID = 00).{1}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                                      # Site ID == 081
                                      .default = str_c(str_extract(df[1,1],regex("(?<=Site ID = 0).{2}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T)))
                                    ),
                                  `Condition ID` = str_extract(df[1,1],regex("(?<=Condition ID = ).{3}",ignore_case = T)),
                                  `Reader ID` = str_extract(df[2,1],regex("(?<=Reader_S_N ).{13}",ignore_case = T)),
                                  `Date Time` = ymd_hms(str_c(Date,Time,sep = " ")),
                                  Type = Type,
                                  Gl = Gl)) |>
        map(\(df) df |> slice(3:n())),bind_rows) |>
      map(\(df) df |> fill(c(`Subject ID`,`Condition ID`,`Reader ID`),.direction = "up")) |>
      map(\(df) df |> fill(`Sensor Serial Number`,.direction = "down")) |>
      map(\(df) df |> relocate(`Subject ID`,`Condition ID`,`Sensor Serial Number`,
                               `Reader ID`,`Date Time`,Type,Gl)) |>
      list_rbind()

  } else {

    # All Upload Data
    map2(
      # First List
      # Import Events
      events |>
        # Consider empty events.csv
        map(possibly(\(path) vroom(path,delim = ",",col_names = T,show_col_types = F,col_types = c(`Col 9` = "c"),col_select = c(Date,Time,Type,`Col 9`)),tibble()),.progress = TRUE) |>
        map(\(df) df |> filter(Type == "SENSOR_STARTED (58)")) |>
        map(\(df) df |> transmute(
          `Date Time` = ymd_hms(str_c(Date,Time,sep = " ")),
          Type = Type,
          `Sensor Serial Number` = `Col 9`
        )) |>
        # Replaced Sensors Only
        map(\(df) df |> slice_max(`Date Time`,n = 1)),

      map2(
        # Second List
        # Import gluc.csv
        gluc |>
          map(possibly(\(path) vroom(path,delim = ",",col_names = T,show_col_types = F,col_types = c(Type = "c"),col_select = c(`Unique Record ID`,Date,Time,Type,Gl),n_max = 2),tibble())),
        gluc |>
          map(possibly(\(path) data.table::fread(path,select = c(1:5),skip = 3,col.names = c("Unique Record ID","Date","Time","Type","Gl"),colClasses = c("V2" = "Date","V4" = "character")),tibble())),
        bind_rows,.progress = TRUE) |>
        map(\(df) df |> transmute(`Subject ID` =
                                    case_when(
                                      # Site ID == ADC
                                      str_extract(df[1,1],regex("(?<=Site ID = ).{3}",ignore_case = T)) == "ADC" ~ str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T)),
                                      # Site ID == 009
                                      str_extract(df[1,1],regex("(?<=Site ID = ).{2}",ignore_case = T)) == "00" ~
                                        str_c(str_extract(df[1,1],regex("(?<=Site ID = 00).{1}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T))),
                                      # Site ID == 081
                                      .default = str_c(str_extract(df[1,1],regex("(?<=Site ID = 0).{2}",ignore_case = T)),str_extract(df[1,1],regex("(?<=Subject ID = ).{4}",ignore_case = T)))
                                    ),
                                  `Condition ID` = str_extract(df[1,1],regex("(?<=Condition ID = ).{3}",ignore_case = T)),
                                  `Reader ID` = str_extract(df[2,1],regex("(?<=Reader_S_N ).{13}",ignore_case = T)),
                                  `Date Time` = ymd_hms(str_c(Date,Time,sep = " ")),
                                  Type = Type,
                                  Gl = Gl)) |>
        map(\(df) df |> slice(3:n())),bind_rows) |>
      map(\(df) df |> fill(c(`Subject ID`,`Condition ID`,`Reader ID`),.direction = "up")) |>
      map(\(df) df |> fill(`Sensor Serial Number`,.direction = "down")) |>
      map(\(df) df |> relocate(`Subject ID`,`Condition ID`,`Sensor Serial Number`,
                               `Reader ID`,`Date Time`,Type,Gl)) |>
      list_rbind() |>
      # Remove Duplicated Uploads
      distinct() |>
      arrange(`Subject ID`,`Condition ID`)
  }
}

# atna(events = events_path, gluc = gluc_path)
