#' Import Sibionics Sensor Data
#'
#' @param sibonics_path File path of xlsx or csv
#' @param index Index to get an individual or specific upload. Default is NULL.
#'
#' @return A tibble
#' @export

sibonics <- function(sibonics_path, index = NULL){

  if (is.numeric(index)) {
    if(str_detect(sibonics_path[index],".xlsx")){
      # .xlsx
      sibonics_path[index][str_detect(sibonics_path[index],".xlsx")] |>
        set_names() |>
        map(\(path) readxl::read_excel(path,col_names = T,col_types = c("guess","guess"))) |>
        list_rbind(names_to = "Path") |>
        transmute(Path = Path,
                  `Subject ID` = str_extract(Path,"(?<=0)[:digit:]{6}"),
                  `Date Time` = ymd_hm(血糖时间),
                  Gl = as.numeric(na_if(血糖值,"异常")))
    } else {
      # .csv
      sibonics_path[index][str_detect(sibonics_path[index],".csv")] |>
        set_names() |>
        map(\(path) vroom::vroom(path,delim = ",",show_col_types = F,col_select = c(ast,t,name,v,vSecond))) |>
        list_rbind(names_to = "Path") |>
        mutate(`Subject ID` = str_extract(Path,"(?<=0)[:digit:]{6}"),
               `Date Time` = floor_date(as_datetime(t/1000,tz = "UTC"),"min"),
               Gl = v) |>
        # One-hour Warn up
        filter(`Date Time` >= first(`Date Time`) + dminutes(59),.by = `Subject ID`) |>
        # Valid Glucose Reading
        filter(vSecond != 0) |>
        # Assign abnormal
        transmute(Path = Path,
                  `Subject ID` = `Subject ID`,
                  `Date Time` = `Date Time`,
                  Gl = case_when(ast == 2 ~ NA,
                                 .default = Gl))
    }
  } else {
    # .xlsx
    sibonics_path[str_detect(sibonics_path,".xlsx")] |>
      set_names() |>
      map(\(path) readxl::read_excel(path,col_names = T,col_types = c("guess","guess"))) |>
      list_rbind(names_to = "Path") |>
      transmute(Path = Path,
                `Subject ID` = str_extract(Path,"(?<=0)[:digit:]{6}"),
                `Date Time` = ymd_hm(血糖时间),
                Gl = as.numeric(na_if(血糖值,"异常"))) |>
      bind_rows(
        # .csv
        sibonics_path[str_detect(sibonics_path,".csv")] |>
          set_names() |>
          map(\(path) vroom::vroom(path,delim = ",",show_col_types = F,col_select = c(ast,t,name,v,vSecond))) |>
          list_rbind(names_to = "Path") |>
          mutate(`Subject ID` = str_extract(Path,"(?<=0)[:digit:]{6}"),
                 `Date Time` = floor_date(as_datetime(t/1000,tz = "UTC"),"min"),
                 Gl = v) |>
          # One-hour Warn up
          filter(`Date Time` >= first(`Date Time`) + dminutes(59),.by = `Subject ID`) |>
          # Valid Glucose Reading
          filter(vSecond != 0) |>
          # Assign abnormal
          transmute(Path = Path,
                    `Subject ID` = `Subject ID`,
                    `Date Time` = `Date Time`,
                    Gl = case_when(ast == 2 ~ NA,
                                   .default = Gl))) |>
      arrange(`Subject ID`)
  }
}
