#' Import Dexcom csv or xlsx upload data
#'
#' @param dexcom_path File path of dexcom
#' @param index Index to get an individual or specific upload. Default is NULL.
#'
#' @return A tibble
#' @export

dexcom <- function(dexcom_path, index = NULL){

  if(is.numeric(index)){
    c(dexcom_path[index] |>
        fs::path_filter(regexp = "Clarity",ignore.case = T) |>
        fs::path_filter(regexp = "csv",ignore.case = T) |>
        set_names() |>
        map(\(path) vroom::vroom(path,show_col_types = F, col_select = c(`Timestamp (YYYY-MM-DDThh:mm:ss)`
                                                                         ,`Event Type`,`Patient Info`,`Glucose Value (mg/dL)`),col_types = c(`Glucose Value (mg/dL)` = "d")),.progress = T),
      # xlsx
      dexcom_path[index] |>
        fs::path_filter(regexp = "Clarity",ignore.case = T) |>
        fs::path_filter(regexp = "xlsx",ignore.case = T) |>
        set_names() |>
        map(\(path) readxl::read_excel(path),.progress = T) |>
        map(\(df) df |> dplyr::transmute(`Timestamp (YYYY-MM-DDThh:mm:ss)` = ymd_hms(`Timestamp (YYYY-MM-DDThh:mm:ss)`),
                                         `Event Type` = `Event Type`,
                                         `Patient Info` = as.character(`Patient Info`),
                                         `Glucose Value (mg/dL)` = as.numeric(`Glucose Value (mg/dL)`)
        ),.progress = T)) |>
      map(\(df) df |> dplyr::mutate(`Patient Info` = str_remove(`Patient Info`,"^0+")),.progress = T)  |>
      map(\(df) df |> transmute(`Subject ID` = case_when(
        str_length(df[1,3]) == 1 ~ str_c(df[2,3],"000",df[1,3]),
        str_length(df[1,3]) == 2 ~ str_c(df[2,3],"00",df[1,3]),
        .default = str_c(df[2,3],df[1,3])),
        `Date Time` = `Timestamp (YYYY-MM-DDThh:mm:ss)`,
        Type = `Event Type`,
        Gl = `Glucose Value (mg/dL)`),.progress = T) |>
      list_rbind(names_to = "Path") |>
      dplyr::mutate(`Condition ID` = str_extract(Path,regex("[:alpha:]{3}(?=_[:digit:]{4}-[:digit:]{2}-[:digit:]{2})")),.before = `Date Time`) |>
      filter(Type == "EGV") |>
      arrange(`Subject ID`)
  } else {
    c(dexcom_path |>
        fs::path_filter(regexp = "Clarity",ignore.case = T) |>
        fs::path_filter(regexp = "csv",ignore.case = T) |>
        set_names() |>
        map(\(path) vroom::vroom(path,show_col_types = F, col_select = c(`Timestamp (YYYY-MM-DDThh:mm:ss)`
                                                                         ,`Event Type`,`Patient Info`,`Glucose Value (mg/dL)`),col_types = c(`Glucose Value (mg/dL)` = "d")),.progress = T),
      # xlsx
      dexcom_path |>
        fs::path_filter(regexp = "Clarity",ignore.case = T) |>
        fs::path_filter(regexp = "xlsx",ignore.case = T) |>
        set_names() |>
        map(\(path) readxl::read_excel(path),.progress = T) |>
        map(\(df) df |> dplyr::transmute(`Timestamp (YYYY-MM-DDThh:mm:ss)` = ymd_hms(`Timestamp (YYYY-MM-DDThh:mm:ss)`),
                                         `Event Type` = `Event Type`,
                                         `Patient Info` = as.character(`Patient Info`),
                                         `Glucose Value (mg/dL)` = as.numeric(`Glucose Value (mg/dL)`)
        ),.progress = T)) |>
      map(\(df) df |> dplyr::mutate(`Patient Info` = str_remove(`Patient Info`,"^0+")),.progress = T)  |>
      map(\(df) df |> transmute(`Subject ID` = case_when(
        str_length(df[1,3]) == 1 ~ str_c(df[2,3],"000",df[1,3]),
        str_length(df[1,3]) == 2 ~ str_c(df[2,3],"00",df[1,3]),
        .default = str_c(df[2,3],df[1,3])),
        `Date Time` = `Timestamp (YYYY-MM-DDThh:mm:ss)`,
        Type = `Event Type`,
        Gl = `Glucose Value (mg/dL)`),.progress = T) |>
      list_rbind(names_to = "Path") |>
      dplyr::mutate(`Condition ID` = str_extract(Path,regex("[:alpha:]{3}(?=_[:digit:]{4}-[:digit:]{2}-[:digit:]{2})")),.before = `Date Time`) |>
      filter(Type == "EGV") |>
      arrange(`Subject ID`)
  }
}
