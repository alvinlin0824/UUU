#' Import YSI csv upload data
#'
#' @param ysi_path File path of YSI.
#' @param index Index to get an individual or specific upload. Default is NULL.
#'
#' @return A tibble
#' @export

import_ysi <- function(ysi_path, index = NULL){

  if (!is.numeric(index) && !is.null(index)) {rlang::abort("Index must be numeric!")}

  if(is.numeric(index)) {
    if (index > length(ysi_path)) {rlang::abort("Index beyond the range!")}
    else {
      ysi_path[index] |>
        purrr::set_names() |>
        map(purrr::possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,skip = 6,.name_repair = ~ janitor::make_clean_names(., case = "big_camel"),col_types = list(ComputerDate = col_date(format = "%m/%d/%Y"),InstrumentTime_2 = col_time(format = "%H:%M:%S"),InstrumentName = "c",SampleId = "c",Result = "d",Result_2 = "d"),col_select = c(InstrumentName,ReportType,ComputerDate,InstrumentDate,InstrumentTime_2,SampleId,Result,Result_2)),tibble::tibble()),.progress = TRUE) |>
        list_rbind(names_to = "Path") |>
        filter(ReportType == "Sample") |>
        transmute(
          Path = str_remove_all(Path,"[:space:]"),
          `Instrument Name` = str_extract(InstrumentName,"[:digit:].+"),
          `Report Type` = ReportType,
          `Instrument Date` = case_when(
            month(ComputerDate) == month(mdy(InstrumentDate)) ~ mdy(InstrumentDate),
            month(ComputerDate) == month(dmy(InstrumentDate)) ~ dmy(InstrumentDate),
            .default = mdy(InstrumentDate)
          ),
          `Instrument Time` = case_when(
            between(hour(InstrumentTime_2),1,4) ~ parse_time(str_c(`InstrumentTime_2`,"PM",sep = " "),"%H:%M:%S %p"),
            .default = InstrumentTime_2
          ),
          `Sample ID` = SampleId,
          `YSI Result Black Probe (mg/dL)` = Result,
          `YSI Result White Probe (mg/dL)` = Result_2,
          Site = str_extract(Path,"[:alpha:]{3,4}(?=/[:digit:]{4})"),
          `File Date` = str_extract(Path,"[:digit:]{4}[:punct:][:digit:]{2}[:punct:][:digit:]{2}")) |>
        suppressWarnings()
    }
  }
  else if (is.null(index)){
    ysi_path |>
      purrr::set_names() |>
      map(purrr::possibly(\(path) vroom::vroom(path,delim = ",",col_names = T,show_col_types = F,skip = 6,.name_repair = ~ janitor::make_clean_names(., case = "big_camel"),col_types = list(ComputerDate = col_date(format = "%m/%d/%Y"),InstrumentTime_2 = col_time(format = "%H:%M:%S"),InstrumentName = "c",SampleId = "c",Result = "d",Result_2 = "d"),col_select = c(InstrumentName,ReportType,ComputerDate,InstrumentDate,InstrumentTime_2,SampleId,Result,Result_2)),tibble::tibble()),.progress = TRUE) |>
      list_rbind(names_to = "Path") |>
      filter(ReportType == "Sample") |>
      transmute(
        Path = str_remove_all(Path,"[:space:]"),
        `Instrument Name` = str_extract(InstrumentName,"[:digit:].+"),
        `Report Type` = ReportType,
        `Instrument Date` = case_when(
          month(ComputerDate) == month(mdy(InstrumentDate)) ~ mdy(InstrumentDate),
          month(ComputerDate) == month(dmy(InstrumentDate)) ~ dmy(InstrumentDate),
          .default = mdy(InstrumentDate)
        ),
        `Instrument Time` = case_when(
          between(hour(InstrumentTime_2),1,4) ~ parse_time(str_c(`InstrumentTime_2`,"PM",sep = " "),"%H:%M:%S %p"),
          .default = InstrumentTime_2
        ),
        `Sample ID` = SampleId,
        `YSI Result Black Probe (mg/dL)` = Result,
        `YSI Result White Probe (mg/dL)` = Result_2,
        Site = str_extract(Path,"[:alpha:]{3,4}(?=/[:digit:]{4})"),
        `File Date` = str_extract(Path,"[:digit:]{4}[:punct:][:digit:]{2}[:punct:][:digit:]{2}")) |>
      suppressWarnings()
  }
}
