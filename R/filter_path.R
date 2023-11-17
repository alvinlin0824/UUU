#' Filter matched patterns from path
#'
#' @param file_path File path of events.csv, gluc.csv, glucPlus.csv, freestyle.csv, anaPlus.csv and oneMinutes.csv.
#' @param filter_text Match a fixed string that you would like to remove. Default is an empty string.
#'
#' @return A list
#' @export

filter_path <- function(file_path, filter_text = "^$|pattern"){
  events_path <- file_path[stringr::str_detect(file_path,"events") &
                             !stringr::str_detect(file_path,stringr::regex("Archive|Archives|Transfer|Transfers",ignore_case = T)) & !stringr::str_detect(file_path,stringr::regex(filter_text,ignore_case = T))]

  gluc_path <- file_path[stringr::str_detect(file_path,"gluc") &
                           !stringr::str_detect(file_path,stringr::regex("Archive|Archives|Transfer|Transfers",ignore_case = T)) & !stringr::str_detect(file_path,stringr::regex(filter_text,ignore_case = T))]

  glucPlus_path <- file_path[stringr::str_detect(file_path,"glucPlus") &
                               !stringr::str_detect(file_path,stringr::regex("Archive|Archives|Transfer|Transfers",ignore_case = T)) & !stringr::str_detect(file_path,stringr::regex(filter_text,ignore_case = T))]

  free_path <- file_path[stringr::str_detect(file_path,stringr::regex("freestyle",ignore_case = T)) &
                           !stringr::str_detect(file_path,stringr::regex("Archive|Archives|Transfer|Transfers",ignore_case = T)) & !stringr::str_detect(file_path,stringr::regex(filter_text,ignore_case = T))]

  anaPlus_path <- file_path[stringr::str_detect(file_path,stringr::regex("anaPlus",ignore_case = T)) &
                           !stringr::str_detect(file_path,stringr::regex("Archive|Archives|Transfer|Transfers",ignore_case = T)) & !stringr::str_detect(file_path,stringr::regex(filter_text,ignore_case = T))]

  oneMinutes_path <- file_path[stringr::str_detect(file_path,stringr::regex("oneMinutes",ignore_case = T)) &
                              !stringr::str_detect(file_path,stringr::regex("Archive|Archives|Transfer|Transfers",ignore_case = T)) & !stringr::str_detect(file_path,stringr::regex(filter_text,ignore_case = T))]

  out <- list(events_path, gluc_path, glucPlus_path, free_path, anaPlus_path, oneMinutes_path)

  return(out)
}
