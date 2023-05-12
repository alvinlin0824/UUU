#' Filter matched patterns from path
#'
#' @param file_path File path of events.csv, gluc.csv, glucPlus.csv, and freestyle.csv.
#' @param filter_text Match a fixed string that you would like to remove. Default is an empty string.
#'
#' @return A list
#' @export

filter_path <- function(file_path, filter_text = "^$|pattern"){
  events_path <- file_path[str_detect(file_path,"events") &
                             !str_detect(file_path,regex("Archive|Archives|Transfer|Transfers",ignore_case = T)) & !str_detect(file_path,regex(filter_text,ignore_case = T))]

  gluc_path <- file_path[str_detect(file_path,"gluc") &
                           !str_detect(file_path,regex("Archive|Archives|Transfer|Transfers",ignore_case = T)) & !str_detect(file_path,regex(filter_text,ignore_case = T))]

  glucPlus_path <- file_path[str_detect(file_path,"glucPlus") &
                               !str_detect(file_path,regex("Archive|Archives|Transfer|Transfers",ignore_case = T)) & !str_detect(file_path,regex(filter_text,ignore_case = T))]

  free_path <- file_path[str_detect(file_path,regex("freestyle",ignore_case = T)) &
                           str_detect(file_path,regex("BGM",ignore_case = T)) &
                           !str_detect(file_path,regex("Archive|Archives|Transfer|Transfers",ignore_case = T)) & !str_detect(file_path,regex(filter_text,ignore_case = T))]

  out <- list(events_path, gluc_path, glucPlus_path, free_path)
  return(out)
}
