#' Concurrence Analysis
#'
#' @param data Paired Data Set
#' @param bin Default is 40,60,80,120,160,200,250,300,350,400. Users can provide their own
#' @param ref Provide Reference Readings
#' @param cgm Provide Sensor Readings such as Gl or ANA
#' @param group_var Any Categorical Variables
#' @param trans Transpose table. Default is FALSE
#'
#' @return Gt Table
#' @export
#'

concurrence <- function(data, bin = c(40,60,80,120,160,200,250,300,350,400), ref, cgm, group_var, trans = FALSE){

  bins <- c(0,bin,Inf)
  lv <- vector("list")
  for (i in 2:length(bins)) {
    if (i == 2){
      lv[[i]] <- c(str_c("<",bins[i]),str_c(bins[i],"-",bins[i+1]))
    }
    else if (i %in% c(3:(length(bins)-2))){
      lv[[i]] <- str_c(bins[i]+1,"-",bins[i+1])
    }
    else if (i == length(bins)-1) {
      lv[[i]] <- str_c(">",bins[length(bins)-1])
    }
  }

  label <- lv |>
           list_c()

  gt_group_var <- group_var

  if (trans == FALSE){
    map2(data |>
           group_split(pick({{group_var}})) |>
           map(\(df) df |> select({{group_var}})) |>
           map(\(df) df |> slice(1:(length(bins)-1))),
         data |>
           # Round half to even, so add noise
           mutate(across(c({{cgm}},{{ref}}), ~ round(.x + 0.0001)),
                  {{ref}} := case_when({{ref}} == bins[2] ~
                                         cut({{ref}} + 0.001, breaks = bins, right = T,include.lowest = T,
                                             labels = label),
                                       .default = cut({{ref}}, breaks = bins, right = T,include.lowest = T,labels = label)),
                  {{cgm}} := case_when({{cgm}} == bins[2] ~
                                         cut({{cgm}} + 0.001, breaks = bins, right = T,include.lowest = T,
                                             labels = label
                                         ),
                                       .default = cut({{cgm}}, breaks = bins, right = T,include.lowest = T,labels = label))) |>
           group_split(pick({{group_var}})) |>
           map(\(df) janitor::tabyl(df,{{cgm}},{{ref}})) |>
           janitor::adorn_totals("col") |>
           janitor::adorn_percentages("row") |>
           janitor::adorn_pct_formatting(digits = 1) |>
           janitor::adorn_ns(),bind_cols) |>
      list_rbind() |>
      mutate(Total = str_remove_all(Total,"100.0%|\\(|\\)")) |>
      gt::gt(groupname_col = c(gt_group_var)) |>
      gt::cols_align(align = "center") |>
      gt::cols_label({{cgm}} := "CGM (mg/dL)") |>
      gt::sub_missing(columns = everything(),missing_text = "") |>
      gt::opt_stylize(style = 6, color = "blue") |>
      gt::tab_spanner(label = "YSI (mg/dL)",columns = !contains("Total")) |>
      gt::tab_header(title = gt::md("Concurrence Analysis by Glucose Level"))
  } else {
    map2(data |>
           group_split(pick({{group_var}})) |>
           map(\(df) df |> select({{group_var}})) |>
           map(\(df) df |> slice(1:(length(bins)-1))),
         data |>
           # Round half to even, so add noise
           mutate(across(c({{cgm}},{{ref}}), ~ round(.x + 0.0001)),
                  {{ref}} := case_when({{ref}} == bins[2] ~
                                         cut({{ref}} + 0.001, breaks = bins, right = T,include.lowest = T,
                                             labels = label),
                                       .default = cut({{ref}}, breaks = bins, right = T,include.lowest = T,labels = label)),
                  {{cgm}} := case_when({{cgm}} == bins[2] ~
                                         cut({{cgm}} + 0.001, breaks = bins, right = T,include.lowest = T,
                                             labels = label
                                         ),
                                       .default = cut({{cgm}}, breaks = bins, right = T,include.lowest = T,labels = label))) |>
           group_split(pick({{group_var}})) |>
           map(\(df) janitor::tabyl(df,{{ref}},{{cgm}})) |>
           janitor::adorn_totals("col") |>
           janitor::adorn_percentages("row") |>
           janitor::adorn_pct_formatting(digits = 1) |>
           janitor::adorn_ns(),bind_cols) |>
      list_rbind() |>
      mutate(Total = str_remove_all(Total,"100.0%|\\(|\\)")) |>
      gt::gt(groupname_col = c(gt_group_var)) |>
      gt::cols_align(align = "center") |>
      gt::cols_label({{ref}} := "YSI (mg/dL)") |>
      gt::sub_missing(columns = everything(),missing_text = "") |>
      gt::opt_stylize(style = 6, color = "blue") |>
      gt::tab_spanner(label = "CGM (mg/dL)",columns = !contains("Total")) |>
      gt::tab_header(title = gt::md("Concurrence Analysis by YSI Glucose Level"))
  }
}
