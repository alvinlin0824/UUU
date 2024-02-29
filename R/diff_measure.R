#' Difference Measures Analysis
#'
#' @param data Paired Data Set
#' @param group_var Any Categorical Variables
#' @param ref Provide Reference Readings such as YSI or BG
#' @param cgm Provide Sensor Readings such as Gl or ANA
#' @param unit Either mg/dL or mmol/L. Default is mg/dL
#' @param reference_breakpoint Default is TRUE
#'
#' @return Gt Table
#' @export

diff_measure <- function(data, ref, cgm, group_var, unit = "mg/dL", reference_breakpoint = TRUE) {

  round_normal <- function(x, digits){
    ifelse(x >= 0,round(x + 10^-(digits + 3),digits),round(x - 10^-(digits + 3),digits))
  }

  diff_calculation <- list(Mean = ~ mean(.x, na.rm = T),
                           SD = ~ sd(.x, na.rm = T),
                           Median = ~ median(.x, na.rm = T),
                           Min = ~ min(.x, na.rm = T),
                           Max = ~ max(.x, na.rm = T),
                           N = ~ sum(!is.na(.x),na.rm = T))
  df <- data |>
    mutate(Difference = {{cgm}} - {{ref}},
           `Absolute Difference` = abs(Difference),
           `Relative Difference(%)` = (Difference/{{ref}})*100,
           `Absolute Relative Difference(%)` = abs(`Relative Difference(%)`))



  if (reference_breakpoint == TRUE) {
    # gt_group_var <- c("Level",group_var)
    df |>
      mutate(# Reference Level
        Level = if (unit == "mg/dL") {
          case_when(round({{ref}} + 0.001) < 100 ~ "<100 mg/dL[5.6 mmol/L]",
                    .default = ">=100 mg/dL[5.6 mmol/L]")}
        else if (unit == "mmol/L"){
          case_when(round({{ref}} + 0.001,1) < 5.6 ~ "<100 mg/dL[5.6 mmol/L]",
                    .default = ">=100 mg/dL[5.6 mmol/L]")
        })|>
      # Overall Reference Level
      bind_rows(df |>
                  mutate(Level = "Overall Levels (100 mg/dL[5.6 mmol/L] breakpoint)")) |>
      pivot_longer(Difference:`Absolute Relative Difference(%)`,names_to = "Measure") |>
      filter((Measure %in% c("Difference", "Absolute Difference") &
                Level %in% c("<100 mg/dL[5.6 mmol/L]", "Overall Levels (100 mg/dL[5.6 mmol/L] breakpoint)")) |
               (Measure %in% c("Relative Difference(%)", "Absolute Relative Difference(%)") &
                  Level %in% c(">=100 mg/dL[5.6 mmol/L]", "Overall Levels (100 mg/dL[5.6 mmol/L] breakpoint)"))) |>
      group_by(pick(Level,Measure)) |>
      # User-Defined group variables
      group_by(pick({{group_var}}),.add = TRUE) |>
      summarise_at(.vars = "value", diff_calculation) |>
      ungroup() |>
      # Round Number
      mutate(across(c(Mean:Max), ~ round_normal(.x,1))) |>
      # Unit
      mutate(Measure = case_when(
        unit == "mg/dL" & Measure %in% c("Difference","Absolute Difference")
        ~ str_c(Measure," (",unit,")"),
        unit == "mmol/L" & Measure %in% c("Difference","Absolute Difference")
        ~ str_c(Measure," (",unit,")"),
        .default = Measure
      ),
      Level = factor(Level, levels = c("<100 mg/dL[5.6 mmol/L]",">=100 mg/dL[5.6 mmol/L]", "Overall Levels (100 mg/dL[5.6 mmol/L] breakpoint)")),
      Measure = factor(Measure, levels = c("Difference (mg/dL)","Absolute Difference (mg/dL)","Difference (mmol/L)","Absolute Difference (mmol/L)","Relative Difference(%)","Absolute Relative Difference(%)"))) |>
      arrange(pick(Level,Measure)) |>
      arrange(pick({{group_var}})) |>
      gt::gt(groupname_col = c(group_var,"Level")) |>
      gt::cols_align(align = "center") |>
      gt::fmt_number(columns = Mean:Max,decimals = 1) |>
      gt::sub_missing(columns = everything(),missing_text = "") |>
      gt::opt_stylize(style = 6, color = "blue")
  } else {
    gt_group_var <- group_var[1:length(group_var) - 1]
    df |>
      group_by(pick({{group_var}})) |>
      summarise(
        across(c(Difference:`Absolute Relative Difference(%)`),
               diff_calculation[1:3],.names = "{.col} {.fn}"), N = n(),.groups = "drop") |>
      relocate(N,.after = last_col()) |>
      # Round Number
      mutate(across(contains(c("Mean","Median")), ~ round_normal(.x,1)),
             across(contains(c("SD")), ~ round_normal(.x,2))) |>
      gt::gt(groupname_col = c(gt_group_var))  |>
      gt::cols_align(align = "center") |>
      gt::tab_spanner(label = str_c("Difference"," (",unit,")"),columns = c("Difference Mean","Difference Median","Difference SD")) |>
      gt::tab_spanner(label = str_c("Abs. Difference"," (",unit,")"),columns = c("Absolute Difference Mean","Absolute Difference Median","Absolute Difference SD")) |>
      gt::tab_spanner(label = "Relative Difference(%)",columns = c("Relative Difference(%) Mean","Relative Difference(%) Median","Relative Difference(%) SD")) |>
      gt::tab_spanner(label = "Absolute Relative Difference(%)",columns = c("Absolute Relative Difference(%) Mean","Absolute Relative Difference(%) Median","Absolute Relative Difference(%) SD")) |>
      gt::fmt_number(columns = contains(c("Mean","Median")),decimals = 1) |>
      gt::fmt_number(columns = contains(c("SD")),decimals = 2) |>
      gt::cols_label(
        `Difference Mean` = "Mean",`Difference Median` = "Median",
        `Difference SD` = "SD",
        `Relative Difference(%) Mean` = "Mean",`Relative Difference(%) Median` = "Median",`Relative Difference(%) SD` = "SD",
        `Absolute Difference Mean` = "Mean",`Absolute Difference Median` = "Median",
        `Absolute Difference SD` =  "SD",`Absolute Relative Difference(%) Mean` = "Mean",
        `Absolute Relative Difference(%) Median` = "Median",
        `Absolute Relative Difference(%) SD` = "SD") |>
      gt::sub_missing(columns = everything(),missing_text = "") |>
      gt::opt_stylize(style = 6, color = "blue")
  }
}
