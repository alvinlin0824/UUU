#' Difference Measures Analysis
#'
#' @param data Paired Data Set
#' @param type Either Glucose or Ketone. Case is not sensitive
#' @param ref Provide Reference Readings
#' @param cgm Provide Sensor Readings such as Gl or ANA
#' @param breakpoint It depends on which type and unit. Default is 100
#' @param col_subject Provide Subject Column
#' @param group_var Any Categorical Variables
#' @param unit Either mg/dL or mmol/L. Default is mg/dL
#' @param reference_breakpoint Default is TRUE

#' @return Gt Table
#' @export

diff_measure  <- function(data, type = "Glucose" ,ref, cgm, breakpoint = 100, col_subject, group_var, unit = "mg/dL", reference_breakpoint = TRUE) {

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
    select(!contains("Difference")) |>
    mutate(Subject = as.character({{col_subject}}),
           Difference = {{cgm}} - {{ref}},
           `Absolute Difference` = abs(Difference),
           `Relative Difference(%)` = (Difference/{{ref}})*100,
           `Absolute Relative Difference(%)` = abs(`Relative Difference(%)`))



  if (reference_breakpoint == TRUE) {
    # Glucose
    if (str_detect(type,regex("Glucose",ignore_case = T))) {
      lower_label <- if_else(unit == "mg/dL",
                             str_c("<",breakpoint," mg/dL[",round_normal(breakpoint/18.016,1)," mmol/L]"),
                             str_c("<",floor(breakpoint*18.016)," mg/dL[",breakpoint," mmol/L]")
      )
      upper_label <- if_else(unit == "mg/dL",
                             str_c(">=",breakpoint," mg/dL[",round_normal(breakpoint/18.016,1)," mmol/L]"),
                             str_c(">=",floor(breakpoint*18.016)," mg/dL[",breakpoint," mmol/L]")
      )
      overall_label <- if_else(unit == "mg/dL",
                               str_c("Overall Levels (",breakpoint," mg/dL[",round_normal(breakpoint/18.016,1)," mmol/L] breakpoint)"),
                               str_c("Overall Levels (",floor(breakpoint*18.016)," mg/dL[",breakpoint," mmol/L] breakpoint)")
      )
    }
    # Ketone
    else if (str_detect(type,regex("Ketone",ignore_case = T))) {
      lower_label <- str_c("<",breakpoint," mmol/L")
      upper_label <- str_c(">=",breakpoint," mmol/L")
      overall_label <- str_c("Overall Levels (",breakpoint," mmol/L breakpoint)")
    }

    df1 <- df |>
      # Reference Level
      mutate(Level = if (str_detect(type,regex("Glucose",ignore_case = T))){
        if (unit == "mg/dL") {
          case_when(round({{ref}} + 0.001) < breakpoint ~ lower_label,
                    .default = upper_label)}
        else if (unit == "mmol/L"){
          case_when(round({{ref}} + 0.001,1) < breakpoint ~ lower_label,
                    .default = upper_label)}}
        else if (str_detect(type,regex("Ketone",ignore_case = T))){
          case_when({{ref}} < breakpoint ~ lower_label,
                    .default = upper_label)
        }
      ) |>
      # Overall Reference Level
      bind_rows(df |>
                  mutate(Level = overall_label)) |>
      pivot_longer(Difference:`Absolute Relative Difference(%)`,names_to = "Measure") |>
      filter((Measure %in% c("Difference", "Absolute Difference") &
                Level %in% c(lower_label, overall_label)) |
               (Measure %in% c("Relative Difference(%)", "Absolute Relative Difference(%)") &
                  Level %in% c(upper_label, overall_label))) |>
      group_by(pick(Level,Measure))

    # Diff Measure
    df1 |>
      # User-Defined group variables
      group_by(pick({{group_var}}),.add = TRUE) |>
      summarise_at(.vars = "value", diff_calculation) |>
      ungroup() |>
      left_join(
        # Confidence Interval
        df1 |>
          # User-Defined group variables `Type of Diabetes`
          group_by(pick({{group_var}}),.add = TRUE) |>
          # Need to think second
          group_by(Subject, .add = TRUE) |>
          summarise(bias_mean_subj = mean(value, na.rm = T),.groups = "drop_last") |>
          summarise(bias_mean = mean(bias_mean_subj, na.rm = T),
                    bias_sd = sd(bias_mean_subj, na.rm = T)/n()**0.5,
                    N = n(),.groups = "drop") |>
          mutate(`Grand Mean (95% CI)` = str_c(round_normal(bias_mean,1), " (",
                                               round_normal(bias_mean - qt(p = 0.025, df = N - 1, lower.tail = F) * bias_sd, digits = 1),
                                               ",",
                                               round_normal(bias_mean + qt(p = 0.025, df = N - 1, lower.tail = F) * bias_sd, digits = 1),
                                               ")"),.keep = "unused")) |>
      relocate(`Grand Mean (95% CI)`,.before = N) |>
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
      Level = factor(Level, levels = c(lower_label,upper_label, overall_label)),
      Measure = factor(Measure, levels = c("Difference (mg/dL)","Absolute Difference (mg/dL)","Difference (mmol/L)","Absolute Difference (mmol/L)","Relative Difference(%)","Absolute Relative Difference(%)"))) |>
      arrange(pick(Level,Measure)) |>
      arrange(pick({{group_var}})) |>
      gt::gt(groupname_col = c(group_var,"Level")) |>
      gt::cols_align(align = "center") |>
      gt::fmt_number(columns = Mean:Max,decimals = 1) |>
      gt::sub_missing(columns = everything(),missing_text = "") |>
      gt::opt_stylize(style = 6, color = "blue") |>
      suppressMessages()
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
