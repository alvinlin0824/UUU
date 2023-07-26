#' Difference Measures Analysis
#'
#' @param data Paired Data Set
#' @param reference Either TRUE or FALSE
#' @param group_var Any Categorical Variables
#'
#' @return Gt Table
#' @export

diff_measure <- function(data, reference, group_var) {

  diff_calculation <- list(Mean = ~ mean(.x, na.rm = T),
                           Median = ~ median(.x, na.rm = T),
                           SD = ~ sd(.x, na.rm = T),
                           Min = ~ min(.x, na.rm = T),
                           Max = ~ max(.x, na.rm = T),
                           N = ~ sum(!is.na(.x),na.rm = T))

  if (reference == TRUE) {
    data |>
      # Filter GM values
      dplyr::filter(dplyr::between(Gl,40/18.016,400/18.016)) |>
      dplyr::mutate(# Reference Level
        Level = dplyr::case_when(round(Reference + 0.001,1) < 5.6 ~ "<5.6 mmol/L [100 mg/dL]",
                                 .default = ">=5.6 mmol/L [100 mg/dL]")) |>
      # Overall Reference Level
      dplyr::bind_rows(
        data |>
          # Filter GM values
          dplyr::filter(dplyr::between(Gl,40/18.016,400/18.016)) |>
          dplyr::mutate(Level = "Overall Levels (5.6 mmol/L [100 mg/dL] breakpoint)")
      ) |>
      tidyr::pivot_longer(`Difference(mmol/L)`:`Absolute Relative Difference(%)`,names_to = "Measure") |>
      dplyr::mutate(
        # Factor order Measure
        Measure = forcats::fct_inorder(Measure)) |>
      # Difference(mg/dL) and Absolute Difference(mg/dL) for Reference Level < 100 mg/dL
      # Relative Difference(%) and Absolute Relative Difference(%) for Reference Level >= 100 mg/dL
      dplyr::filter((Measure %in% c("Difference(mmol/L)", "Absolute Difference(mmol/L)") &
                Level %in% c("<5.6 mmol/L [100 mg/dL]", "Overall Levels (5.6 mmol/L [100 mg/dL] breakpoint)")) |
               (Measure %in% c("Relative Difference(%)", "Absolute Relative Difference(%)") &
                  Level %in% c(">=5.6 mmol/L [100 mg/dL]", "Overall Levels (5.6 mmol/L [100 mg/dL] breakpoint)"))) |>
      # User-Defined group variables
      dplyr::group_by(dplyr::pick({{group_var}})) |>
      dplyr::group_by(Measure,.add = TRUE) |>
      dplyr::summarise_at(.vars = "value", diff_calculation) |>
      gt::gt() |>
      gt::cols_align(align = "center") |>
      gt::fmt_number(columns = Mean:Max,decimals = 1) |>
      gt::opt_stylize(style = 6, color = "blue")
  } else {
    data |>
      # Filter GM values
      dplyr::filter(dplyr::between(Gl,40/18.016,400/18.016)) |>
      dplyr::group_by(dplyr::pick({{group_var}})) |>
      dplyr::summarise(
        dplyr::across(c(`Difference(mmol/L)`:`Absolute Relative Difference(%)`),
               diff_calculation[1:3],.names = "{.col} {.fn}"), N = n(),.groups = "drop") |>
      dplyr::relocate(N,.after = last_col()) |>
      gt::gt()  |>
      gt::cols_align(align = "center") |>
      gt::tab_spanner(label = "Difference(mmol/L)",columns = c("Difference(mmol/L) Mean","Difference(mmol/L) Median","Difference(mmol/L) SD")) |>
      gt::tab_spanner(label = "Abs. Difference (mmol/L)",columns = c("Absolute Difference(mmol/L) Mean","Absolute Difference(mmol/L) Median","Absolute Difference(mmol/L) SD")) |>
      gt::tab_spanner(label = "Relative Difference(%)",columns = c("Relative Difference(%) Mean","Relative Difference(%) Median","Relative Difference(%) SD")) |>
      gt::tab_spanner(label = "Absolute Relative Difference(%)",columns = c("Absolute Relative Difference(%) Mean","Absolute Relative Difference(%) Median","Absolute Relative Difference(%) SD")) |>
      gt::fmt_number(columns = contains(c("Mean","Median")),decimals = 1) |>
      gt::fmt_number(columns = contains(c("SD")),decimals = 2) |>
      gt::cols_label(
        `Difference(mmol/L) Mean` = "Mean",`Difference(mmol/L) Median` = "Median",
        `Difference(mmol/L) SD` = "SD",
        `Relative Difference(%) Mean` = "Mean",`Relative Difference(%) Median` = "Median",`Relative Difference(%) SD` = "SD",
        `Absolute Difference(mmol/L) Mean` = "Mean",`Absolute Difference(mmol/L) Median` = "Median",
        `Absolute Difference(mmol/L) SD` =  "SD",`Absolute Relative Difference(%) Mean` = "Mean",
        `Absolute Relative Difference(%) Median` = "Median",
        `Absolute Relative Difference(%) SD` = "SD") |>
      gt::opt_stylize(style = 6, color = "blue")
  }
}
