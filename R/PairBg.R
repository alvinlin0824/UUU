#' Pair BG Reference
#'
#' @param sensor_data Sensor Data
#' @param apol Boolean. Is sensor Apol or Mobi
#'
#' @return Tibble
#' @export

PairBg <- function(sensor_data, apol = NULL){
  # BG Strips Data
  BG |>
    # Remove Extreme Reference readings <20 or >500
    filter(between(Reference,20,500)) |>
    inner_join(
      # Sensor Data
      sensor_data |>
        dplyr::mutate(
          `Date Time` = force_tz(`Date Time`,tz = "UTC"),
          `Lower Bound` =
            if (apol == TRUE) {
              case_when(Type %in% c("904","906") ~ `Date Time` - dminutes(5),
                        .default = `Date Time` - dminutes(8))
            }
          else `Date Time` - dminutes(5),

          `Upper Bound` =
            if (apol == TRUE) {
              case_when(Type %in% c("904","906") ~ `Date Time` + dminutes(5),
                        .default = `Date Time` + dminutes(8))
            }
          else `Date Time` + dminutes(5)
        ),
      by = join_by("Subject ID",between(`BG Date Time`,`Lower Bound`,`Upper Bound`)),
      multiple = "all"
    ) |>
    # Select the nearest time
    group_by(`Subject ID`,`BG Date Time`) |>
    arrange(desc(`Date Time`)) |>
    slice_min(abs(`BG Date Time` - `Date Time`),n = 1,with_ties = F) |>
    ungroup() |>
    # Select the nearest time
    group_by(`Subject ID`,`Date Time`) |>
    arrange(desc(`Date Time`)) |>
    slice_min(abs(`BG Date Time` - `Date Time`),n = 1,with_ties = F) |>
    ungroup() |>
    select(!c(contains("Bound")))
}
