#' Pair BG Reference
#'
#' @param data Upload Data
#'
#' @return Tibble
#' @export

PairBg <- function(data){
  # BG Strips Data
  BG |>
    # Remove Extreme Reference readings <20 or >500
    dplyr::filter(dplyr::between(Reference,20,500)) |>
    dplyr::inner_join(
      # Sensor Data
      data |>
        dplyr::mutate(
          `Lower Bound` = dplyr::case_when(Type %in% c("904","906") ~ `Date Time` - lubridate::dminutes(5),
                                    .default = `Date Time` - lubridate::dminutes(8)),
          `Upper Bound` = dplyr::case_when(Type %in% c("904","906") ~ `Date Time` + lubridate::dminutes(5),
                                    .default = `Date Time` + lubridate::dminutes(8))
        ),
      by = dplyr::join_by("Subject ID",dplyr::between(`BG Date Time`,`Lower Bound`,`Upper Bound`)),
      multiple = "all"
    ) |>
    # Select the nearest time
    dplyr::group_by(`Subject ID`,`BG Date Time`) |>
    dplyr::arrange(desc(`Date Time`)) |>
    dplyr::slice_min(abs(`BG Date Time` - `Date Time`),n = 1,with_ties = F) |>
    dplyr::ungroup() |>
    # Select the nearest time
    dplyr::group_by(`Subject ID`,`Date Time`) |>
    dplyr::arrange(desc(`Date Time`)) |>
    dplyr::slice_min(abs(`BG Date Time` - `Date Time`),n = 1,with_ties = F) |>
    dplyr::ungroup() |>
    dplyr::select(!c(contains("Bound")))
}
