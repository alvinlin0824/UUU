#' Convert UID to Sensor Serial Number
#'
#' @param data Data of interest
#' @param col Col of UID you would like to convert to Sensor Serial Number
#'
#' @return A tibble
#' @export

convert_UID_to_sensor_serial_number <- function(data,col){

  lookup <- c(seq(0,9),LETTERS[!str_detect(LETTERS,"B|I|O|S")])

  data |>
    mutate(UID = {{col}},.before = {{col}}) |>
    # Length should be 16
    filter(str_length(UID) == 16) |>
    # Big O in the sensor serial number
    mutate(UID = str_replace(UID,"O","0")) |>
    separate_wider_position(UID,c(4,binary1 = 2,binary2 = 2,binary3 = 2,binary4 = 2,binary5 = 2,binary6 = 2),cols_remove = F) |>
    # Hex to Binary
    mutate(across(c(binary1:binary6), ~ formatC(as.integer(R.utils::intToBin(strtoi(.x, base = 16))),width = 8, flag = "0")),
           binary = str_c(binary1,binary2,binary3,binary4,binary5,binary6)) |>
    separate_wider_position(binary, c(3,num1 = 5,num2 = 5,num3 = 5,num4 = 5,num5 = 5,num6 = 5,
                                      num7 = 5,num8 = 5,num9 = 5)) |>
    # Binary to integer
    mutate(across(c(num1:num9), ~ strtoi(.x, base = 2) + 1),
           {{col}} := str_c(lookup[num1],lookup[num2],lookup[num3],lookup[num4],
                            lookup[num5],lookup[num6],lookup[num7],lookup[num8],
                            lookup[num9])) |>
    select(!c(contains("binary"),num_range("num",1:9))) |>
    bind_rows(data |>
                filter(str_length({{col}}) != 16) |>
                rename(UID = {{col}}))
}
