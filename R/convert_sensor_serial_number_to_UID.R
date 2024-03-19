#' Convert Sensor Serial Number to UID
#'
#' @param x Column of Sensor Serial Number you would like to convert to UID
#'
#' @return A Vector
#' @export

convert_sensor_serial_number_to_UID <- function(x){
  lookup <- c(seq(0,9),LETTERS[!str_detect(LETTERS,"B|I|O|S")])

  tibble(snr = x) |>
    filter(str_length(snr) == 9) |>
    separate_wider_position(cols = snr,c(num1 = 1,num2 = 1,num3 = 1,num4 = 1,num5 = 1,num6 = 1,num7 = 1,num8 = 1,num9 = 1),cols_remove = F) |>
    # Find the index
    mutate(across(num1:num9, ~ match(.x,lookup) - 1),
           # convert integer to binary
           across(num1:num9, ~ formatC(as.integer(R.utils::intToBin(.x)),width = 5, flag = "0")),
           # concatenate
           binary = str_c("000",num1,num2,num3,num4,num5,num6,num7,num8,num9)) |>
    # Convert binary to hex
    separate_wider_position(binary,c(binary1 = 8,binary2 = 8,binary3 = 8,binary4 = 8,binary5 = 8,binary6 = 8),cols_remove = F) |>
    mutate(across(c(binary1:binary6), ~ str_pad(R.utils::intToHex(strtoi(.x, base = 2)),2,pad = "0")),
           UID = str_to_upper(str_c("E07A",binary1,binary2,binary3,binary4,binary5,binary6))) |>
    select(!starts_with(c("num","binary"))) |>
    bind_rows(tibble(snr = x) |>
                filter(str_length(snr) != 9 | is.na(snr))) |>
    pull(UID)
}
