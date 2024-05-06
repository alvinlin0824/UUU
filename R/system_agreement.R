#' System Agreement Analysis
#'
#' @param data Paired Data Set
#' @param type Either Glucose or Ketone. Case is not sensitive
#' @param breakpoint It depends on which type and unit. Default is 70
#' @param ref Provide Reference Readings
#' @param cgm Provide Sensor Readings such as Gl or ANA
#' @param unit Either mg/dL or mmol/L for glucose but mmol/L for ketone. Default is mg/dL
#' @param group_var Any Categorical Variables
#' @param transpose Transpose table. Default is FALSE
#' @param wider_name If transpose is True, please provide the transposed column
#'
#' @return Gt Table
#' @export
#'

system_agreement <- function(data, type = "Glucose", breakpoint = 70, ref, cgm, unit = "mg/dL",group_var, transpose = FALSE, wider_name = NULL) {

  data <- data |>
    select(!contains("Difference")) |>
    mutate(# Subject = as.character({{col_subject}}),
      Difference = {{cgm}} - {{ref}},
      `Absolute Difference` = abs(Difference),
      `Relative Difference(%)` = (Difference/{{ref}})*100,
      `Absolute Relative Difference(%)` = abs(`Relative Difference(%)`))

  # Type = Glucose
  if (str_detect(type,regex("Glucose",ignore_case = T))){
    glmmol_breaks <- c(0,0.8,1.1,2.2,Inf)
    gl_breaks <- c(0,15,20,40,Inf)
    breakpoint_string <- as.character(breakpoint)

    # If unit = mg/dL
    if (unit == "mg/dL"){
      sys <- data |>
        mutate(Level = case_when(round({{ref}} + 0.001) < breakpoint
                                 ~ str_c("<",breakpoint_string," mg/dL"),
                                 .default = str_c(">=",breakpoint_string," mg/dL"))) |>
        # Overall
        bind_rows(data |>
                    mutate(Level = "Overall")) |>
        mutate(Level = factor(Level,levels = c(str_c("<",breakpoint_string," mg/dL"),str_c(">=",breakpoint_string," mg/dL"),"Overall")),
               Group = case_when(round({{ref}} + 0.001) < breakpoint ~ cut(round(`Absolute Difference` + 0.001,0), breaks = gl_breaks,include.lowest = T),
                                 .default = cut(round(`Absolute Relative Difference(%)` + 0.001,0), breaks = gl_breaks,include.lowest = T))) |>
        # User-defined
        group_by(Level) |>
        group_by(pick({{group_var}}),.add = T) |>
        count(Level,Group,.drop = F) |>
        mutate(Sum = sum(n),
               cum = case_when(row_number() %in% c(1:3) ~ cumsum(n),
                               .default = n),
               Percent = case_when(Sum == 0 ~ 0,
                                   .default = round((cum/Sum)*100,1))) |>
        ungroup() |>
        filter(!is.na(Level))


      gt_group_var <- group_var[1:length(group_var) - 1]

      if (transpose == TRUE) {
        # gt_group_var <- group_var[1:length(group_var) - 1]
        sys |>
          filter(!is.na({{wider_name}})) |>
          mutate(Group = case_when(
            Level == str_c("<",breakpoint_string," mg/dL")
            & Group == "[0,15]" ~ "Within +- 15 mg/dL [0.8 mmol/L]",
            Level == str_c("<",breakpoint_string," mg/dL")
            & Group == "(15,20]" ~ "Within +- 20 mg/dL [1.1 mmol/L]",
            Level == str_c("<",breakpoint_string," mg/dL")
            & Group == "(20,40]" ~ "Within +- 40 mg/dL [2.2 mmol/L]",
            Level == str_c("<",breakpoint_string," mg/dL")
            & Group == "(40,Inf]" ~ "Outside +- 40 mg/dL [2.2 mmol/L]",
            Level == str_c(">=",breakpoint_string," mg/dL")
            & Group == "[0,15]" ~ "Within +- 15%",
            Level == str_c(">=",breakpoint_string," mg/dL")
            & Group == "(15,20]" ~ "Within +- 20%",
            Level == str_c(">=",breakpoint_string," mg/dL")
            & Group == "(20,40]" ~ "Within +- 40%",
            Level == str_c(">=",breakpoint_string," mg/dL")
            & Group == "(40,Inf]" ~ "Outside +- 40%",
            Level == "Overall" & Group == "[0,15]" ~ "Within ± 15 mg/dL or 15%",
            Level == "Overall" & Group == "(15,20]" ~ "Within ± 20 mg/dL or 20%",
            Level == "Overall" & Group == "(20,40]" ~ "Within ± 40 mg/dL or 40%",
            Level == "Overall" & Group == "(40,Inf]" ~ "Outside ± 40 mg/dL or 40%"),
            `N(%)` = str_c(cum,"/",Sum," ","(",Percent,"%)")) |>
          arrange(pick({{group_var}})) |>
          pivot_wider(id_cols = !c(n:Percent),names_from = {{wider_name}},values_from = `N(%)`) |>
          gt::gt(groupname_col = c(gt_group_var,"Level")) |>
          gt::cols_align(align = "center") |>
          gt::sub_missing(columns = everything(),missing_text = "") |>
          gt::opt_stylize(style = 6, color = "blue")

      } else {
        # gt_group_var <- group_var[1:length(group_var) - 1]
        sys |>
          mutate(Group = case_when(
            Group == "[0,15]" ~ "Within +- 15% / Within +- 15 mg/dL [0.8 mmol/L]",
            Group == "(15,20]" ~ "Within +- 20% / Within +- 20 mg/dL [1.1 mmol/L]",
            Group == "(20,40]" ~ "Within +- 40% / Within +- 40 mg/dL [2.2 mmol/L]",
            Group == "(40,Inf]" ~ "Outside +- 40%/ Outside +- 40 mg/dL [2.2 mmol/L]"),
            `N(%)` = str_c(cum,"/",Sum," ","(",Percent,"%)")) |>
          arrange(Level,pick({{group_var}})) |>
          pivot_wider(id_cols = !c(n:Percent),names_from = Group,values_from = `N(%)`) |>
          gt::gt(groupname_col = c(gt_group_var,"Level")) |>
          gt::cols_align(align = "center") |>
          gt::cols_width(everything() ~ px(200)) |>
          gt::opt_stylize(style = 6, color = "blue")
      }
    }  else if (unit == "mmol/L") {

      sys <- data |>
        mutate(Level = case_when(round({{ref}}*18.016 + 0.001,1) < breakpoint
                                 ~ str_c("<",breakpoint_string," mg/dL"),
                                 .default = str_c(">=",breakpoint_string," mg/dL"))) |>
        # Overall
        bind_rows(data |>
                    mutate(Level = "Overall")) |>
        mutate(Level = factor(Level,levels = c(str_c("<",breakpoint_string," mg/dL"),str_c(">=",breakpoint_string," mg/dL"),"Overall")),
               Group = case_when(round(Reference + 0.001,1) < 70 ~ cut(round(`Absolute Difference` + 0.001,1), breaks = glmmol_breaks,include.lowest = T),
                                 .default = cut(round(`Absolute Relative Difference(%)` + 0.001,0), breaks = gl_breaks,include.lowest = T)),
               # Combine Groups
               Group = factor(case_when(Group == "[0,0.8]" ~ "[0,15]",
                                        Group == "(0.8,1.1]" ~ "(15,20]",
                                        Group == "(1.1,2.2]" ~ "(20,40]",
                                        Group == "(2.2,Inf]" ~ "(40,Inf]",
                                        .default = Group),levels = c("[0,15]","(15,20]","(20,40]","(40,Inf]"))) |>

        group_by(Level) |>
        # User-defined
        group_by(pick({{group_var}}),.add = T) |>
        count(Level,Group,.drop = F) |>
        mutate(Sum = sum(n),
               cum = case_when(row_number() %in% c(1:3) ~ cumsum(n),
                               .default = n),
               Percent = case_when(Sum == 0 ~ 0,
                                   .default = round((cum/Sum)*100,1))) |>
        ungroup() |>
        filter(!is.na(Level))

      gt_group_var <- group_var[1:length(group_var) - 1]

      if (transpose == TRUE) {
        sys |>
          filter(!is.na({{wider_name}})) |>
          mutate(Group = case_when(
            Level == str_c("<",breakpoint_string," mg/dL") & Group == "[0,15]" ~ "Within ± 0.8 mmol/L [15 mg/dL]",
            Level == str_c("<",breakpoint_string," mg/dL") & Group == "(15,20]" ~ "Within ± 1.1 mmol/L [20 mg/dL]",
            Level == str_c("<",breakpoint_string," mg/dL") & Group == "(20,40]" ~ "Within ± 2.2 mmol/L [40 mg/dL]",
            Level == str_c("<",breakpoint_string," mg/dL") & Group == "(40,Inf]" ~ "Outside ± 2.2 mmol/L [40 mg/dL]",
            Level == str_c(">=",breakpoint_string," mg/dL") & Group == "[0,15]" ~ "Within ± 15%",
            Level == str_c(">=",breakpoint_string," mg/dL") & Group == "(15,20]" ~ "Within ± 20%",
            Level == str_c(">=",breakpoint_string," mg/dL") & Group == "(20,40]" ~ "Within ± 40%",
            Level == str_c(">=",breakpoint_string," mg/dL") & Group == "(40,Inf]" ~ "Outside ± 40%",
            Level == "Overall" & Group == "[0,15]" ~ "Within ± 0.8 mmol/L or 15%",
            Level == "Overall" & Group == "(15,20]" ~ "Within ± 1.1 mmol or 20%",
            Level == "Overall" & Group == "(20,40]" ~ "Within ± 2.2 mmol/L or 40%",
            Level == "Overall" & Group == "(40,Inf]" ~ "Outside ± 2.2 mmol/L or 40%"),
            `N(%)` = str_c(cum,"/",Sum," ","(",Percent,"%)")) |>
          arrange(pick({{group_var}})) |>
          pivot_wider(id_cols = !c(n:Percent),names_from = {{wider_name}},values_from = `N(%)`) |>
          gt::gt(groupname_col = c(gt_group_var,"Level")) |>
          gt::cols_align(align = "center") |>
          gt::sub_missing(columns = everything(),missing_text = "") |>
          gt::opt_stylize(style = 6, color = "blue")

      } else {
        sys |>
          mutate(Group = case_when(
            Group == "[0,15]" ~ "Within ± 15% / 15 mg/dL [0.8 mmol/L]",
            Group == "(15,20]" ~ "Within ± 20% / 20 mg/dL [1.1 mmol/L]",
            Group == "(20,40]" ~ "Within ± 40% / 40 mg/dL [2.2 mmol/L]",
            Group == "(40,Inf]" ~ "Outside ± 40%/ 40 mg/dL [2.2 mmol/L]"),
            `N(%)` = str_c(cum,"/",Sum," ","(",Percent,"%)")) |>
          arrange(Level,pick({{group_var}})) |>
          pivot_wider(id_cols = !c(n:Percent),names_from = Group,values_from = `N(%)`) |>
          gt::gt(groupname_col = c(gt_group_var,"Level")) |>
          gt::cols_align(align = "center") |>
          gt::cols_width(everything() ~ px(200)) |>
          gt::opt_stylize(style = 6, color = "blue")
      }
    }
  } else if (str_detect(type,regex("Ketone",ignore_case = T))) {
    ketone_breaks <- c(0,0.1,0.2,0.3,0.4,Inf)
    ketone_breaks_percent <- c(0,10,20,30,40,Inf)
    sys <- data |>
      mutate(Level = case_when({{ref}} < 1 ~ "< 1 mmol/L",
                               .default = ">= 1 mmol/L")) |>
      bind_rows(data |>
                  mutate(Level = "Overall")) |>
      mutate(Level = factor(Level,levels = c("< 1 mmol/L",">= 1 mmol/L","Overall")),
             Group = case_when(Level %in% c("< 1 mmol/L","Overall") & {{ref}} < 1 ~ cut(round(`Absolute Difference` + 0.0001,1), breaks = ketone_breaks,include.lowest = T),
                               Level %in% c(">= 1 mmol/L","Overall") & {{ref}} >= 1 ~ cut(round(`Absolute Relative Difference(%)` + 0.001,0), breaks = ketone_breaks_percent,include.lowest = T)),
             # Modify Overall Group
             Group = case_when(Level == "Overall" & Group == "[0,0.1]" ~ "[0,10]",
                               Level == "Overall" & Group == "(0.1,0.2]" ~ "(10,20]",
                               Level == "Overall" & Group == "(0.2,0.3]" ~ "(20,30]",
                               Level == "Overall" & Group == "(0.3,0.4]" ~ "(30,40]",
                               Level == "Overall" & Group == "(0.4,Inf]" ~ "(40,Inf]",
                               .default = Group),
             Group = factor(Group,
                            levels = c("[0,0.1]","(0.1,0.2]","(0.2,0.3]","(0.3,0.4]","(0.4,Inf]"
                                       ,"[0,10]","(10,20]","(20,30]","(30,40]","(40,Inf]"))) |>
      group_by(Level) |>
      # User-defined
      group_by(pick({{group_var}}),.add = T) |>
      count(Level,Group,.drop = F) |>
      # Remove redundant factor levels
      filter(!(Level == "< 1 mmol/L"
               & Group %in% c("[0,10]","(10,20]","(20,30]","(30,40]","(40,Inf]")) &
               !(Level %in% c(">= 1 mmol/L","Overall")
                 & Group %in% c("[0,0.1]","(0.1,0.2]","(0.2,0.3]","(0.3,0.4]","(0.4,Inf]"))) |>
      mutate(Sum = sum(n),
             cum = case_when(row_number() %in% c(1:4) ~ cumsum(n),
                             .default = n),
             Percent = case_when(Sum == 0 ~ 0,
                                 .default = round((cum/Sum)*100,1))) |>
      ungroup() |>
      filter(!is.na(Level))

    if (transpose == TRUE){
      gt_group_var <- group_var[1:length(group_var) - 1]
      sys |>
        filter(!is.na({{wider_name}})) |>
        mutate(Group = case_when(
          Level == "< 1 mmol/L" & Group == "[0,0.1]" ~ "Within ± 0.1 mmol/L",
          Level == "< 1 mmol/L" & Group == "(0.1,0.2]" ~ "Within ± 0.2 mmol/L",
          Level == "< 1 mmol/L" & Group == "(0.2,0.3]" ~ "Within ± 0.3 mmol/L",
          Level == "< 1 mmol/L" & Group == "(0.3,0.4]" ~ "Within ± 0.4 mmol/L",
          Level == "< 1 mmol/L" & Group == "(0.4,Inf]" ~ "Outside ± 0.4 mmol/L",
          Level == ">= 1 mmol/L" & Group == "[0,10]" ~ "Within ± 10%",
          Level == ">= 1 mmol/L" & Group == "(10,20]" ~ "Within ± 20%",
          Level == ">= 1 mmol/L" & Group == "(20,30]" ~ "Within ± 30%",
          Level == ">= 1 mmol/L" & Group == "(30,40]" ~ "Within ± 40%",
          Level == ">= 1 mmol/L" & Group == "(40,Inf]" ~ "Outside ± 40%",
          Level == "Overall" & Group == "[0,10]" ~ "Within ± 10% / Within ± 0.1 mmol/L",
          Level == "Overall" & Group == "(10,20]" ~ "Within ± 20% / Within ± 0.2 mmol/L",
          Level == "Overall" & Group == "(20,30]" ~ "Within ± 30% / Within ± 0.3 mmol/L",
          Level == "Overall" & Group == "(30,40]" ~ "Within ± 40% / Within ± 0.4 mmol/L",
          Level == "Overall" & Group == "(40,Inf]" ~ "Outside ± 40%/ Outside ± 0.4 mmol/L"),
          `N(%)` = str_c(cum,"/",Sum," ","(",Percent,"%)")) |>
        arrange(pick({{group_var}})) |>
        pivot_wider(id_cols = !c(n:Percent),names_from = {{wider_name}},values_from = `N(%)`) |>
        gt::gt(groupname_col = c(gt_group_var,"Level")) |>
        gt::cols_align(align = "center") |>
        gt::sub_missing(columns = everything(),missing_text = "") |>
        gt::opt_stylize(style = 6, color = "blue")
    } else {
      gt_group_var <- group_var[1:length(group_var)]

      sys |>
        mutate(Group = case_when(
          Group %in% c("[0,10]","[0,0.1]") ~ "Within ± 10% / Within ± 0.1 mmol/L",
          Group %in% c("(10,20]","(0.1,0.2]") ~ "Within ± 20% / Within ± 0.2 mmol/L",
          Group %in% c("(20,30]","(0.2,0.3]") ~ "Within ± 30% / Within ± 0.3 mmol/L",
          Group %in% c("(30,40]","(0.3,0.4]") ~ "Within ± 40% / Within ± 0.4 mmol/L",
          Group %in% c("(40,Inf]","(0.4,Inf]") ~ "Outside ± 40%/ Outside ± 0.4 mmol/L"),
          `N(%)` = str_c(cum,"/",Sum," ","(",Percent,"%)")) |>
        arrange(Level,pick({{group_var}})) |>
        pivot_wider(id_cols = !c(n:Percent),names_from = Group,values_from = `N(%)`) |>
        gt::gt(groupname_col = c(gt_group_var)) |>
        gt::cols_align(align = "center") |>
        gt::cols_width(everything() ~ px(200)) |>
        gt::opt_stylize(style = 6, color = "blue")
    }
  }
}
