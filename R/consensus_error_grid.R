#' Consensus Error Grid Analysis
#'
#' @param df Paired Data Set
#' @param ref Provide Reference Readings
#' @param cgm Provide Sensor Readings such as Gl or ANA
#' @param unit Either "gram" or "mol"
#' @param group_var Any Categorical Variables. Default is NULL
#' @param show_plot Either TRUE or FALSE. Default is FALSE.
#' @param title Title of Plot
#' @param xlab xlab of Plot
#' @param ylab ylab of Plot
#'
#' @return GT Table or GGplot2
#' @export
#'

consensus_error_grid <- function (df, ref, cgm, unit = "gram", group_var = NULL,
                                  show_plot = FALSE, title = "", xlab = "", ylab = "")
{
  data1 <- df |>
          mutate(ref = {{ref}},
                 test = {{cgm}})

  if (unit != "mol" & unit != "gram") {
    stop("'unit' must be either 'mol' or 'gram'.")
  }
  if (title == "") {
    title <- "Consensus Error Grid"
  }
  if (unit == "mol") {
    n <- 18.016
    if (xlab == "") {
      xlab = "Reference Glucose Concentration (mmol/L)"
    }
    if (ylab == "") {
      ylab = "Test Glucose Concentration (mmol/L)"
    }
  }
  else {
    n <- 1
    if (xlab == "") {
      xlab = "Reference Glucose Concentration (mg/dL)"
    }
    if (ylab == "") {
      ylab = "Test Glucose Concentration (mg/dL)"
    }
  }

  zones <- ega::getParkesZones(data1$ref, data1$test, type = 1, unit)

  ref <- test <- NULL

  data <- data1 |>
          mutate(Zones = zones)

  maxX <- max(max(data$ref) + 20/n, 550/n)
  maxY <- max(max(data$test) + 20/n, 550/n)
  labels <- data.frame(x = c(450, 220/n, 385/n, 140/n, 405/n,
                             415/n, 75/n, 21/n),
                       y = c(450, 360/n, 235/n, 375/n,145/n,
                             50/n, 383/n, 383/n),
                       label = c("A", "B","B", "C", "C", "D", "D","E"),
                       color = c("gray30", "gray30", "gray30","gray30", "gray30", "gray30", "gray30", "gray30"))

  ce <- ega:::.coef(35, 155, 50, 550)
  cdu <- ega:::.coef(80, 215, 125, 550)
  cdl <- ega:::.coef(250, 40, 550, 150)
  ccu <- ega:::.coef(70, 110, 260, 550)
  ccl <- ega:::.coef(260, 130, 550, 250)
  cbu <- ega:::.coef(280, 380, 430, 550)
  cbl <- ega:::.coef(385, 300, 550, 450)
  x1 <- y1 <- xend <- yend <- NULL
  border <- data.frame(x1 = c(0/n, 0/n, 30/n, 140/n, 280/n,
                              50/n, 50/n, 170/n, 385/n, 0/n, 30/n, 50/n, 70/n,
                              120/n, 120/n, 260/n, 0/n, 25/n, 50/n, 80/n, 250/n,
                              250/n, 0/n, 35/n),
                       y1 = c(0/n, 50/n, 50/n, 170/n,
                              380/n, 0/n, 30/n, 145/n, 300/n, 60/n, 60/n, 80/n,
                              110/n, 0/n, 30/n, 130/n, 100/n, 100/n, 125/n, 215/n,
                              0/n, 40/n, 150/n, 155/n),
                       xend = c(min(maxX, maxY),
                                30/n, 140/n, 280/n,
                                ega:::.endx(280/n, 380/n, maxY, cbu),
                                50/n, 170/n, 385/n, maxX, 30/n, 50/n, 70/n,
                                ega:::.endx(70/n,110/n, maxY, ccu),
                                120/n, 260/n, maxX, 25/n,50/n, 80/n,
                                ega:::.endx(80/n, 215/n, maxY, cdu), 250/n,
                                maxX, 35/n, ega:::.endx(35/n, 155/n, maxY, ce)),
                       yend = c(min(maxX,maxY), 50/n, 170/n, 380/n, maxY, 30/n, 145/n, 300/n,
                                ega:::.endy(385/n, 300/n, maxX, cbl), 60/n, 80/n, 110/n,
                                maxY, 30/n, 130/n, ega:::.endy(260/n, 130/n, maxX, ccl),
                                100/n, 125/n, 215/n, maxY, 40/n, ega:::.endy(410/n, 110/n,
                                                                             maxX, cdl), 155/n, maxY))

  if (show_plot == TRUE) {
    plot <- ggplot(data, aes(x = ref, y = test)) +
            scale_x_continuous(breaks = c(0,round(70/n,digits = 1), round(100/n, digits = 1), round(150/n, digits = 1),round(180/n, digits = 1), round(240/n, digits = 1), round(300/n, digits = 1), round(350/n, digits = 1), round(400/n,digits = 1), round(450/n, digits = 1), round(500/n,digits = 1), round(550/n, digits = 1), round(600/n,digits = 1), round(650/n, digits = 1), round(700/n,digits = 1), round(750/n, digits = 1), round(800/n,digits = 1), round(850/n, digits = 1), round(900/n,digits = 1), round(950/n, digits = 1), round(1000/n, digits = 1)), expand = c(0, 0)) +
            scale_y_continuous(breaks = c(0,round(70/n, digits = 1), round(100/n, digits = 1), round(150/n, digits = 1),round(180/n, digits = 1), round(240/n, digits = 1), round(300/n,digits = 1), round(350/n, digits = 1), round(400/n,digits = 1), round(450/n, digits = 1), round(500/n,digits = 1), round(550/n, digits = 1), round(600/n,digits = 1), round(650/n, digits = 1), round(700/n, digits = 1), round(750/n, digits = 1), round(800/n, digits = 1),round(850/n,digits = 1), round(900/n,digits = 1), round(950/n, digits = 1), round(1000/n, digits = 1)), expand = c(0, 0)) +
            geom_point(size = 1, alpha = 0.6) +
            geom_segment(aes(x = x1,y = y1, xend = xend, yend = yend), data = border, linetype = "solid") +
            geom_text(data = labels, aes(x = x, y = y, label = label)) +
            theme_bw() +
            theme(panel.background = element_rect(fill = "white",
                                                  colour = "black",
                                                  linewidth = 0.5, linetype = "solid"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line = element_line(linewidth = 0.5, linetype = "solid",colour = "black"),
                  legend.position = "bottom",
                  plot.title = element_text(hjust = 0.5),
                  # legend.position="none",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 8),
                  aspect.ratio = 1) +
            ggtitle(title) +
            xlab(xlab) +
            ylab(ylab)

    if (unit == "gram"){
      plot
    } else if (unit == "mol"){
      plot +
        coord_cartesian(xlim = c(0,30.0), ylim = c(0,30.0))
    }

  } else {
    gt_group_var <- group_var
    map2(data |>
           group_split(pick({{group_var}})) |>
           map(\(df) df |> select({{group_var}})) |>
           map(\(df) df |> slice(1:5)),
         data |>
           mutate(Zones = factor(Zones,levels = c("A","B","C","D"))) |>
           group_split(pick({{group_var}})) |>
           map(\(df) janitor::tabyl(df, Zones)) |>
           map(\(df) df |> janitor::adorn_totals("row")) |>
           map(\(df) df |> janitor::adorn_pct_formatting(digits = 1)),bind_cols) |>
      list_rbind() |>
      arrange(pick({{group_var}})) |>
      gt::gt(groupname_col = c(gt_group_var)) |>
      gt::cols_align(align = "center",columns = everything()) |>
      gt::cols_label(n = "N", percent = "Percent") |>
      gt::opt_stylize(style = 6, color = "blue")
  }
}
