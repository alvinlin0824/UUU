% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diff_measure.R
\name{diff_measure}
\alias{diff_measure}
\title{Difference Measures Analysis}
\usage{
diff_measure(
  data,
  type = "Glucose",
  ref,
  cgm,
  breakpoint = 100,
  col_subject,
  group_var,
  unit = "mg/dL",
  reference_breakpoint = TRUE
)
}
\arguments{
\item{data}{Paired Data Set}

\item{type}{Either Glucose or Ketone. Case is not sensitive}

\item{ref}{Provide Reference Readings}

\item{cgm}{Provide Sensor Readings such as Gl or ANA}

\item{breakpoint}{It depends on which type and unit. Default is 100}

\item{col_subject}{Provide Subject Column}

\item{group_var}{Any Categorical Variables}

\item{unit}{Either mg/dL or mmol/L. Default is mg/dL}

\item{reference_breakpoint}{Default is TRUE}
}
\value{
Gt Table
}
\description{
Difference Measures Analysis
}
