% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setter_choice_functions.R
\name{ov_plot_distribution}
\alias{ov_plot_distribution}
\title{Court plot of a real and simulated setter distribution}
\usage{
ov_plot_distribution(
  ssd,
  label_setters_by = "id",
  font_size = 11,
  title_wrap = NA,
  output = "plot"
)
}
\arguments{
\item{ssd}{simulated setter distribution output as returned by \code{\link[=ov_simulate_setter_distribution]{ov_simulate_setter_distribution()}}}

\item{label_setters_by}{string: either "id" or "name"}

\item{font_size}{numeric: font size}

\item{title_wrap}{numeric: if non-\code{NA}, use \code{\link[=strwrap]{strwrap()}} to break the title into lines of this width}

\item{output}{string: either "plot" or "list"}
}
\description{
Court plot of a real and simulated setter distribution
}
\examples{
dvw <- ovdata_example("NCA-CUB")
setter <- ov_simulate_setter_distribution(dvw = dvw, play_phase = c("Reception", "Transition"),
                                          n_sim = 100, attack_by = "code")
ov_plot_distribution(setter)
}
