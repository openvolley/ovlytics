% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setter_choice_functions.R
\name{ov_table_mssd}
\alias{ov_table_mssd}
\title{Table of a simulated multi-game setter distribution sequence}
\usage{
ov_table_mssd(
  mssd,
  label_setters_by = "name",
  team = NULL,
  nrows = 50,
  groupBy = TRUE
)
}
\arguments{
\item{mssd}{simulated multi-game setter distribution output as returned by \code{\link[=ov_simulate_multiple_setter_distribution]{ov_simulate_multiple_setter_distribution()}}}

\item{label_setters_by}{string: either "id" or "name"}

\item{team}{NULL or string: if non-NULL, show sequence just for this team name}

\item{nrows}{integer: number of rows per page in the table}

\item{groupBy}{boolean: if TRUE, will group the rows by Opponent}
}
\description{
Table of a simulated multi-game setter distribution sequence
}
\examples{
\dontrun{
 list_dv <- list(dv_read(ovdata_example("NCA-CUB"))) # would normally be multiple games
 mssd <- ov_simulate_multiple_setter_distribution(list_dv = list_dv,
             play_phase = c("Reception", "Transition"), attack_by = "attacker_name",
             n_sim = 100, setter_position_by = "front_back")

 res <- ov_table_mssd(mssd, team = "NICARAGUA")
}
}
