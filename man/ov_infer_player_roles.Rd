% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rotations.R
\name{ov_infer_player_roles}
\alias{ov_infer_player_roles}
\title{Infer the role of each player}
\usage{
ov_infer_player_roles(
  x,
  target_team,
  method,
  fall_back = TRUE,
  setter_tip_codes = c("PP")
)
}
\arguments{
\item{x}{: a datavolley object (as returned by \code{\link[datavolley:dv_read]{datavolley::dv_read()}}), a list of datavolley objects, or the \code{plays} component of a datavolley object}

\item{target_team}{string or function: team to report on. If this is a function, it should return \code{TRUE} when passed the target team name}

\item{method}{string: "meta" (rely on player metadata), "SHM" (assume a setter-hitter-middle rotation order), "SMH" (setter-middle-hitter), or "data" (figure out positions from scouting data). Method "meta" is the default if a datavolley object or list of objects is provided}

\item{fall_back}{logical: if TRUE and \code{method} is "meta" and x is a single datavolley object BUT player roles are not provided in the DataVolley file metadata section, fall back to method="data"}

\item{setter_tip_codes}{character: vector of attack combination codes that correspond to setter tips}
}
\value{
A data.frame
}
\description{
Infer the role of each player
}
\examples{
x <- ovdata_example("mlafin_braslovce_nkbm", as = "parsed")
## guess roles according to the actions that the players made
rx <- ov_infer_player_roles(x, target_team = "Nova KBM Branik", method = "data")

}
