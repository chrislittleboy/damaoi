% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjustreservoirpolygon.R
\name{adjustreservoirpolygon}
\alias{adjustreservoirpolygon}
\title{adjust polygon of reservoir to reference surface water extent map}
\usage{
adjustreservoirpolygon(
  reservoir,
  water_bodies,
  dem,
  poss_expand = 20000,
  wbjc = 0
)
}
\arguments{
\item{reservoir}{An sf polygon, with an unstandardised raw reservoir}

\item{water_bodies}{A rast, where 1 indicates water, NA otherwise}

\item{dem}{A rast, showing elevation}

\item{poss_expand}{A number, indicating the number of meters away from the raw reservoir the reservoir may expand to. Default is 20000 (20km).}

\item{wbjc}{A number indicating a "water body join correction". This is a small buffer zone for the reservoir polygon to ensure that it is contiguous (important where there are small channels connecting different parts of the same water body). Default is 0 but it is necessary for some dams to include a small buffer, particularly if the reservoir has areas which are less wide than the resolution of the underlying DEM.}
}
\value{
An sf polygon with an adjusted reservoir polygon
}
\description{
adjust polygon of reservoir to reference surface water extent map
}
