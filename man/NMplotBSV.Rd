% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NMplotBSV.R
\name{NMplotBSV}
\alias{NMplotBSV}
\title{Generate distribution plots of between-occasion variability terms
from Nonmem}
\usage{
NMplotBSV(
  data,
  regex.eta,
  names.eta = NULL,
  parameters = NULL,
  col.id = "ID",
  covs.num,
  covs.char,
  save = FALSE,
  show = TRUE,
  return.data = FALSE,
  title = NULL,
  file.mod,
  structure = "flat",
  use.phi,
  auto.map,
  ...
)
}
\arguments{
\item{data}{A dataset - will be converted to data.frame so
data.table is OK.}

\item{regex.eta}{A regular expression defining the naming of the
ETA's of interest. See \code{file.mod} too.}

\item{parameters}{Character vector of model parameters to
include. This will drop ETA's that are not associated with a
parameter in this vector.}

\item{col.id}{The name of the id column in data. Default is ID
like Nonmem. This is not fully working if col.id is different
from \code{ID}.}

\item{covs.num}{Names of columns containing numerical covariates
to plot the random effects against.}

\item{covs.char}{Names of columns containing categorical
covariates to plot the random effects against.}

\item{save}{Save the generated plots?}

\item{return.data}{If TRUE, the identified ETA's together with
subject id and covariates will be returned in both wide and
long format. If FALSE, you just get the plots.}

\item{file.mod}{If used, parameter names that ETA's are associated
with will be derived by looking at \code{$PRED} or \code{$PK}
in the control stream. Essentially, it looks for where the
ETA's are found and look for a parameter name to the left of a
\code{=} sign. Alternatively, you can hard-code the ETA-parameter
relationship using \code{regex.eta}.}

\item{fun.file}{If saving plots, this function can be used to
translate the file names. The inputs given to the function
argument are "iov_pairs.png" and "iov_covs_n.png".}

\item{script}{If saving the plots, a stamp to add. See ggstamp.}

\item{debug}{Start by running browser()?}
}
\description{
Generate distribution plots of between-occasion variability terms
from Nonmem
}
\seealso{
Other Plotting: 
\code{\link{NMplotBSVold}()},
\code{\link{NMplotGOF}()},
\code{\link{NMplotIndProfs}()},
\code{\link{theme_pp_facet}()},
\code{\link{theme_pp}()}
}
\concept{Plotting}
