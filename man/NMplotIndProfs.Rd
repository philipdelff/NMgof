% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NMplotIndProfs.R
\name{NMplotIndProfs}
\alias{NMplotIndProfs}
\title{Plot individual profiles and doses based on NM style dataset}
\usage{
NMplotIndProfs(
  data,
  run,
  x = "TIME",
  dv = "DV",
  pred = "PRED",
  ipred = c("IPRED", "IPRE"),
  grp,
  amt,
  id = "ID",
  xlab = NULL,
  ylab = NULL,
  ylab2 = NULL,
  scales = "fixed",
  logy = F,
  NPerSheet = 12,
  LLOQ = NULL,
  use.evid2,
  facet = id,
  ncol.facet = 3,
  par.prof = NULL,
  x.inc,
  grp.label = grp,
  labels = "facet",
  nullIfEmpty = FALSE,
  quiet = FALSE,
  debug = FALSE,
  debug.sheet
)
}
\arguments{
\item{data}{The dataset to plot.}

\item{run}{The main title of the plot. Called run becaus you often
want a Nonmem run name here.}

\item{x}{The name of the column to be plotted on the x-axis
(string).}

\item{dv}{The name of the column containing observations (string).}

\item{pred}{The name of the population predictions in data
(string).}

\item{ipred}{The name of the individual predictions in data
(string).}

\item{grp}{A grouping variable. Subjects will be split into groups
of common values (string).}

\item{amt}{The name of the column containing dose amounts. If this
argument is given, the doses will be included as vertical bars
in the plots, with a secondary axis representing the dose
amounts on the right.}

\item{id}{The name of the subject ID column}

\item{xlab}{label for x-axis.}

\item{ylab}{label for y-axis.}

\item{ylab2}{label for y-axis to the right representing dose
amounts.}

\item{scales}{passed to facet_wrap.}

\item{logy}{Show y-axis on logarithmic scale?}

\item{NPerSheet}{Number of subjects per sheet}

\item{LLOQ}{Lower limit of quantification (will be shown as a
horizontal line in plots).}

\item{use.evid2}{Should EVID 2 records be used for pred and ipred
plotting? The default is to use EVID==2 records if data
contains a column called EVID, and this column has at least
one value equalling 2.}

\item{facet}{splits plots in pages}

\item{ncol.facet}{The number of columns used in facet_wrap.}

\item{par.prof}{Distinguish multiple profiles in dataset.}

\item{x.inc}{Values that must be included in the span of the
x-axis. This can be multiple values, like c(5,1000).}

\item{grp.label}{Column to use for labeling the \emph{sheets} (when
sorting by grp). A typical example is that grp is numeric (say
dose including 80 and 280) while grp.label is a character
(including 80 mg and 280 mg). In order to sort correctly, you
must use the numeric variable for grp. But in order to get
nice labels, use the character variable for labels.}

\item{labels}{The default is to include the "strip" when faceting
- i.e. the boxed text above each plot in a facet. These text
boxes can take a lot of space if you want to plot many
subjects together. Set to "facet" (default) to let this be
controlled by the faceting, use labels="none" to remove, or
use labels="top-right" to not use the strip, but include the same text
in the top-right corner inside the plotting area.}

\item{nullIfEmpty}{By default, submitting an empty data set in the
data argument will give an error. However, sometimes this may
be annoying. An example is a wrapper that runs over multiple
models splitting into subsets of data. Some models may not
contain all data, and then ggIndProfs would fail. If you want
it to return NULL and not an error in this case, use
nullIfEmpty=TRUE.}

\item{debug}{Start by calling debug()?}

\item{debug.sheet}{If something goes wrong when plotting, this may
be the debug method to use. Pass an integer to call browser()
when creating the corresponding sheet.}
}
\description{
Plot individual profiles and doses based on NM style dataset
}
\details{
The resulting plot object has been limited on x axis by
coord_cartesian. So if you want to adjust x limits on the
output from this function, you must use coord_cartesian. xlim
does not work.

The resulting object can be saved with ggwrite.
}
\seealso{
Other Plotting: 
\code{\link{NMplotBSVold}()},
\code{\link{NMplotBSV}()},
\code{\link{NMplotGOF}()},
\code{\link{theme_pp_facet}()},
\code{\link{theme_pp}()}
}
\concept{Plotting}
