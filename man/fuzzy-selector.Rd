% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selector.R
\name{fuzzy-selector}
\alias{fuzzy-selector}
\alias{sel_all_of}
\alias{sel_any_of}
\alias{sel_none_of}
\title{Apply a selector to multiple statements}
\usage{
sel_all_of(...)

sel_any_of(...)

sel_none_of(...)
}
\arguments{
\item{...}{\code{fuzzy statement}\cr
Statements to apply a selector to. Each statement must have the same length.}
}
\value{
Fuzzy values of length equal to the length of any parameter passed to
\code{...}.
}
\description{
TODO
}
\examples{
data(PimaIndiansDiabetes2, package = "mlbench")
PimaIndiansDiabetes2 \%>\%
  define_quality(age = qual_age(), mass = qual_bmi()) \%>\%
  truth_of(q_most(sel_none_of(age \%is\% young, bmi \%is\% obese)))

}
