\name{simpleRake}
\alias{simpleRake}
\title{Perform a complete Raking}
\description{
  \code{simpleRake} is provided as a wrapper function for the most
  common method of Rake analysis. It calls \code{rake} and \code{rakeadj},
  then returns the result of \code{predict.rake}.
}
\usage{
simpleRake(data, marg,
           colCol = 1, rowCol = 2,
           col = 3, forcefactor = FALSE,
           weight = if("weight" \%in\% names(data)) "weight" else 1,
           verbose = FALSE)
}
\arguments{
  \item{data}{data.frame of sample observations.}
  \item{marg}{the population marginal total weights. This may be
    specified in any of the following ways:

    numeric: vector of marginal total weights, matched to the values
    in \code{rake} by name.

    data.frame: column "weight" must contain the weights, matched by
    names specified in column "name".

    character: filename of an appropriate table that will be read as a
    \code{data.frame}.}
  \item{colCol}{column of \code{data} whose data will be put in the
    columns of the rake object.}
  \item{rowCol}{column of \code{data} whose data will be put in the
    rows of the rake object.}
  \item{col}{column of \code{data} from which re-weighted data will be
    gathered and returned.}
  \item{forcefactor}{logical: should \code{data[,col]} be treated as a
    factor, even if it is numeric data?}
  \item{weight}{weights of sample observations, specified as either the
    column name of \code{data} that contains the weights, or a numeric
    vector of weights that will be replicated to be as long as
    \code{nrow(data)}.}
  \item{verbose}{logical: Should the number of iterations for the rake
    adjustment to converge be printed?}
}
\value{
  List with entries:
  \item{data}{the original data of interest.}
  \item{weight}{the adjusted weights from \code{rake}, formatted as a
    vector that corresponds to a column of \code{data}.}
  \item{data.est}{the data of interest after re-weighting.}
}

\references{Sharon L. Lohr. Sampling: Design and Analysis,
  pp. 269-271. Brooks/Cole, Pacific Grove, CA, 1999.}

\author{Toby Dylan Hocking <tdhock@ocf.berkeley.edu>}
\seealso{
  \code{rake} for creating the initial "rake" class object,
  \code{rakeadj} for performing the rake adjustment,
  \code{predict.rake} for making estimates with the adjusted weight
  values.
}
\examples{
data(certify)

# Merge some classes since we have limited population data
# We define classes of COLLEGE as P or not P = N
certify$COLLEGE[   certify$COLLEGE  !=    "P"      ] <- "N"
# We define classes of WORKENV to be I, A, or else = O
certify$WORKENV[ ! certify$WORKENV \%in\% c("I","A") ] <- "O"

rakeresult <- simpleRake(certify, statpoptotal, "WORKENV", "COLLEGE",
"WOULDYOU", TRUE)

data <- rakeresult$data
pred <- rakeresult$data.est
print(rbind(data,pred))
}
\keyword{design}
\keyword{survey}
\keyword{category}
