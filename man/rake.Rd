\name{rake}
\alias{rake}
\title{Create a weight summary Rake for a sample}
\description{

  A \code{rake} class object contains sample weights for a sample
  dataset,
  broken down by the factors of the two categorical response variables
  specified by
  \code{colCol} and \code{rowCol}.

  This is the first function in the \code{rake}, \code{rakeadj},
  \code{predict.rake} Rake analysis series.

}
\usage{
rake(data,
     colCol = 1,
     rowCol = 2,
     weight = if("weight" \%in\% names(data)) "weight" else 1 )
}
\arguments{
  \item{data}{data.frame of sample observations.}
  \item{colCol}{column of \code{data} whose data will be put in the
    columns of the rake object.}
  \item{rowCol}{column of \code{data} whose data will be put in the
    rows of the rake object.}
  \item{weight}{weights of sample observations, specified as either the
    column name of \code{data} that contains the weights, or a numeric
    vector of weights that will be replicated to be as long as
    \code{nrow(data)}.}
}
\details{
  \code{rake} treats the columns of \code{data} specified by
  \code{colCol} and \code{rowCol} as factors and constructs a matrix
  that contains sample weights.

  The \code{rake} class object has
  \code{format}, \code{predict}, and \code{print} S3 methods.

  Raking is the 2D analog of weighting-class adjustment. See References.
}
\value{
  Object of class \code{rake} that contains sample weights broken down
  by class.
}

\references{Sharon L. Lohr. Sampling: Design and Analysis,
pp. 269-271. Brooks/Cole, Pacific Grove, CA, 1999.}

\author{Toby Dylan Hocking <tdhock@ocf.berkeley.edu>}
\seealso{
  \code{\link{rakeadj}} for performing the rake adjustment,
  \code{\link{predict.rake}} for making estimates with the adjusted weight
  values,
  \code{\link{simpleRake}} for performing the entire Raking process.
}
\examples{
data(certify)

# Merge some classes since we have limited population data
# We define classes of COLLEGE as P or not P = N
certify$COLLEGE[   certify$COLLEGE  !=    "P"      ] <- "N"
# We define classes of WORKENV to be I, A, or else = O
certify$WORKENV[ ! certify$WORKENV \%in\% c("I","A") ] <- "O"

r <- rake( certify, "COLLEGE", "WORKENV" )
print(r) # Uses print.rake
}
\keyword{design}
\keyword{survey}
\keyword{category}
