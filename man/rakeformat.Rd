\name{rakeformat}
\alias{rakeTot}
\alias{print.rake}
\alias{format.rake}
\title{Reformat the rake object for informative display}
\description{
  \code{rakeTot} adds row and column totals to the rake matrix. This is
  a subroutine called by both \code{print.rake} and \code{format.rake}.
  
  \code{print.rake} prints row and column totals to the screen.

  \code{format.rake} adds row and column totals and makes names long for
  more readable paper output.
}
\usage{
rakeTot(x)
\method{print}{rake}(x, ...)
\method{format}{rake}(x, ...)
}
\arguments{
  \item{x}{object of class "rake" created with the \code{rake}
    function.}
  \item{\dots}{other arguments to pass to default methods.}
}
\note{
  Do not use the functions \code{print.rake} and \code{format.rake}
  explicitly. Use \code{print} and \code{format} on a "rake" class
  object to call these functions using S3 methods.
}
\author{Toby Dylan Hocking <tdhock@ocf.berkeley.edu>}
\seealso{\code{\link{rake}}, \code{\link{rakeadj}}, \code{\link{predict.rake}}}
\examples{
data(certify)
r <- rake(certify,1,7)
# Note the differences between these display methods
rakeTot(r)
print(r)
format(r,digits=5)
format(r,digits=10)
}
\keyword{methods}
\keyword{attribute}
\keyword{IO}
\keyword{print}
\keyword{utilities}