\name{predict.rake}
\alias{predict.rake}
\title{Predict sample values after Rake adjustment re-weighting}
\description{

  Use \code{predict} on a "rake" class object to obtain adjusted sample values
  that reflect the sample weights in \code{rake}.
  
  This is the third function in the \code{rake}, \code{rakeadj},
  \code{predict.rake} Rake analysis series.
  
}
\usage{
\method{predict}{rake}(object,
           col = stop("Prediction column (argument 'col') must be specified."),
           forcefactor = FALSE,
           data = eval(parse(text=dataname(rake)),parent.frame(1)),
           ... )
}
\arguments{
  \item{object}{object of class "rake" created with the function \code{rake}.}
  \item{col}{column of \code{data} from which re-weighted data will be
    gathered and returned.}
  \item{forcefactor}{logical: should \code{data[,col]} be treated as a
    factor, even if it is numeric data?}
  \item{data}{usually the data.frame from which the rake was
    generated.}
  \item{\dots}{other arguments to pass to default methods.}
}
\value{
  List with entries:
  \item{data}{the original data of interest.}
  \item{weight}{the adjusted weights from the rake, formatted as a
    vector that corresponds to a column of \code{data}.}
  \item{data.est}{the data of interest after re-weighting.}
}
\note{
  Simply use \code{predict(rake)} rather than \code{predict.rake(rake)}.
}

\references{Sharon L. Lohr. Sampling: Design and Analysis,
  pp. 269-271. Brooks/Cole, Pacific Grove, CA, 1999.}

\author{Toby Dylan Hocking <tdhock@ocf.berkeley.edu>}
\seealso{
  \code{\link{rake}} for creating the initial "rake" class object,
  \code{\link{rakeadj}} for performing the rake adjustment,
  \code{\link{simpleRake}} for performing the entire Raking process.
}
\examples{
example(rakeadj)
for( i in 1:5 ) {
  cat("Variable:",names(certify)[i],"\n")
  rakeresult <- predict(r,i,forcefactor=TRUE)
  data <- rakeresult$data
  pred <- rakeresult$data.est
  print(rbind(data,pred))
  cat("\n")
}
}
\keyword{design}
\keyword{survey}
\keyword{category}
\keyword{methods}
