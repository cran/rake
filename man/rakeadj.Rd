\name{rakeadj}
\alias{rakeadj}
\title{Adjust the marginal total weights of a "rake" class object}
\description{

  The marginal total weights of the rake object are adjusted to match
  the population marginal total weights specified by \code{marg}. As a
  result, the total weights of each class of \code{rake} will be
  adjusted, and these adjusted weights can be used for estimation.

  This is the second function in the \code{rake}, \code{rakeadj},
  \code{predict.rake} Rake analysis series.

}
\usage{
rakeadj(rake, marg, verbose)
}
\arguments{
  \item{rake}{Object of class "rake" created with \code{rake}.}
  \item{marg}{The population marginal total weights. This may be
    specified in any of the following ways:

    numeric: vector of marginal total weights, matched to the values
    in \code{rake} by name.

    data.frame: column "weight" must contain the weights, matched by
    names specified in column "name".

    character: filename of an appropriate table that will be read as a
    \code{data.frame}.}
  \item{verbose}{logical: Should the number of iterations for the rake
    adjustment to converge be printed?}
}
\details{

  Rake adjustment via \code{rakeadj} is accomplished by, for each class,
  dividing by the marginal total weights of the class, then multiplying
  by the marginal total weight specified by \code{marg}.

  After performing the rake adjustment above on each class in the rows
  of \code{rake}, the
  marginal totals of the rows of \code{rake} match the target marginal totals
  specified by \code{marg}, but the marginal totals of the columns of
  \code{rake} do not match the target marginal totals. So, the rake
  adjustment is then performed on each class of the columns of
  \code{rake}, which then syncs the column marginal totals but unsyncs
  the row marginal totals.

  By repeatedly performing the rake adjustment, the marginal
  totals of the columns and rows of \code{rake} eventually converge to
  the marginal totals specified by \code{marg}.
  
}
\value{
  Object of class "rake", with adjusted weights.
}

\references{Sharon L. Lohr. Sampling: Design and Analysis,
  pp. 269-271. Brooks/Cole, Pacific Grove, CA, 1999.}

\author{Toby Dylan Hocking <tdhock@ocf.berkeley.edu>}
\seealso{
  \code{\link{rake}} for creating the initial "rake" class object,
  \code{\link{predict.rake}} for making estimates with the adjusted weight
  values,
  \code{\link{simpleRake}} for performing the entire Raking process.
}
\examples{
example(rake)
r <- rakeadj( r, statpoptotal, TRUE )
print(r)
}
\keyword{design}
\keyword{survey}
\keyword{category}

