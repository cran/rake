\name{rakeaccessors}
\alias{colvar}
\alias{rowvar}
\alias{dataname}
\title{Rake attribute accessors}
\description{
  Accessor functions for attributes of "rake" class objects.
}
\usage{
dataname(rake)
rowvar(rake)
colvar(rake)
}
\arguments{
  \item{rake}{Object of class "rake".}
}
\value{
  \code{dataname} returns attribute "dataname" ("character").
  
  \code{rowvar} returns attribute "rowCol" ("integer" or "character").
  
  \code{colvar} returns attribute "colCol" ("integer" or "character").
}
\author{Toby Dylan Hocking <tdhock@ocf.berkeley.edu>}
\seealso{
  \code{\link{rake}} for creating "rake" class objects.
}
\keyword{programming}
\keyword{misc}
\keyword{utilities}
