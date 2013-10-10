\name{score}
\alias{score}
\title{
Score
}
\description{
  Calculates the average weighted overlap between two sequences of items.
  The sequences are expected to be the same length and not empty.
}
\usage{
  score(l1, l2, depth = length(l1))
}
\arguments{
  \item{l1}{
    \code{l1} is the first sequence of items
  }
  \item{l2}{
    \code{l2} is the second sequence of items
  }
  \item{depth}{
    \code{depth} describes up to which rank the overlap will be calculated. It can't be more than the length of the the sequence
  }  
}
\value{
  The returned value is the overlap score
}
\references{
  "A Similarity Measure for Indefinite Rankings" by Webber at al\cr
  Link: http://www.umiacs.umd.edu/~wew/papers/wmz10_tois.pdf
}
\author{
  Fatih Sunor
}
\examples{
  v1<-c("KEBAB","BURGER","SALAD","SODA")
  v2<-c("SALAD","SODA","BURGER","KEBAB")
  score(v1,v2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ average }
\keyword{ rank }
\keyword{ overlap }
\keyword{ coefficient }
