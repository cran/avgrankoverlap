\name{avgrankoverlap}
\alias{avgrankoverlap}
\title{
  Average Rank Overlap
}

\description{
  Given a table with two level keys, it calculates the average rank overlap
}
\usage{
  avgrankoverlap(items, settings = items[, 2], key = items[, 1])
}
\arguments{
  \item{items}{
    \code{items} is a table with two levels of keys, for example:\cr
    RESTAURANT 1 | MOST EXPENSIVE FIRST | KEBAB | BURGER | SALAD | SODA |\cr
    RESTAURANT 1 | LEAST CALORIES | SALAD | SODA | BURGER | KEBAB |\cr
    RESTAURANT 2 | LEAST CALORIES | SALAD | TIRAMISU | SUSHI | SODA |
 
}
  \item{settings}{
    \code{settings} is a vector to describe the nature of the sorting
}
  \item{key}{
    \code{key} is a vector to associate a rank to an owner
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
  v1<-c("RESTAURANT 1","MOST EXPENSIVE FIRST","KEBAB","BURGER","SALAD","SODA")
  v2<-c("RESTAURANT 1","LEAST CALORIES","SALAD","SODA","BURGER","KEBAB")
  v3<-c("RESTAURANT 2","LEAST CALORIES","SALAD","TIRAMISU","SUSHI","SODA")
  v4<-c("RESTAURANT 2","MOST EXPENSIVE FIRST","SUSHI","SALAD","TIRAMISU","SODA")
  data<-matrix(0,nrow=4,ncol=6);
  data[1,]<-v1; data[2,]<-v2; data[3,]<-v3; data[4,]<-v4;
  avgrankoverlap(data)  
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ average }
\keyword{ rank }
\keyword{ overlap }
\keyword{ coefficient }