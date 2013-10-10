\name{avgrankoverlap-package}
\alias{avgrankoverlap-package}
\docType{package}
\title{
  Average Rank Overlap
}
\description{
  Rank overlap is a metric to measure the relation between two sets of ranked items. This package is a convenience to calculate average rank overlap across different settings.
}
\details{
\tabular{ll}{
Package: \tab avgrankoverlap\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-10-10\cr
License: \tab GPL (>=2)\cr
}
}
\author{
Fatih Sunor

Maintainer: sunor@stanford.edu
}
\references{
  "A Similarity Measure for Indefinite Rankings" by Webber at al\cr
   Link: http://www.umiacs.umd.edu/~wew/papers/wmz10_tois.pdf
}

\keyword{ package }
\keyword{ average }
\keyword{ rank }
\keyword{ overlap }
\keyword{ coefficient }

\examples{
  v1<-c("RESTAURANT 1","MOST EXPENSIVE FIRST","KEBAB","BURGER","SALAD","SODA")
  v2<- c("RESTAURANT 1","LEAST CALORIES","SALAD","SODA","BURGER","KEBAB")
  v3<-c("RESTAURANT 2","LEAST CALORIES","SALAD","TIRAMISU","SUSHI","SODA")
  v4<-c("RESTAURANT 2","MOST EXPENSIVE FIRST","SUSHI","SALAD","TIRAMISU","SODA")
  data<-matrix(0,nrow=4,ncol=6);
  data[1,]<-v1; data[2,]<-v2; data[3,]<-v3; data[4,]<-v4;
  avgrankoverlap(data)
}
