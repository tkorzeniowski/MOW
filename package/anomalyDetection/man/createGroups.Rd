\name{createGroups}
\alias{createGroups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
createGroups
}
\description{
Creates clustering model for given data, specified number of classes and algorithm. Currently kemans, kmedoids and hierarchical models available.
}
\usage{
createGroups(trainData, numberOfClasses, algorithm = "kmeans")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trainData}{
 train observations to create model (data frame)
}
  \item{numberOfClasses}{
 number of classes to create clusters
}
  \item{algorithm}{
'kmeans' - standard k-means algorithm, 'kmedoids' - pam implementation, 'hierarchic' - agnes implementation
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
Returns
  \item{labels }{list of labels for each training observation}
  \item{centers }{cluster centers}
  \item{name}{specified algorithm}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data <- data.frame(matrix(rnorm(50), ncol=5, nrow=10))
result <- createGroups(data, 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
