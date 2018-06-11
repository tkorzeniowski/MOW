\name{predictAnomalies}
\alias{predictAnomalies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
predictAnomalies
}
\description{
Classify data or get anomalies. For each observation  CBLOF, uCBLOF and LDCOF measures are computed to determine whether observation is an anomaly. Results are given as a confusion matrix as well as accuracy and precision.
}
\usage{
predictAnomalies(model, testData, testClasses, alfa = 0.8, beta = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
 model created by createGroups method
}
  \item{testData}{
 data frame of test observations
}
  \item{testClasses}{
 labels of test observations
}
  \item{alfa}{
 parameter to determine small and large clusters
}
  \item{beta}{
parameter to determine small and large clusters
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
 \item{classNameSize }{matrix containing class name, its' size and boolean flag = 1 if class is a large cluster, 0 otherwise}
 \item{outlierM }{matrix of outlier measures - values of CBLOF measures}
 \item{anomalies }{matrix of boolean values = 1 if observation is an anomally, 0 otherwise}
 \item{confusionMatrix }{confusion matrix computed for each CBLOF measure}
 \item{accuracy }{accuracy for each CBLOF measure}
 \item{precision }{precision for each CBLOF measure}
%% ...

return(list('classNameSize' = classNameSize, 'outlier' = outlierM, 'anomalies' = accTable, 'confusionMatrix' = confusionMatrix, 'accuracy' = accuracy, 'precision' = precision))
}
\references{
Small and large clusters:
http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.20.4242&rep=rep1&type=pdf

CBLOF measures:
https://www.goldiges.de/publications/Anomaly_Detection_Algorithms_for_RapidMiner.pdf
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
model <- createGroups(data, 2)

testData <- data.frame(matrix(rnorm(15), ncol=5, nrow=3))
testClasses <- c(1, 2, 3)

predictResult <- predictAnomalies(model, testData, testClasses)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
