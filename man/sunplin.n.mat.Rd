\name{sunplin.n.mat}
\alias{sunplin.n.mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bla bla bla
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sunplin.n.mat(fileName, nth)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fileName}{
%%     ~~Describe \code{fileName} here~~
}
  \item{nth}{
%%     ~~Describe \code{nth} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (fileName, nth) 
{
    ret <- scan(fileName, what = character(), nlines = 1)
    if (is.numeric(ret[1]) == TRUE) 
        qtdNodes <- as.numeric(ret[1])
    else qtdNodes <- as.numeric(ret[2])
    ret <- scan(fileName, what = character(), sep = "\n", skip = (((nth - 
        1) * (qtdNodes + 1) + 1)), nlines = (qtdNodes) + 1)
    ret <- strsplit(ret, "\t")
    mat <- round(lower.tri(matrix(1, qtdNodes + 1, qtdNodes + 
        1)))
    for (i in 1:qtdNodes + 1) mat[1, i] <- ret[[1]][i]
    for (i in 2:qtdNodes + 1) {
        for (j in 1:i - 1) {
            mat[i, j] <- ret[[i]][j]
        }
    }
    return(mat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
