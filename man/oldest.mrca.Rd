\name{oldest.mrca}
\alias{oldest.mrca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finds the oldest node (not monophyletic) that captures ALL species given by user
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
oldest.mrca(tree, tips)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
%%     ~~Describe \code{tree} here~~
}
  \item{tips}{
%%     ~~Describe \code{tips} here~~
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
function (tree, tips) 
{
    H <- nodeHeights(tree)
    X <- mrca(tree)
    n <- length(tips)
    nodes <- height <- vector()
    k <- 1
    for (i in 1:(n - 1)) for (j in (i + 1):n) {
        nodes[k] <- X[tips[i], tips[j]]
        height[k] <- H[match(nodes[k], tree$edge[, 1]), 1]
        k <- k + 1
    }
    z <- match(min(height), height)
    node.num <- nodes[z]
    return(node.num)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
