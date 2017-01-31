\name{sunplin.expd}
\alias{sunplin.expd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bla bla bla
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sunplin.expd(fileTree, filePuts, numTree, method)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fileTree}{
%%     ~~Describe \code{fileTree} here~~
}
  \item{filePuts}{
%%     ~~Describe \code{filePuts} here~~
}
  \item{numTree}{
%%     ~~Describe \code{numTree} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
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
function (fileTree, filePuts, numTree, method) 
{
    dyn.load("sunplin.spn")
    return(.Call("expd", fileTree, filePuts, numTree, method))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
