\name{sunplin.n.tree}
\alias{sunplin.n.tree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bla bla bla
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sunplin.n.tree(fileOrText, nth, typeInput)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fileOrText}{
%%     ~~Describe \code{fileOrText} here~~
}
  \item{nth}{
%%     ~~Describe \code{nth} here~~
}
  \item{typeInput}{
%%     ~~Describe \code{typeInput} here~~
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
function (fileOrText, nth, typeInput) 
{
    if (typeInput == "tree") {
        write(fileOrText, "temp", sep = "\n")
        fileOrText = "temp"
    }
    ret <- strsplit(scan(fileOrText, what = character(), skip = nth, 
        nlines = 1, quiet = T), " ")[[4]]
    return(read.tree(text = ret))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
