\name{prosunplin}
\alias{prosunplin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bla bla bla
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prosunplin(alltrees, sisters, numtrees, destination)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alltrees}{
%%     ~~Describe \code{alltrees} here~~
}
  \item{sisters}{
%%     ~~Describe \code{sisters} here~~
}
  \item{numtrees}{
%%     ~~Describe \code{numtrees} here~~
}
  \item{destination}{
%%     ~~Describe \code{destination} here~~
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
function (alltrees, sisters, numtrees, destination) 
{
    sisters <- clean.sisters(sisters, alltrees)
    monos <- lapply(alltrees, function(tree) {
        lapply(sisters, function(desloc.sp) {
            get_mono(tree, desloc.sp)
        })
    })
    allnodes <- lapply(monos, sample.monos)
    source("sunplin-functions.r")
    dest.file <- paste0(destination, "results.expanded.trees.tre")
    tree.file <- paste0(tempdir(), "/tree")
    nodes.file <- paste0(tempdir(), "/nodes")
    expanded.trees <- mapply(FUN = function(tree, nodes) {
        write.tree(tree, file = tree.file)
        write.table(nodes, file = nodes.file, quote = F, col.names = F, 
            row.names = F, sep = " ")
        exp.trees <- sunplin.expd(tree.file, nodes.file, numtrees, 
            2)
        tree.list <- lapply(1:numtrees, function(x) {
            sunplin.n.tree(exp.trees, x, "tree")
        })
        class(tree.list) <- "multiPhylo"
        write.tree(tree.list, file = dest.file, append = T)
        return(tree.list)
    }, alltrees, allnodes)
    return(expanded.trees)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
