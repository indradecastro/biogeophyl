\name{clean.sisters}
\alias{clean.sisters}
\title{
Check that sister species provided are present in the trees and they are enough to locate all unsequenced species.
}
\description{
Checks that all sister species provided are present in the trees. Removes those that are missing in the trees. Checks that all unsequenced species (=target species) have at least one sister species (after the cleaning of sister species).
}
\usage{
clean.sisters(sisters, mytrees)
}
\arguments{
  \item{sisters}{
\emph{list} of sister species. The list should be \emph{named} with the names of unsequenced species. Each element in the list should be a character vector of names of sister species. For an example, see \code{?mysisters}.
}
  \item{mytrees}{
Typically an object of class \code{multiPhylo}, although only the first tree is used as \code{tree$tip.labels} for all trees should be identical (but possibly in different order).
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
\code{\link{read.list}}, \code{\link{mysisters}}
}
\examples{
# Load data
data(mysisters)
data(mytrees)

# Inspect original data
mysisters
plot(mytrees[[1]], show.node.labels=TRUE)

# Use function
my.clean.sisters <- clean.sisters(mysisters, mytrees)

# Inspect resulting clean data
# and messages showing removed data
my.clean.sisters


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
