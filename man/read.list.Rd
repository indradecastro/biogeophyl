\name{read.list}
\alias{read.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Helps reading the sisters file
}
\description{
Sisters file is easy to write but difficult to read into R. The file is typically writen by hand as it is based on expert knowledge. It should follow the format of the example data \code{\link{mysisters}}.
}
\usage{
read.list(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
A path to a plain text file.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A \emph{named} \code{list} containing character vectors of the sister species of each Phylogenetic Uncertain Taxa (=PUT).
List
  \item{PUT1:}{char}
  \item{PUT2:}{Description of 'comp2'}
}
\references{
\link{http://stackoverflow.com/questions/6602881/text-file-to-list-in-r}
}
\author{
Indradatta deCastro-Arrazola
}

\seealso{
\code{\link{mysisters}}
}
\examples{
}
