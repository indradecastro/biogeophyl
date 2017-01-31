\name{sample.monos}
\alias{sample.monos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Probabilistically selects one of the monophyletic clades (=node/tip) from \code{data.frames} containing a list of monophyletic clades (=nodes/tips)
}
\description{
For each Phylogenetic Uncertain Taxa (=PUT) a single monophylletic node is selected from several provided by user or function \code{\link{get_mono}}. By default selection is done according to the number of tips under each of the possible nodes to choose from. That is, if PUT1 has sister species divided in 2 monophiletic nodes containing 2 and 6 sister species, node2 (with 6 sister species) will have 75\% proabability of being selected, while node1 (with 2 sister species) will only be selected 25\% of the times.
}
\usage{sample.monos(monos)}

\arguments{
  \item{monos}{
A \code{list} of \code{data.frame}s each with two columns: (1) node names, (2) number of tips under each node. The \code{list} should be \emph{named} with names of Phylogenetic Uncertain Taxa (=PUT).
}
}

\value{

%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Indradatta deCastro-Arrazola
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
}
