\name{prosunplin}
\alias{prosunplin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Probabilistically includes unsequenced species into a set of given phylogenetic trees
}
\description{
Phylogenetic trees are built from sequenced species (from GenBank or own sequencing). However, it is typical that some species in the study are lacking sequences, generally called \emph{Phylogenetic Uncertain Taxa}, PUTs. Thus, it is not possible to include them in the phylogenetic tree. Function \code{sunplin.exp \{sunplin\}} allows to randomly insert such species in a clade (=under a node given by the resercher).

To provide nodes under which a given PUT should be found is relatively easy for a single tree, but to do this for multiple trees (as a result of many tree construction processes that allow for molecular uncertainty) can be tedious.

Function \code{prosunplin} provides a set of functions to: (1) read a file of unsequenced species and their sister species, (2) check that sister species provided are present in the N trees provided, (3) find monophyletic nodes that capture those sister species for each unsequenced species (PUT), (4) probabilistically select one of those nodes, (5) randomly insert the unsequenced species below those selected nodes M times (as required by the researcher), in each and all N original trees.
}
\usage{
prosunplin(alltrees, sisters, numtrees)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alltrees}{
An object of class multiPhylo\{ape\}, containing phylogenetic trees. Typically created as part of a molecular uncertainty procedure.
}
  \item{sisters}{
A list containing character vectors of the sister species of each Phylogenetic Uncertain Taxa (=PUT). The list should be named with the names of the PUTs.
}
  \item{numtrees}{
integer. Number of trees (N trees) to generate from each \code{alltrees} (M trees). Thus, prosumplin function returns N*M number of expanded trees including original species plus as many valid PUTs are provided in the \code{sisters} object.
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
\code{\link{get_mono}}
}
\examples{
# Load data
data(mytrees)
data(mysisters)

# Execute prosunplin
fulltrees <- prosunplin(mytrees, mysisters, 10)
}

