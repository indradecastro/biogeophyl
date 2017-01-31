\name{mysisters}
\alias{mysisters}
\docType{data}
\title{
Example of unsequenced species in a list, each with a variable number of sister species (from taxonomic or expert-based knowledge).
}
\description{
List of sister species of 4 unsequenced species (Onth.X, Scar.X, Anom.X and Bodi.X) that serve as names for list elements.
}
\usage{data("mysisters")}
\format{

List of 4
  
\code{$ Onth.X: chr [1:7] "Onth1" "Onth2" "Anomius" "Onth4" ...}

\code{$ Scar.X: chr [1:3] "Scar1" "Scar2" "Scar3"}

\code{$ Anom.X: chr "Anomius.baeticus"}

\code{$ Bodi.X: chr "Anomius.baeticus"}
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Invented data, non realistic.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(mysisters)
str(mysisters)
}
\keyword{datasets}
