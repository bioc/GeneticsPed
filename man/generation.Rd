% generation.Rd
%--------------------------------------------------------------------------
% What: Calculate generation value man page
% $Id: generation.Rd 1092 2006-11-12 12:28:38Z ggorjan $
% Time-stamp: <2006-09-30 01:38:53 ggorjan>
%--------------------------------------------------------------------------

\name{generation}

\alias{generation}
\alias{generation<-}

\title{Calculate generation value}

\description{

\code{generation} calculates generation value of individuals in given
pedigree. \code{generation<-} provides mean to properly add generation
information into the pedigree.

}

\usage{
generation(x, start=1, generationOrder=NULL)
generation(x, generationOrder=NULL, col=NULL) <- value
}

\arguments{
  \item{x}{pedigree object}
  \item{start}{first generation value}
  \item{generationOrder}{character, should be generation values
    "increasing" or "decreasing" through generations, see details}
  \item{col}{character, column name in \code{x} for generation}
  \item{value}{generation values for subjects in the pedigree}
}

\details{

Generation value for founders is set to value \code{start}, which is by
default 1, while other individuals get it according to:

\deqn{G_s = max(G_{1a} + G_{2a} + ... G_{na}) + 1}

where G represents generation value for s - subject, a - ascendant
e.g. father and mother, where n=2. N might be higher if there are
multiple ascendants i.e. this function can also handle pedigrees with
higher order ascendants e.g. grandfather.

\code{generationOrder} can be used to define "increasing" or
"decreasing" order of generation values. If this argument is
\code{NULL}, which is default, then this information is taken from
the pedigree - see \code{\link{Pedigree}} for more on this issue.

\code{col} provides a mean to name or possibly also rename generation
column with user specified value, say "generazione" in Italian. When
\code{col=NULL}, which is default, "generation" is used.

}

\value{A vector of generation values (integers)}

\author{Gregor Gorjanc}

\seealso{\code{\link{Pedigree}}}

\examples{
  # Nonoverlapping pedigree
  ped <- generatePedigree(nId=5, nGeneration=4, nFather=1, nMother=2)
  ped$generation1 <- generation(ped)
  ped

  # Overlapping Pedigree
  ped <- data.frame(     id=c(1, 2, 3, 4, 5, 6, 7),
                     father=c(0, 0, 2, 2, 2, 4, 4),
                     mother=c(0, 0, 1, 0, 3, 3, 5),
                    dtBirth=c(2, 1, 3, 4, 5, 6, 7))
  ped <- Pedigree(ped, unknown=0, dtBirth="dtBirth")
  generation(ped) <- generation(ped)

  # Overlapping pedigree + one individual (4) comes late in pedigree and
  # has no ascendants
  ped <- data.frame(     id=c(1, 2, 3, 4, 5, 6, 7),
                     father=c(0, 0, 2, 0, 2, 4, 4),
                     mother=c(0, 0, 1, 0, 3, 3, 5),
                    dtBirth=c(2, 1, 3, 2, 5, 6, 7))
  ped <- Pedigree(ped, unknown=0, dtBirth="dtBirth")
  generation(ped)
  generation(ped, generationOrder="decreasing",
             col="generazione") <- generation(ped, generationOrder="decreasing")
}

\keyword{manip}
\keyword{misc}

%--------------------------------------------------------------------------
% generation.Rd ends here
