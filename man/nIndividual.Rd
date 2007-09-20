% nIndividual.Rd
%--------------------------------------------------------------------------
% What: Number of individuals in a pedigree
% $Id: nIndividual.Rd 1092 2006-11-12 12:28:38Z ggorjan $
% Time-stamp: <2007-04-19 02:40:49 ggorjan>
%--------------------------------------------------------------------------

\name{nIndividual}

\alias{nIndividual}

\title{Number of individuals in a pedigree}

\description{

\code{nIndividual} returns number of individuals (individuals and/or
ascendants) in a pedigree object.

}

\usage{nIndividual(x, col=NULL, extend=TRUE, drop=TRUE)}

\arguments{
  \item{x}{pedigree}
  \item{col}{character, which id column should be the source: "id"
    (default) or particular ascendant i.e. "father" and "mother"}
  \item{extend}{logical, extend pedigree}
  \item{drop}{logical, drop unused levels in case factors are used}
}

\details{
  FIXME - this will change a lot!!!!
  There is always one additional level in levels in case factors are
  used to represent individuals in a pedigree as described in
  \code{\link{Pedigree}}. However, \code{nlevels.Pedigree} prints out
  the number of levels actually used to represent individuals i.e. level
  unknown is not included into the result.
}

\author{Gregor Gorjanc}

\seealso{\code{summary.Pedigree}, \code{extend}}

\examples{
  # Deafult example
  ped <- generatePedigree(5)
  nIndividual(ped)

  # Other id columns
  nIndividual(ped, col="father")
  nIndividual(ped, col="mother")

  # Remove individuals with unknown fathers - FIXME
  # ped <- ped[!is.na(ped, col="father"), ]
  # nIndividual(ped)
  # nIndividual(ped, extend=FALSE)
}

\keyword{misc}

%--------------------------------------------------------------------------
% nIndividual.Rd ends here
