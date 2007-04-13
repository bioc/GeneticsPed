% founder.Rd
%--------------------------------------------------------------------------
% What: Founder and non-founder individuals in the pedigree
% $Id: founder.Rd 1097 2006-11-14 06:10:43Z ggorjan $
% Time-stamp: <2006-10-09 17:13:05 ggorjan>
%--------------------------------------------------------------------------

\name{isFounder}

\alias{isFounder}

\concept{founders}
\concept{base indiviual}

\title{Founder and non-founder individuals in the pedigree}

\description{

\code{isFounder} classifies individuals in the pedigree as founders
(base) or non-founders (non-base individuals).

}

\usage{
  isFounder(x, col=attr(x, ".ascendant"))
}

\arguments{
  \item{x}{pedigree object}
  \item{col}{character, which columns should be checked, see examples}
}

\details{

By definition founders do not have any known ascendants, while the
opossite is the case for non-founders i.e. they have at least one known
ascendant.

FIXME: any relation with founderGeneSet in GeneticsBase

}

\value{Boolean vector.}

\author{Gregor Gorjanc}

\seealso{\code{\link{Pedigree}}}

\examples{
  ped <- generatePedigree(nId=5)
  isFounder(ped)
  ## Based only on fathers
  isFounder(ped, col=c("father"))
  ## Works also only on a part of a pedigree
  isFounder(ped[1:5, ])
}

\keyword{misc}

%--------------------------------------------------------------------------
% founder.Rd ends here
