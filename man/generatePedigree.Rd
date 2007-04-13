% generatePedigree.Rd
%--------------------------------------------------------------------------
% What: Generate pedigree example man page
% $Id: generatePedigree.Rd 1092 2006-11-12 12:28:38Z ggorjan $
% Time-stamp: <2006-09-30 01:39:14 ggorjan>
%--------------------------------------------------------------------------

\name{generatePedigree}

\alias{generatePedigree}

\title{Generate Pedigree example}

\description{\code{generatePedigree} creates nonoverlapping pedigree
  example, which can be used for demos and code testing.
}

\usage{generatePedigree(nId, nGeneration=3, nFather=round(nId/3),
  nMother=nId - nFather, start=1, generationOrder="increasing",
  colClass="integer")}

\arguments{
  \item{nId}{integer, number of individuals per generation, at least 2}
  \item{nGeneration}{integer, number of generations}
  \item{nFather}{integer, number of fathers per generation}
  \item{nMother}{integer, number of mothers per generation}
  \item{start}{first generation value}
  \item{generationOrder}{character, generation value is "increasing" or
    "decreasing" through generations}
  \item{colClass}{character, class for columns: "integer" or "factor"}
}

\value{
  An extended, sorted and possibly coded pedigree object with
  following columns: id, father, mother, generation and sex.
}

\references{Rico Gutierrez, M. (1999) Los modelos lineales en la mejora
  genetica animal. Ediciones Peninsular. ISBN 84-605-9910-8.}

\author{Marcos Rico Gutierrez (author of MATLAB code) and Gregor Gorjanc
  (R implementation)}

\seealso{\code{\link{Pedigree}}}

\examples{
  generatePedigree(5)
  generatePedigree(nId=5, nGeneration=4, nFather=1, nMother=2)
  generatePedigree(nId=5, nGeneration=4, nFather=1, nMother=2,
                   start=0, generationOrder="decreasing")
  generatePedigree(nId=100, nGeneration=20, nFather=10, nMother=50)
}

\keyword{manip}
\keyword{misc}

%--------------------------------------------------------------------------
% generatePedigree.Rd ends here
