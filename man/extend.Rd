% extend.Rd
%--------------------------------------------------------------------------
% What: Extend pedigree man page
% $Id: extend.Rd 1092 2006-11-12 12:28:38Z ggorjan $
% Time-stamp: <2007-04-19 02:37:55 ggorjan>
%--------------------------------------------------------------------------

\name{extend}

\alias{extend}

\title{Extend pedigree}

\description{\code{extend} finds ascendants, which do not appear as
  individuals in pedigree and assigns them as individuals with unknown
  ascendants in extended pedigree.
}

\usage{extend(x, ascendant=NULL, col=NULL, top=TRUE)}

\arguments{
  \item{x}{pedigree object}
  \item{ascendant}{character, column names of ascendant(s), see details}
  \item{col}{character, column name(s) of attribute(s), see details}
  \item{top}{logical, add ascendants as individuals on the top or
    bottom of the pedigree}
}

\details{

Argument \code{ascendant} can be used to define, which ascendants will
be extended. If \code{ascendant=NULL}, which is the default, all
ascendant columns in the pedigree are used. The same approach is used
with other pedigree attributes such as sex, generation, etc. with
argument \code{col}. Use \code{col=NA}, if none of the pedigree
attributes should be extended.

Sex of \dQuote{new} individuals is infered from attribute
\code{ascendantSex} as used in \code{Pedigree} function. Generation of
\dQuote{new} individuals is infered as minimal
(\code{generationOrder="increasing"}) or maximal
(\code{generationOrder="decreasing"}) generation value in descendants -
1. See \code{\link{Pedigree}} on this issue. Family values are extended
with means of \code{\link{family}}.

}

\value{

Extended pedigree, where all ascendants also appear as individuals with
unknown ascendants and infered other attributes such as sex, generation,
etc. if this attributes are in the pedigree.

}

\author{Gregor Gorjanc}

\seealso{\code{\link{Pedigree}}, \code{\link{family}},
  geneticGroups???}

\examples{
  # --- Toy example ---
  ped <- generatePedigree(nId=5, nGeneration=4, nFather=1, nMother=2)
  ped <- ped[10:20,]
  ped[5, "father"] <- NA # to test robustnes of extend on NA
  extend(ped)
  extend(ped, top=FALSE)

  ## Extend only ascendant and their generation
  extend(ped, col="generation")
  extend(ped, col=c("generation", "sex"))

  # --- Bigger example ---
  ped <- generatePedigree(nId=1000, nGeneration=10, nFather=100,
                          nMother=500)
  nrow(ped)
  # Now keep some random individuals
  ped <- ped[unique(sort(round(runif(n=nrow(ped)/2, min=1,
                                     max=nrow(ped))))), ]
  nrow(ped)
  nrow(extend(ped))
}

\keyword{manip}
\keyword{misc}

%--------------------------------------------------------------------------
% extend.Rd ends here
