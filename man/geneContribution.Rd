% geneContribution.Rd
%--------------------------------------------------------------------------
% What: Gene contribution or proportion of genes in pedigree by
%       individual man page
% $Id: prop.pedigree.Rd 895 2006-06-16 14:28:51Z dnadave $
% Time-stamp: <2007-04-19 02:37:24 ggorjan>
%--------------------------------------------------------------------------

\name{geneContribution}

\alias{geneContribution}

\title{Gene contribution or proportion of genes in pedigree by individual}

\description{\code{geneContribution} calculates gene contribution
  as proportion of genes in pedigree by individual with higher number of
  descendants will have higher values.
}

\usage{geneContribution(x, relative=TRUE)}

\arguments{
  \item{x}{pedigree}
  \item{relative}{logical, should results be presented relative to
    number of individuals in the pedigree}
}

\value{

Gene contribution values i.e. higher the values higher the contribution
of genes by particular individual in the pedigree. When
\code{relative=FALSE}, values represent number of individuals (in
conceptually additive manner i.e. 0.5 + 0.75 = 1.25 individual) in the
pedigree that carry genes of a particular individual. With
\code{relative=TRUE}, values represent the same result as ratios to all
individuals in the pedigree. Value 0 indicates that individual did not
pass its genes to next generations.

}

\author{Gregor Gorjanc}

\seealso{\code{\link{Pedigree}}}

\examples{
  ped <- generatePedigree(nId=5, nGeneration=4, nFather=1, nMother=2)
  geneContribution(ped)
  geneContribution(ped, relative=FALSE)
  ## geneContribution(ped[5:15, ]) ## needs [ method

  ## More than one father example
  ped <- data.frame(     id=c(1, 2, 3, 4, 5, 6, 7),
                    father1=c(0, 0, 0, 2, 1, 1, 2),
                    father2=c(0, 0, 0, 0, 0, 2, 0),
                     mother=c(0, 0, 0, 0, 3, 3, 3),
                    generat=c(1, 1, 1, 2, 2, 2, 2))
  ped <- Pedigree(ped, ascendant=c("father1", "father2", "mother"),
                  ascendantSex=c(1, 1, 2), ascendantLevel=c(1, 1, 1),
                  unknown=0, generation="generat")
  geneContribution(ped)
}

\keyword{array}
\keyword{misc}

%--------------------------------------------------------------------------
% geneContribution.Rd ends here
