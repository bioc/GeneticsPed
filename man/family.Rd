% family.Rd
%--------------------------------------------------------------------------
% What: Find families (lines) in the pedigree
% $Idn$
% Time-stamp: <2006-07-22 23:32:03 ggorjan>
%--------------------------------------------------------------------------

\name{family}

\alias{family}
\alias{family<-}

\concept{line}
\concept{lineage}

\title{Find families (lines) in the pedigree}

\description{

\code{family} classifies individuals in the pedigree to distinct
families or lines. Two individuals are members of one family if they
have at least one common ascendant. \code{family<-} provides mean to
properly add family information into the pedigree.

}

\usage{
family(x)
family(x, col=NULL) <- value
}

\arguments{
  \item{x}{pedigree object}
  \item{col}{character, column name in \code{x} for family}
  \item{value}{family values for subjects in the pedigree}
}

\details{

\code{col} provides a mean to name or possibly also rename family column
with user specified value, say "familia" in Spanish. When
\code{col=NULL}, which is default, "family" is used.

}

\value{A vector of family values (integers)}

\author{Gregor Gorjanc}

\seealso{\code{\link{Pedigree}}}

\examples{
  ## Two families examples
  ped <- data.frame(    id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                    father=c(0, 0, 0, 0, 0, 0, 5, 1, 3,  8,  7),
                    mother=c(0, 0, 0, 0, 0, 0, 6, 2, 4,  9, 10),
                generation=c(1, 1, 1, 1, 1, 1, 2, 2, 2,  3,  4))
  ped <- Pedigree(ped, unknown=0, generation="generation")
  family(ped)

  ## After break we get two families
  ped1 <- removeIndividual(ped, individual=11)
  family(ped1)

  ## Subsetting can also be used
  family(ped[1:10,])
  family(ped[7:10,])

  ## Pedigree need not be sorted in advance
  ped2 <- ped[sample(1:10), ]
  family(ped2)

  ## Assign family values to pedigree
  family(ped) <- family(ped)
  ped
  family(ped, col="familia") <- family(ped)
  ped
}

\keyword{misc}

%--------------------------------------------------------------------------
% family.Rd ends here
