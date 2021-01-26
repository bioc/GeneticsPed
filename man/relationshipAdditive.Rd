% relationshipAdditive.Rd
%--------------------------------------------------------------------------
% What: Additive relationship matrix (A) man page
% $Id: relationshipAdditive.Rd 1167 2007-04-03 14:02:23Z ggorjan $
% Time-stamp: <2007-04-01 23:12:01 ggorjan>
%--------------------------------------------------------------------------

\name{relationshipAdditive}

\alias{relationshipAdditive}
\alias{inverseAdditive}
\alias{kinship}

\concept{relationship}
\concept{relatedness}
\concept{genetic covariance}
\concept{coefficient of coancestry}
\concept{coefficient of consanguinity}
\concept{coefficient de parente}

\title{Additive relationship matrix and its inverse}

\description{

\code{relationshipAdditive} creates additive relationship matrix, while
\code{inverseAdditive} creates its inverse directly from a
pedigree. \code{kinship} is another definition of relationship and is
equal to half of additive relationship.

}

\usage{

relationshipAdditive(x, sort=TRUE, names=TRUE, \ldots)
inverseAdditive(x, sort=TRUE, names=TRUE, \ldots)
kinship(x, sort=TRUE, names=TRUE, \ldots)

}

\arguments{
  \item{x}{Pedigree}
  \item{sort}{logical, for the computation the pedigree needs to be
    sorted, but results are sorted back to original sorting (sort=TRUE)
    or not (sort=FALSE)}
  \item{names}{logical, should returned matrix have row/colnames; this
    can be used to get leaner matrix}
  \item{\ldots}{arguments for other methods}
}

\details{

Additive or numerator relationship matrix is symetric and contains
\eqn{1 + F_i} on diagonal, where \eqn{F_i} is an inbreeding coefficients
(see \code{\link{inbreeding}}) for subject \eqn{i}. Off-diagonal
elements represent numerator or relationship coefficient bewteen
subjects \eqn{i} and \eqn{j} as defined by Wright (1922). Henderson
(1976) showed a way to setup inverse of relationship matrix
directly. Mrode (2005) has a very nice introduction to these concepts.

Take care with \code{sort=FALSE, names=FALSE}. It is your own
responsibility to assure proper handling in this case.

}

\value{A matrix of \eqn{n * n} dimension, where \eqn{n} is number of
subjects in \code{x}}

\references{

Henderson, C. R. (1976) A simple method for computing the inverse of a
numerator relationship matrix used in prediction of breeding
values. \emph{Biometrics} \bold{32}(1):69-83

Mrode, R. A. (2005) Linear models for the prediction of animal breeding
values. 2nd edition. CAB International. ISBN 0-85199-000-2
\url{http://www.amazon.com/gp/product/0851990002}

Wright, S. (1922) Coefficients of inbreeding and relationship.
\emph{American Naturalist} 56:330-338

}

\author{Gregor Gorjanc and Dave A. Henderson}

\seealso{\code{\link{Pedigree}}, \code{\link{inbreeding}} and
\code{\link{geneFlowT}}}

\examples{
  data(Mrode2.1)
  Mrode2.1$dtB <- as.Date(Mrode2.1$dtB)
  x2.1 <- Pedigree(x=Mrode2.1, subject="sub", ascendant=c("fat", "mot"),
                   ascendantSex=c("M", "F"), family="fam", sex="sex",
                   generation="gen", dtBirth="dtB")

  (A <- relationshipAdditive(x2.1))
  fractions(A)
  solve(A)
  inverseAdditive(x2.1)
  relationshipAdditive(x2.1[3:6, ])
  
  ## Compare the speed
  ped <- generatePedigree(nId=10, nGeneration=3, nFather=1, nMother=2)
  system.time(solve(relationshipAdditive(ped)))
  system.time(inverseAdditive(ped))
}

\keyword{array}
\keyword{misc}

%--------------------------------------------------------------------------
% relationshipAdditive.Rd ends here
