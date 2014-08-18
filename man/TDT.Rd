% geneFlowT.Rd
%--------------------------------------------------------------------------
% What: Create gene and gamete flow matrices
% $Id: TDT.Rd 1168 2007-04-03 14:03:43Z ggorjan $
% Time-stamp: <2007-04-01 23:11:34 ggorjan>
%--------------------------------------------------------------------------

\name{geneFlowT}

\alias{geneFlowT}
\alias{geneFlowTinv}
\alias{gameteFlowM}
\alias{mendelianSamplingD}

\concept{relationship}
\concept{relatedness}
\concept{genetic covariance}

\title{Gene and gamete flow matrices}

\description{

\code{geneFlowT} and \code{geneFlowTinv} creates gene flow matrix (T)
and its inverse (Tinv), while \code{gameteFlowM} creates gamete flow
matrix (M). \code{mendelianSamplingD} creates a mendelian sampling
covariance matrix (D).

}

\usage{

geneFlowT(x, sort=TRUE, names=TRUE, \ldots)
geneFlowTinv(x, sort=TRUE, names=TRUE, \ldots)
gameteFlowM(x, sort=TRUE, names=TRUE, \ldots)
mendelianSamplingD(x, matrix=TRUE, names=TRUE, \ldots)

}

\arguments{
  \item{x}{Pedigree}
  \item{sort}{logical, for the computation the pedigree needs to be
    sorted, but results are sorted back to original sorting (sort=TRUE)
    or not (sort=FALSE)}
  \item{names}{logical, should returned matrix have row/colnames; this
    can be used to get leaner matrix}
  \item{matrix}{logical, should returned value be a diagonal matrix or a
    vector}
  \item{\ldots}{arguments for other methods}
}

\details{

\code{geneFlowT} returns a matrix with coefficients that show the flow
of genes from one generation to the next one etc. \code{geneFlowTinv} is
simply the inverse of \code{geneFlowT}, but calculated as \eqn{I - M},
where \eqn{M} is gamete flow matrix with coefficients that represent
parent gamete contribution to their offspring. \code{mendelianSamplingD}
is another matrix (\eqn{D}) for construction of relationship additive
matrix via decomposition i.e. \eqn{A=TDT'} (Henderson, 1976). Mrode
(2005) has a very nice introduction to these concepts.

Take care with \code{sort=FALSE, names=FALSE}. It is your own
responsibility to assure proper handling in this case.

}

\value{Matrices of \eqn{n * n} dimension, with coeficients as described in the
details, where \eqn{n} is number of subjects in \code{x}}

\references{

Henderson, C. R. (1976) A simple method for computing the inverse of a
numerator relationship matrix used in prediction of breeding
values. \emph{Biometrics} \bold{32}(1):69-83

Mrode, R. A. (2005) Linear models for the prediction of animal breeding
values. 2nd edition. CAB International. ISBN 0-85199-000-2
\url{http://www.amazon.com/gp/product/0851990002}

}

\author{Gregor Gorjanc}

\seealso{\code{\link{Pedigree}}, \code{\link{relationshipAdditive}},
\code{\link{kinship}} and \code{\link{inbreeding}}}

\examples{
if(require(gdata))
  data(Mrode2.1)
  Mrode2.1$dtB <- as.Date(Mrode2.1$dtB)
  x2.1 <- Pedigree(x=Mrode2.1, subject="sub", ascendant=c("fat", "mot"),
                   ascendantSex=c("M", "F"), family="fam", sex="sex",
                   generation="gen", dtBirth="dtB")

  fractions(geneFlowT(x2.1))
  fractions(geneFlowTinv(x2.1))
  fractions(gameteFlowM(x2.1))
  mendelianSamplingD(x2.1)
}
}

\keyword{array}
\keyword{misc}

%--------------------------------------------------------------------------
% geneFlowT.Rd ends here
