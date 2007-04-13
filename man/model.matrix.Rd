% model.matrix.Rd
%--------------------------------------------------------------------------
% What: Model matrix for individuals with and without records
% $Id: model.matrix.Rd 1163 2007-04-03 13:58:58Z ggorjan $
% Time-stamp: <2007-04-03 01:42:24 ggorjan>
%--------------------------------------------------------------------------

\name{model.matrix.Pedigree}

\alias{model.matrix.Pedigree}

\title{Model matrix for individuals with and without records}

\description{

\code{model.matrix} for pedigree creates design matrix (\eqn{Z}) for
individuals with and without records. Used mainly for educational
purposes.

}

\usage{

\method{model.matrix}{Pedigree}(object, y, id, left=TRUE, names=TRUE,
                                \ldots)
}

\arguments{
  \item{object}{Pedigree}
  \item{names}{logical, should returned matrix have row/colnames; this
    can be used to get leaner matrix}
  \item{y}{numeric, vector of (phenotypic) records}
  \item{id}{vector of subjects for \code{y}}
  \item{left}{logical, bind columns of individuals without records to
    left (\code{left=TRUE}) or right (\code{left=FALSE}) side of
    \eqn{Z}}
  \item{\ldots}{arguments passed to \code{\link{model.matrix}}}
}

\value{A model matrix of \eqn{n * q} dimension, where \eqn{n} is number
  of records in \code{y} and \eqn{q} is number of subjects in the
  pedigree}

\author{Gregor Gorjanc}

\seealso{\code{\link{Pedigree}}, \code{\link{relationshipAdditive}},
\code{\link{inverseAdditive}} and \code{\link{model.matrix}}}

\examples{
  data(Mrode3.1)
  (x <- Pedigree(x=Mrode3.1, subject="calf", ascendant=c("sire", "dam"),
                 ascendantSex=c("Male", "Female"), sex="sex"))
  model.matrix(object=x, y=x$pwg, id=x$calf)
}

\keyword{models}

%--------------------------------------------------------------------------
% model.matrix.Rd ends here
