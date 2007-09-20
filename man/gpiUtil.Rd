% gpiUtil.Rd
%--------------------------------------------------------------------------
% What: Utility functions for gpi()
% $Id$
% Time-stamp: <2007-09-13 03:23:16 ggorjan>
%--------------------------------------------------------------------------

\name{hwp}
\alias{hwp}
\alias{gpLong2Wide}

\title{Utility functions for gpi()}

\description{

\code{gpLong2Wide} changes data.frame with genotype probabilities in
long form (one genotype per row) to wide form (one individual per row)
for use in \code{\link{gpi}}.

\code{hwp} calculates genotype probabilities according to Hardy-Weinberg
law for use in \code{\link{gpi}}.

}

\usage{
  gpLong2Wide(x, id, genotype, prob, trim=TRUE)
  hwp(x, trim=TRUE)
}

\arguments{
  \item{x}{data.frame for \code{gpLong2Wide},
    \code{\link[genetics]{genotype}} for \code{hwp}}
  \item{id}{character, column name in \code{x} holding individual
    identifications}
  \item{genotype}{character, column name in \code{x} holding genotypes}
  \item{prob}{character, column name in \code{x} holding genotype
    probabilities}
  \item{trim}{logical, remove last column (for \code{gpLong2Wide}) or
    value (for \code{hwp}) of a result}
}

\details{

Hardy-Weinberg probabilities for a gene with two alleles A and B, with
probabilities \eqn{Pr(A)} and \eqn{Pr(B)} are:
\item \eqn{Pr(AA) = Pr(A)^2}
\item \eqn{Pr(AB) = 2 * Pr(A) * Pr(A)}
\item \eqn{Pr(BB) = Pr(B)^2}

}

\value{

\code{gpLong2Wide} returns a matrix with number of rows equal to number
of individuals and number of columns equal to number of possible
genotypes.

\code{hwp} returns a vector with Hardy-Weinberg genotype probabilities.

}

\author{Gregor Gorjanc}

\seealso{
  \code{\link{gpi}},
  \code{\link[genetics]{genotype}},
  \code{\link[genetics]{expectedGenotypes}}
}

\examples{

  if(require(genetics)) {
    gen <- genotype(c("A/A", "A/B"))
    hwp(x=gen)
    hwp(x=gen, trim=FALSE)
  }
}

\keyword{misc}

%--------------------------------------------------------------------------
% gpi.Rd ends here
