% gpi.Rd
%--------------------------------------------------------------------------
% What: Genotype probability index
% $Id: gpi.Rd 1175 2007-04-03 14:09:20Z ggorjan $
% Time-stamp: <2007-03-31 19:12:05 ggorjan>
%--------------------------------------------------------------------------

\name{gpi}

\alias{gpi}

\concept{segregation}
\concept{genotype probability}
\concept{information}

\title{Genotype probability index}

\description{

\code{gpi} calculates Genotype Probability Index (GPI), which indicates
the information content of genotype probabilities derived from
segregation analysis.

}

\usage{
  gpi(gp, hwp)
}

\arguments{
  \item{gp}{numeric vector or matrix, individual genotype probabilities}
  \item{hwp}{numeric vector or matrix, Hard-Weinberg genotype
    probabilities}
}

\details{

Genotype Probability Index (GPI; Kinghorn, 1997; Percy and Kinghorn,
2005) indicates information that is contained in multi-allele genotype
probabilities for diploids derived from segregation analysis, say
Thallman et. al (2001a, 2001b). GPI can be used as one of the criteria
to help identify which ungenotyped individuals or loci should be
genotyped in order to maximise the benefit of genotyping in the
population (e.g. Kinghorn, 1999).

\code{gp} and \code{hwp} arguments accept genotype probabilities for
multi-allele loci. If there are two alleles (1 and 2), you should pass
vector of probabilities for genotypes (11 and 12) i.e. one value for
heterozygotes (12 and 21) and always skipping last homozygote. With
three alleles this vector should hold probabilities for genotypes (11,
12, 13, 22, 23) as also shown bellow and in examples.

\preformatted{
     2 alleles: 1 and 2
     11 12
     --> no. dimensions = 2

     3 alleles: 1, 2, and 3
     11 12 13
        22 23
     --> no. dimensions = 5

     ...

     5 alleles: 1, 2, 3, 4, and 5
     11 12 13 14 15
        22 23 24 25
           33 34 35
              44 45
     --> no. dimensions = 14
}

In general, number of dimensions (\eqn{k}) for \eqn{n} alleles is equal
to:

\deqn{k = (n * (n + 1) / 2) - 1.}

If you have genotype probabilities for more than one individual, you can
pass them to \code{gp} in a matrix form, where each row represents
genotype probabilities of an individual. In case of passing matrix to
\code{gp}, \code{hwp} can still accept a vector of Hardy-Weinberg
genotype probabilities, which will be used for all individuals due to
recycling. If \code{hwp} also gets a matrix, then it must be of the same
dimension as that one passed to \code{gp}.

}

\value{Vector of \eqn{N} genotype probability indices, where \eqn{N} is
  number of individuals}

\references{

Kinghorn, B. P. (1997) An index of information content for genotype
probabilities derived from segregation analysis. \emph{Genetics}
\bold{145}(2):479-483
\url{http://www.genetics.org/cgi/content/abstract/145/2/479}

Kinghorn, B. P. (1999) Use of segregation analysis to reduce genotyping
costs. \emph{Journal of Animal Breeding and Genetics}
\bold{116}(3):175-180
\url{http://dx.doi.org/10.1046/j.1439-0388.1999.00192.x}

Percy, A. and Kinghorn, B. P. (2005) A genotype probability index for
multiple alleles and haplotypes. \emph{Journal of Animal Breeding and
Genetics} \bold{122}(6):387-392
\url{http://dx.doi.org/10.1111/j.1439-0388.2005.00553.x}

Thallman, R. M. and Bennet, G. L. and Keele, J. W. and Kappes,
S. M. (2001a) Efficient computation of genotype probabilities for loci
with many alleles: I. Allelic peeling. \emph{Journal of Animal
Science} \bold{79}(1):26-33
\url{http://jas.fass.org/cgi/reprint/79/1/34}

Thallman, R. M. and Bennet, G. L. and Keele, J. W. and Kappes,
S. M. (2001b) Efficient computation of genotype probabilities for loci
with many alleles: II. Iterative method for large, complex
pedigrees. \emph{Journal of Animal Science}
\bold{79}(1):34-44
\url{http://jas.fass.org/cgi/reprint/79/1/34}

}

\author{Gregor Gorjanc R code, documentation, wrapping into a package;
  Andrew Percy and Brian P. Kinghorn Fortran code}

\seealso{\code{\link{Pedigree}}}

\examples{
  ## --- Example 1 from Percy and Kinghorn (2005) ---
  ## No. alleles: 2
  ## No. individuals: 1
  ## Individual genotype probabilities:
  ##  * p(11, 12, 22) = (.1, .5, .4)
  ## Hardy-Weinberg probabilities:
  ##  *   p(1, 2) = (.75, .25)
  ##  * p(11, 12,   (.75^2, 2*.75*.25,
  ##          22) =             .25^2)
  ##              = (.5625, .3750,
  ##                        .0625)

  gp <- c(.1, .5)
  hwp <- c(.5625, .3750)
  gpi(gp=gp, hwp=hwp)

  ## --- Example 1 from Percy and Kinghorn (2005) extended ---
  ## No. alleles: 2
  ## No. individuals: 2
  ## Individual genotype probabilities:
  ##  * p_1(11, 12, 22) = (.1, .5, .4)
  ##  * p_2(11, 12, 22) = (.2, .5, .3)

  (gp <- matrix(c(.1, .5, .2, .5), nrow=2, ncol=2, byrow=TRUE))
  gpi(gp=gp, hwp=hwp)

  ## --- Example 2 from Percy and Kinghorn (2005) ---
  ## No. alleles: 3
  ## No. individuals: 1
  ## Individual genotype probabilities:
  ##  * p(11, 12, 13,   (.1, .5, .0,
  ##          22, 23  =      .4, .0,
  ##              33)            .0)
  ## Hardy-Weinberg probabilities:
  ##  *    p(1, 2, 3) = (.75, .25, .0)
  ##  * p(11, 12, 13,   (.75^2, 2*.75*.25, .0,
  ##          22, 23, =            0.25^2, .0,
  ##              33)                      .0)
  ##                  = (.5625, .3750, .0
  ##                            .0625, .0,
  ##                                   .0)

  gp <- c(.1, .5, .0, .4, .0)
  hwp <- c(.5625, .3750, .0, .0625, .0)
  gpi(gp=gp, hwp=hwp)

  ## --- Example 3 from Percy and Kinghorn (2005) ---
  ## No. alleles: 5
  ## No. individuals: 1
  ## Hardy-Weinberg probabilities:
  ##  * p(1, 2, 3, 4, 5) = (.2, .2, .2, .2, .2)
  ##  * p(11, 12, 13, ...) = (p(1)^2, 2p(1)p(2), 2p(1)p(3), ...)
  ## Individual genotype probabilities:
  ##  * p(11, 12, 13, ...) = gp / 2
  ##    p(12) = p(12) + .5

  (hwp <- rep(.2, times=5) \%*\% t(rep(.2, times=5)))
  hwp <- c(hwp[upper.tri(hwp, diag=TRUE)])
  (hwp <- hwp[1:(length(hwp) - 1)])
  gp <- hwp / 2
  gp[2] <- gp[2] + .5
  gp

  gpi(gp=gp, hwp=hwp)

  ## --- Simulate gp for n alleles and i subjects ---
  n <- 3
  i <- 10

  kAll <- (n*(n+1)/2) # without -1 here!
  k <- kAll - 1
  if(require("gtools")) {
    gp <- rdirichlet(n=i, alpha=rep(x=1, times=kAll))[, 1:k]
    hwp <- as.vector(rdirichlet(n=1, alpha=rep(x=1, times=kAll)))[1:k]
    gpi(gp=gp, hwp=hwp)
  }
}

\keyword{misc}

%--------------------------------------------------------------------------
% gpi.Rd ends here
