% inbreeding.Rd
%--------------------------------------------------------------------------
% What: Inbreeding coefficient (F) man page
% $Id: inbreeding.Rd 1172 2007-04-03 14:05:59Z ggorjan $
% Time-stamp: <2007-04-01 23:11:29 ggorjan>
%--------------------------------------------------------------------------

\name{inbreeding}

\alias{inbreeding}

\concept{consanguinity}

\title{Inbreeding coefficient}

\description{\code{inbreeding} calculates inbreeding coefficients of
  individuals in the pedigree}

\usage{inbreeding(x, method="meuwissen", sort=TRUE, names=TRUE, \ldots)}

\arguments{
  \item{x}{pedigree object}
  \item{method}{character, method of calculation "tabular",
    "meuwissen" or "sargolzaei", see details}
  \item{sort}{logical, for the computation the pedigree needs to be
    sorted, but results are sorted back to original sorting (sort=TRUE)
    or not (sort=FALSE)}
  \item{names}{logical, should returned vector have names; this
    can be used to get leaner returned object}
  \item{\ldots}{arguments for other methods}
}

\details{

Coefficient of inbreeding (\eqn{F}) represents probability that two
alleles on a loci are identical by descent (Wright, 1922; Falconer and
Mackay, 1996). Wright (1922) showed how \eqn{F} can be calculated but
his method of paths is not easy to wrap in a program. Calculation of
\eqn{F} can also be performed using tabular method for setting the
additive relationship matrix (Henderson, 1976), where \eqn{F_i = A_{ii}
- 1}. Meuwissen and Luo (1992) and VanRaden (1992) developed faster
algorithms for \eqn{F} calculation. Wiggans et al. (1995) additionally
explains method in VanRaden (1992). Sargolzaei et al. (2005) presented
yet another fast method.

Take care with \code{sort=FALSE, names=FALSE}. It is your own
responsibility to assure proper handling in this case.

}

\references{

Falconer, D. S. and Mackay, T. F. C. (1996) Introduction to Quantitative
Genetics. 4th edition. Longman, Essex, U.K.
\url{http://www.amazon.com/gp/product/0582243025}

Henderson, C. R. (1976) A simple method for computing the inverse of a
numerator relationship matrix used in prediction of breeding
values. \emph{Biometrics} \bold{32}(1):69-83

Meuwissen, T. H. E. and Luo, Z. (1992) Computing inbreeding coefficients
in large populations. \emph{Genetics Selection and Evolution}
\bold{24}:305-313

Sargolzaei, M. and Iwaisaki, H. and Colleau, J.-J. (2005) A fast
algorithm for computing inbreeding coefficients in large
populations. \emph{Journal of Animal Breeding and Genetics}
\bold{122}(5):325--331
\url{http://dx.doi.org/10.1111/j.1439-0388.2005.00538.x}

VanRaden, P. M. (1992) Accounting for inbreeding and crossbreeding in
genetic evaluation for large populations. \emph{Journal of Dairy
Science} 75(11):3136-3144
\url{http://jds.fass.org/cgi/content/abstract/75/11/3136}

Wiggans, G. R. and VanRaden, P. M. and Zuurbier, J. (1995) Calculation
and use of inbreeding coefficients for genetic evaluation of United
States dairy cattle. \emph{Journal of Dairy Science}
\bold{78}(7):1584-1590
\url{http://jds.fass.org/cgi/content/abstract/75/11/3136}

Wright, S. (1922) Coefficients of inbreeding and relationship.
\emph{American Naturalist} 56:330-338

}

\value{A vector of length \eqn{n} with inbreeding coefficients, where
\eqn{n} is number of subjects in \code{x}}

\author{Gregor Gorjanc and Dave A. Henderson}

\seealso{\code{\link{Pedigree}}, \code{\link{relationshipAdditive}},
  \code{\link{kinship}} and \code{\link{geneFlowT}}}

\examples{
  data(Mrode2.1)
  Mrode2.1$dtB <- as.Date(Mrode2.1$dtB)
  x2.1 <- Pedigree(x=Mrode2.1, subject="sub", ascendant=c("fat", "mot"),
                   ascendantSex=c("M", "F"), family="fam", sex="sex",
                   generation="gen", dtBirth="dtB")
  fractions(inbreeding(x=x2.1))

  ## Compare the speed
  ped <- generatePedigree(nId=25)
  system.time(inbreeding(x=ped))
  # system.time(inbreeding(x=ped, method="sargolzaei")) # not yet implemented
  system.time(inbreeding(x=ped, method="tabular"))
}

\keyword{array}
\keyword{misc}

%--------------------------------------------------------------------------
% inbreeding.Rd ends here
