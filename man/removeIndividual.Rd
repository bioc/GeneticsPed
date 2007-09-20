% removeIndividual.Rd
%--------------------------------------------------------------------------
% What: Remove individual from pedigree
% $Id: removeIndividual.Rd 1092 2006-11-12 12:28:38Z ggorjan $
% Time-stamp: <2007-04-19 02:36:13 ggorjan>
%--------------------------------------------------------------------------

\name{removeIndividual}

\alias{removeIndividual}

\title{Remove individual from pedigree}

\description{

\code{removeIndividual} provides utility for removing individuals from a
pedigree.

}

\usage{removeIndividual(x, individual, remove="all")}

\arguments{
  \item{x}{pedigree}
  \item{individual}{vector of individuals}
  \item{remove}{character, column names of id columns and/or "all", see
    details}
}

\details{

Individuals passed to argument \code{individual} will be removed from
the pedigree. If there is a pedigree with individual "id" and two
ascendants, say "father" and "mother", then one can pass any combination
of these three id columns or "all" for all of them in short to argument
\code{remove}. In case only "id" is passed to \code{remove}, individuals
will be removed from the pedigree, but not from ascendant id columns, which
might be a matter of interest only if specified individuals show up as
ascendants for some other individuals. In case you want to remove an
individual completely from the pedigree "all" must be used.

Individuals in id column are removed via removal of the whole record
from the pedigree. Individuals in ascendant id columns are only replaced by
\code{attr(x, "unknown")}.

If founder is removed, attribute extended status is changed to FALSE.

}

\author{Gregor Gorjanc}

\seealso{\code{\link{Pedigree}}}

\examples{
  ped <- generatePedigree(3)
  summary(ped)
  removeIndividual(ped, individual=c(1, 3, 4), remove="father")
  removeIndividual(ped, individual=c(1, 3, 4), remove=c("mother", "father"))
  (ped <- removeIndividual(ped, individual=c(1, 3, 4), remove=c("all")))
  summary(ped)
}

\keyword{misc}

%--------------------------------------------------------------------------
% removeIndividual.Rd ends here
