% sort.pedigree.Rd
%--------------------------------------------------------------------------
% What: Sort pedigree
% $Id: check.Rd 825 2006-04-30 16:35:58Z ggorjan $
% Time-stamp: <2007-03-07 00:15:26 ggorjan>
%--------------------------------------------------------------------------

\name{sort.Pedigree}

\alias{sort.Pedigree}

\title{Sort pedigree}

\description{Pedigree sort}

\usage{
\method{sort}{Pedigree}(x, decreasing=FALSE, na.last=TRUE, \ldots, by="default")
}

\arguments{
  \item{x}{pedigree, object to be sorted}
  \item{decreasing}{logical, sort order}
  \item{na.last}{logical, control treatment of \code{NA}s; check
    \code{\link{order}}}
  \item{\ldots}{arguments passed to \code{order}, see details}
  \item{by}{character, sort by "default", "pedigree", "generation", or
    "dtBirth" information, see details}
}

\details{

Sorting of the pedigree can be performed in different ways. Since
pedigree can contain date of birth, sorting by this would be the most
obvious way and it would be the most detailed sort. However, there might
be the case that date of birth is not available for some or all
individuals. Therefore, this function by default (when
\code{by="default"}) tries to figure out what would be the best way to
perform the sort. If date of birth is available for all individuals then
date of birth is used for sorting. If not, generation information is
used, but only if it is known for all individuals (it should be more or
less easy to figure out the generation for all individuals in the
pedigree CHECK). Again if not, sorting is done via information in
pedigree i.e. ascendants will precede descendants or vice versa. User
can always define it's own preference by argument \code{by}. When
\code{by="dtBirth"} or \code{by="generation"} sorting is performed via
\code{\link{order}} and its arguments \code{na.last} and
\code{decreasing} can be used. With \code{by="pedigree"} argument
\code{decreasing} has an effect.

Generation values can have different meaning i.e. values might either
increase or decrease from ascendants to descendant with the same
meaning. This information is stored in attribute \code{generationOrder}
(at the time of creating the pedigree object via \code{\link{Pedigree}})
and used for determining the order of sorting if sorting is by
generation. The output of the result might therefore be opposite of what
user might expect. If that is the case, use argument \code{decreasing}
as defined in \code{\link{order}}. Look also into examples bellow.

}

\value{Sorted pedigree}

\author{Gregor Gorjanc}

\seealso{\code{\link{Pedigree}} and \code{\link{order}}}

\examples{
  ped <- generatePedigree(nId=5)
  ped <- ped[sample(1:nrow(ped)), ]
  sort(ped)
  ## sort(ped, by="dtBirth")       ## TODO
  sort(ped, by="generation")
  ## try(sort(ped, by="pedigree")) ## TODO

  ## Sorting with decreasing generation values from ascendants to descendants
  ped1 <- generatePedigree(nId=5, generationOrder="decreasing")
  sort(ped1, by="generation")
  sort(ped1, decreasing=TRUE, by="generation")
  sort(ped1, decreasing=FALSE, by="generation")

  ## Sorting with unknown values
  ped[1, "generation"] <- NA
  sort(ped, na.last=TRUE, by="generation")
  sort(ped, na.last=FALSE, by="generation")
  sort(ped, na.last=NA, by="generation")
}

\keyword{misc}

%--------------------------------------------------------------------------
% sort.pedigree.Rd ends here
