% prune.Rd
%--------------------------------------------------------------------------
% What: Prune pedigree - help
% $Id$
% Time-stamp: <2007-09-07 00:36:24 ggorjan>
%--------------------------------------------------------------------------

\name{prune}

\alias{prune}

\concept{trim}
\concept{prune}

\title{Prune pedigree}

\description{

\code{prune} removes noninformative individuals from a
pedigree. This process is usually called trimming or
pruning. Individuals are removed if they do not provide any ancestral
ties between other individuals. It is possible to add some additional
criteria. See details.

}

\usage{

prune(x, id, father, mother, unknown=NA, testAdd=NULL, verbose=FALSE)

}

\arguments{
  \item{x}{data.frame, pedigree data}
  \item{id}{character, individuals's identification column name}
  \item{father}{character, father's identification column name}
  \item{mother}{character, mother's identification column name}
  \item{unknown}{value(s) used for representing unknown parent in
    \code{x}}
  \item{testAdd}{logical, additional criteria; see details}
  \item{verbose}{logical, print some more info}
}

\details{

NOTE: this function does not yet work with Pedigree class.

There are always some individuals in the pedigree that jut out. Usually
this are older individuals without known ancestors, founders. If such
individuals have only one (first) descendant and no phenotype/genotype
data, then they do not give us any additional information and can be
safely removed from the pedigree. This process resembles cutting/pruning
the branches of a tree.

By default \code{prune} iteratively removes individuals from the
pedigree (from top to bottom) if:
\itemize{
  \item they are founders, have both ancestors i.e. father and mother
    unknown and
  \item have only one or no (first) descendants i.e. children
}

If there is a need to take into account availability of say
phenotype/genotype data or any other information, argument
\code{testAdd} can be used. Value of this argument must be logical and
with length equal to number of rows in the pedigree. The easiest way to
achieve this is to \code{\link{merge}} any data to the pedigree and then
to perform a test, which will return logical values. Note that value of
\code{TRUE} in \code{testAdd} means to remove an individual - this
function is removing individuals! To keep an individual without known
parents and one or no children, value of \code{testAdd} must be
\code{FALSE} for that particular individual. Take a look at the
examples.

There are various conventions on representing unknown/missing ancestors,
say 0. \R's default is to use \code{NA}. If other values than \code{NA}
are present, argument \code{unknown} can be used to convert
unknown/missing values to \code{NA}.

It is assumed that pedigree is in extended form i.e. that each father
and mother has each own record as an individual. Otherwise error is
returned with information on which parents do not appear as
individuals.

\code{prune} does not only remove lines for pruned individuals
but also removes them from \code{father} and \code{mother} columns.

Pruning is done from top to bottom of the pedigree i.e. from
oldest individuals towards younger ones. Take for example the following
part of the pedigree in example section:

\preformatted{
     0   7
     |   |
     -----
       |
  10   8
   |   |
   -----
     |
     9
}

Individual 7 is not removed since it has two (first) descendants i.e. 8
and 5 (not shown here). Consecutively, individuals 8 and 9 are also not
removed from the pedigree. Individual 10 is removed, since it has only
one descendant. Why should individuals 8 and 9 and therefore also 7 stay
in the pedigree? Current behaviour is reasonable if pedigree is built in
such a way that first individuals with some phenotype or genotype data
are gathered and then their pedigree is being built. Say, individual 9
has pehnotype/genotype data and its pedigree is build and there is
therefore no need to remove such an individual. However, if pedigree is
not built in such a way, then \code{prunPedigree} function can not prune
all noninformative individuals. Argument \code{testAdd} can not help
with this issue, since basic tests (founder and one or no first
descendants) and \code{testAdd} are combined with
\code{\link[base:Logic]{&}}.

}

\value{

\code{prune} returns a data.frame with possibly fewer
individuals. Read also the details.

}

\seealso{
  \code{\link{Pedigree}}
}

\author{Gregor Gorjanc}

\examples{

  ## Pedigree example
  x <- data.frame(oseba=c(1,  9, 11, 2, 3, 10, 8, 12, 13,  4, 5, 6, 7, 14, 15, 16, 17),
                    oce=c(2, 10, 12, 5, 5,  0, 7,  0,  0,  0, 7, 0, 0,  0,  0,  0,  0),
                   mama=c(3,  8, 13, 0, 4,  0, 0,  0,  0, 14, 6, 0, 0, 15, 16, 17,  0),
                   spol=c(2,  2,  2, 1, 2,  1, 2,  1,  2,  2, 1, 2, 1,  1,  1,  1,  1),
             generacija=c(1,  1,  1, 2, 2,  2, 2,  2,  2,  3, 3, 4, 4,  5,  6,  7,  8),
                   last=c(2, NA,  8, 4, 1,  6,NA, NA, NA, NA,NA,NA,NA, NA, NA, NA, NA))

  ## Default case
  prune(x=x, id="oseba", father="oce", mother="mama", unknown=0)

  ## Use of additional test i.e. do not remove individual if it has
  ## known value for "last"
  prune(x=x, id="oseba", father="oce", mother="mama", unknown=0,
                testAdd=is.na(x$last))

  ## Use of other data
  y <- data.frame(oseba=c( 11,  15, 16),
                  last2=c(8.5, 7.5, NA))

  x <- merge(x=x, y=y, all.x=TRUE)
  prune(x=x, id="oseba", father="oce", mother="mama", unknown=0,
                testAdd=is.na(x$last2))

}

\keyword{manip}

%--------------------------------------------------------------------------
% prune.Rd ends here
