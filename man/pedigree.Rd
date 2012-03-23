% pedigree.Rd
%--------------------------------------------------------------------------
% What: Pedigree class and methods man page
% $Id: pedigree.Rd 1175 2007-04-03 14:09:20Z ggorjan $
% Time-stamp: <2007-04-01 23:06:57 ggorjan>
%--------------------------------------------------------------------------

\name{Pedigree}

\alias{Pedigree}

\alias{is.Pedigree}

\alias{as.Pedigree}
\alias{as.Pedigree.Pedigree}
\alias{as.Pedigree.data.frame}
\alias{as.Pedigree.matrix}

% \alias{"[.Pedigree"}

\alias{as.integer.Pedigree}
\alias{as.factor.Pedigree}
\alias{as.character.Pedigree}

\concept{genealogy}

\title{Pedigree}

\description{\code{Pedigree} function creates a pedigree object}

\usage{

Pedigree(x, subject="id", ascendant=c("father", "mother"),
  ascendantSex=c(1, 2), ascendantLevel=c(1, 1), unknown=NA, sex=NA,
  dtBirth=NA, generation=NA, family=NA, generationOrder="increasing",
  check=TRUE, sort=FALSE, extend=FALSE, drop=TRUE, codes=FALSE)

}

\arguments{
  \item{x}{data.frame or matrix}
  \item{subject}{character, column name in \code{x} for subjects}
  \item{ascendant}{character, column name(s) in \code{x} for ascendants}
  \item{family}{character, column name in \code{x} for family}
  \item{ascendantSex}{integer orcharacter, sex of ascendant(s); see details}
  \item{ascendantLevel}{integer, generation level of ascendant(s); see
    details}
  \item{unknown}{vector or list, uknown representation of identification
    and other data in the pedigree; see details}
  \item{sex}{character, column name in \code{x} for sex}
  \item{dtBirth}{character, column name in \code{x} for date of birth}
  \item{generation}{character, column name in \code{x} for generation}
  \item{generationOrder}{character, generation value is "increasing" or
    "decreasing" through generations; see details}
  \item{check}{logical, check for common errors}
  \item{sort}{logical, sort pedigree}
  \item{extend}{logical, extend pedigree}
  \item{drop}{logical, drop unused levels if factors are used}
  \item{codes}{logical, code individuals into integers}
}

\details{

FIXME: study geneSet class

Pedigree can be one source of information on genetic relationship
between relatives. Take for example the following pedigree:

  \preformatted{
  paternal    paternal     maternal    maternal
  grandfather grandmother  grandfather grandmother
      |           |            |           |
      -------------            -------------
            |                        |
          father                   mother
            |                        |
            -------------------------
                        |
                     subject
                   }

  This information can be stored in a data.frame as

  \tabular{lll}{
    mother  \tab maternal grandfather \tab maternal grandmother \cr
    father  \tab paternal grandfather \tab paternal grandmother \cr
    subject \tab father               \tab mother
  }

There is considerable variability in terminology between as well as
within various fields of genetics. We use the following terms throughout
the help and code: individual (any individual in a pedigree), subject
(individual whose pedigree is given i.e. individuals in the first column
in upper data.frame), ascendant and descendant. Additionally, family,
sex, dtBirth and generation are used for additional data in the
pedigree. Their meaning should be clear. For these, argument \code{col}
is usually used in function calls.

\code{\link{family}} TODO

\code{ascendantSex} can be used to define sex of ascendant(s); for
example c("Male", "Female") or c("M", "F") or even c(1, 2) for father
and mother or c(2, 1, 1) for mother and two possible fathers or c(1, 1)
for father and maternal father etc. This data is needed only for the
structure of the class and defaults should be ok for the majority. But
you need to make sure that data defined here must be in accordance with
values in \code{sex} column.

\code{ascendantLevel} can be used to define generation level of
ascendant(s) in relation to a subject; for example c(1, 1) for father
and mother or c(1, 1, 1) for mother and two possible fathers or c(1, 2)
for father and maternal father etc. This data is needed only for the
structure of the class and defaults should be ok for the majority.

There is no need for \code{as.integer} TODO in arguments
\code{ascendantLevel} as this is done internally.

unknown TODO

% FIXME: check unknown here and in unknown.R
% When pedigree object is created, argument \code{unknown} can be used to
% specify which values in the source data i.e. pedigree represent unknown
% individuals as well as for family, sex, dtBirth and generation. By
% default \code{NA} is used. Argument \code{unknown} can be either vector
% (single value such as \code{0} or true vector such as c(0, 999,
% 1000000)) or a list. If it is not a list, it is assumed that
% \code{unknown} holds for all specified columns above. List can be used
% to specify different unknown values for each column such as
% \code{list(id=c("-", 0) family="no-name", sex=0, dtBirth=TODO,
% genereation=-1)}. If only some columns are specified as in
% \code{list(id="NA", sex=0)}, it is assumed that \code{NA} will suffice
% for the rest. All unknown values are changed to NA via
% \code{\link{unknownToNA.Pedigree}}. If this is not wanted,
% \code{\link{NAToUnknown.Pedigree}} can be used for reverse effect.

Sex TODO

Date of birth TODO

\code{generationOrder} defines in which order are
\code{\link{generation}} values: "increasing" if values increase from
ascendants to descendants and "decreasing" if values decrease from
ascendants to descendants.

\code{\link{check}}, \code{\link{sort}}, \code{\link{extend}}, and
\code{\link{codes}} are actions on the pedigree and have their own help
pages.

Individuals can be stored as either integer, numeric or factor TODO. In
any case all id columns must have the same class and this is
automatically checked. Argument \code{drop} can be used to drop unused
levels, if factors are used.

\code{as.Pedigree.*} FIXME
\code{as.*.Pedigree} FIXME

Object of Pedigree class is a data.frame with columns that can be
divided into core columns (subject, ascendant(s), sex, dtBirth and
generationTODO) and possibly other columns such as data on phenotype and
genotype and other subject attributes, for example factors and
covariates TODO. 

Additionally, the following attributes are set on pedigree:

\itemize{
  \item{.subject}{character, column name of subject identification in
    pedigree}
  \item{.ascendant}{character, column name(s) of ascendant(s)
    identification in pedigree}
  \item{.family}{character, column name of family identification in
    pedigree}
  \item{.ascendantSex}{integer, sex of ascendant(s)}
  \item{.ascendantLevel}{integer, generation level of ascendant(s)}
  \item{.sex}{character, column name of subject's sex}
  \item{.dtBirth}{character, column name of subject's date of birth}
  \item{.generation}{character, column name of subject's generation}
  \item{.generationOrder}{character, generation value is "increasing" or
    "decreasing" through generations}
  \item{.colClass}{character, storage class for id columns: "integer",
    "numeric" or "factor"}
  \item{.checked}{logical, is pedigree checked for common errors}
  \item{.sorted}{logical, is pedigree sorted; by TODO}
  \item{.extended}{logical, is pedigree extended}
  \item{.coded}{logical, is pedigree coded}
  \item{.unknown}{list, uknown representation for individual
    identification and other data in the pedigree; names of the list are
    c(".id", ".family", ".sex", ".dtBirth", ".generation")}
}

}

\value{Pedigree object as described in the details}

\author{Gregor Gorjanc}

\seealso{

% \code{\link{NAToUnknown.Pedigree}}

\code{\link{check}},
\code{\link{sort}}, and \code{\link{extend}} provide help on pedigree
utility functions. 

}

\examples{

data(Mrode2.1)
Mrode2.1$dtB <- as.Date(Mrode2.1$dtB)
x2.1 <- Pedigree(x=Mrode2.1, subject="sub", ascendant=c("fat", "mot"),
                 ascendantSex=c("M", "F"), family="fam", sex="sex",
                 generation="gen", dtBirth="dtB")

%   ## --- unknown usage ---
%   ped <- generatePedigree(2)
%   ped[2, 1] <- 11111
%   ped[3, 2] <- 999
%   ped[1, 3] <- 0
%   Pedigree(ped, unknown=c(0, 999))
%   Pedigree(ped, unknown=c(0, 999, 11111))
%   ## FIXME: we get recycling for unknown here, so this does not work -->
%   ##        how to fix this? --> also FIXME note in unknown.R

if (FALSE) {
  ## How to handle different pedigree types
  ## * multiple parents
  ped2 <- ped
  ped2$father1 <- ped$father
  ped2$father2 <- ped$father
  ped2$father <- NULL
  ped2 <- as.data.frame(ped2)
  str(Pedigree(ped2, ascendant=c("father1", "father2", "mother"),
               ascendantSex=c(1, 1, 2), ascendantLevel=c(1, 1, 1)))

  ## * different level of parents
  ped3 <- as.data.frame(ped)
  ped3$m.grandfather <- ped3$mother
  ped3$mother <- NULL
  str(Pedigree(ped3, ascendant=c("father", "m.grandfather"),
               ascendantSex=c(1, 1), ascendantLevel=c(1, 2)))
}

}

\keyword{manip}
\keyword{misc}

%--------------------------------------------------------------------------
% pedigree.Rd ends here
