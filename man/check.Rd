% check.Rd
%--------------------------------------------------------------------------
% What: Check Pedigree attributes
% $Id: check.Rd 1092 2006-11-12 12:28:38Z ggorjan $
% Time-stamp: <2007-09-20 13:21:45 ggorjan>
%--------------------------------------------------------------------------

\name{check}

\alias{check}
\alias{check.Pedigree}
\alias{checkId}

\title{Check consistency of data in pedigree}

\description{\code{check} performs a series of checks on
  pedigree object to ensure consistency of data.
}

\usage{
check(x, \ldots)
checkId(x)
}

\arguments{
  \item{x}{pedigree, object to be checked}
  \item{\ldots}{arguments to other methods, none for now}
}

\details{

  \code{checkId} performs various checks on individuals and
  their ascendants. These checks are:
  \itemize{
    \item idClass: all ids must have the same class
    \item idIsNA: individual can not be \code{NA}
    \item idNotUnique: individual must be unique
    \item idEqualAscendant: individual can not be equal
      to its ascendant
    \item ascendantEqualAscendant: ascendant can not be equal to another
      ascendant
    \item ascendantInAscendant: ascendant can not appear again as
      asescendant of other sex i.e. father can not be a mother to someone
      else
    \item unusedLevels: in case factors are used for id presentation,
      there might be unused levels for some ids - some functions rely on
      number of levels and a check is provided for this
  }

  \code{checkAttributes} is intended primarly for internal use and
  performs a series of checks on attribute values needed in various
  functions. It causes stop with error messages for all given attribute
  checks.
}

\value{List of more or less self-explanatory errors and "pointers" to
  these errors for ease of further work i.e. removing errors.
}

\author{Gregor Gorjanc}

\seealso{\code{\link{Pedigree}}}

\examples{
  ## EXAMPLES BELLOW ARE ONLY FOR TESTING PURPOSES AND ARE NOT INTENDED
  ## FOR USERS, BUT IT CAN NOT DO ANY HARM.

  ## --- checkAttributes ---
  tmp <- generatePedigree(5)
  attr(tmp, "sorted") <- FALSE
  attr(tmp, "coded") <- FALSE
  GeneticsPed:::checkAttributes(tmp)
  try(GeneticsPed:::checkAttributes(tmp, sorted=TRUE, coded=TRUE))

  ## --- idClass ---
  tmp <- generatePedigree(5)
  tmp$id <- factor(tmp$id)
  class(tmp$id)
  class(tmp$father)
  try(GeneticsPed:::idClass(tmp))

  ## --- idIsNA ---
  tmp <- generatePedigree(2)
  tmp[1, 1] <- NA
  GeneticsPed:::idIsNA(tmp)

  ## --- idNotUnique ---
  tmp <- generatePedigree(2)
  tmp[2, 1] <- 1
  GeneticsPed:::idNotUnique(tmp)

  ## --- idEqualAscendant ---
  tmp <- generatePedigree(2)
  tmp[3, 2] <- tmp[3, 1]
  GeneticsPed:::idEqualAscendant(tmp)

  ## --- ascendantEqualAscendant ---
  tmp <- generatePedigree(2)
  tmp[3, 2] <- tmp[3, 3]
  GeneticsPed:::ascendantEqualAscendant(tmp)

  ## --- ascendantInAscendant ---
  tmp <- generatePedigree(2)
  tmp[3, 2] <- tmp[5, 3]
  GeneticsPed:::ascendantInAscendant(tmp)
  ## Example with multiple parents
  tmp <- data.frame(id=c("A", "B", "C", "D"),
                    father1=c("E", NA, "F", "H"),
                    father2=c("F", "E", "E", "I"),
                    mother=c("G", NA, "H", "E"))
  tmp <- Pedigree(tmp, ascendant=c("father1", "father2", "mother"),
                  ascendantSex=c(1, 1, 2),
                  ascendantLevel=c(1, 1, 1))
  GeneticsPed:::ascendantInAscendant(tmp)

  ## --- unusedLevels ---
  tmp <- generatePedigree(2, colClass="factor")
  tmp[3:4, 2] <- NA
  GeneticsPed:::unusedLevels(tmp)
}

\keyword{misc}
\keyword{attribute}

%--------------------------------------------------------------------------
% check.Rd ends here
