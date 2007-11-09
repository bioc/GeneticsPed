% datasets.Rd
%--------------------------------------------------------------------------
% What: Pedigree and data examples
% $Id$
% Time-stamp: <2007-04-19 02:36:38 ggorjan>
%--------------------------------------------------------------------------

\name{Mrode}

\alias{Falconer}
\alias{Falconer5.1}
\alias{Mrode}
\alias{Mrode2.1}
\alias{Mrode3.1}

\docType{data}

\title{Pedigree and data examples}

\description{Various pedigree and data examples}

\usage{
data(Falconer5.1)
data(Mrode2.1)
data(Mrode3.1)
}

\format{
  \code{Falconer5.1} is a rather complex (inbreed) pedigree example from
  book by Falconer and Mackay (1996) - page 84 with 18 individuals and
  following columns:

  \describe{
    \item{sub}{individual}
    \item{fat}{father}
    \item{mot}{mother}
  }

  \code{Mrode2.1} is an extended pedigree example from book by
  Mrode (2005) - page 27 with 6 individuals and following columns:

  \describe{
    \item{sub}{individual}
    \item{fat}{father}
    \item{mot}{mother}
    \item{fam}{family}
    \item{sex}{sex}
    \item{gen}{generation}
    \item{dtB}{date of birth}
  }

  \code{Mrode3.1} is a pedigree and data example from book by
  Mrode (2005) - page 43: it shows a beef breeding scenario with 8
  individuals (animals), where 5 of them have phenotypic records
  (pre-weaning gain) and 3 three of them are without records and link
  others through the pedigree:

  \describe{
    \item{calf}{calf}
    \item{sex}{sex of a calf}
    \item{sire}{father of a calf}
    \item{dam}{mother of a calf}
    \item{pwg}{pre-weaning gain of a calf in kg}
  }
}

\references{

Falconer, D. S. and Mackay, T. F. C. (1996) Introduction to Quantitative
Genetics. 4th edition. Longman, Essex, U.K.
\url{http://www.amazon.com/gp/product/0582243025}

Mrode, R. A. (2005) Linear models for the prediction of animal breeding
values. 2nd edition. CAB International. ISBN 0-85199-000-2
\url{http://www.amazon.com/gp/product/0851990002}

}

\examples{
  data(Falconer5.1)
  Pedigree(x=Falconer5.1, subject="sub", ascendant=c("fat", "mot"))

  data(Mrode2.1)
  Mrode2.1$dtB <- as.Date(Mrode2.1$dtB)
  Pedigree(x=Mrode2.1, subject="sub", ascendant=c("fat", "mot"),
           ascendantSex=c("M", "F"), family="fam", sex="sex",
           generation="gen", dtBirth="dtB")

  data(Mrode3.1)
  Pedigree(x=Mrode3.1, subject="calf", ascendant=c("sire", "dam"),
           ascendantSex=c("Male", "Female"), sex="sex")

}

\keyword{datasets}

%--------------------------------------------------------------------------
% datasets.Rd ends here
