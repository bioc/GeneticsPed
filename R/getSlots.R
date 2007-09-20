### getSlots.R
###------------------------------------------------------------------------
### What: Retrieve slot values from the pedigree
### $Id$
### Time-stamp: <2007-09-07 09:30:07 ggorjan>
###------------------------------------------------------------------------

getIdName          <- function(x) attr(x, ".subject") ## FIXME: change to id
getId              <- function(x) x[, getIdName(x)]

getAscendantName   <- function(x) attr(x, ".ascendant")
getAscendant       <- function(x) x[, getAscendantName(x)]

getFatherName      <- function(x) attr(x, ".ascendant")[1]
getFather          <- function(x) x[, getFatherName(x)]

getMotherName      <- function(x) attr(x, ".ascendant")[1]
getMother          <- function(x) x[, getMotherName(x)]

getFamilyName      <- function(x) attr(x, ".family")
getFamily          <- function(x) x[[getFamilyName(x)]]

getSexName         <- function(x) attr(x, ".sex")
getSex             <- function(x) x[[getSexName(x)]]

getDtBirthName     <- function(x) attr(x, ".dtBirth")
getDtBirth         <- function(x) x[[getDtBirthName(x)]]

getGenerationName  <- function(x) attr(x, ".generation")
getGeneration      <- function(x) x[[getGenerationName(x)]]

getAscendantSex    <- function(x) attr(x, ".ascendantSex")
getAscendantLevel  <- function(x) attr(x, ".ascendantLevel")
getGenerationOrder <- function(x) attr(x, ".generationOrder")
getColClass        <- function(x) attr(x, ".colClass")
getChecked         <- function(x) attr(x, ".checked")
getExtended        <- function(x) attr(x, ".extended")
getSorted          <- function(x) attr(x, ".sorted")
getCoded           <- function(x) attr(x, ".coded")
getUnknown         <- function(x) attr(x, ".unknown")

###------------------------------------------------------------------------
### getSlots.R ends here
