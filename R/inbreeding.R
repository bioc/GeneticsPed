### inbreeding.R
###------------------------------------------------------------------------
### What: Inbreeding coefficient (F)
### $Id: inbreeding.R 1172 2007-04-03 14:05:59Z ggorjan $
### Time-stamp: <2007-03-31 19:42:29 ggorjan>
###------------------------------------------------------------------------

inbreeding <- function(x, method="meuwissen", sort=TRUE, names=TRUE, ...)
{
  ## --- Setup ---

  subject <- attr(x, ".subject")
  ascendant <- attr(x, ".ascendant")
  if(length(ascendant) > 2)
    stop("no method for pedigree with more than two ascendants")
  if(sort) idOrig <- as.character(x[[subject]]) # for sorting-back

  ## Pedigree must be sorted, extended and unknowns must be NA
  ## x <- checkAttributes(x=x, sorted=TRUE, extended=TRUE, unknownNA=TRUE)

  n <- nrow(x)
  ret <- vector(mode="numeric", length=n)
  ## paste is used due to possibility of having NA and as.character returns
  ## NA and not "NA" i.e. paste(NA) returns "NA"
  na.value <- paste(as.character(attr(x, ".unknown")$.id))

  METHODS <- c("meuwissen", "tabular")
  method <- METHODS[pmatch(method, METHODS)]
  if(is.na(method)) stop("invalid method")

  ## --- Kernel ---

  if(method == "tabular") {          # Tabular method i.e. F_i = A_ii - 1
    ret <- diag(relationshipAdditive(x, sort=sort, names=FALSE, ...)) - 1
  } else if(method == "meuwissen") { # Meuwissen and Luo method
    idx <- vector(mode="integer", length=n)
    ret <- .C(R_meuwissen,
              ## 1 number of individuals
              as.integer(n),
              ## 2 individual
              as.character(x[[subject]]),
              ## 3 father
              as.character(x[[ascendant[1]]]),
              ## 4 mother
              as.character(x[[ascendant[2]]]),
              ## 5 coefficients
              as.double(ret),
              ## 6 vector of sorted indexes
              idx,
              ## 7 character string indicating missing values
              as.character(na.value),
              ## 8 length of character string indicating missing values
              as.integer(length(na.value)),
              PACKAGE="GeneticsPed")[c(5, 6)]
    ret <- ret[[1]][order(ret[[2]])]
  }

  ## --- End ---

  names(ret) <- x[[subject]]
  if(sort) ret <- ret[idOrig]
  if(!names) names(ret) <- NULL
  ret
}

###------------------------------------------------------------------------
### inbreeding.R ends here
