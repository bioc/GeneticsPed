### inverseAdditive.R
###------------------------------------------------------------------------
### What: Inverse of Additive Relationship Matrix
### $Id: inverseAdditive.R 1167 2007-04-03 14:02:23Z ggorjan $
### Time-stamp: <2007-04-03 16:01:34 ggorjan>
###------------------------------------------------------------------------

inverseAdditive <- function(x, sort=TRUE, names=TRUE, ...)
{
  ## --- Setup ---

  subject <- attr(x, ".subject")
  ascendant <- attr(x, ".ascendant")
  if(length(ascendant) > 2)
    stop("no method for pedigree with more than two ascendants")
  if(sort) idOrig <- as.character(x[[subject]])  # for sorting-back

  ## FIXME: all this is not needed! - check Dave's code
  ## Pedigree must be sorted, extended and unknown equal to NA
  ##x <- GeneticsPed:::checkAttributes(x, sorted=TRUE, extended=TRUE, unknownNA=TRUE)
  ## I've commented out the above as my code does sort and extend, but does require
  ##  that unknowns be coded correctly.  This will likely save considerable time
  ##  for large pedigrees.
  x <- GeneticsPed:::checkAttributes(x, sorted=FALSE, extended=FALSE, unknownNA=TRUE)

  ## Find unknown value (NA, "NA", 0, ...)
  ## paste is used due to possibility of having NA and as.character returns
  ## NA and not "NA" i.e. paste(NA) returns "NA"
  na.value <- paste(as.character(attr(x, ".unknown")$.id))

  ## --- Core ---

  n <- nrow(x)
  idx <- vector(mode="integer" , length=n)
  ret <- .C(R_inverseAdditive,
            ## 1 number of individuals
            as.integer(n),
            ## 2 individual ID
            as.character(x[[subject]]),
            ## 3 father ID
            as.character(x[[ascendant[1]]]),
            ## 4 mother ID
            as.character(x[[ascendant[2]]]),
            ## 5 matrix coefficients
            as.double(matrix(0, nrow=n, ncol=n)),
            ## 6 vector of sorted indexes
            idx,
            ## 7 character string indicating missing values
            as.character(na.value),
            ## 9 length of character string indicating missing values
            as.integer(length(na.value)),
            PACKAGE="GeneticsPed")[c(2, 5, 6)]
  ret[[2]] <- matrix(ret[[2]], nrow=n)
  idx <- order(ret[[3]])
  ret[[2]] <- ret[[2]][idx, idx]
  colnames(ret[[2]]) <- rownames(ret[[2]]) <- ret[[1]]

  ## --- End ---

  if(sort) ret <- ret[[2]][idOrig, idOrig]
  if(!names) attr(ret, which="dimnames") <- NULL
  ret
}

###------------------------------------------------------------------------
### inverseAdditive.R ends here
