### relationshipAdditive.R
###------------------------------------------------------------------------
### What: Additive relationship matrix (A)
### $Id: relationshipAdditive.R 1184 2007-04-04 07:06:22Z ggorjan $
### Time-stamp: <2007-04-04 09:04:37 ggorjan>
###------------------------------------------------------------------------

### {{{ relationshipAdditive
###------------------------------------------------------------------------

## relationshipAdditive <- function(x, sort=TRUE, names=TRUE, ...)
## {
##   ## --- Setup ---

##   subject <- attr(x, ".subject")
##   ascendant <- attr(x, ".ascendant")
##   if(length(ascendant) > 2)
##       stop("no method for pedigree with more than two ascendants")
##   if(sort) idOrig <- as.character(x[[subject]])  # for sorting-back

##   ## Pedigree must be sorted and extended
##   x <- GeneticsPed:::checkAttributes(x, sorted=TRUE, extended=TRUE)

##   n <- nrow(x)
##   ret <- matrix(0, n, n)          # n*n matrix
##   diag(ret) <- 1                  # diagonal is at least 1
##   rownames(ret) <- colnames(ret) <- x[[subject]]

##   asc1 <- as.character(x[[ascendant[1]]])
##   asc2 <- as.character(x[[ascendant[2]]])
##   testAsc1 <- !isUnknown(x[[ascendant[1]]])
##   testAsc2 <- !isUnknown(x[[ascendant[2]]])
##   testAsc  <- testAsc1 & testAsc2
##   ## subjects with at least one ascendant known
##   set <- which(!(!testAsc1 & !testAsc2))

##   ## --- Core ---

##   for(i in set) {
##     ## Diagonal
##     if(testAsc[i]) ret[i, i] <- 1 + 0.5 * ret[asc1[i], asc2[i]]
##     ## Off-diagonal
##     j <- 1:(i - 1)                # working with lower triangle
##     if(testAsc1[i]) tmp <-       0.5 * ret[asc1[i], j]
##     if(testAsc2[i]) tmp <- tmp + 0.5 * ret[asc2[i], j]
##     ret[i, j] <- ret[j, i] <- tmp
##   }

##   ## --- End ---

##   if(sort) ret <- ret[idOrig, idOrig]
##   if(!names) attr(ret, which="dimnames") <- NULL
##   ret
## }

relationshipAdditive <- function(x, sort=TRUE, names=TRUE, ...){
    ## --- Setup ---
    subject <- attr(x, ".subject")
    ascendant <- attr(x, ".ascendant")
    if(length(ascendant) > 2)
        stop("no method for pedigree with more than two ascendants")
    if(sort) idOrig <- as.character(x[[subject]]) # for sorting-back
    ## Pedigree must be sorted and extended
    x <- GeneticsPed:::checkAttributes(x, sorted=TRUE, extended=TRUE)
    n <- nrow(x)
    ret <- matrix(0, n, n) # n*n matrix
    diag(ret) <- 1 # diagonal is at least 1
    rownames(ret) <- colnames(ret) <- x[[subject]]
    asc1 <- as.character(x[[ascendant[1]]])
    asc2 <- as.character(x[[ascendant[2]]])
    testAsc1 <- !isUnknown(x[[ascendant[1]]])
    testAsc2 <- !isUnknown(x[[ascendant[2]]])
    testAsc <- testAsc1 & testAsc2

    ## subjects with at least one ascendant known
    set <- which(!(!testAsc1 & !testAsc2))
    ## --- Core ---
    for(i in set) {
        ## Diagonal
        if(testAsc[i]) ret[i, i] <- 1 + 0.5 * ret[asc1[i], asc2[i]]
        ## Off-diagonal
        j <- 1:(i - 1) # working with lower triangle
        if( testAsc1[i] )
        {
            tmp1 <- 0.5 * ret[asc1[i], j]
        }
        else
        {
            tmp1 <- 0
        }
        if( testAsc2[i] )
        {
            tmp2 <- 0.5 * ret[asc2[i], j]
        }
        else
        {
            tmp2 <- 0
        }
        ret[i, j] <- ret[j, i] <- tmp1 + tmp2
    }

    ## --- End ---
    if(sort) ret <- ret[idOrig, idOrig]
    if(!names) attr(ret, which="dimnames") <- NULL
    ret
}

### }}}
### {{{ kinship
###------------------------------------------------------------------------

kinship <- function(x, sort=TRUE, names=TRUE, ...)
  relationshipAdditive(x, sort=sort, names=names, ...) / 2

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### relationshipAdditive.R ends here
