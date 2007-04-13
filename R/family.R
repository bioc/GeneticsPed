### family.R
###------------------------------------------------------------------------
### What: Find families (lines) in the pedigree
### $Id: family.R 1170 2007-04-03 14:04:37Z ggorjan $
### Time-stamp: <2007-04-03 00:30:24 ggorjan>
###------------------------------------------------------------------------

## FIXME: Any way to use original family names here i.e. we output integer
## code for families, but it might be the case that we already have some
## data and we can either confirm this or refute. Confirmation would be OK,
## but in case of refute we would need to paste all family names together.
##
## Example
## id dad mom fam
## 1  0   0   A
## 2  0   0   A
## 3  0   0   B
## 4  1   2   A
## 5  1   2   A
## 6  1   3   B
## 8  0   0   C
## 9  0   0   C
## 7  8   9   C
## Here it is clear that family C should stay family C and the same with
## A. What to do with individual 6? Should his family be A or B?

## x <- data.frame( id=c(1, 2, 3, 4, 5, 6, 8, 9, 7),
##                 dad=c(0, 0, 0, 1, 1, 1, 0, 0, 8),
##                 mom=c(0, 0, 0, 2, 2, 3, 0, 0, 9),
##                 fam=c("A", "A", "B", "A", "A", "B", "C", "C", "C"))
## x <- Pedigree(x=x, subject="id", ascendant=c("dad", "mom"), unknown=0)
## family(x)
## FIXME: It should definitely not say the same family for individual 3
## as 1 and 2 or should it? Perhaps we need to change the order of the
## algorithm
## 1 2 3 4 5 6 8 9 7
## 2 2 2 2 2 2 1 1 1

family <- function(x)
{
  ## --- Setup ---

  subject <- attr(x, ".subject")
  idInput <- as.character(x[[subject]]) ## for sort-back

  ## Pedigree must be sorted and extended
  x <- GeneticsPed:::checkAttributes(x, sort=TRUE, extend=TRUE)
  n <- nrow(x)
  f <- n:1                           ## at the start each id is its own family
  names(f) <- x[[subject]]
  nFounder <- sum(isFounder(x))

  ## --- Find families ---

  ## FIXME: test if geneFlowT would be faster
  A <- relationshipAdditive(x)
  for(i in n:(n - nFounder + 1)) {  ## Go from bottom up to founders
    ## FIXME: another problem here with founders. It is not necesarry that
    ## founders appear on top of the pedigree, they can also be somewhere
    ## in between if sorting is by date of birth or generation!
    test <- A[i, 1:(i - 1)] > 0     ## Find relatives and
    f[1:(i - 1)][test] <- f[i]      ##   put them in the same family
  }

  ## --- Renumber from 1:nFamily ---

  fTmp <- sort(unique(f))
  fTmp1 <- 1:length(fTmp)
  for(i in seq(along=fTmp)) {
    f[f %in% fTmp[i]] <- fTmp1[i]
  }

  ## --- End ---

  f[idInput]
}

"family<-" <- function(x, col=NULL, value)
{
  ## --- Setup ---

  famAttr <- attr(x, ".family")
  colTest <- is.null(col)

  if(is.na(famAttr) & colTest) attr(x, ".family") <- col <- "family"
  if(!is.na(famAttr) & colTest) col <- attr(x, ".family")
  if(!is.na(famAttr) & !colTest) { ## rename column with col
    colNamesT <- names(x)
    colNamesT[colNamesT == famAttr] <- col
    colnames(x) <- colNamesT
    attr(x, ".family") <- col
  }

  ## --- Apply ---

  x[[col]] <- value

  x
}

###------------------------------------------------------------------------
### family.R ends here
