### generation.R
###------------------------------------------------------------------------
### What: Calculate generation value
### $Id: generation.R 1092 2006-11-12 12:28:38Z ggorjan $
### Time-stamp: <2006-09-30 17:33:41 ggorjan>
###------------------------------------------------------------------------

generation <- function(x, start=1, generationOrder=NULL)
{
  ## --- Setup ---

  subject <- attr(x, ".subject")
  idInput <- as.character(x[[subject]])  ## for sorting back

  ## Pedigree must be sorted, extended and unknowns must be NA
  x <- checkAttributes(x, sorted=TRUE, extended=TRUE, unknownNA=TRUE)

  N <- nrow(x)
  ascendant <- attr(x, ".ascendant")
  ascendantN <- length(ascendant)
  ascendantLevel <- attr(x, ".ascendantLevel")
  generation <- vector(mode="numeric", length=N)
  names(generation) <- x[[subject]]
  if(is.null(generationOrder)) generationOrder <- attr(x, ".generationOrder")

  ## --- Machine ---

  for(i in 1:N) {
    founder <- isFounder(x[i, ])
    if(founder) {
      generation[i] <- start
    } else {
      genlist <- NULL
      for(j in 1:ascendantN) {
        k <- x[i, ascendant[j]]
        if(is.na(k)) {
          genlist <- c(genlist, start)
        } else {
          k <- which(x[[subject]] %in% k)
          genlist <- c(genlist, generation[k] + ascendantLevel[j] - 1)
        }
      }
      generation[i] <- max(genlist) + 1
    }
  }

  if(generationOrder != "increasing")
    generation <- abs(generation - max(generation)) + start

  ## --- End ---

  generation[idInput]
}

"generation<-" <- function(x, generationOrder=NULL, col=NULL, value)
{
  ## --- Setup ---

  generAttr <- attr(x, ".generation")
  generOrdAttr <- attr(x, ".generationOrder")
  colTest <- is.null(col)
  ordTest <- is.null(generationOrder)

  if(is.na(generAttr) & colTest) attr(x, ".generation") <- col <- "generation"
  if(!is.na(generAttr) & colTest) col <- attr(x, ".generation")
  if(!is.na(generAttr) & !colTest) { ## rename column with col
    colNamesT <- names(x)
    colNamesT[colNamesT == generAttr] <- col
    colnames(x) <- colNamesT
    attr(x, ".generation") <- col
  }

  if(is.na(generOrdAttr) & ordTest) attr(x, ".generationOrder") <- generationOrder <- "increasing"
  if(!is.na(generOrdAttr) & !ordTest) attr(x, ".generationOrder") <- generationOrder

  ## --- Apply ---

  x[[col]] <- value

  x
}

###------------------------------------------------------------------------
### generation.R ends here
