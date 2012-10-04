### sort.Pedigree.R
###------------------------------------------------------------------------
### What: Sort Pedigree
### $Id: sort.pedigree.R 1092 2006-11-12 12:28:38Z ggorjan $
### Time-stamp: <2006-11-12 10:14:24 ggorjan>
###------------------------------------------------------------------------

sort.Pedigree <- function(x, decreasing=FALSE, na.last=TRUE, ...,
                          by="default")
{
  ## --- Setup ---

  dtBirth <- attr(x, ".dtBirth")
  generation <- attr(x, ".generation")

  if(by == "default") {                  ## Find way to sort pedigree
    if(!is.na(dtBirth)) {                  ## Is dtBirth available?
      if(sum(is.na(x[[dtBirth]])) == 0)    ## Do all have dtBirth?
        by <- "dtBirth"
    }
  }
  if(by == "default") {
    if(!is.na(generation)) {               ## Is generation available
      if(sum(is.na(x[[generation]])) == 0) ## Do all have generation?
        by <- "generation"
    }
  }
  if(by == "default") by <- "pedigree"     ## Just use pedigree information

  ## --- Sort on different information ---

  if(by == "pedigree") {
    warning("sorting by pedigree information is not yet implemented")
    return(x)
  }

  if(by == "generation") {
    if(!is.na(generation)) {
      args <- list()
      args$decreasing <- decreasing
      args$na.last <- na.last
      if(is.null(args$decreasing)) {
        args$decreasing <- ifelse(attr(x, ".generationOrder") == "increasing",
                             FALSE, TRUE)
      }
      attr(x, ".sorted") <- TRUE
      if ( decreasing )
      {
        subject <- attr(x, ".subject")
        ascendant <- attr(x, ".ascendant")
        na.value <- paste(as.character(attr(x, ".unknown")$.id))
        n.na <- length(na.value)
        n <- nrow(x)
      	ret <- .C(R_pedSort,
            ## 1 number of individuals
            as.integer(n),
            ## 2 individual ID
            as.character(x[[subject]]),
            ## 3 father ID
            as.character(x[[ascendant[1]]]),
            ## 4 mother ID
            as.character(x[[ascendant[2]]]),
            ## 5 matrix coefficients
            ## 7 character string indicating missing values
            as.character(na.value),
            ## 9 length of character string indicating missing values
            as.integer(n.na),
            PACKAGE="GeneticsPed")
        x[[subject]] <- ret[[2]]
        x[[ascendant[1]]] <- ret[[3]]
        x[[ascendant[2]]] <- ret[[4]]
      	return( x )
      }
      else
      {
        return(x[do.call(order, c(list(x[[generation]]), args)), ])
      }
#      return(x[do.call("order", c(list(x[[generation]]), args)), ])
    } else {
      stop("there is no data on 'generation' in pedigree")
    }
  }

  if(by == "dtBirth") {
    if(!is.na(dtBirth)) {
      attr(x, ".sorted") <- TRUE
      return(x[order(x[[dtBirth]], na.last=na.last, decreasing=decreasing), ])
    } else {
      stop("there is no data on 'dtBirth' in pedigree")
    }
  }
}

###------------------------------------------------------------------------
### sort.Pedigree.R ends here
