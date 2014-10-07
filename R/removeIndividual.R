### removeIndividual.R
###------------------------------------------------------------------------
### What: Remove individual from Pedigree
### $Id: removeIndividual.R 1092 2006-11-12 12:28:38Z ggorjan $
### Time-stamp: <2006-09-30 01:11:07 ggorjan>
###------------------------------------------------------------------------

removeIndividual <- function(x, individual, remove="all")
{
  ## --- Check ---
  x <- checkAttributes(x)
  subject <- attr(x, ".subject")
  ascendant <- attr(x, ".ascendant")

  if(!any(c(subject, ascendant, "all") %in% remove))
    stop(sprintf("'remove' can only be: %s",
                 paste(dQuote(c(subject, ascendant, "all")), collapse=", ")))

  ## --- Core ---
  ## all equals to all id
  if(length(remove) == 1 && remove == "all") remove <- c(subject, ascendant)

  ## any individuals that are not in x?
  tmp <- individual %in% unlist(x[, remove])
  if(any(!tmp))
    warning(sprintf("no individual(s) %s in 'x'", paste(individual[!tmp], collapse=",")))

  ## remove them
  for(i in seq(along=remove)) {
    if(remove[i] == attr(x, ".subject")) { ## remove row if this is subject
      test <- which(x[[subject]] %in% individual)
      ## fix extend status if founders are removed
      if(any(isFounder(x[test, ]))) attr(x, ".extended") <- FALSE
      x <- x[-test, ]
    } else { ## set to unknown if this is ascendant
      x[x[[remove[i]]] %in% individual, remove[i]] <- attr(x, ".unknown")$.id
    }
  }

  ## fix levels for factors
  if(attr(x, ".colClass") == "factor")
    x[, remove] <- lapply(x[, remove], factor)

  x
}

###------------------------------------------------------------------------
### removeIndividual.R ends here
