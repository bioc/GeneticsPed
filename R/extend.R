### extend.R
###------------------------------------------------------------------------
### What: Extend Pedigree
### $Id: extend.R 1166 2007-04-03 14:00:55Z ggorjan $
### Time-stamp: <2007-04-03 13:02:45 ggorjan>
###------------------------------------------------------------------------

extend <- function(x, ascendant=NULL, col=NULL, top=TRUE)
{
  ## --- Setup ---

  x <- checkAttributes(x)
  subject <- attr(x, ".subject")
  if(is.null(ascendant)) ascendant <- attr(x, ".ascendant")

  ## --- Kernel ---

  for(i in seq(along=ascendant)) {

    ## Find ascendants that do not appear as subjects
    notIn <- (!(x[[ascendant[i]]] %in% x[[subject]]) &
              !x[[ascendant[i]]] %in% attr(x, ".unknown")$.id)
    tmp <- unique(x[[ascendant[i]]][notIn])
    tmpn <- length(tmp)
    if(tmpn > 0) {

      ## Collect structure from x, epand it and put in founders
      new <- x[0, ]
      new <- new[1:tmpn, ]
      row.names(new) <- NULL
      new[[subject]] <- tmp

      ## Sex
      sexAttr <- attr(x, ".sex")
      if(!is.na(sexAttr) & (is.null(col) | sexAttr %in% col)) {
        if(is.factor(new[[sexAttr]])) {
          new[[sexAttr]] <- factor(attr(x, ".ascendantSex")[i],
                                   levels=sort(attr(x, ".ascendantSex")))
        } else {
          new[[sexAttr]] <- attr(x, ".ascendantSex")[i]
        }
      }

      ## Generation
      genAttr <- attr(x, ".generation")
      if(!is.na(genAttr) & (is.null(col) | genAttr %in% col)) {
        if(attr(x, ".generationOrder") == "increasing") {
          fun <- min
        } else {
          fun <- max
        }
        ## Using c() to convert table to a vector
        new[[genAttr]] <- c(tapply(x[notIn, genAttr],
                                   c(x[notIn, ascendant[i]]), fun,
                                   na.rm=TRUE)) - 1
      }

      ## Date of birth
      ## FIXME: some method to possibly apply calculation of aproximate
      ## date of birth on average generation interval

      ## Extend
      x <- rbind(new, x)
      if(!top) {
        newN <- nrow(new)
        x <- x[c((newN + 1):nrow(x), 1:newN), ]
        ## FIXME: check if this order is appropriate one or should it be in
        ## any particular order specific to top
      }
    }
  }

  attr(x, ".extended") <- TRUE

  ## Family
  ## FIXME: commented out for the moment as there are many unresolved
  ## issues --> look into family.R
  ## famAttr <- attr(x, ".family")
  ## if(!is.na(famAttr) & (is.null(col) | famAttr %in% col))
  ##  family(x) <- family(x)

  ## --- End ---
  x
}

###------------------------------------------------------------------------
### extend.R ends here
