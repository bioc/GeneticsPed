### check.Pedigree.R
###------------------------------------------------------------------------
### What: Check Pedigree for common errors
### $Id: check.pedigree.R 1096 2006-11-14 06:08:39Z ggorjan $
### Time-stamp: <2006-11-14 07:07:16 ggorjan>
###------------------------------------------------------------------------

check <- function(x, ...)
  UseMethod("check")

check.Pedigree <- function(x, ...)
{
  checkId(x)
  if(!is.na(attr(x, ".sex")))
    checkSex(x)
  if(!is.na(attr(x, ".dtBirth")))
    checkDtBirth(x)
}

checkAttributes <- function(x, coded=FALSE, sorted=FALSE, extended=FALSE,
                            unknownNA=FALSE)
{
  if(!is.Pedigree(x)) {
    msg <- sprintf("Coercing to %s class\n", dQuote("Pedigree"))
    cat(msg)
    x <- Pedigree(x)
  }

  if(!attr(x, ".coded") & coded) {
    msg <- "Coding - (not yet implemented)\n"
    cat(msg)
    ## x <- ???(x)
  }

  if(!attr(x, ".sorted") & sorted) {
    msg <- "Sorting\n"
    cat(msg)
    #x <- sort(x)
    x <- sort.Pedigree(x)
  }

  if(!attr(x, ".extended") & extended) {
    msg <- "Extending\n"
    cat(msg)
    x <- extend(x)
  }

  if(any(!is.na(attr(x, ".unknown")$.id)) & unknownNA) {
    msg <- paste("Unknown to NA\n")
    cat(msg)
    stop("no unknown handling at the moment")
    ## x <- unknownToNA.Pedigree(x)
  }
  ##  cat("End\n")
  x
}

checkId <- function(x)
{
  error <- FALSE
  ret <- list()

  idClass(x)

  ret$subjectIsNA <- subjectNA(x)
  if(is.list(ret$subjectIsNA)) error <- TRUE

  ret$subjectNotUnique <- subjectNotUnique(x)
  if(is.list(ret$subjectNotUnique)) error <- TRUE

  ret$subjectEqualAscendant <- subjectEqualAscendant(x)
  if(is.list(ret$subjectEqualAscendant)) error <- TRUE

  ret$ascendantEqualAscendant <- ascendantEqualAscendant(x)
  if(is.list(ret$ascendantEqualAscendant)) error <- TRUE

  ret$ascendantInAscendant <- ascendantInAscendant(x)
  if(is.list(ret$ascendantInAscendant)) error <- TRUE

  ret$unusedLevels <- unusedLevels(x)
  if(is.list(ret$unusedLevels)) error <- TRUE

  if(error) {
    return(ret)
  } else {
    return(0)
  }
}

idClass <- function(x)
{
  col <- c(attr(x, ".subject"), attr(x, ".ascendant"))
  tmp <- lapply(x[, col], class)
  if(any(tmp != class(x[[attr(x, ".subject")]])))
    stop(sprintf("columns %s must have the same class",
                 paste(sQuote(col), collapse=", ")))
}

subjectIsNA <- function(x)
{
  error <- FALSE
  ret <- list()

  ## --- Subject can not be NA ---
  tmp <- is.na(x[[attr(x, ".subject")]])
  if(any(tmp)) {
    error <- TRUE
    ret$subjectIsNA <- list()
    ret$subjectIsNA$message <- sprintf("%s id can not be NA",
                                       sQuote(attr(x, ".subject")))
    ret$subjectIsNA$rowIndex <- which(tmp)
  }
  if(error) {
    return(ret)
  } else {
    return(0)
  }
}

subjectNotUnique <- function(x)
{
  error <- FALSE
  ret <- list()

  ## --- Subject must be unique ---
  if(nrow(x) != length(unique(x[[attr(x, ".subject")]]))) { # test is cheap
    tmp <- table(x[[attr(x, ".subject")]])
    tmp <- tmp[tmp > 1]
    tmp <- x[[attr(x, ".subject")]] %in% names(tmp)
    error <- TRUE
    ret$subjectNotUnique <- list()
    ret$subjectNotUnique$message <- sprintf("%s must be unique",
                                            sQuote(attr(x, ".subject")))
    ret$subjectNotUnique$rowIndex <- which(tmp)
  }
  if(error) {
    return(ret)
  } else {
    return(0)
  }
}

subjectEqualAscendant <- function(x)
{
  error <- FALSE
  ret <- list()

  ## --- Subject can not be equal to ascendant ---
  col <- attr(x, ".ascendant")
  for(i in seq(along=col)) {
    if(attr(x, ".colClass") == "factor") {
      tmp <- as.character(x[[attr(x, ".subject")]]) == as.character(x[[col[i]]])
    } else {
      tmp <- x[[attr(x, ".subject")]] == x[[col[i]]]
    }
    tmp[is.na(tmp)] <- FALSE # NA come in if subject is NA - I get this
                             # one above or if ascendant is NA, which is OK
    if(any(tmp)) {
      error <- TRUE
      retTmp <- vector("list", 1)
      retName <- paste(attr(x, ".subject"), "Equals", col[i], sep="")
      names(retTmp) <- retName
      ret <- c(ret, retTmp)
      ret[[retName]]$message <- sprintf("%s id can not be equal to id of %s",
                                        sQuote(attr(x, ".subject")),
                                        sQuote(col[i]))
      ret[[retName]]$rowIndex <- which(tmp)
    }
  }
  if(error) {
    return(ret)
  } else {
    return(0)
  }
}

ascendantEqualAscendant <- function(x)
{
  error <- FALSE
  ret <- list()

  ## --- Ascendant can not be equal to ascendant ---
  col <- attr(x, ".ascendant")
  for(i in seq(along=col)) {
    for(j in seq(along=col)) {
      if(j > i) {
        if(attr(x, ".colClass") == "factor") {
          tmp <- as.character(x[[col[i]]]) == as.character(x[[col[j]]])
        } else {
          tmp <- x[[col[i]]] == x[[col[j]]]
        }
        tmp[is.na(tmp)] <- FALSE # NA come if ascendants are unknown, which
                                 # is OK
        if(any(tmp)) {
          error <- TRUE
          retTmp <- vector("list", 1)
          retName <- paste(col[i], "Equals", col[j], sep="")
          names(retTmp) <- retName
          ret <- c(ret, retTmp)
          ret[[retName]]$message <-
            sprintf("%s id can not be equal to id of %s",
                    sQuote(col[i]), sQuote(col[j]))
          ret[[retName]]$rowIndex <- which(tmp)
        }
      }
    }
  }
  if(error) {
    return(ret)
  } else {
    return(0)
  }
}

ascendantInAscendant <- function(x)
{
  error <- FALSE
  ret <- list()

  ## --- Ascendant can not be in ascendant of a different sex ---
  col <- attr(x, ".ascendant")
  colSex <- attr(x, ".ascendantSex")

  ## Put ascendants of equal sex together
  sex <- unique(colSex)
  tmp <- vector(mode="list", length=length(sex))
  names(tmp) <- sex
  for(i in seq(along=sex)) {
    if(attr(x, ".colClass") == "factor") {
      map <- mapLevels(x=x[, col[colSex == sex[i]]], combine=TRUE, codes=FALSE)
      mapLevels(x[, col[colSex == sex[i]]]) <- map
    }
    for(j in seq(along=col[colSex == sex[i]])) {
      tmp[[sex[i]]] <- c(tmp[[sex[i]]], x[, col[colSex == sex[i]][j]])
    }
    if(attr(x, ".colClass") == "factor") {
      tmp[[sex[i]]] <- factor(tmp[[sex[i]]])
      mapLevels(tmp[[sex[i]]]) <- map
    }
  }

  ## Check
  for(i in seq(along=sex)) {
    for(j in seq(along=sex)) {
      if(j > i) {
        if(attr(x, ".colClass") == "factor") {
          tmp1 <- as.character(tmp[[i]]) %in% as.character(tmp[[j]]) & !is.na(tmp[[i]])
        } else {
          tmp1 <- tmp[[i]] %in% tmp[[j]] & !is.na(tmp[[i]])
        }
        if(any(tmp1)) {
          error <- TRUE
          retTmp <- vector("list", 1)
          retName <- paste(paste(col[colSex == sex[i]], collapse="|"),
                           "In",
                           paste(col[colSex == sex[j]], collapse="|"), sep="")
          names(retTmp) <- retName
          ret <- c(ret, retTmp)
          ret[[retName]]$message <- sprintf("%s id can not be in id of %s",
                                            sQuote(paste(col[colSex == sex[i]], collapse="|")),
                                            sQuote(paste(col[colSex == sex[j]], collapse="|")))
          colSexN <- length(col[colSex == sex[i]])
          dim(tmp1) <- c(nrow(x), colSexN)
          if(colSexN > 1) {
            tmp1 <- rowSums(tmp1)
            tmp1 <- ifelse(tmp1 == 0, FALSE, TRUE)
          }
          ret[[retName]]$rowIndex <- which(tmp1)
        }
      }
    }
  }
  if(error) {
    return(ret)
  } else {
    return(0)
  }
}

unusedLevels <- function(x)
{
  error <- FALSE
  ret <- list()

  ## --- Unused levels ---
  col <- attr(x, ".ascendant")
  if(attr(x, ".colClass") == "factor") {
    col <- c(attr(x, ".subject"), attr(x, ".ascendant"))
    for(i in seq(along=col)) {
      lev1 <- levels(factor(x[[col[i]]]))
      tmp <- length(lev1)
      lev2 <- levels(x[[col[i]]])
      tmp1 <- length(lev2)
      if(tmp1 > tmp) {
        error <- TRUE
        retTmp <- vector("list", 1)
        retName <- paste("unusedLevelsIn", col[i], sep="")
        names(retTmp) <- retName
        ret <- c(ret, retTmp)
        ret[[retName]]$message <- sprintf("unused levels in %s", sQuote(col[i]))
        ret[[retName]]$levels <- lev2[!(lev2 %in% lev1)]
      }
    }
  }
  if(error) {
    return(ret)
  } else {
    return(0)
  }
}

## checkSex <- function(x)
## {
##   ## - sex could be checked from knowledge that sires are in second column or
##   ##   something like that from attrs
##   ## - subject that appears as ascendant can also be checked for its sex value
## }

## checkDtBirth <- function(x)
## {
## }

###------------------------------------------------------------------------
### check.Pedigree.R ends here
