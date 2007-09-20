### check.Pedigree.R
###------------------------------------------------------------------------
### What: Check Pedigree for common errors
### $Id: check.pedigree.R 1189 2007-04-06 10:58:28Z ggorjan $
### Time-stamp: <2007-09-07 09:30:17 ggorjan>
###------------------------------------------------------------------------

check <- function(x, ...)
  UseMethod("check")

check.Pedigree <- function(x, ...)
{
  checkId(x)
  if(!is.na(getSexName(x))) checkSex(x)
  ##  if(!is.na(getDtBirthName(x))) checkDtBirth(x)
}

checkAttributes <- function(x, coded=FALSE, extended=FALSE, sorted=FALSE,
                            unknownNA=FALSE, ...)
{
  ## --- Class ---

  if(!is.Pedigree(x)) {
    if(options()$verbose)
      cat(sprintf("Coercing to %s class\n", dQuote("Pedigree")))
    x <- Pedigree(x)
  }

  ## --- Renumber ---

  if(!getCoded(x) & coded) {
    if(options()$verbose) cat("Coding - (not yet implemented)\n")
    ## x <- ???(x)
  }

  ## --- Extend ---

  if(!getExtended(x) & extended) {
    if(options()$verbose) cat("Extending\n")
    x <- extend(x, ...)
  }

  ## --- Sort ---

  if(!getSorted(x) & sorted) {
    if(options()$verbose) cat("Sorting\n")
    x <- sort(x, ...)
  }

  ## --- Unknown ---

  if(any(!is.na(getUnknown(x)$.id)) & unknownNA) {
    if(options()$verbose) cat("Unknown to NA\n")
    stop("no unknown handling at the moment")
    ## FIXME: x <- unknownToNA.Pedigree(x)
  }
  x
}

checkId <- function(x)
{
  error <- FALSE
  ret <- list()

  idClass(x)

  ret$idIsNA <- idIsNA(x)
  if(is.list(ret$idIsNA)) error <- TRUE

  ret$idNotUnique <- idNotUnique(x)
  if(is.list(ret$idNotUnique)) error <- TRUE

  ret$idEqualAscendant <- idEqualAscendant(x)
  if(is.list(ret$idEqualAscendant)) error <- TRUE

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
  col <- c(getIdName(x), getAscendantName(x))
  tmp <- lapply(x[, col], class)
  if(any(tmp != class(getId(x))))
    stop(sprintf("columns %s must have the same class",
                 paste(sQuote(col), collapse=", ")))
}

idIsNA <- function(x)
{
  error <- FALSE
  ret <- list()

  ## --- Id can not be NA ---

  tmp <- is.na(getId(x))
  if(any(tmp)) {
    error <- TRUE
    ret$idIsNA <- list()
    ret$idIsNA$message <- sprintf("%s id can not be NA",
                                  sQuote(getIdName(x)))
    ret$idIsNA$rowIndex <- which(tmp)
  }
  if(error) {
    return(ret)
  } else {
    return(0)
  }
}

idNotUnique <- function(x)
{
  error <- FALSE
  ret <- list()

  ## --- Id must be unique ---

  if(nrow(x) != length(unique(getId(x)))) { # test is cheap
    tmp <- table(getId(x))
    tmp <- tmp[tmp > 1]
    tmp <- getId(x) %in% names(tmp)
    error <- TRUE
    ret$idNotUnique <- list()
    ret$idNotUnique$message <- sprintf("%s must be unique",
                                       sQuote(getIdName(x)))
    ret$idNotUnique$rowIndex <- which(tmp)
  }
  if(error) {
    return(ret)
  } else {
    return(0)
  }
}

idEqualAscendant <- function(x)
{
  error <- FALSE
  ret <- list()

  ## --- Id can not be equal to ascendant ---

  col <- getAscendantName(x)
  for(i in seq(along=col)) {
    if(getColClass(x) == "factor") {
      tmp <- as.character(getIdName(x)) == as.character(x[[col[i]]])
    } else {
      tmp <- getIdName(x) == x[[col[i]]]
    }
    tmp[is.na(tmp)] <- FALSE # NA come in if id is NA - I get this
                             # one above or if ascendant is NA, which is OK
    if(any(tmp)) {
      error <- TRUE
      retTmp <- vector("list", 1)
      retName <- paste(getIdName(x), "Equals", col[i], sep="")
      names(retTmp) <- retName
      ret <- c(ret, retTmp)
      ret[[retName]]$message <- sprintf("%s id can not be equal to id of %s",
                                        sQuote(getIdName(x)),
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
  col <- getAscendantName(x)
  for(i in seq(along=col)) {
    for(j in seq(along=col)) {
      if(j > i) {
        if(getColClass(x) == "factor") {
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
  col <- getAscendantName(x)
  colSex <- getAscendantSex(x)

  ## Put ascendants of equal sex together
  sex <- unique(colSex)
  tmp <- vector(mode="list", length=length(sex))
  names(tmp) <- sex
  for(i in seq(along=sex)) {
    if(getColClass(x) == "factor") {
      map <- mapLevels(x=x[, col[colSex == sex[i]]], combine=TRUE, codes=FALSE)
      mapLevels(x[, col[colSex == sex[i]]]) <- map
    }
    for(j in seq(along=col[colSex == sex[i]])) {
      tmp[[sex[i]]] <- c(tmp[[sex[i]]], x[, col[colSex == sex[i]][j]])
    }
    if(getColClass(x) == "factor") {
      tmp[[sex[i]]] <- factor(tmp[[sex[i]]])
      mapLevels(tmp[[sex[i]]]) <- map
    }
  }

  ## Check
  for(i in seq(along=sex)) {
    for(j in seq(along=sex)) {
      if(j > i) {
        if(getColClass(x) == "factor") {
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
  col <- getAscendantName(x)
  if(getColClass(x) == "factor") {
    col <- c(getIdName(x), getAscendantName(x))
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

checkSex <- function(x)
{
  ## --- Setup ---

  error <- FALSE
  ret <- list()

  ascendant <- getAscendantName(x)
  ascendantSex <- getAscendantSex(x)
  sexV <- getSex(x)

  ## --- Core ---

  for(i in seq(along=ascendant)) {
    ## Test sex in individuals that appear as ascendants and that have known sex
    test <- sexV[getId(x) %in% getAscendant(x)[[i]] &
                 !(sexV %in% getUnknown(x)$.sex)] != ascendantSex[i]
    if(any(test)) {
      ## Test to get an individual with positive upper test
      test <- which(getId(x) %in%
                    getAscendant(x)[[i]] &
                    !(sexV %in% getUnknown(x)$.sex) &
                    !(sexV %in% ascendantSex[i]))
      error <- TRUE
      retTmp <- vector("list", 1)
      retName <- paste("wrongSexFor", ascendant[i], sep="")
      names(retTmp) <- retName
      ret <- c(ret, retTmp)
      ret[[retName]]$message <- sprintf("Should be %s", ascendantSex[i])
      ret[[retName]]$rowIndex <- test
    }
  }

  ## --- End ---

  if(error) {
    return(ret)
  } else {
    return(0)
  }
}

## checkDtBirth <- function(x)
## {
## }

###------------------------------------------------------------------------
### check.Pedigree.R ends here
