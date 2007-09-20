### generatePedigree.R
###------------------------------------------------------------------------
### What: Generate Pedigree example
### $Id: generatePedigree.R 1096 2006-11-14 06:08:39Z ggorjan $
### Time-stamp: <2007-04-17 13:45:02 ggorjan>
###------------------------------------------------------------------------

## TODO: check for VB? note in slides
## TODO: implement option for multiple parents (fathers)

generatePedigree <- function(nId, nGeneration=3, nFather=round(nId/3),
                             nMother=nId-nFather, start=1,
                             generationOrder="increasing",
                             colClass="integer")
{
  ## --- Setup ---

  if(nId < 2) stop("'nId' must be at least 2")
  if((nFather + nMother) > nId)
    stop("'nFather' + 'nMother' must be lower or equal 'nId'")

  s <- round(nId * nFather/ (nFather + nMother))
  N <- nId * nGeneration
  x <- cbind(1:N, matrix(data=0, nrow=N, ncol=4))
  x[1:nId, 4] <- start
  for(i in 2:nGeneration) {
    ## Get parents
    base <- (i - 1) * nId
    fa <- (base - nId) + 1:nFather
    mo <- (base - nId + s) + 1:nMother
    ## Assign parents randomly
    x[(base + 1):(base + nId), 2] <- fa[ceiling(nFather * runif(n=nId))]
    x[(base + 1):(base + nId), 3] <- mo[ceiling(nMother * runif(n=nId))]
    x[(base + 1):(base + nId), 4] <- max(x[, 4]) + 1
  }

  ## Add sex
  x[x[, 1] %in% x[, 2], 5] <- 1
  x[x[, 1] %in% x[, 3], 5] <- 2
  tmp <- x[, 5] == 0
  x[tmp, 5] <- round(runif(n=sum(tmp), min=1, max=2))

  ## Column names and class stuff
  colnames(x) <- c("id", "father", "mother", "generation", "sex")
  if(colClass == "factor") {
    x <- data.frame(x)
    cols <- c("id", "father", "mother")
    x[, cols] <- as.data.frame(lapply(x[, cols], as.factor))
    cols <- c("generation", "sex")
    x[, cols] <- as.data.frame(lapply(x[, cols], as.integer))
  } else { # integers use less space
    x <- as.data.frame(lapply(data.frame(x), as.integer))
  }
  if(generationOrder == "decreasing")
    x$generation <- abs(x$generation - max(x$generation)) + start
  x <- as.Pedigree(x=x, unknown=0, sex="sex", generation="generation",
                   generationOrder=generationOrder)
  attr(x, ".extended") <- TRUE
  attr(x, ".sorted") <- TRUE
  if(colClass == "integer") {
    attr(x, ".coded") <- TRUE
  } else {
    attr(x, ".coded") <- FALSE
  }
  x
}

###------------------------------------------------------------------------
### generatePedigree.R
