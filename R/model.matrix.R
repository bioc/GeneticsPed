### model.matrix.R
###------------------------------------------------------------------------
### What: Model matrix for individuals with and without records
### $Id: model.matrix.R 1163 2007-04-03 13:58:58Z ggorjan $
### Time-stamp: <2007-04-01 03:40:28 ggorjan>
###------------------------------------------------------------------------

model.matrix.Pedigree <- function(object, y, id, left=TRUE, names=TRUE, ...)
{
  ## --- Setup ---

  if(is.matrix(y) && ncol(y) > 1) stop("no method for multivariate data")
  subject <- attr(object, ".subject")

  n <- length(id)
  ## Ensuring proper order in the pedigree, based on id
  rownames(object) <- object[[subject]]
  object <- object[id, ]
  ## Expanding the pedigree to get number of founders
  object <- extend(object)
  k <- nrow(object) - n

  ## --- Core ---

  ret <- model.matrix(~ id - 1, ...)
  if(left) {
    ret <- cbind(matrix(0, nrow=n, ncol=k), ret)
  } else {
    ret <- cbind(ret, matrix(0, nrow=n, ncol=k))
  }

  ## --- End ---

  if(names) colnames(ret) <- object[[subject]]
  if(!names) attr(ret, which="dimnames") <- NULL
  ret
}

###------------------------------------------------------------------------
### model.matrix.R ends here
