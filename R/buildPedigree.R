### buildPedigree.R
###------------------------------------------------------------------------
### What: Build pedigree for given individual(s)
### $Id: buildPedigree.R 1092 2006-11-12 12:28:38Z ggorjan $
### Time-stamp: <2006-09-26 20:34:44 ggorjan>
###------------------------------------------------------------------------

if(FALSE) {

buildPedigree <- function(x, id)
{
  ## Build pedigree (up or down) for given individual(s)
  ## - I know hot to do it up
  ## - how to do it down?
  ##
  ## write getAscendant() and getDescendant() to get individuals and then
  ## simply collect rows from x and put them together
}

## Get something better than up for argument name; there should also be
## possibilty to use both "up" and "down" in buildPedigree()!

getAscendant <- function(x, id, up=TRUE)
{
  ## Get ascendants (relatives up) for given individuals(s)
}

getDescendant <- function(x, id)
{
  ## Get descendants (relatives down) for given individuals(s)
  getAscendant(x, id, up=FALSE)
}

getRelative <- function(x, id)
{
  ## Get relatives (up or down) for given individuals(s)
  ## But is this only
  ##
  ## ...
  ## ff fm  mf mm
  ##   f      m
  ##      a
  ##   p1   p2
  ## gp1 gp2 gp3 gp4
  ## ...
  ##
  ## Or should we list all individuals that are in relationship with
  ## individual i.e. all individuals that have A[,] > 0?

  ## The second option, as first one will be covered with getAscendant()
  ## and getDescendant()
}

}

###------------------------------------------------------------------------
### buildPedigree.R ends here
