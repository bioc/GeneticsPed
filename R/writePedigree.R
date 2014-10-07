### writePedigree.R
###------------------------------------------------------------------------
### What: Write pedigree in various formats
### $Id: writePedigree.R 1092 2006-11-12 12:28:38Z ggorjan $
### Time-stamp: <2006-09-30 01:11:07 ggorjan>
###------------------------------------------------------------------------

## LINKAGE in PRE-MAKEPED format --> this should be alpha and omega as
## Mega2, has lots of possibilities to convert to other formats, but we
## need mapLevels() before to get integer codes - is mapLevels() fast
## enough for large scale?

writePreMakeped <- function()
{
  ## do we need write.fwf here?
}

## MENDEL FORMAT --> should be easy due to CSV so I write it here

writeMendel <- function(x, alleleSep="-")
{
  x <- checkAttributes(x)
  if(is.na(attr(x, ".family"))) family(x) <- ???

  ## Genotypes ?

  x <- NAToUnknown(x, unknown="")
  write.csv(x)
}

###------------------------------------------------------------------------
### writePedigree.R ends here
