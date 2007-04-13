### generateData.R
###------------------------------------------------------------------------
### What: Generate data based on pedigree
### $Id: generateData.R 1092 2006-11-12 12:28:38Z ggorjan $
### Time-stamp: <2006-09-26 20:33:31 ggorjan>
###------------------------------------------------------------------------

## - create base
## - "evolve" i.e. random mating, selection
##   would we need addIndividual() here?

## generateData(nGenerations=10,
##              nBase=1000,
##              base=generateBase(),
##              evolve/select=evolve/select(),
##              mate="random|else correlation for assortative",
##              effects="is it possible to create specific for lab. experiments
##                       and national gen. system",
##              ...)

## generateBase(n=nBase,
##              mu,
##              Sigma)
## {
##   mvrnorm(n=n, mu=, Sigma)
##   ## Multitrait
## }

## a[i] <- 0.5 * sire[i] + 0.5 * dam[i] + rnorm(0, sigma^2_mendelian)

###------------------------------------------------------------------------
### generateData.R ends here
