### runit.unknown.R
###------------------------------------------------------------------------
### What: Unit tests for "unknown" methods
### $Id: runit.unknown.R 1097 2006-11-14 06:10:43Z ggorjan $
### Time-stamp: <2006-11-12 10:29:48 ggorjan>
###------------------------------------------------------------------------

### {{{ --- Test setup ---

if(FALSE) {
  library("RUnit")
  library("GeneticsPed")
}

### }}}
### {{{ --- TODO ---

test.TODO <- function() {
  a <- 1
  checkEquals(sum(a, 1), 2)
}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### runit.unknown.R ends here
