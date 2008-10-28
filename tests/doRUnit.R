### doRUnit.R
###------------------------------------------------------------------------
### What: Run unit tests with RUnit
### $Id$
### Time-stamp: <2007-04-19 00:32:42 ggorjan>
###------------------------------------------------------------------------

## unit tests will not be done if RUnit is not available
if(require("RUnit", quietly=TRUE)) {

  ## --- Setup ---

  pkg <- "GeneticsPed"
  if(Sys.getenv("RCMDCHECK") == "FALSE") {
    ## Path to unit tests for standalone running under Makefile (not R CMD check)
    ## PKG/tests/../inst/unitTests
    path <- file.path(getwd(), "..", "inst", "unitTests")
  } else {
    ## Path to unit tests for R CMD check
    ## PKG.Rcheck/tests/../PKG/unitTests
    path <- system.file(package=pkg, "unitTests")
  }
  cat("\nRunning unit tests\n")
  print(list(pkg=pkg, getwd=getwd(), pathToUnitTests=path))

  library(package=pkg, character.only=TRUE)

  ## --- Testing ---

  ## Define tests
  testSuite <- defineTestSuite(name=paste(pkg, "unit testing"),
                                          dirs=path)
  ## Run
  tests <- runTestSuite(testSuite)

  ## Default report name
  pathReport <- file.path(path, "report")

  ## Report to stdout and text files
  cat("------------------- UNIT TEST SUMMARY ---------------------\n\n")
  printTextProtocol(tests, showDetails=FALSE)
  printTextProtocol(tests, showDetails=FALSE,
                    fileName=paste(pathReport, "Summary.txt", sep=""))
  printTextProtocol(tests, showDetails=TRUE,
                    fileName=paste(pathReport, ".txt", sep=""))

  ## Report to HTML file
  ## NOTE from H. Pages <hpages@fhcrc.org> from the Gentleman Lab (Oct 16,
  ##   2008): I've added the test below because printHTMLProtocol() seems
  ##   to be broken in RUnit 0.4.19 on the OS X platform.
  if (substring(R.Version()$os, 1, 6) != "darwin") 
    printHTMLProtocol(tests, fileName=paste(pathReport, ".html", sep=""))

  ## Return stop() to cause R CMD check stop in case of
  ##  - failures i.e. FALSE to unit tests or
  ##  - errors i.e. R errors
  tmp <- getErrors(tests)
  if(tmp$nFail > 0 | tmp$nErr > 0) {
    stop(paste("\n\nunit testing failed (#test failures: ", tmp$nFail,
               ", #R errors: ",  tmp$nErr, ")\n\n", sep=""))
  }
} else {
  warning("cannot run unit tests -- package RUnit is not available")
}

###------------------------------------------------------------------------
### doRUnit.R ends here
