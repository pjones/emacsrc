#!/usr/bin/env Rscript

##############################################################################
package <- function (name, ...) {
  ## TODO: skip packages that are already installed
  mirror <- "http://cran.cnr.Berkeley.edu"
  lib <- path.expand("~/.R/lib")
  install.packages(name, repos=mirror, lib=lib, dependencies=TRUE, ...)  
}

##############################################################################
package("financial")
package("portfolio")
