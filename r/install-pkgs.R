#!/usr/bin/env Rscript

##############################################################################
.libPaths(path.expand("~/.R/lib"))
all.packages <- installed.packages()

##############################################################################
package <- function (name, ...) {
  installed <- as.character(all.packages[,1][name])
  
  if (identical(installed, as.character(NA))) {
    mirror <- "http://cran.cnr.Berkeley.edu"
    lib <- .libPaths()[1]
    install.packages(name, lib=lib, repos=mirror, dependencies=TRUE, ...)
  }
}

##############################################################################
package("financial")
package("portfolio")
package("zoo")
package("ggplot2")
