#!/usr/bin/env Rscript

.libPaths(path.expand("~/.R/lib"))
mirror <- "http://cran.cnr.Berkeley.edu"

update.packages(repos=mirror,
                ask=FALSE,
                lib.loc=.libPaths(),
                lib=.libPaths()[1])
