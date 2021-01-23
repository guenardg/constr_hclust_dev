#!/usr/bin/env Rscript
setwd("constr.hclust")
system("R CMD SHLIB src/constr.hclust.c")
roxygen2::roxygenise()
system("rm -f src/*.so")
system("rm -f src/*.o")
system("find -type f \\( -not -name \"MD5\" \\) -exec md5sum '{}' \\; > MD5")
