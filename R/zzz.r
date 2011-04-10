######################################################################
#
# zzz.r
#
# .First.lib is run when the package is loaded with library(tnet)
#
######################################################################

.First.lib <- function(lib, pkd){
  cat("tnet: Analysis of Weighted, Two-mode, and Longitudinal networks.\nType ?tnet for help.\n")
}
