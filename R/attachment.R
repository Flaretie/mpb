# install required packages
list.of.packages <- c("dplyr",
                      "rwstats",
                      "pbapply",
                      "progress",
                      "stats",
                      "graphics",
                      "cli",
                      "crayon")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, require, character.only = TRUE))

