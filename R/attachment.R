# install required packages
list.of.packages <- c("readr",
                      "reshape2",
                      "dplyr",
                      "plyr",
                      "tidyr",
                      "data.table",
                      "psych",
                      "rwstats",
                      "pbapply",
                      "progress")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, require, character.only = TRUE))
