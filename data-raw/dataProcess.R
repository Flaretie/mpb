one_char <- rwstats::oneChar
two_char <- rwstats::twoChar
drugnames <- read.delim2("data-raw/data.txt", encoding = 'UTF-8', header = F)

drugnames <- as.data.frame(drugnames)

tempChar <- do.call(rbind,sapply(as.character(two_char$character), FUN = function(x) strsplit(x,"")))
for(j in 1:2) {
  two_char[,LETTERS[j]]<- tempChar[,j] # create new column to store single character
  two_char[,LETTERS[j]] <- unlist(lapply(two_char[,LETTERS[j]],
                                        FUN = function(x) one_char[one_char$character == x,]$pct))
  #replace the single char by its global frequency
}
two_char$real_log <- with(two_char, log2(pct/(A*B)))

Encoding(levels(two_char$character)) <- "UTF-8"
Encoding(levels(drugnames$V1)) <- "UTF-8"


usethis::use_data(drugnames, overwrite = TRUE)
usethis::use_data(two_char, overwrite = TRUE)
