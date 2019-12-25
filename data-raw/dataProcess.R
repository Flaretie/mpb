one_char <- read.csv2('data-raw/1f.csv', encoding = "UTF-8", header = T)
twoChar <- read.csv2('data-raw/2f.csv', encoding = "UTF-8", header = F)

colnames(twoChar) <- colnames(one_char)

Encoding(levels(one_char$character)) <- "UTF-8"
Encoding(levels(twoChar$character)) <- "UTF-8"

tempChar <- do.call(rbind,sapply(as.character(twoChar$character), FUN = function(x) strsplit(x,"")))
for(j in 1:2) {
  twoChar[,LETTERS[j]]<- tempChar[,j] # create new column to store single character
  twoChar[,LETTERS[j]] <- unlist(lapply(twoChar[,LETTERS[j]],
                                        FUN = function(x) one_char[one_char$character == x,]$pct))
  #replace the single char by its global frequency
}
twoChar$real_log <- with(twoChar, log2(pct/(A*B)))


drugnames <- read.delim2("data-raw/data.txt", encoding = 'UTF-8', header = F)
drugnames <- as.data.frame(drugnames)

usethis::use_data(drugnames, overwrite = TRUE)
usethis::use_data(twoChar, overwrite = TRUE)
