onecharFreq <- function(data) {
  tem <- strsplit(data,"")[[1]]
  temFeq <- as.data.frame(table(tem))
  temFeq <- temFeq[order(temFeq$Freq, decreasing = T),]
  colnames(temFeq) <- c('character','freq')
  rownames(temFeq) <- NULL
  temFeq$pct <- with(temFeq, freq/sum(temFeq$freq))

  return(temFeq)
}
ncharFreq <- function(data, x, charfeq = NULL) {
  #split raw data
  data <- strsplit(data,"")[[1]]
  tem <- data

  # create word list with "n" character
  # eg:   BEFROE: ABCDEFGH
  #       AFTER : AB BC CD DE EF FG GH
  for(i in 1:(x-1)) {
    tem<- paste0(tem,data[i+1:length(data)])
  }
  tem <- tem[1:length(data)]
  temFeq <- as.data.frame(table(tem))

  # calculate word frequency and sort the value in descending order
  # reset column names and index
  temFeq <- temFeq[order(temFeq$Freq, decreasing = T),]
  colnames(temFeq) <- c('character','freq')
  rownames(temFeq) <- NULL
  temFeq$pct <- with(temFeq, freq/sum(temFeq$freq))

  # drop row with non-chinese character
  temFeq <- temFeq[temFeq$character %in%
                     regmatches(as.character(temFeq$character),
                                regexpr("^[\u4e00-\u9fa5]{0,}$",
                                        as.character(temFeq$character),
                                        perl = TRUE)),]

  if(x == 2 & identical(charfeq, userOneChar)) {
    # calcualte character frequency in each word
    temp_df <- do.call(rbind,sapply(as.character(temFeq$character), FUN = function(x) strsplit(x,"")))
    for(j in 1:x) {
      temFeq[,LETTERS[j]]<- temp_df[,j] # create new column to store single character
      temFeq[,LETTERS[j]] <- unlist(lapply(temFeq[,LETTERS[j]],
                                           FUN = function(x) charfeq[charfeq$character == x,]$pct))
      #replace the single char by its global frequency
    }
    temFeq$test_log <- with(temFeq, log2(pct/(A*B)))
  }
  return(temFeq)
}
