#' @title The branch entropy of Chinese words with two characters.
#' @description
#' This is a function that lists the next word appearing frequency
#' by a given chinese character based on real world
#' chinese character frequency Statistics table.
#' @param candidateWord A single Chinese character
#' @param topN The number of raws of output dataframe
#' @inheritParams segWords
#' @return A dataframe containing the next word and its occurrence and frequency
#' @export
#' @examples
#' \dontrun{
#' innerH("普通")
#' outerH("可以")
#' }

# inner entropy calculator
innerH <- function(two_char) {

  if(exists("segWords")) {
    h_l_r <- feq_counter_R(strsplit(two_char,"")[[1]][1], segWords)
    h_r_l <- feq_counter_L(strsplit(two_char,"")[[1]][2], segWords)
    return(min(h_l_r,h_r_l))
  } else {
    warning("Please loading your file first.")
  }
}

#' @export
# outer entropy calculator
outerH <- function(two_char) {

  if(exists("segWords")) {
    h_r <- feq_counter_R(strsplit(two_char,"")[[1]][2], segWords)
    h_l <- feq_counter_L(strsplit(two_char,"")[[1]][1], segWords)
    return(min(h_r,h_l))
  } else {
    warning("Please loading your file first.")
  }
}


# Nested function -----
feq_counterR <- function(cand_word, segWords) {

  word_list <<- c()

  for (j in seq_along(segWords)) {
    if (cand_word == segWords[j])
      word_list <<- c(word_list, segWords[j+1])
    else
      next
  }
  char_table <- as.data.frame(table(word_list))
  char_table <- char_table[order(char_table$Freq, decreasing = T),]
  rownames(char_table) <- NULL

  char_table$pct <- with(char_table, Freq/sum(char_table$Freq))
  char_table$global_fequency <- with(char_table, Freq/length(segWords))
  entropy <<- sum(unlist(lapply(char_table$pct, function(x) -x*log2(x))))
  return(char_table)
}

feq_counter_R <- function(cand_word, segWords) {
  feq_counterR(cand_word, segWords)
  return(entropy)
}

feq_counterL <- function(cand_word, segWords) {

  word_list <<- c()

  for (j in seq_along(segWords)) {
    if (j == 1)
      next
    if (cand_word == segWords[j])
      word_list <<- c(word_list, segWords[j-1])
    else
      next
  }
  char_table <- as.data.frame(table(word_list))
  char_table <- char_table[order(char_table$Freq, decreasing = T),]
  rownames(char_table) <- NULL

  char_table$pct <- with(char_table, Freq/sum(char_table$Freq))
  entropy <<- sum(unlist(lapply(char_table$pct, function(x) -x*log2(x))))
  return(char_table)
}

feq_counter_L <- function(cand_word, segWords) {
  feq_counterL(cand_word, segWords)
  return(entropy)
}
