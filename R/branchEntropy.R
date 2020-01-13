#' @name branchEntropy
#' @title The branch entropy of Chinese words with two characters.
#' @description This function ccalculate both inner and outer entropy of candidate words.
#' chinese character frequency Statistics table.
#' @param two_chars Chinese word with two characters.
#' @return branch entropy of target word.
#' @export
#' @examples
#' \donttest{
#' further examples for users (not used for checks)
#' branchEntropy("test character")
#' }

branchEntropy <- function(two_chars) {
  candidate <- strsplit(two_chars,"")[[1]]
  #inner
  h_l_r <- feq_counter_R(candidate[1], segWords)
  h_r_l <- feq_counter_L(candidate[2], segWords)

  #outer
  h_r <- feq_counter_R(candidate[2], segWords)
  h_l <- feq_counter_L(candidate[1], segWords)
  return(min(h_r,h_l)-min(h_l_r,h_r_l))
}

feq_counterR <- function(cand_word, segWords) {

  word_list <- c()
  #assign("word_list", c(), envir = .GlobalEnv)

  for (j in seq_along(segWords)) {
    if (cand_word == segWords[j])
      word_list <- c(word_list, segWords[j+1])
    else
      next
  }
  word_list <- word_list
  #assign("word_list", word_list, envir = .GlobalEnv)
  char_table <- as.data.frame(table(word_list))
  char_table <- char_table[order(char_table$Freq, decreasing = T),]
  rownames(char_table) <- NULL

  char_table$pct <- with(char_table, Freq/sum(char_table$Freq))
  char_table$global_fequency <- with(char_table, Freq/length(segWords))
  entropy <- sum(unlist(lapply(char_table$pct, function(x) -x*log2(x))))
  #assign("entropy", sum(unlist(lapply(char_table$pct, function(x) -x*log2(x)))), envir = .GlobalEnv)
  return(char_table)
}

feq_counter_R <- function(cand_word, segWords) {
  feq_counterR(cand_word, segWords)
  return(entropy)
}

feq_counterL <- function(cand_word, segWords) {
  word_list <- c()
  #assign("word_list", c(), envir = .GlobalEnv)

  for (j in seq_along(segWords)) {
    if (j == 1)
      next
    if (cand_word == segWords[j])
      word_list <- c(word_list, segWords[j-1])
    else
      next
  }
  word_list <- word_list
  #assign("word_list", word_list, envir = .GlobalEnv)
  char_table <- as.data.frame(table(word_list))
  char_table <- char_table[order(char_table$Freq, decreasing = T),]
  rownames(char_table) <- NULL

  char_table$pct <- with(char_table, Freq/sum(char_table$Freq))
  entropy <- sum(unlist(lapply(char_table$pct, function(x) -x*log2(x))))
  #assign("entropy", sum(unlist(lapply(char_table$pct, function(x) -x*log2(x)))), envir = .GlobalEnv)
  return(char_table)
}

feq_counter_L <- function(cand_word, segWords) {
  feq_counterL(cand_word, segWords)
  return(entropy)
}


# inner entropy calculate
#innerH <- function(two_chars) {
#    h_l_r <- feq_counter_R(strsplit(two_chars,"")[[1]][1], segWords)
#    h_r_l <- feq_counter_L(strsplit(two_chars,"")[[1]][2], segWords)
#    return(min(h_l_r,h_r_l))
#}

# outer entropy calculate
#outerH <- function(two_chars) {
#    h_r <- feq_counter_R(strsplit(two_chars,"")[[1]][2], segWords)
#    h_l <- feq_counter_L(strsplit(two_chars,"")[[1]][1], segWords)
#    return(min(h_r,h_l))
#}
