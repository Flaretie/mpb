#' @name nextWordMiner
#' @title Mining next word
#' @description Extend candidate words based on Bayesian probability.
#' @param n_char_final word table with n characters
#' @param numOfChar The number of character of candidate word, which should be equal to the number of characters in n_char_final
#' @param bayesianCutoff Bayesian conditional probability of occurrence of next word. The algorithm will drop
#'
#' @return List of new candidates words and words which cannot be extended.
#' @export
#' @examples
#' \dontrun{
#' nextWordMiner(df, 2, 0.3)
#' }

nextWordMiner <- function(n_char_final, numOfChar, bayesianCutoff) {
  if(length(n_char_final$character) == 0) {
    cl <- NULL
  } else {
    # initialize output list
    cl <- list("cont" = NULL, "stop" = NULL)
    # Split word with n characters
    # Combine the result into n_char_final dataframe
    temp_df <- do.call(rbind,sapply(as.character(n_char_final$character), FUN = function(x) strsplit(x,"")))
    for(j in 1:numOfChar) {
      n_char_final[,LETTERS[j]]<- temp_df[,j]
    }

    tail_char <- n_char_final[,LETTERS[numOfChar]] # the last character of the given word
    head_char <- n_char_final[,LETTERS[1]]        # the first character of the given word


    steps <- 1:length(unique(tail_char)) # create step for loop by traversal unique last character
    for(i in steps) {
      # if the last word "A" appear in the first word pool
      if(unique(tail_char)[i] %in% head_char) {
        # filte with bayesian stats
        tem_char_df <- feq_counterR(unique(tail_char)[i], segWords)
        tem_charB <- levels(droplevels(tem_char_df[tem_char_df$pct >= bayesianCutoff,]$word_list))
        if(length(tem_charB) == 0) {
          next()
        } else {
          tem_char <- n_char_final[n_char_final[,LETTERS[1]] == unique(tail_char)[i],]
          tem_char <- tem_char[tem_char$B %in% tem_charB,]$character
          # Get all characters before "A"
          tem_first_char <- n_char_final[rownames(
            n_char_final[n_char_final[,LETTERS[numOfChar]] ==
                           unique(tail_char)[i],]),][,LETTERS[numOfChar-1]]
          cl$cont <- append(cl$cont,unlist(lapply(tem_first_char,
                                                  function(tem_first_char) paste0(
                                                    tem_first_char,tem_char))))
        }
      } else {
        cl$stop <- append(cl$stop,
                          as.character(n_char_final[n_char_final[,LETTERS[numOfChar]] == unique(tail_char)[i],]$character))
      }
    }
  }
  return(cl)
}
