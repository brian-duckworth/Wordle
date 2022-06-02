

library(dplyr)
library(stringr)
library(crayon)


#### VARIABLES ####  

find_answer_automatically = FALSE
select_answer_method <- "nyt"  # input OR random OR nyt
input_answer <- "SCOFF"

answer_list <- (read.csv('/Users/brian/R/Projects/Wordle/answer_list.csv', header = FALSE))[[1]] # The Wordle answer list.
allowed_words_with_freq <- read.csv('/Users/brian/R/Projects/Wordle/allowed_words_with_freq.csv', header = TRUE)
candidateWordlist <- allowed_words_with_freq$word

guess <- "CRATE"
tries <- 0

exact <- "....."
included <- '' 
excluded <- ''
wrongSpot <- c('','','','','')
progressBars <- c()


####  GAME  ####


play <- function() {
  answer <<- select_answer(select_answer_method)
  cat("Guess the Wordle in six tries. \nEach guess must be a valid 5 letter word. Hit enter to submit.\n\n")
  while(guess != answer && tries < 7)
  { 
    if (tries == 6) {
      cat("Nice try. Better luck next time.\n")
      tries <<- tries + 1
    }else{
      guess <- newGuess()
      if (guess == answer){
        buildProgressBar(answer, guess)
        printProgressBars()
        cat(paste0("\nCongratulations, you nailed it on try ", tries, ".\n"))
      }
      else{
        evaluateGuess(answer, guess)
      }
    }
  }
}


####   UTILITIES  ####

select_answer <- function(method){
  if (method == "input"){
    answer = toupper(input_answer)
  } else if (method == "random"){
    answer = sample(answer_list, 1)
  } else if (method == "nyt"){
    answer = answer_list[as.integer(Sys.Date() - as.Date("2021-06-10"))]
  } else {
    select_answer_method <<- readline(prompt = "You need to choose a method to select and answer (input/random/nyt): ")
    select_answer(select_answer_method)
  }
  
}

newGuess <- function(){
  if (find_answer_automatically == FALSE){
    guess <- readline(prompt = "Enter your guess: ")
    guess <- toupper(guess)
  } else {
    ifelse(tries == 0, "CRATE", guess <- candidateWordlist[1])
  }
  if (qcGuess(guess)){
    tries <<- tries + 1
    return(guess)
  }
  return(newGuess())
}

qcGuess <- function(guess){
  if (!nchar(guess)==5){
    cat("Your guess must be 5 characters in lenght.\n")
    return(FALSE)
  } else if (!guess %in% allowed_words_with_freq$word) {
    cat("Your guess is not in the word list.\n")
    return(FALSE)
  } else {
    return (TRUE)
  }
}

buildProgressBar <- function(answer, guess){
  progressBar <- c(".", ".", ".", ".", ".")
  for (i in 1:5) {                                      # find exact matches
    if (substring(guess,i,i) == substring(answer,i,i)){
      progressBar[i] <- green(substring(guess,i,i))
      substring(answer,i,i) <- "-"                      # mark unavailable
      substring(guess,i,i) <- "-"
    }
  }
  for (i in 1:5){                                       # find included
    if (substring(guess,i,i) != "-" && grepl(substring(guess,i,i),answer)){
      progressBar[i] <- yellow(substring(guess,i,i))
      index = unlist(gregexpr(substring(guess,i,i), answer))
      substring(answer,index,index) <- "-"              # mark unavailable
      substring(guess,i,i) <- "-"
    }
  }
  for (i in 1:5) {                                       # find excluded
    if (substring(guess,i,i) != "-" && !grepl(substring(guess,i,i), answer)){
      progressBar[i] <- substring(guess,i,i)
    }
  }
  progressBars[tries] <<- paste0(progressBar, collapse = "")
  return(progressBar)
}

printProgressBars <- function(){
  for (i in 1:length(progressBars)){
    cat(paste0(progressBars[i], collapse = ""))
    cat("\n")
  }
}

evaluateGuess <- function(answer, guess){
  buildProgressBar(answer,guess)
  buildRegex(answer, guess)
  filterWords()
  printProgressBars()
  cat(paste0("You have narrorwed your candidate words down to ", length(candidateWordlist), ".\n"))
  if (find_answer_automatically == TRUE) {
    suffix <- ifelse(length(candidateWordlist) > 10, '...', '')
    cat(as.character(na.omit(candidateWordlist[1:10])))
    cat(suffix)
    cat("\n\n")
  }
  if (find_answer_automatically == FALSE){
    printWordlist <- readline(prompt = "Would you like to print the candidate words? (Y/N): ")
    if (printWordlist %in% c('Y', 'y', 'yes', 'Yes', 'YES')){
      cat(candidateWordlist)
      cat("\n\n")
    } else {
      cat("\n")
    }
  }
}

filterWords <- function(){
  exactFilter()
  includedFilter()
  excludedFilter()
  wrongSpotFilter()
}

exactFilter <- function(){
  exactRegex = ""
  if (exact != "....."){
    for (i in 1:5){
      if(substring(exact,i,i) == "."){
        exactRegex <- paste0(exactRegex, ".")
      }else{
        exactRegex <- paste0(exactRegex, "[", substring(exact,i,i), "]")
      }
    }
  }
  candidateWordlist <<- str_subset(candidateWordlist, exactRegex)
}

includedFilter <- function(){
  if (included != ""){
    for (i in 1:5){
      candidateWordlist <<- str_subset(candidateWordlist, substring(included,i,i))
    }
  }
}

excludedFilter <- function(){
  if (excluded != ""){
    candidateWordlist <<- str_subset(candidateWordlist, paste0("[", excluded, "]"), negate = TRUE)
  }
}

wrongSpotFilter <- function(){
  for (i in 1:5)
    if (wrongSpot[i] != ""){
      for (j in 1:nchar(wrongSpot[i])){
        if (i == 1) {candidateWordlist <<- str_subset(candidateWordlist, paste0("^", substring(wrongSpot[i], j, j)), negate = TRUE)}
        if (i == 2) {candidateWordlist <<- str_subset(candidateWordlist, paste0("^.", substring(wrongSpot[i], j, j)), negate = TRUE)}
        if (i == 3) {candidateWordlist <<- str_subset(candidateWordlist, paste0("^..", substring(wrongSpot[i], j, j)), negate = TRUE)}
        if (i == 4) {candidateWordlist <<- str_subset(candidateWordlist, paste0("^...", substring(wrongSpot[i], j, j)), negate = TRUE)}
        if (i == 5) {candidateWordlist <<- str_subset(candidateWordlist, paste0("^....", substring(wrongSpot[i], j, j)), negate = TRUE)}
      }
    }
}

buildRegex <- function(answer, guess){
  for (i in 1:5) {                                      # Go thru guess one letter at a time.
    if (substring(guess,i,i) == substring(answer,i,i)){ # If exact match...
      substring(exact, i, i) <- substring(guess,i,i)    # ...add letter to exact string.
    }
    else if (grepl(substring(guess,i,i),answer)){       # Else if included in word...
      included <- paste(included, substring(guess,i,i), sep = "")  # ...add to included string...
      wrongSpot[i] <- paste(wrongSpot[i], substr(guess,i,i), sep = "")        # ...and wrongSpot array.
    }
    else{
      excluded <- paste(excluded, substring(guess,i,i), sep = "")
    }
  }
  exact <<- exact
  included <<- included
  excluded <<- excluded
  wrongSpot <<- wrongSpot
}


####  RESOURCES  ####

# WORD FREQUENCY LIST - https://www.kaggle.com/datasets/rtatman/english-word-frequency?resource=download



####  PLAYGROUND  ####


# # test efficiency
# total_games = 0
# score_total = 0
# for (i in 1:1000){
#   play()
#   total_games <<- total_games + 1
#   score_total <<- score_total + tries
# }
# print(paste("Average score:", score))

# words %>% str_subset('..ert') 
# words %>% str_subset('.urge')  %>% str_subset("[catny]", negate = T)
# words %>% str_subset("[nos]")
# candidateWordlist <- words %>% str_subset("[N]") %>%  str_subset("[O]") %>%  str_subset("[S]") %>% str_subset("[CRATEIY]", negate = T) %>% str_subset("^[C]", negate = T) %>% str_subset("^.[O]", negate = T) %>% str_subset("^...[S]", negate = T) 
# print(candidateWordlist)




# df %>% 
#   rowwise() %>% 
#   filter(sum(str_count(words, letters))==nchar(words)) 

# words <- readLines("/usr/share/dict/words")
# words <- str_subset(words, '^.....$')

#  Getting word frequencies. Table downloaded from https://www.kaggle.com/datasets/rtatman/english-word-frequency?resource=download
# word_frequency <- read.csv('/Users/brian/R/Projects/Wordle/unigram_freq.csv', header = TRUE)
# word_frequency$word <- toupper(word_frequency$word)
# allowed_words_minus_answers_nyt <- (read.csv('/Users/brian/R/Projects/Wordle/allowed_words_minus_answers_nyt.csv', header = FALSE))[[1]] # From Wordle.
# all_allowed_words <- toupper(c(answer_list, allowed_words_minus_answers_nyt))
# all_allowed_words_tibble <- bind_rows(tibble(word = all_allowed_words))
# word_frequency_table <- all_allowed_words_tibble %>% left_join(word_frequency, by = c("word"))
# word_frequency_table <- word_frequency_table %>% arrange(desc(count))
# write.csv(word_frequency_table, '/Users/brian/R/Projects/Wordle/allowed_words_with_freq.csv')



