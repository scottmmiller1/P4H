# Element count function
# -------------------------------------------------------
# Count the number of times each answer was used
# results in a `quest'X`choices' matrix 
# quest and choices must be numeric
# Each represents the relevant number of questions and answer choices
# data is a matrix or dataframe of numeric answers
# results is the name of the resulting matrix
# percent = TRUE will calculate the percentage of responses for each answer choice
# percent == FALSE by default

elem_count <- function(data,quest,choices,percent = FALSE) {
  results <- matrix(0,quest,choices+1)
  if (percent == FALSE) {
    for (i in 1:quest) {
      for (j in 0:choices) {
        results[i,j+1] <- length(which(data[,i]==j))
      }
    }
    print(results)
  } else {
    for (i in 1:quest) {
      for (j in 0:choices) {
        results[i,j+1] <- length(which(data[,i]==j))/length(data[,i]==j)
      }
    }
    print(results)
  }
}
# -------------------------------------------------------