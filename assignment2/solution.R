#Problem1: string indicator
#Create a function called is_string that takes in a variable and returns True if the variable is a string. We say a variable is a string if it is a character vector with length 1
is_string <- function(x) {
  if (is.character(x) && length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#Problem2: cumsum function
#Create a function called my_cumsum that mimic the functionality of cumsum in base R using a for-loop. For example if our input is c(1, 2, 3), we expect the output as c(1, 3, 6).
my_cumsum <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be a numeric vector.")
  }
  
  n <- length(x)
  cum_sum <- numeric(n)
  cum_sum[1] <- x[1]
  
  for (i in 2:n) {
    cum_sum[i] <- cum_sum[i - 1] + x[i]
  }
  
  return(cum_sum)
}

#Problem3: root mean square error
#Create a function called rmse that will return the root mean square error of a given numeric vector.
rmse <- function(x, na_rm = TRUE) {
  if (!is.numeric(x)) {
    stop("x must be a numeric vector.")
  }
  
  if (na_rm) {
    x <- x[!is.na(x)]
  }
  
  if (length(x) == 0) {
    stop("No non-NA elements in the input vector.")
  }
  
  rmse_value <- sqrt(sum(x^2) / length(x))
  return(rmse_value)
}

#Problem4: describe difference
#Create a function called describe_difference that takes in two strings and compare their lengths.
describe_difference <- function(x, y) {
  if (!is.character(x)) {
    stop("x must be a string.")
  }
  
  if (!is.character(y)) {
    stop("y must be a string.")
  }
  
  length_difference <- nchar(x) - nchar(y)
  if (length_difference > 0) {
    cat("Your first string is longer by", abs(length_difference), "characters\n")
  } else if (length_difference < 0) {
    cat("Your second string is longer by", abs(length_difference), "characters\n")
  } else {
    cat("Your strings are the same length\n")
  }
  
  invisible(length_difference)
}
