#Problem1
# Create a variable hometown that stores the city in which you were born

hometown <- "Mountain View"

# Assign your name to the variable my_name
my_name <- "Emily"

# Assign the maximum number of push ups you can do to the variable num_push_ups
num_push_ups <- 10

# Create a boolean variable need_exercise to be TRUE if num_push_ups is lower than 5 else FALSE
need_exercise <- num_push_ups < 5

#Problem2
# Create an atomic vector vec1 with values 1, 2, 3, 4, 5
vec1 <- c(1, 2, 3, 4, 5)

# Use rep function to create a vector vec2 with all elements to be 10 and length to be 100
vec2 <- rep(10, times = 100)

# Repeat vector vec1 twice into c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5) and assign it to variable vec3
vec3 <- rep(vec1, times = 2)

# Repeat vector vec1 twice into c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5) and assign it to variable vec4
vec4 <- rep(vec1, each = 2)

# Use seq function to create an atomic vector vec5 with elements 0, 0.01, 0.02, ..., 1
vec5 <- seq(0, 1, by = 0.01)

#Problem3
#access rivers dataset
data(rivers)

#What is the type of rivers? (Atomic vector or list?)
typeof(rivers)
#answer = atomic vector 

#What is the type of the element in rivers? (double, integer, logical or string?)
class(rivers)
#answer = double 

# Log transform rivers and assign it a variable called log_rivers
log_rivers <- log(rivers)

# Create an atomic vector called summary with proper names for each element
summary <- c(
  length = length(log_rivers),
  sum = sum(log_rivers),
  mean = mean(log_rivers),
  median = median(log_rivers),
  min = min(log_rivers),
  max = max(log_rivers),
  sd = sd(log_rivers)
)

# Sort log_rivers in ascending order
log_rivers <- sort(log_rivers, decreasing = FALSE)

# Trim the 10 largest and 10 smallest elements in log_rivers
trimmed_log_rivers <- log_rivers[-c(1:10, (length(log_rivers) - 9):length(log_rivers))]

# Create summaries of trimmed_log_rivers and assign it to trimmed_summary
trimmed_summary <- c(
  length = length(trimmed_log_rivers),
  sum = sum(trimmed_log_rivers),
  mean = mean(trimmed_log_rivers),
  median = median(trimmed_log_rivers),
  min = min(trimmed_log_rivers),
  max = max(trimmed_log_rivers),
  sd = sd(trimmed_log_rivers)
)


#Problem4
# Create a list info with two items x and y
info <- list(x = c(5, 6, 7, 8), y = c("a", "b", "c", "d"))

# Modify y in info to have values c(1, 2, 3, 4)
info$y <- c(1, 2, 3, 4)

# Compute the mean of x and y separately and together
mean_x <- mean(info$x)
mean_y <- mean(info$y)
mean_both <- mean(c(info$x, info$y))

# Add log_x = log(x) into the list info
info$log_x <- log(info$x)

# Add distance = sqrt(x^2 + y^2) into the list info
info$distance <- sqrt(info$x^2 + info$y^2)

# Remove log_x from info
info$log_x <- NULL

#Problem5
#Create a vector words with elements "brilliant", "joy", "positivity", "strength", "health", "insight"
words <- c("brilliant", "joy", "positivity", "strength", "health", "insight")

# Append "is the word of the day!" to each element in words and assign it to words_of_the_day
words_of_the_day <- paste0(words, " is the word of the day!")

# Create a vector a_h_words which are elements in words that start with "a" through "h"
a_h_words <- words[substring(words, 1, 1) %in% c("a", "b", "c", "d", "e", "f", "g", "h")]

# Create a vector h_y_words which are elements in words that end with "h" through "y"
h_y_words <- words[substring(words, nchar(words), nchar(words)) %in% c("h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y")]

