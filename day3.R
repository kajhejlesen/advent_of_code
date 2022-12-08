library(tidyverse)

input <- read_lines("input/day3.txt")

### Day 3-1
divide <- function(x) {
  len <- str_length(x)
  list(half_1 = str_sub(x, 1, len/2), half_2 = str_sub(x, len/2 + 1, len))
}

char_to_int <- function(x) {
  res <- utf8ToInt(x) - 96
  if (res < 0) { res <- res + 58}   # Capital letters
  return(res)
}

# Brute force ftw
find_common_char <- function(s1, s2) {
  s1 <- str_split(s1, pattern = "")
  s2 <- str_split(s2, pattern = "")
  for (char in s1[[1]]) {
    for (char2 in s2[[1]]) {
      if (char == char2) {
         return(char)
       }
    }
  }
}

splitted <- divide(input)
map2_chr(.x = splitted[[1]], .y = splitted[[2]], .f = find_common_char) %>%
  map_dbl(char_to_int) %>%
  sum()


### day 3-2

# more brute forcing, three strings
find_common_char <- function(s) {
  s1 <- str_split(s[[1]], pattern = "")
  s2 <- str_split(s[[2]], pattern = "")
  s3 <- str_split(s[[3]], pattern = "")
  for (char in s1[[1]]) {
    for (char2 in s2[[1]]) {
      for (char3 in s3[[1]]) {
        if (char == char2 & char2 == char3) {
          return(char)
        }
      }
    }
  }
}

# Creating matrix to force the right structure
input_matrix <- matrix(input, nrow = 3, ncol = length(input) / 3)
input_list <- apply(input_matrix, 2, as.list)

map_chr(input_list, find_common_char) %>%
  map_dbl(char_to_int) %>%
  sum()





  




