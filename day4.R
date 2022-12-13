library(tidyverse)

input <- read_lines("input/day4.txt")

input2 <- str_split(input, ",")


### Day 4-1

contains <- function(intervals) {
  int_1 <- as.numeric(str_split(intervals[[1]], "-")[[1]])
  int_2 <- as.numeric(str_split(intervals[[2]], "-")[[1]])
  if (int_1[[1]] >= int_2[[1]] & int_1[[2]] <= int_2[[2]] |
      int_2[[1]] >= int_1[[1]] & int_2[[2]] <= int_1[[2]]) {
    return(TRUE)
  }
  return(FALSE)
}

sum(map_lgl(input2, contains))

### Day 4-2

overlaps <- function(intervals) {
  int_1 <- as.numeric(str_split(intervals[[1]], "-")[[1]])
  int_2 <- as.numeric(str_split(intervals[[2]], "-")[[1]])
  if (int_1[[1]] <= int_2[[2]] & int_1[[2]] >= int_2[[1]] |
      int_2[[1]] <= int_1[[2]] & int_2[[2]] >= int_1[[1]]) {
    return(TRUE)
  }
  return(FALSE)
}

sum(map_lgl(input2, overlaps))
