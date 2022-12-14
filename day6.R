library(tidyverse)

input <- read_lines("input/day6.txt")

input_vector <- str_split(input, pattern = "") %>% unlist()

is_different <- function(x, n) {
  return(length(unique(x)) == n)
}

### Day 6-1
for (i in seq(1,(length(input_vector) - 3))) {
  if (is_different(input_vector[i:(i + 3)], 4)) {
    print(i + 3)
    break
  }
}

### Day 6-2
for (i in seq(1,(length(input_vector) - 13))) {
     if (is_different(input_vector[i:(i + 13)], 14)) {
       print(i + 13)
       break
     }
}
