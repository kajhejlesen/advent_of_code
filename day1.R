library(tidyverse)

input <- read_lines("input/day1.txt") %>%
  as.numeric()

### Day 1 - 1
sum <- 0
maxsum <- 0
for (i in seq(1:length(input))) {
  value <- input[i]
  if (is.na(value)) {
    maxsum <- ifelse(sum > maxsum, sum, maxsum)
    sum <- 0
  } else {
    sum <- value + sum
  }
}

maxsum


### day 1 - 2
top_3_cal <- input %>%
  as_tibble() %>%
  mutate( group_n = cumsum(is.na(value)) ) %>%
  filter(!is.na(value)) %>%
  group_by(group_n) %>%
  summarize( elves_totcal = sum(value) ) %>%
  arrange(elves_totcal) %>%
  slice_tail(n = 3) %>%
  pull(elves_totcal)

sum(top_3_cal)
