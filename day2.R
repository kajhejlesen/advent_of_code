library(tidyverse)

input <- read_delim("input/day2.txt", col_names = FALSE)

### Day 2-1
opponent <- c("A" = 0, "B" = 1, "C" = 2)
hero     <- c("X" = 0, "Y" = 1, "Z" = 2)

score <- input %>%
  mutate(
    result = (opponent[X1] - hero[X2]) %% 3,
    score = hero[X2] + 1 + case_when(
      result == 1 ~ 0,
      result == 2 ~ 6,
      TRUE        ~ 3
      )
  )

sum(score$score)


### Day 2-2
should_win <- c("X" = 2, "Y" = 0, "Z" = 1)
hero_choose <- c("X", "Y", "Z")

score2 <- input %>%
  mutate(
    choice = hero_choose[(opponent[X1] + should_win[X2]) %% 3 + 1],
    result = (opponent[X1] - hero[choice]) %% 3,
    score = hero[choice] + 1 + case_when(
      result == 1 ~ 0,
      result == 2 ~ 6,
      TRUE        ~ 3
    )
  )
  
sum(score2$score)

