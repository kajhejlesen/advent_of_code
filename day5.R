library(tidyverse)

input <- read_lines("input/day5.txt")

# Dividing into stacks and instructions
instructions <- input[which(input %>% str_starts("move"))]
boxes_wide <- input[which(input %>% str_starts("(\\[)|\\ \\ "))]

# Getting number of stacks and setting up for extracting just the information we need
number_stacks <- as.numeric(str_sub(boxes_wide[length(boxes_wide)], -2, -2))
to_extract <- seq(2, number_stacks * 4 - 2, 4)

# data wrangling
boxes_vector <- lapply(boxes_wide, str_sub, start = to_extract, end = to_extract) %>% unlist()

stacks <- matrix(boxes_vector, ncol = length(boxes_vector) / number_stacks) %>% t() %>%
  apply(2, list) %>%
  lapply(unlist) %>%
  lapply(rev) %>%
  lapply(str_extract, pattern = "[A-Z]") %>%
  lapply(function(x) {x[x != " " & !is.na(x)]})

stacks_for_part2 <- stacks


### Day 5-1

# Defining functions to do the muscle work
move_box <- function(stacks, from, to) {
  to_move <- stacks[[from]][length(stacks[[from]])]
  stacks[[from]] <- stacks[[from]][-length(stacks[[from]])]  # Remove element
  stacks[[to]] <- c(stacks[[to]], to_move)                   # Add element
  return(stacks)
}

move_boxes <- function(stacks, from, to, times) {
  for (i in seq(1:times)) {
    stacks <- move_box(stacks, from, to)
  }
  return(stacks)
}

# handling instructions
inst <- instructions %>%
  lapply(str_split, pattern = " ") %>%
  lapply(unlist) %>%
  lapply(str_extract, pattern = "[0-9]+") %>%
  lapply(function(x){as.numeric(x[!is.na(x)])})


# Execute orders!
for (order in inst) {
  stacks <- move_boxes(stacks, from = order[2], to = order[3], times = order[1])
}

lapply(stacks, function(x) {x[length(x)]}) %>% unlist() %>% str_c(collapse = "")


### Day 5-2
move_boxes_retain <- function(stacks, from, to, times) {
  number_boxes <- length(stacks[[from]])
  boxes_to_move <- stacks[[from]][(number_boxes - times + 1):number_boxes]
  stacks[[from]] <- stacks[[from]][0:(number_boxes - times)]  # Remove elements
  stacks[[to]] <- c(stacks[[to]], boxes_to_move)     
  return(stacks)
}

stacks <- stacks_for_part2

# Execute orders!
for (order in inst) {
  stacks <- move_boxes_retain(stacks, from = order[2], to = order[3], times = order[1])
}

lapply(stacks, function(x) {x[length(x)]}) %>% unlist() %>% str_c(collapse = "")

