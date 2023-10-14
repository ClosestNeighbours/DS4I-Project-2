
library(stringr)
library(tidytext)
library(tidyverse)



source("https://raw.githubusercontent.com/ClosestNeighbours/DS4I-Project-2/LDA---Puja/editted_sona_first_steps.R")


sona$date[36] <- "09-02-2023"

x <- sona$speech


y <- sub('^\\w*\\s*\\w*\\s*\\w*\\s*', '', x[1:34])

sona$speech[1:34] <- y

z <- sub("^[A-Za-z]+, \\d{1,2} [A-Za-z]+ \\d{4}  ", "", x[35])

sona$speech[35] <- z

a <- sub("\\d{1,2} [A-Za-z]+ \\d{4}", "", x[36])

sona$speech[36] <- a
