# 
# Exploring literary data
#

library(shiny)
library(tidyverse)
library(readxl)
library(cowplot)
library(shinythemes)
library(shinyWidgets)
library(RColorBrewer)
library(ggiraph)
library(DescTools)

bookAwards <- read_csv('./input/bookAwards.csv') %>%
  mutate(sitePrefRating = ifelse((ratingGR - ratingLT) <= 0, 'LibraryThing', 'Goodreads')) %>%
  mutate(awardName = ifelse(awardName == 'Pulitzer Prize (Fiction)', 'Pulitzer Prize', awardName))

bookLists <- read_csv('./input/bookLists.csv') %>%
  mutate(sitePrefRating = ifelse((ratingGR - ratingLT) <= 0, 'LibraryThing', 'Goodreads'))

bookAuthors <- read_csv('./input/bookAuthors.csv') %>%
  mutate(sitePrefRating = ifelse((ratingGR - ratingLT) <= 0, 'LibraryThing', 'Goodreads'),
         authorName = paste(authorFirst, authorLast))

