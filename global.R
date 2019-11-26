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

# TODO list
# DONE: 15-11: Finish updating original books Excel workbook
# DONE: 16-11: Finish initial awards: Pulitzer, Hugo, Nebula, Otherwise
# DONE: 17-11: Add 2019 winners (except Otherwise)
# DONE: 16-11: Create script (scrapeGRLT.R) to pull GR and LT data
# DONE: 16-11: Create base scatterplot
# DONE: 17-11: Add reactivity to scatterplot
# Create base histogram / density plot
# Create base boxplot
# DONE: 19-11: Update reading lists in workbook data
# DONE: 18-11: Add tab for similar exploration of reading lists
# DONE: 20-11: Changed GAM spline method to have 3 basis
# DONE: 21-11: Add size option for all scatterplots
# DONE: 21-11: Add tab for individual prolific authors: 
# # Iain Banks
# # Richard Brautigan
# # DONE: Michael Crichton
# # Don DeLillo
# # Philip K. Dick
# # William Faulkner
# # Neil Gaiman
# # Robert Heinlein
# # Frank Herbert (& Brian)
# # Russell Hoban
# # DONE: Stephen King
# # Arnold Lobel
# # Terry Pratchett
# # William Shakespeare
# # DONE (partial): R.L. Stine
# Add filter options for author page:
# # pseudonym = N
# # authorCount = Primary
# # posthumous = N
# look into graphic novel options?
# Add series tab:
# # Star Wars
# # Goosebumps (transfer)
# # Star Trek
# Add additional awards:
# # BSFA Award (1969)
# # Ditmar Award (1969)
# # Ditmar Award for International Writing (1969)
# # Locus Award for SF (1971)
# # Mythopoeic Award (1971)
# # August Derleth Award (1972)
# # Campbell Award (1973)
# # World Fantasy Award (1975)
# # Locus Award for Fantasy (1978)
# # Prometheus Award (1979)
# # Locus Award for First Novel (1981)
# # PKD Award (1982)
# # Aurora Award (1982)
# # Bram Stoker Award (1987)
# # Clarke Award (1987)
# # Hal Clement Award (1992)
# # Golden Duck Paberback Award (1992)
# # Eleanor Cameron Award (1992)
# # Mythopoeic Award for YA (1992)
# # Aurealis Award for YA (1995)
# # Aurealis Award for SF (1995)
# # Aurealis Award for H (1995)
# # Aurealis Award for F (1995)
# # Ditmar Award for Stories (2000)
# # Aurealis Award for Children's (2001)
# # Locus Award for YA (2003)
# # Cybils Award for SFF YA (2006)
# # Andre Norton Award (2006)
# # Cybils Award for SFF Children's (2007)
# # Shirley Jackson Award (2007)
# # Aurealis Award for Anthology (2008)
# # Aurealis Award for GN (2008)
# # Legend Award (2009)
# # Red Tentacle Award (2009)
# # Morningstar Award (2010)
# # Golden Tentacle Award (2010)
# # Robert Holdstock Award (2012)
# Add a request form for authors or lists or whatever
# Add a dynamic personal rating form

