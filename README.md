# Exploring literary data
Shiny dashboard for exploring book-related data for popular awards, best-of lists, and authors.

## Project update history

2020-03-01:
* Added World Fantasy Award.

2019-11-21: 
* Added tab for individual prolific authors, starting with Neil Gaiman, Stephen King, Alice Sheldon, R.L. Stine, and Michael Crichton.
* Added size option for scatterplots.

2019-11-20:
* Changed GAM spline method to have 3 basis.

2019-11-19:
* Updated lists tab to fill out missing data.

2019-11-18:
* Added tab for famous reading lists, starting with American Book Review, and Modern Library.

2019-11-17:
* Added 2019 award winners to all awards except the Otherwise Award.
* Added reactivity to scatterplots.

2019-11-16:
* Finished initial awards dataset, starting with Pulitzer, Hugo, Nebula, and the Otherwise Awards.
* Created toolset to automatically update statistics.
* Created base scatterplot.

2019-11-15:
* Updated books data workbook (.xlsx) from original 2018 dataset.

## TODO list

* **Authors**: 
  * Iain Banks
  * Richard Brautigan
  * Don DeLillo
  * Philip K. Dick
  * William Faulkner
  * Robert Heinlein
  * Frank Herbert & Brian Herbert
  * Russell Hoban
  * Arnold Lobel
  * Terry Pratchett
  * William Shakespeare
* **Author tab filters**: 
  * pseudonym == 'N'
  * authorCount == 'Primary'
  * posthumous == 'N'
* **Awards**:  
  * BSFA Award
  * Ditmar Award
  * Ditmar Award for International Writing
  * Locus Award for SF
  * Mythopoeic Award
  * August Derleth Award
  * Campbell Award
  * World Fantasy Award
  * Locus Award for Fantasy
  * Prometheus Award
  * Locus Award for First Novel
  * PKD Award
  * Aurora Award
  * Bram Stoker Award
  * Clark Award
  * Hal Clement Award
  * Golden Duck Paperback Award
  * Eleanor Cameron Award
  * Mythopoeic Award for YA
  * Aurealis Award for YA
  * Aurealis Award for SF
  * Aurealis Award for Horror
  * Aurealis Award for Fantasy
  * Ditmar Award for Stories
  * Aurealis Award for Children's Literature
  * Locus Award for YA
  * Cybils Award for SFF YA
  * Shirley Jackson Award
  * Aurealis Award for Anthology
  * Aurealis Award for GN
  * Legend Award
  * Red Tentacle Award
  * Morningstar Award
  * Golden Tentacle Award
  * Robert Holdstock Award
* **Sub-tabs**: 
  * Histograms and/or density plots.
  * Boxplot.
* **Tabs**: 
  * Personal reading history.
  * Series -- starting with Star Wars, Star Trek, Goosebumps
  * Request form
  * About
  * Dynamic form for personal entries
