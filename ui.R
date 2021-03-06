# 
# Exploring literary data
#

shinyUI(
  tagList(
    navbarPage(
      theme = shinytheme('superhero'),
      'Exploring literary data',
      tabPanel('Book awards',
               sidebarPanel(
                 pickerInput('awards_input',
                             'Awards',
                             c('Hugo Award',
                               'Nebula Award',
                               'Otherwise Award',
                               'Pulitzer Prize',
                               'World Fantasy Award'),
                             options = list(
                               `actions-box` = TRUE,
                               size = 10, 
                               `selected-text-format` = 'count > 3'
                             ),
                             multiple = TRUE,
                             c('Hugo Award',
                               'Nebula Award',
                               'Otherwise Award',
                               'World Fantasy Award')),
                 hr(),
                 fluidRow(
                   column(8,
                          pickerInput('awards_y_axis',
                                      'Y axis',
                                      c('# of owners (GR)' = 'countGR', 
                                        'Avg. rating (GR)' = 'ratingGR', 
                                        '# of owners (LT)' = 'countLT', 
                                        'Avg. rating (LT)' = 'ratingLT', 
                                        '# of pages' = 'pageCount', 
                                        'Publication year' = 'pubYear', 
                                        'Award year' = 'awardYear', 
                                        'Author birth date' = 'authorDOB', 
                                        'Author age' = 'authorAge'),
                                      'countGR')),
                   column(4,
                          pickerInput('awards_y_transformation',
                                      'Trans.',
                                      c('None', 'Log', 'Square-root'),
                                      'Log'))),
                 fluidRow(
                   column(8,
                          pickerInput('awards_x_axis',
                                      'X axis',
                                      c('# of owners (GR)' = 'countGR', 
                                        'Avg. rating (GR)' = 'ratingGR', 
                                        '# of owners (LT)' = 'countLT', 
                                        'Avg. rating (LT)' = 'ratingLT', 
                                        '# of pages' = 'pageCount', 
                                        'Publication year' = 'pubYear', 
                                        'Award year' = 'awardYear', 
                                        'Author birth date' = 'authorDOB', 
                                        'Author age' = 'authorAge'),
                                      'ratingGR')),
                   column(4,
                          pickerInput('awards_x_transformation',
                                      'Trans.',
                                      c('None', 'Log', 'Square-root'),
                                      'None'))),
                 pickerInput('awards_color_var',
                             'Color variable',
                             c('None',
                               'Award' = 'awardName',
                               'Awarding agency' = 'awardAgency',
                               'Awarding process' = 'awardType',
                               'Book format' = 'bookType',
                               'Target audience' = 'bookAudience',
                               'Original language' = 'language',
                               'Primary genre' = 'primaryGenre',
                               'Author\'s gender' = 'authorGender',
                               'Lead character(s\') gender' = 'charGender',
                               'Author\'s race' = 'authorRace',
                               'Author\'s nationality' = 'authorNationality',
                               'User preferences (rating)' = 'sitePrefRating'),
                             'awardName'),
                 pickerInput('awards_size_var',
                             'Size variable',
                             c('None',
                               '# of owners (GR)' = 'countGR', 
                               'Avg. rating (GR)' = 'ratingGR', 
                               '# of owners (LT)' = 'countLT', 
                               'Avg. rating (LT)' = 'ratingLT', 
                               '# of pages' = 'pageCount', 
                               'Publication year' = 'pubYear', 
                               'Award year' = 'awardYear', 
                               'Author birth date' = 'authorDOB', 
                               'Author age' = 'authorAge'),
                             'None'),
                 pickerInput('awards_linetype_var',
                             'Linetype variable',
                             c('None',
                               'Award' = 'awardName',
                               'Awarding agency' = 'awardAgency',
                               'Awarding process' = 'awardType',
                               'Book format' = 'bookType',
                               'Target audience' = 'bookAudience',
                               'Original language' = 'language',
                               'Primary genre' = 'primaryGenre',
                               'Author\'s gender' = 'authorGender',
                               'Lead character(s\') gender' = 'charGender',
                               'Author\'s race' = 'authorRace',
                               'Author\'s nationality' = 'authorNationality',
                               'User preferences (rating)' = 'sitePrefRating'),
                             'None'),
                 pickerInput('awards_facet_var',
                             'Faceting variable',
                             c('None',
                               'Award' = 'awardName',
                               'Awarding agency' = 'awardAgency',
                               'Awarding process' = 'awardType',
                               'Book format' = 'bookType',
                               'Target audience' = 'bookAudience',
                               'Original language' = 'language',
                               'Primary genre' = 'primaryGenre',
                               'Author\'s gender' = 'authorGender',
                               'Lead character(s\') gender' = 'charGender',
                               'Author\'s race' = 'authorRace',
                               'User preferences (rating)' = 'sitePrefRating'),
                             'None'),
                 hr(),
                 h6('LibraryThing & Goodreads data last updated on 01-03-2020.'),
                 h6('Please send questions or suggestions to toddellis.wa@gmail.com')
               ),
               mainPanel(
                 ggiraphOutput('awards_scatterplot', 'auto', '740px')
               )
      ),
      #tabPanel('Histogram'),
      tabPanel('Popular lists',
               sidebarPanel(
                 pickerInput('lists_input',
                             'Lists',
                             c('American Book Review',
                               'Modern Library Board',
                               'Modern Library Readers'),# UPDATE!
                             options = list(
                               `actions-box` = TRUE,
                               size = 10, 
                               `selected-text-format` = 'count > 3'
                             ),
                             multiple = TRUE,
                             c('American Book Review',
                               'Modern Library Board',
                               'Modern Library Readers')),
                 hr(),
                 fluidRow(
                   column(8,
                          pickerInput('lists_y_axis',
                                      'Y axis',
                                      c('# of owners (GR)' = 'countGR', 
                                        'Avg. rating (GR)' = 'ratingGR', 
                                        '# of owners (LT)' = 'countLT', 
                                        'Avg. rating (LT)' = 'ratingLT', 
                                        '# of pages' = 'pageCount', 
                                        'Publication year' = 'pubYear', 
                                        'List ranking' = 'listRank', 
                                        'Author birth date' = 'authorDOB', 
                                        'Author age' = 'authorAge'),
                                      'countLT')),
                   column(4,
                          pickerInput('lists_y_transformation',
                                      'Trans.',
                                      c('None', 'Log', 'Square-root'),
                                      'Log'))),
                 fluidRow(
                   column(8,
                          pickerInput('lists_x_axis',
                                      'X axis',
                                      c('# of owners (GR)' = 'countGR', 
                                        'Avg. rating (GR)' = 'ratingGR', 
                                        '# of owners (LT)' = 'countLT', 
                                        'Avg. rating (LT)' = 'ratingLT', 
                                        '# of pages' = 'pageCount', 
                                        'Publication year' = 'pubYear', 
                                        'List ranking' = 'listRank', 
                                        'Author birth date' = 'authorDOB', 
                                        'Author age' = 'authorAge'),
                                      'ratingLT')),
                   column(4,
                          pickerInput('lists_x_transformation',
                                      'Trans.',
                                      c('None', 'Log', 'Square-root'),
                                      'None'))),
                 pickerInput('lists_color_var',
                             'Color variable',
                             c('None',
                               'List' = 'listName',
                               'Book format' = 'bookType',
                               'Target audience' = 'bookAudience',
                               'Original language' = 'language',
                               'Primary genre' = 'primaryGenre',
                               'Author\'s gender' = 'authorGender',
                               'Lead character(s\') gender' = 'charGender',
                               'Author\'s race' = 'authorRace',
                               'Author\'s nationality' = 'authorNationality',
                               'User preferences (rating)' = 'sitePrefRating'),
                             'listName'),
                 pickerInput('lists_size_var',
                             'Size variable',
                             c('None',
                               '# of owners (GR)' = 'countGR', 
                               'Avg. rating (GR)' = 'ratingGR', 
                               '# of owners (LT)' = 'countLT', 
                               'Avg. rating (LT)' = 'ratingLT', 
                               '# of pages' = 'pageCount', 
                               'Publication year' = 'pubYear', 
                               'List ranking' = 'listRank', 
                               'Author birth date' = 'authorDOB', 
                               'Author age' = 'authorAge'),
                             'None'),
                 pickerInput('lists_linetype_var',
                             'Linetype variable',
                             c('None',
                               'List' = 'listName',
                               'Book format' = 'bookType',
                               'Target audience' = 'bookAudience',
                               'Original language' = 'language',
                               'Primary genre' = 'primaryGenre',
                               'Author\'s gender' = 'authorGender',
                               'Lead character(s\') gender' = 'charGender',
                               'Author\'s race' = 'authorRace',
                               'Author\'s nationality' = 'authorNationality',
                               'User preferences (rating)' = 'sitePrefRating'),
                             'None'),
                 pickerInput('lists_facet_var',
                             'Faceting variable',
                             c('None',
                               'List' = 'listName',
                               'Book format' = 'bookType',
                               'Target audience' = 'bookAudience',
                               'Original language' = 'language',
                               'Primary genre' = 'primaryGenre',
                               'Author\'s gender' = 'authorGender',
                               'Lead character(s\') gender' = 'charGender',
                               'Author\'s race' = 'authorRace',
                               'User preferences (rating)' = 'sitePrefRating'),
                             'None'),
                 hr(),
                 h6('LibraryThing & Goodreads data last updated on 19-11-2019.'),
                 h6('Please send questions or suggestions to toddellis.wa@gmail.com')
               ),
               mainPanel(
                 #verbatimTextOutput('lists_head'),
                 ggiraphOutput('lists_scatterplot', 'auto', '740px')
               )),
      tabPanel('Prolific authors',
               sidebarPanel(
                 pickerInput('authors_input',
                             'Authors',
                             c('Michael Crichton',
                               'Neil Gaiman',
                               'Stephen King',
                               'R.L. Stine',
                               'James Tiptree, Jr. / Alice Sheldon' = 'James Tiptree, Jr.'),
                             options = list(
                               `actions-box` = TRUE,
                               size = 10, 
                               `selected-text-format` = 'count > 3'
                             ),
                             multiple = TRUE,
                             c('Stephen King')),
                 hr(),
                 fluidRow(
                   column(8,
                          pickerInput('authors_y_axis',
                                      'Y axis',
                                      c('# of owners (GR)' = 'countGR', 
                                        'Avg. rating (GR)' = 'ratingGR', 
                                        '# of owners (LT)' = 'countLT', 
                                        'Avg. rating (LT)' = 'ratingLT', 
                                        '# of pages' = 'pageCount', 
                                        'Publication year' = 'pubYear', 
                                        'Author birth date' = 'authorDOB', 
                                        'Author age' = 'authorAge'),
                                      'countLT')),
                   column(4,
                          pickerInput('authors_y_transformation',
                                      'Trans.',
                                      c('None', 'Log', 'Square-root'),
                                      'Log'))),
                 fluidRow(
                   column(8, 
                          pickerInput('authors_x_axis',
                                      'X axis',
                                      c('# of owners (GR)' = 'countGR', 
                                        'Avg. rating (GR)' = 'ratingGR', 
                                        '# of owners (LT)' = 'countLT', 
                                        'Avg. rating (LT)' = 'ratingLT', 
                                        '# of pages' = 'pageCount', 
                                        'Publication year' = 'pubYear', 
                                        'Author birth date' = 'authorDOB', 
                                        'Author age' = 'authorAge'),
                                      'ratingLT')),
                   column(4,
                          pickerInput('authors_x_transformation',
                                      'Trans.',
                                      c('None', 'Log', 'Square-root'),
                                      'None'))),
                 pickerInput('authors_color_var',
                             'Color variable',
                             c('None',
                               'Author' = 'authorName',
                               'Pseudonymous' = 'pseudonym',
                               'Book format' = 'bookType',
                               'Target audience' = 'bookAudience',
                               'Original language' = 'language',
                               'Primary genre' = 'primaryGenre',
                               'Author\'s gender' = 'authorGender',
                               'Lead character(s\') gender' = 'charGender',
                               'Author\'s race' = 'authorRace',
                               'Author\'s nationality' = 'authorNationality',
                               'User preferences (rating)' = 'sitePrefRating'),
                             'authorName'),
                 pickerInput('authors_size_var',
                             'Size variable',
                             c('None',
                               '# of owners (GR)' = 'countGR', 
                               'Avg. rating (GR)' = 'ratingGR', 
                               '# of owners (LT)' = 'countLT', 
                               'Avg. rating (LT)' = 'ratingLT', 
                               '# of pages' = 'pageCount', 
                               'Publication year' = 'pubYear', 
                               'Author birth date' = 'authorDOB', 
                               'Author age' = 'authorAge'),
                             'pageCount'),
                 pickerInput('authors_linetype_var',
                             'Linetype variable',
                             c('None',
                               'Author' = 'authorName',
                               'Pseudonymous' = 'pseudonym',
                               'Book format' = 'bookType',
                               'Target audience' = 'bookAudience',
                               'Original language' = 'language',
                               'Primary genre' = 'primaryGenre',
                               'Author\'s gender' = 'authorGender',
                               'Lead character(s\') gender' = 'charGender',
                               'Author\'s race' = 'authorRace',
                               'Author\'s nationality' = 'authorNationality',
                               'User preferences (rating)' = 'sitePrefRating'),
                             'None'),
                 pickerInput('authors_facet_var',
                             'Faceting variable',
                             c('None',
                               'Author' = 'authorName',
                               'Pseudonymous' = 'pseudonym',
                               'Book format' = 'bookType',
                               'Target audience' = 'bookAudience',
                               'Original language' = 'language',
                               'Primary genre' = 'primaryGenre',
                               'Author\'s gender' = 'authorGender',
                               'Lead character(s\') gender' = 'charGender',
                               'Author\'s race' = 'authorRace',
                               'User preferences (rating)' = 'sitePrefRating'),
                             'None'),
                 hr(),
                 h6('LibraryThing & Goodreads data last updated on 21-11-2019.'),
                 h6('Please send questions or suggestions to toddellis.wa@gmail.com')
               ),
               mainPanel(
                 ggiraphOutput('authors_scatterplot', 'auto', '740px')
               )
      )
    )
  )
)  
