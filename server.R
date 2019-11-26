# 
# Exploring literary data
#

shinyServer( function(input, output) {
 
  
  bookAwards_input <- reactive( {
    paste0('%', input$awards_input, '%')
  })
  bookAwards_react <- reactive( {
    return(
      tbl_df( bookAwards ) %>%
        filter(awardName %like% bookAwards_input()) %>%
        mutate(authorRace = ifelse(authorRace == 'White', 'White', 'POC'))
    )
  })
  
  bookLists_input <- reactive( {
    paste0('%', input$lists_input, '%')
  })
  
  bookLists_react <- reactive( {
    return(
      tbl_df( bookLists ) %>%
        filter(listName %like% bookLists_input()) %>%
        mutate(authorRace = ifelse(authorRace == 'White', 'White', 'POC'))
    )
  })
  
  bookAuthors_input <- reactive( {
    paste0('%',  input$authors_input, '%')
  })
  
  bookAuthors_react <- reactive( {
    return(
      tbl_df( bookAuthors ) %>%
        filter(authorName %like% bookAuthors_input(),
               bookTitle != 'It Came from Ohio!: My Life as a Writer',
               bookTitle != 'The Graveyard Book: The Graphic Novel') %>%
        mutate(authorRace = ifelse(authorRace == 'White', 'White', 'POC'))
    )
  })
  
  output$awards_scatterplot <- renderggiraph({
    # create shadow if faceting
    bookAwards_shadow <- bookAwards_react() %>%
      dplyr::select('countGR', 'countLT', 'ratingGR', 'ratingLT', 'pubYear', 'pageCount', 'awardYear', 'authorDOB', 'authorAge')
    
    # draw base plot
    bookAwards_plot <- ggplot(bookAwards_react(), 
                              aes(x = !! sym(input$awards_x_axis),
                                  y = !! sym(input$awards_y_axis))) +
      theme_classic() +
      geom_point(alpha = 0.5, size = 4.5, shape = 19) +
      #geom_line(stat = 'smooth', method = 'lm', se = F, alpha = 0.5) +
      geom_smooth(method = 'gam', se = F, alpha = 0.1, level = 0.9, size = 1.4,
                  formula = y ~ s(x, bs = 'cr', k = 3))
    
    # add axis transformations
    if (input$awards_y_transformation == 'Log')
      bookAwards_plot <- bookAwards_plot + 
      aes(y = !! sym( input$awards_y_axis) + 1) + 
      scale_y_log10()
    if (input$awards_y_transformation == 'Square-root')
      bookAwards_plot <- bookAwards_plot +
      aes(y = !! sym(input$awards_y_axis) + 1) +
      scale_y_sqrt()
    if(input$awards_x_transformation == 'Log')
      bookAwards_plot <- bookAwards_plot +
      aes(x = !! sym(input$awards_x_axis) + 1) +
      scale_x_log10()
    if(input$awards_x_transformation == 'Square-root') 
      bookAwards_plot <- bookAwards_plot +
      aes(x = !! sym(input$awards_x_axis) + 1) +
      scale_x_sqrt()
    
    # add grouping information
    if (input$awards_color_var != 'None')
      bookAwards_plot <- bookAwards_plot + 
      aes(color = !! sym(input$awards_color_var)) + 
      scale_color_brewer(palette = 'Pastel2') # Spectral
    #scale_color_manual(values = faveColors)
    #scale_color_viridis_d()
    ifelse(input$awards_size_var != 'None',
           bookAwards_plot <- bookAwards_plot + aes(size = !! sym(input$awards_size_var)) + scale_size_continuous(range = c(1, 12)), 
           bookAwards_plot <- bookAwards_plot)
    if (input$awards_linetype_var != 'None')
      bookAwards_plot <- bookAwards_plot +
      aes(linetype = !! sym(input$awards_linetype_var))
    if (input$awards_facet_var != 'None') 
      bookAwards_plot <- bookAwards_plot +
      geom_point(data = bookAwards_shadow, color = 'grey90', alpha = 0.2) +
      facet_wrap(as.formula(paste(input$awards_facet_var, '~ .')))
    #facet_grid(as.formula(paste(input$awards_facet_var, '~ .')))
    
    ## labels
    bookAwards_plot <- switch(input$awards_x_axis,
                              'countGR' = bookAwards_plot +
                                labs(x = 'Number of Goodreads users'),
                              'countLT' = bookAwards_plot +
                                labs(x = 'Number of LibraryThing users'), 
                              'ratingGR' = bookAwards_plot +
                                labs(x = 'Average Goodreads score'), 
                              'ratingLT' = bookAwards_plot +
                                labs(x = 'Average LibraryThing score'), 
                              'pageCount' = bookAwards_plot +
                                labs(x = 'Book length'), 
                              'pubYear' = bookAwards_plot +
                                labs(x = 'Publication year'), 
                              'awardYear' = bookAwards_plot +
                                labs(x = 'Award year'), 
                              'authorDOB' = bookAwards_plot +
                                labs(x = 'Author date of birth'), 
                              'authorAge' = bookAwards_plot +
                                labs(x = 'Author age at time of publication'))
    
    bookAwards_plot <- switch(input$awards_y_axis,
                              'countGR' = bookAwards_plot +
                                labs(y = 'Number of Goodreads users'),
                              'countLT' = bookAwards_plot +
                                labs(y = 'Number of LibraryThing users'), 
                              'ratingGR' = bookAwards_plot +
                                labs(y = 'Average Goodreads score'), 
                              'ratingLT' = bookAwards_plot +
                                labs(y = 'Average LibraryThing score'), 
                              'pageCount' = bookAwards_plot +
                                labs(y = 'Book length'), 
                              'pubYear' = bookAwards_plot +
                                labs(y = 'Publication year'), 
                              'awardYear' = bookAwards_plot +
                                labs(y = 'Award year'), 
                              'authorDOB' = bookAwards_plot +
                                labs(y = 'Author date of birth'), 
                              'authorAge' = bookAwards_plot +
                                labs(y = 'Author age at time of publication'))
    
    bookAwards_plot <- switch(input$awards_color_var,
                              'None' = bookAwards_plot,
                              'awardName' = bookAwards_plot +
                                labs(color = 'Award'),
                              'awardAgency' = bookAwards_plot +
                                labs(color = 'Awarding agency'),
                              'awardType' = bookAwards_plot +
                                labs(color = 'Type of award'),
                              'bookType' = bookAwards_plot +
                                labs(color = 'Book format'),
                              'bookAudience' = bookAwards_plot +
                                labs(color = 'Target audience'),
                              'language' = bookAwards_plot +
                                labs(color = 'Original language'),
                              'primaryGenre' = bookAwards_plot +
                                labs(color = 'Primary genre'),
                              'authorGender' = bookAwards_plot +
                                labs(color = 'Author\'s gender'),
                              'charGender' = bookAwards_plot +
                                labs(color = 'Lead character(s\') gender'),
                              'authorRace' = bookAwards_plot +
                                labs(color = 'Author\'s race'),
                              'authorNationality' = bookAwards_plot + 
                                labs(color = 'Author\'s nationality'),
                              'sitePrefRating' = bookAwards_plot +
                                labs(color = 'User preferences (rating)'))
    
    bookAwards_plot <- switch(input$awards_linetype_var,
                              'None' = bookAwards_plot,
                              'awardName' = bookAwards_plot +
                                labs(linetype = 'Award'),
                              'awardAgency' = bookAwards_plot +
                                labs(linetype = 'Awarding agency'),
                              'awardType' = bookAwards_plot +
                                labs(linetype = 'Type of award'),
                              'bookType' = bookAwards_plot +
                                labs(linetype = 'Book format'),
                              'bookAudience' = bookAwards_plot +
                                labs(linetype = 'Target audience'),
                              'language' = bookAwards_plot +
                                labs(linetype = 'Original language'),
                              'primaryGenre' = bookAwards_plot +
                                labs(linetype = 'Primary genre'),
                              'authorGender' = bookAwards_plot +
                                labs(linetype = 'Author\'s gender'),
                              'charGender' = bookAwards_plot +
                                labs(linetype = 'Lead character(s\') gender'),
                              'authorRace' = bookAwards_plot +
                                labs(linetype = 'Author\'s race'),
                              'authorNationality' = bookAwards_plot + 
                                labs(linetype = 'Author\'s nationality'),
                              'sitePrefRating' = bookAwards_plot +
                                labs(linetype = 'User preferences (rating)'))
    
    bookAwards_plot <- bookAwards_plot + 
      theme(panel.background = element_rect(fill = "#4E5D6C"), #4E5D6C or #2B3E50
            plot.background = element_rect(fill = '#4E5D6C'),
            legend.background = element_rect(fill = "#4E5D6C"),
            strip.background = element_rect(fill = '#2B3E50'),
            panel.grid = element_line(color = '#2B3E50'),
            axis.title = element_text(colour = "grey85", size = 22),
            axis.title.x = element_text(margin = margin(t = 10)),
            axis.title.y = element_text(margin = margin(r = 10, l = 10)),
            axis.text = element_text(color = 'grey65', size = 16),
            legend.position = 'bottom',
            legend.text = element_text(color = 'grey65', size = 14),
            legend.title = element_text(color = 'grey85', size = 15),
            strip.text = element_text(color = 'grey65', size = 16)) + 
      geom_point_interactive(
        aes(tooltip = paste0(bookTitle, ' (', pubYear, ')\n', authorFirst, ' ', authorLast, '\nX: ', !! sym(input$awards_x_axis), ', Y: ', !! sym(input$awards_y_axis))))
    
    girafe(code = print(bookAwards_plot),
           width = 14,
           height = 12)
  })
  
  # output$lists_head <- renderPrint( { bookLists_react() })
  
  output$lists_scatterplot <- renderggiraph({
    # create shadow if faceting
    bookLists_shadow <- bookLists_react() %>%
      dplyr::select('countGR', 'countLT', 'ratingGR', 'ratingLT', 'pubYear', 'pageCount', 'listRank', 'authorDOB', 'authorAge')
    
    # draw base plot
    bookLists_plot <- ggplot(bookLists_react(), 
                             aes(x = !! sym(input$lists_x_axis),
                                 y = !! sym(input$lists_y_axis))) +
      theme_classic() +
      geom_point(alpha = 0.5, size = 4.5, shape = 19) +
      #geom_line(stat = 'smooth', method = 'lm', se = F, alpha = 0.5) +
      geom_smooth(method = 'gam', se = F, alpha = 0.1, level = 0.9, size = 1.4,
                  formula = y ~ s(x, bs = 'cr', k = 3))
    
    # add axis transformations
    if (input$lists_y_transformation == 'Log')
      bookLists_plot <- bookLists_plot + 
      aes(y = !! sym( input$lists_y_axis) + 1) + 
      scale_y_log10()
    if (input$lists_y_transformation == 'Square-root')
      bookLists_plot <- bookLists_plot +
      aes(y = !! sym(input$lists_y_axis) + 1) +
      scale_y_sqrt()
    if(input$lists_x_transformation == 'Log')
      bookLists_plot <- bookLists_plot +
      aes(x = !! sym(input$lists_x_axis) + 1) +
      scale_x_log10()
    if(input$lists_x_transformation == 'Square-root') 
      bookLists_plot <- bookLists_plot +
      aes(x = !! sym(input$lists_x_axis) + 1) +
      scale_x_sqrt()
    
    # add grouping information
    if (input$lists_color_var != 'None')
      bookLists_plot <- bookLists_plot + 
      aes(color = !! sym(input$lists_color_var)) + 
      scale_color_brewer(palette = 'Pastel2') # Spectral
    #scale_color_manual(values = faveColors)
    #scale_color_viridis_d()
    ifelse(input$lists_size_var != 'None',
           bookLists_plot <- bookLists_plot + aes(size = !! sym(input$lists_size_var)) + scale_size_continuous(range = c(1, 12)), 
           bookLists_plot <- bookLists_plot)
    if (input$lists_linetype_var != 'None')
      bookLists_plot <- bookLists_plot +
      aes(linetype = !! sym(input$lists_linetype_var))
    if (input$lists_facet_var != 'None') 
      bookLists_plot <- bookLists_plot +
      geom_point(data = bookLists_shadow, color = 'grey90', alpha = 0.2) +
      facet_wrap(as.formula(paste(input$lists_facet_var, '~ .')))
    #facet_grid(as.formula(paste(input$lists_facet_var, '~ .')))
    
    ## labels
    bookLists_plot <- switch(input$lists_x_axis,
                             'countGR' = bookLists_plot +
                               labs(x = 'Number of Goodreads users'),
                             'countLT' = bookLists_plot +
                               labs(x = 'Number of LibraryThing users'), 
                             'ratingGR' = bookLists_plot +
                               labs(x = 'Average Goodreads score'), 
                             'ratingLT' = bookLists_plot +
                               labs(x = 'Average LibraryThing score'), 
                             'pageCount' = bookLists_plot +
                               labs(x = 'Book length'), 
                             'pubYear' = bookLists_plot +
                               labs(x = 'Publication year'), 
                             'listRank' = bookLists_plot +
                               labs(x = 'List ranking'), 
                             'authorDOB' = bookLists_plot +
                               labs(x = 'Author date of birth'), 
                             'authorAge' = bookLists_plot +
                               labs(x = 'Author age at time of publication'))
    
    bookLists_plot <- switch(input$lists_y_axis,
                             'countGR' = bookLists_plot +
                               labs(y = 'Number of Goodreads users'),
                             'countLT' = bookLists_plot +
                               labs(y = 'Number of LibraryThing users'), 
                             'ratingGR' = bookLists_plot +
                               labs(y = 'Average Goodreads score'), 
                             'ratingLT' = bookLists_plot +
                               labs(y = 'Average LibraryThing score'), 
                             'pageCount' = bookLists_plot +
                               labs(y = 'Book length'), 
                             'pubYear' = bookLists_plot +
                               labs(y = 'Publication year'), 
                             'listRank' = bookLists_plot +
                               labs(y = 'List ranking'), 
                             'authorDOB' = bookLists_plot +
                               labs(y = 'Author date of birth'), 
                             'authorAge' = bookLists_plot +
                               labs(y = 'Author age at time of publication'))
    
    bookLists_plot <- switch(input$lists_color_var,
                             'None' = bookLists_plot,
                             'listName' = bookLists_plot +
                               labs(color = 'List'),
                             'bookType' = bookLists_plot +
                               labs(color = 'Book format'),
                             'bookAudience' = bookLists_plot +
                               labs(color = 'Target audience'),
                             'language' = bookLists_plot +
                               labs(color = 'Original language'),
                             'primaryGenre' = bookLists_plot +
                               labs(color = 'Primary genre'),
                             'authorGender' = bookLists_plot +
                               labs(color = 'Author\'s gender'),
                             'charGender' = bookLists_plot +
                               labs(color = 'Lead character(s\') gender'),
                             'authorRace' = bookLists_plot +
                               labs(color = 'Author\'s race'),
                             'authorNationality' = bookLists_plot + 
                               labs(color = 'Author\'s nationality'),
                             'sitePrefRating' = bookLists_plot +
                               labs(color = 'User preferences (rating)'))
    
    bookLists_plot <- switch(input$lists_linetype_var,
                             'None' = bookLists_plot,
                             'listName' = bookLists_plot +
                               labs(linetype = 'List'),
                             'bookType' = bookLists_plot +
                               labs(linetype = 'Book format'),
                             'bookAudience' = bookLists_plot +
                               labs(linetype = 'Target audience'),
                             'language' = bookLists_plot +
                               labs(linetype = 'Original language'),
                             'primaryGenre' = bookLists_plot +
                               labs(linetype = 'Primary genre'),
                             'authorGender' = bookLists_plot +
                               labs(linetype = 'Author\'s gender'),
                             'charGender' = bookLists_plot +
                               labs(linetype = 'Lead character(s\') gender'),
                             'authorRace' = bookLists_plot +
                               labs(linetype = 'Author\'s race'),
                             'authorNationality' = bookLists_plot + 
                               labs(linetype = 'Author\'s nationality'),
                             'sitePrefRating' = bookLists_plot +
                               labs(linetype = 'User preferences (rating)'))
    
    bookLists_plot <- bookLists_plot + 
      theme(panel.background = element_rect(fill = "#4E5D6C"), #4E5D6C or #2B3E50
            plot.background = element_rect(fill = '#4E5D6C'),
            legend.background = element_rect(fill = "#4E5D6C"),
            strip.background = element_rect(fill = '#2B3E50'),
            panel.grid = element_line(color = '#2B3E50'),
            axis.title = element_text(colour = "grey85", size = 22),
            axis.title.x = element_text(margin = margin(t = 10)),
            axis.title.y = element_text(margin = margin(r = 10, l = 10)),
            axis.text = element_text(color = 'grey65', size = 16),
            legend.position = 'bottom',
            legend.text = element_text(color = 'grey65', size = 14),
            legend.title = element_text(color = 'grey85', size = 15),
            strip.text = element_text(color = 'grey65', size = 16)) + 
      geom_point_interactive(
        aes(tooltip = paste0(bookTitle, ' (', pubYear, ')\n', authorFirst, ' ', authorLast, '\nX: ', !! sym(input$lists_x_axis), ', Y: ', !! sym(input$lists_y_axis))))
    
    girafe(code = print(bookLists_plot),
           width = 14,
           height = 12)
  })
  
  output$authors_scatterplot <- renderggiraph({
    # create shadow if faceting
    bookAuthors_shadow <- bookAuthors_react() %>%
      dplyr::select('countGR', 'countLT', 'ratingGR', 'ratingLT', 'pubYear', 'pageCount', 'authorDOB', 'authorAge')
    
    # draw base plot
    bookAuthors_plot <- ggplot(bookAuthors_react(), 
                               aes(x = !! sym(input$authors_x_axis),
                                   y = !! sym(input$authors_y_axis))) +
      theme_classic() +
      geom_point(alpha = 0.5, size = 4.5, shape = 19) +
      #geom_line(stat = 'smooth', method = 'lm', se = F, alpha = 0.5) +
      geom_smooth(method = 'gam', se = F, alpha = 0.1, level = 0.9, size = 1.4,
                  formula = y ~ s(x, bs = 'cr', k = 3))
    
    # add axis transformations
    if (input$authors_y_transformation == 'Log')
      bookAuthors_plot <- bookAuthors_plot + 
      aes(y = !! sym( input$authors_y_axis) + 1) + 
      scale_y_log10()
    if (input$authors_y_transformation == 'Square-root')
      bookAuthors_plot <- bookAuthors_plot +
      aes(y = !! sym(input$authors_y_axis) + 1) +
      scale_y_sqrt()
    if(input$authors_x_transformation == 'Log')
      bookAuthors_plot <- bookAuthors_plot +
      aes(x = !! sym(input$authors_x_axis) + 1) +
      scale_x_log10()
    if(input$authors_x_transformation == 'Square-root') 
      bookAuthors_plot <- bookAuthors_plot +
      aes(x = !! sym(input$authors_x_axis) + 1) +
      scale_x_sqrt()
    
    # add grouping information
    if (input$authors_color_var != 'None')
      bookAuthors_plot <- bookAuthors_plot + 
      aes(color = !! sym(input$authors_color_var)) + 
      scale_color_brewer(palette = 'Pastel2') # Spectral
    ifelse(input$authors_size_var != 'None',
           bookAuthors_plot <- bookAuthors_plot + aes(size = !! sym(input$authors_size_var)) + scale_size_continuous(range = c(1, 12)), 
           bookAuthors_plot <- bookAuthors_plot)
    #scale_color_manual(values = faveColors)
    #scale_color_viridis_d()
    if (input$authors_linetype_var != 'None')
      bookAuthors_plot <- bookAuthors_plot +
      aes(linetype = !! sym(input$authors_linetype_var))
    if (input$authors_facet_var != 'None') 
      bookAuthors_plot <- bookAuthors_plot +
      geom_point(data = bookAuthors_shadow, color = 'grey90', alpha = 0.2) +
      facet_wrap(as.formula(paste(input$authors_facet_var, '~ .')))
    #facet_grid(as.formula(paste(input$authors_facet_var, '~ .')))
    
    
    ## labels
    bookAuthors_plot <- switch(input$authors_x_axis,
                               'countGR' = bookAuthors_plot +
                                 labs(x = 'Number of Goodreads users'),
                               'countLT' = bookAuthors_plot +
                                 labs(x = 'Number of LibraryThing users'), 
                               'ratingGR' = bookAuthors_plot +
                                 labs(x = 'Average Goodreads score'), 
                               'ratingLT' = bookAuthors_plot +
                                 labs(x = 'Average LibraryThing score'), 
                               'pageCount' = bookAuthors_plot +
                                 labs(x = 'Book length'), 
                               'pubYear' = bookAuthors_plot +
                                 labs(x = 'Publication year'), 
                               'authorDOB' = bookAuthors_plot +
                                 labs(x = 'Author date of birth'), 
                               'authorAge' = bookAuthors_plot +
                                 labs(x = 'Author age at time of publication'))
    
    bookAuthors_plot <- switch(input$authors_y_axis,
                               'countGR' = bookAuthors_plot +
                                 labs(y = 'Number of Goodreads users'),
                               'countLT' = bookAuthors_plot +
                                 labs(y = 'Number of LibraryThing users'), 
                               'ratingGR' = bookAuthors_plot +
                                 labs(y = 'Average Goodreads score'), 
                               'ratingLT' = bookAuthors_plot +
                                 labs(y = 'Average LibraryThing score'), 
                               'pageCount' = bookAuthors_plot +
                                 labs(y = 'Book length'), 
                               'pubYear' = bookAuthors_plot +
                                 labs(y = 'Publication year'), 
                               'authorDOB' = bookAuthors_plot +
                                 labs(y = 'Author date of birth'), 
                               'authorAge' = bookAuthors_plot +
                                 labs(y = 'Author age at time of publication'))
    
    bookAuthors_plot <- switch(input$authors_color_var,
                               'None' = bookAuthors_plot,
                               'authorName' = bookAuthors_plot +
                                 labs(linetype = 'Author'),
                               'pseudonym' = bookAuthors_plot + 
                                 labs(linetype = 'Pseudonymous'),
                               'bookType' = bookAuthors_plot +
                                 labs(color = 'Book format'),
                               'bookAudience' = bookAuthors_plot +
                                 labs(color = 'Target audience'),
                               'language' = bookAuthors_plot +
                                 labs(color = 'Original language'),
                               'primaryGenre' = bookAuthors_plot +
                                 labs(color = 'Primary genre'),
                               'authorGender' = bookAuthors_plot +
                                 labs(color = 'Author\'s gender'),
                               'charGender' = bookAuthors_plot +
                                 labs(color = 'Lead character(s\') gender'),
                               'authorRace' = bookAuthors_plot +
                                 labs(color = 'Author\'s race'),
                               'authorNationality' = bookAuthors_plot + 
                                 labs(color = 'Author\'s nationality'),
                               'sitePrefRating' = bookAuthors_plot +
                                 labs(color = 'User preferences (rating)'))
    
    bookAuthors_plot <- switch(input$authors_linetype_var,
                               'None' = bookAuthors_plot,
                               'authorName' = bookAuthors_plot +
                                 labs(linetype = 'Author'),
                               'pseudonym' = bookAuthors_plot + 
                                 labs(linetype = 'Pseudonymous'),
                               'bookType' = bookAuthors_plot +
                                 labs(linetype = 'Book format'),
                               'bookAudience' = bookAuthors_plot +
                                 labs(linetype = 'Target audience'),
                               'language' = bookAuthors_plot +
                                 labs(linetype = 'Original language'),
                               'primaryGenre' = bookAuthors_plot +
                                 labs(linetype = 'Primary genre'),
                               'authorGender' = bookAuthors_plot +
                                 labs(linetype = 'Author\'s gender'),
                               'charGender' = bookAuthors_plot +
                                 labs(linetype = 'Lead character(s\') gender'),
                               'authorRace' = bookAuthors_plot +
                                 labs(linetype = 'Author\'s race'),
                               'authorNationality' = bookAuthors_plot + 
                                 labs(linetype = 'Author\'s nationality'),
                               'sitePrefRating' = bookAuthors_plot +
                                 labs(linetype = 'User preferences (rating)'))
    
    bookAuthors_plot <- bookAuthors_plot + 
      theme(panel.background = element_rect(fill = "#4E5D6C"), #4E5D6C or #2B3E50
            plot.background = element_rect(fill = '#4E5D6C'),
            legend.background = element_rect(fill = "#4E5D6C"),
            strip.background = element_rect(fill = '#2B3E50'),
            panel.grid = element_line(color = '#2B3E50'),
            axis.title = element_text(colour = "grey85", size = 22),
            axis.title.x = element_text(margin = margin(t = 10)),
            axis.title.y = element_text(margin = margin(r = 10, l = 10)),
            axis.text = element_text(color = 'grey65', size = 16),
            legend.position = 'bottom',
            legend.text = element_text(color = 'grey65', size = 14),
            legend.title = element_text(color = 'grey85', size = 15),
            strip.text = element_text(color = 'grey65', size = 16)) + 
      geom_point_interactive(
        aes(tooltip = paste0(bookTitle, ' (', pubYear, ')\n', authorFirst, ' ', authorLast, '\nX: ', !! sym(input$authors_x_axis), ', Y: ', !! sym(input$authors_y_axis))))
    
    girafe(code = print(bookAuthors_plot),
           width = 14,
           height = 12)
  })
  
}
)
