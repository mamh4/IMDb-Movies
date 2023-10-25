# Load required libraries
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)


scrape_movies <- function(n=10000,min_number_votes = 10000, print_progress = FALSE){

  movies <- tibble(
    title = character(),
    year = integer(),
    certificate = character(),
    run_time = integer(),
    genre = character(),
    directors = character(),
    stars = character(),
    rating = numeric(),
    metascore = integer(),
    votes = integer(),
    gross = numeric()
  )
  
  
    
  start_time <- Sys.time()
  
  url <- paste0('https://www.imdb.com/search/title/?title_type=feature&num_votes=',
                min_number_votes,
                ',&sort=user_rating,desc&start=1&ref_=adv_nxt')
  
  
  if(n<=50){
    n_full_pages <- 1
  } else {
    n_full_pages <- n %/% 50 
  }
  
  
  n_movies_last_page <- n %% 50
  
  

  for (i in 1:n_full_pages) {
      response <- httr::GET(url)
      # Extract movie information
      page <-
        read_html(response, "text")#read_html(content(response, "text"))#read_html(content(response, "text"))
      
      if(n<=50){
        number_of_movies_per_page <- n
      } else{
        number_of_movies_per_page <- 50
      }
      
      for (movie_index in 1:number_of_movies_per_page) {
        title_xpath <-
          paste0('//*[@id="main"]/div/div[3]/div/div[',
                 movie_index,
                 ']/div[3]/h3/a')
        year_xpath <-
          paste0('//*[@id="main"]/div/div[3]/div/div[',
                 movie_index,
                 ']/div[3]/h3/span[2]')
        #run_time_xpath <- paste0('//*[@id="main"]/div/div[3]/div/div[',movie_index,']/div[3]/p[1]/span[3]')
        #certificate_xpath <- paste0('//*[@id="main"]/div/div[3]/div/div[',movie_index,']/div[3]/p[1]/span[1]')
        #genre_xpath <- paste0('//*[@id="main"]/div/div[3]/div/div[',movie_index,']/div[3]/p[1]/span[5]')
        directors_and_stars_xpath <-
          paste0('//*[@id="main"]/div/div[3]/div/div[',
                 movie_index,
                 ']/div[3]/p[3]')
        rating_xpath <-
          paste0(
            '//*[@id="main"]/div/div[3]/div/div[',
            movie_index,
            ']/div[3]/div/div[1]/strong'
          )
        metascore_xpath <-
          paste0(
            '//*[@id="main"]/div/div[3]/div/div[',
            movie_index,
            ']/div[3]/div/div[3]/span'
          )
        votes_xpath <-
          paste0(
            '//*[@id="main"]/div/div[3]/div/div[',
            movie_index,
            ']/div[3]/p[4]/span[2]'
          )
        gross_xpath <-
          paste0(
            '//*[@id="main"]/div/div[3]/div/div[',
            movie_index,
            ']/div[3]/p[4]/span[5]'
          )
        
        
        
        title <-
          page |> html_nodes(xpath = title_xpath) |> html_text()
        year <-
          page |> html_nodes(xpath = year_xpath) |> html_text() |> str_sub(start = -5, end = -2) |> as.integer()
        #run_time <- page |>html_nodes(xpath = run_time_xpath) |> html_text() |> str_sub(start = 1, end = -5) |> as.integer()
        #certificate <- page |>html_nodes(xpath = certificate_xpath) |> html_text()
        #genre <- page |>html_nodes(xpath = genre_xpath) |> html_text() |> str_replace(pattern='\n', replacement = '')
        directors_and_stars <-
          page |> html_nodes(xpath = directors_and_stars_xpath) |> html_text()
        
        stars_start_index <-
          gregexpr(pattern = "Stars", text =  directors_and_stars) |> unlist()
        
        directors <-
          str_sub(directors_and_stars,
                  start = 1,
                  end = stars_start_index - 1) |>
          str_remove_all(pattern = paste(c(
            "Director:", "Directors:", "\n", "\\|"
          ), collapse = "|")) |> str_squish()
        
        stars <-
          str_sub(directors_and_stars, start = stars_start_index, end = -1) |>
          str_remove_all(pattern = paste(c("Star:", "Stars:", "\n", "\\|"), collapse = "|")) |> str_squish()
        
        rating <-
          page |> html_nodes(xpath = rating_xpath) |> html_text() |> as.numeric()
        
        metascore <-
          page |> html_nodes(xpath = metascore_xpath) |> html_text() |> as.integer()
        
        votes <-
          page |> html_nodes(xpath = votes_xpath) |> html_text() |> str_remove_all(",") |> as.integer()
        
        gross <-
          (page |> html_nodes(xpath = gross_xpath) |> html_attrs()
           |> unlist())[[2]] |> str_remove_all(",") |> as.integer()
        
        
        
        #####
        item_1_xpath <-
          paste0(
            '//*[@id="main"]/div/div[3]/div/div[',
            movie_index,
            ']/div[3]/p[1]/span[1]'
          )
        item_2_xpath <-
          paste0(
            '//*[@id="main"]/div/div[3]/div/div[',
            movie_index,
            ']/div[3]/p[1]/span[3]'
          )
        item_3_xpath <-
          paste0(
            '//*[@id="main"]/div/div[3]/div/div[',
            movie_index,
            ']/div[3]/p[1]/span[5]'
          )
        item_1 <-
          page |> html_nodes(xpath = item_1_xpath) |> html_text()
        item_2 <-
          page |> html_nodes(xpath = item_2_xpath) |> html_text()
        item_3 <-
          page |> html_nodes(xpath = item_3_xpath) |> html_text()
        #TODO FUNcutionalprogramming
        is_run_time_vector <- c(
          grepl(item_1, pattern = " min"),
          grepl(item_2, pattern = " min"),
          grepl(item_3, pattern = " min")
        )
        
        #classification
        if (is_run_time_vector[1]) {
          certificate <- NA
          run_time <-
            item_1 |> str_sub(start = 1, end = -5) |> as.integer()
          genre <-
            item_2 |> str_replace(pattern = '\n', replacement = '')
        } else{
          certificate <- item_1
          run_time <-
            item_2 |> str_sub(start = 1, end = -5) |> as.integer()
          genre <-
            item_3 |> str_replace(pattern = '\n', replacement = '')
        }
        
        
        
        
        
        #Replace missing data with NA
        year <- ifelse(length(year) == 0, NA, year)
        run_time <- ifelse(length(run_time) == 0, NA, run_time)
        #run_time <- ifelse(grepl(pattern = "min", certificate),as.integer(str_sub(certificate, start = 1, end = -5)) ,run_time)
        certificate <-
          ifelse(length(certificate) == 0 , NA, certificate)
        genre <- ifelse(length(genre) == 0, NA, genre)
        directors <- ifelse(length(directors) == 0, NA, directors)
        stars <- ifelse(length(stars) == 0, NA, stars)
        rating <- ifelse(length(rating) == 0, NA, rating)
        metascore <- ifelse(length(metascore) == 0, NA, metascore)
        votes <- ifelse(length(votes) == 0, NA, votes)
        gross <- ifelse(length(gross) == 0, NA, gross)
        
        
        movies <- add_row(
          movies,
          title = title,
          year = year,
          certificate = certificate,
          run_time = run_time,
          genre = genre,
          directors = directors,
          stars = stars,
          rating = rating,
          metascore = metascore,
          votes = votes,
          gross = gross
        )
        
        
      }
      
      
      url_extension <-
        (page |> html_nodes("a.lister-page-next") |> html_attr("href"))[1]
      #print(paste(i,url_extension))
      
      
      if (print_progress) {
        ##logging info:
        time_difference <-
          as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        cat("Page ", i, " - Time Elapsed: ", time_difference, " seconds\n")
      }
      
      
      url <- paste0('https://www.imdb.com', url_extension)
      
    }

  
  if(n_movies_last_page != 0 & n>50){
    
    url_extension <-
      (page |> html_nodes("a.lister-page-next") |> html_attr("href"))[1]
    
    url <- paste0('https://www.imdb.com', url_extension)
    
    response <- httr::GET(url)
    # Extract movie information
    page <-
      read_html(response, "text")#read_html(content(response, "text"))#read_html(content(response, "text"))
    
    for (movie_index in 1:n_movies_last_page) {
      title_xpath <-
        paste0('//*[@id="main"]/div/div[3]/div/div[',
               movie_index,
               ']/div[3]/h3/a')
      year_xpath <-
        paste0('//*[@id="main"]/div/div[3]/div/div[',
               movie_index,
               ']/div[3]/h3/span[2]')
      #run_time_xpath <- paste0('//*[@id="main"]/div/div[3]/div/div[',movie_index,']/div[3]/p[1]/span[3]')
      #certificate_xpath <- paste0('//*[@id="main"]/div/div[3]/div/div[',movie_index,']/div[3]/p[1]/span[1]')
      #genre_xpath <- paste0('//*[@id="main"]/div/div[3]/div/div[',movie_index,']/div[3]/p[1]/span[5]')
      directors_and_stars_xpath <-
        paste0('//*[@id="main"]/div/div[3]/div/div[',
               movie_index,
               ']/div[3]/p[3]')
      rating_xpath <-
        paste0(
          '//*[@id="main"]/div/div[3]/div/div[',
          movie_index,
          ']/div[3]/div/div[1]/strong'
        )
      metascore_xpath <-
        paste0(
          '//*[@id="main"]/div/div[3]/div/div[',
          movie_index,
          ']/div[3]/div/div[3]/span'
        )
      votes_xpath <-
        paste0('//*[@id="main"]/div/div[3]/div/div[',
               movie_index,
               ']/div[3]/p[4]/span[2]')
      gross_xpath <-
        paste0('//*[@id="main"]/div/div[3]/div/div[',
               movie_index,
               ']/div[3]/p[4]/span[5]')
      
      
      
      title <- page |> html_nodes(xpath = title_xpath) |> html_text()
      year <-
        page |> html_nodes(xpath = year_xpath) |> html_text() |> str_sub(start = -5, end = -2) |> as.integer()
      #run_time <- page |>html_nodes(xpath = run_time_xpath) |> html_text() |> str_sub(start = 1, end = -5) |> as.integer()
      #certificate <- page |>html_nodes(xpath = certificate_xpath) |> html_text()
      #genre <- page |>html_nodes(xpath = genre_xpath) |> html_text() |> str_replace(pattern='\n', replacement = '')
      directors_and_stars <-
        page |> html_nodes(xpath = directors_and_stars_xpath) |> html_text()
      
      stars_start_index <-
        gregexpr(pattern = "Stars", text =  directors_and_stars) |> unlist()
      
      directors <-
        str_sub(directors_and_stars, start = 1, end = stars_start_index - 1) |>
        str_remove_all(pattern = paste(c(
          "Director:", "Directors:", "\n", "\\|"
        ), collapse = "|")) |> str_squish()
      
      stars <-
        str_sub(directors_and_stars, start = stars_start_index, end = -1) |>
        str_remove_all(pattern = paste(c("Star:", "Stars:", "\n", "\\|"), collapse = "|")) |> str_squish()
      
      rating <-
        page |> html_nodes(xpath = rating_xpath) |> html_text() |> as.numeric()
      
      metascore <-
        page |> html_nodes(xpath = metascore_xpath) |> html_text() |> as.integer()
      
      votes <-
        page |> html_nodes(xpath = votes_xpath) |> html_text() |> str_remove_all(",") |> as.integer()
      
      gross <-
        (page |> html_nodes(xpath = gross_xpath) |> html_attrs()
         |> unlist())[[2]] |> str_remove_all(",") |> as.integer()
      
      
      
      #####
      item_1_xpath <-
        paste0('//*[@id="main"]/div/div[3]/div/div[',
               movie_index,
               ']/div[3]/p[1]/span[1]')
      item_2_xpath <-
        paste0('//*[@id="main"]/div/div[3]/div/div[',
               movie_index,
               ']/div[3]/p[1]/span[3]')
      item_3_xpath <-
        paste0('//*[@id="main"]/div/div[3]/div/div[',
               movie_index,
               ']/div[3]/p[1]/span[5]')
      item_1 <-
        page |> html_nodes(xpath = item_1_xpath) |> html_text()
      item_2 <-
        page |> html_nodes(xpath = item_2_xpath) |> html_text()
      item_3 <-
        page |> html_nodes(xpath = item_3_xpath) |> html_text()
      #TODO FUNcutionalprogramming
      is_run_time_vector <- c(
        grepl(item_1, pattern = " min"),
        grepl(item_2, pattern = " min"),
        grepl(item_3, pattern = " min")
      )
      
      #classification
      if (is_run_time_vector[1]) {
        certificate <- NA
        run_time <-
          item_1 |> str_sub(start = 1, end = -5) |> as.integer()
        genre <-
          item_2 |> str_replace(pattern = '\n', replacement = '')
      } else{
        certificate <- item_1
        run_time <-
          item_2 |> str_sub(start = 1, end = -5) |> as.integer()
        genre <-
          item_3 |> str_replace(pattern = '\n', replacement = '')
      }
      
      
      
      
      
      #Replace missing data with NA
      year <- ifelse(length(year) == 0, NA, year)
      run_time <- ifelse(length(run_time) == 0, NA, run_time)
      #run_time <- ifelse(grepl(pattern = "min", certificate),as.integer(str_sub(certificate, start = 1, end = -5)) ,run_time)
      certificate <-
        ifelse(length(certificate) == 0 , NA, certificate)
      genre <- ifelse(length(genre) == 0, NA, genre)
      directors <- ifelse(length(directors) == 0, NA, directors)
      stars <- ifelse(length(stars) == 0, NA, stars)
      rating <- ifelse(length(rating) == 0, NA, rating)
      metascore <- ifelse(length(metascore) == 0, NA, metascore)
      votes <- ifelse(length(votes) == 0, NA, votes)
      gross <- ifelse(length(gross) == 0, NA, gross)
      
      
      movies <- add_row(
        movies,
        title = title,
        year = year,
        certificate = certificate,
        run_time = run_time,
        genre = genre,
        directors = directors,
        stars = stars,
        rating = rating,
        metascore = metascore,
        votes = votes,
        gross = gross
      )
      
      
    }
    
    
    url_extension <-
      (page |> html_nodes("a.lister-page-next") |> html_attr("href"))[1]
    #print(paste(i,url_extension))
    
    
    if(print_progress){
    ##logging info:
    time_difference <-
      as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat("Page ", i, " - Time Elapsed: ", time_difference, " seconds\n")
    }
  }
  return(movies)
}

