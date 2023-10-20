# Load required libraries
library(rvest)
library(dplyr)
library(stringr)
library(xml2)
library(XML)
library("readr")
library(ggplot2)
library(patchwork)
library(tidyr)
library(zoo)

scrape_movies <- function(n_start=1, n_end=10000,min_number_votes = 1000, print_progress = FALSE, return_next_url = FALSE, start_url = 'empty'){
  
  n <- 1 + n_end - n_start
  
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
  
  n_full_pages <- n %/% 50 
  n_movies_last_page <- n %% 50
  if (n_movies_last_page == 0 ) {
    n_movies_last_page <- 50
    n_pages_to_scrape <- n_full_pages
  } else {
    n_pages_to_scrape <- (n_full_pages + 1)
  }
  
  if (start_url == 'empty'){
    url <- paste0('https://www.imdb.com/search/title/?title_type=feature&num_votes=', min_number_votes, ',&sort=user_rating,desc&start=', (n_start%% 50), '&ref_=adv_nxt')
    
    if ((n_start %/% 50) > 0 ){
      for (i in 1:(n_start %/% 50 )){
        response <- httr::GET(url)
        # Extract movie information
        page <- read_html(response, "text") #read_html(content(response, "text"))#read_html(content(response, "text"))
        url_extension <- (page |> html_nodes("a.lister-page-next") |> html_attr("href"))[1]
        url <- paste0('https://www.imdb.com', url_extension)
        if(print_progress){
          time_difference <-
            as.numeric(difftime(Sys.time(), start_time, units = "secs"))
          cat("Page ", ((i*50)+(n_start%% 50)), " - Time Elapsed: ", time_difference, " seconds\n")
        }
      }
    }
  }
  else{
    url <- start_url
  }
  
  
  for (i in 1:n_pages_to_scrape) {
    cat("Progress: ", (100*i/n_pages_to_scrape), "percent \n")
    time_difference <-
      as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat("Page: ", i , "Time Elapsed: ", time_difference, " seconds\n")
    cat(url, "\n")
    response <- httr::GET(url)
    # Extract movie information
    page <- read_html(response, "text") #read_html(content(response, "text"))#read_html(content(response, "text"))

    if ( i==n_pages_to_scrape ){
      n_movies_to_scrape <- n_movies_last_page
    } else {
      n_movies_to_scrape <- 50
    }
    cat("Movie: ")
    for (movie_index in 1:n_movies_to_scrape) {
      cat(movie_index, " ")
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

      title <- page |> html_nodes(xpath = title_xpath) |> html_text()
      year <- page |> html_nodes(xpath = year_xpath) |> html_text() |> str_sub(start = -5, end = -2) |> as.integer()
      directors_and_stars <- page |> html_nodes(xpath = directors_and_stars_xpath) |> html_text()
      stars_start_index <- gregexpr(pattern = "Stars", text =  directors_and_stars) |> unlist()
      directors <- str_sub(directors_and_stars, start = 1, end = stars_start_index - 1) |>
        str_remove_all(pattern = paste(c("Director:", "Directors:", "\n", "\\|"), collapse = "|")) |> str_squish()
      stars <- str_sub(directors_and_stars, start = stars_start_index, end = -1) |>
        str_remove_all(pattern = paste(c("Star:", "Stars:", "\n", "\\|"), collapse = "|")) |> str_squish()
      rating <- page |> html_nodes(xpath = rating_xpath) |> html_text() |> as.numeric()
      metascore <- page |> html_nodes(xpath = metascore_xpath) |> html_text() |> as.integer()
      votes <- page |> html_nodes(xpath = votes_xpath) |> html_text() |> str_remove_all(",") |> as.integer()
      gross <- (page |> html_nodes(xpath = gross_xpath) |> html_attrs() |> unlist())[[2]] |> 
        str_remove_all(",") |> as.integer()
      
      
      
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
      if (length(item_1) != 0 ){
        is_run_time_vector <- c(
          grepl(item_1, pattern = " min"),
          grepl(item_2, pattern = " min")
          #grepl(item_3, pattern = " min")
        )
        if (length(item_2) == 0) {
          is_run_time_vector[2] <- FALSE
        }
        #cat(as.character(item1_tester),"\n", as.character(item2_tester),"\n", is_run_time_vector, "\n")
        if (is_run_time_vector[1]) {
          certificate <- NA
          run_time <- item_1 |> str_sub(start = 1, end = -5) |> as.integer()
          genre <- item_2 |> str_replace(pattern = '\n', replacement = '') |> str_squish()
        } 
        else if (is_run_time_vector[2]) {
          certificate <- item_1
          run_time <- item_2 |> str_sub(start = 1, end = -5) |> as.integer()
          genre <- item_3 |> str_replace(pattern = '\n', replacement = '') |> str_squish()
        }
        else {
          if (grepl(item_1, pattern = "\n")){
            certificate <- NA
            run_time <- NA
            genre <- item_1 |> str_replace(pattern = '\n', replacement = '') |> str_squish()
          }
          else {
            certificate <- item_1
            run_time <- NA
            genre <- item_2 |> str_replace(pattern = '\n', replacement = '') |> str_squish()
          }
        }
      }
      else {
        certificate <- NA 
        run_time <- NA
        genre <- NA 
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
        title = as.character(title),
        year = year,
        certificate = as.character(certificate),
        run_time = run_time,
        genre = as.character(genre),
        directors = as.character(directors),
        stars = as.character(stars),
        rating = rating,
        metascore = metascore,
        votes = votes,
        gross = gross
      )
      
      
    }
    cat(" \n")
    cat("Supposed number of movies", (i*50) , "vs: ", count(movies)[[1]] , " \n")
    
    
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
  
  if (return_next_url) {
    return(list(movies,url))
  }
  else {
    return(movies)
  }
}

test_movies <- scrape_movies(n_start=53, n_end=205)

test_movies_1 <- scrape_movies(n_start=1, n_end=205, min_number_votes = 1000, print_progress = FALSE, return_next_url = TRUE)

test_movies_2 <- scrape_movies(n_start=253, n_end=307, min_number_votes = 1000, print_progress = FALSE, return_next_url = TRUE, start_url = test_movies_1[[2]])

movies_2 <- test_movies_2[[1]]


fullMovies <- scrape_movies(n_start=1, n_end=5000, min_number_votes = 0, print_progress = FALSE, return_next_url = TRUE, start_url = 'empty')



fullMovies <- scrape_movies(n_start=1, n_end=25000, min_number_votes = 0, print_progress = FALSE, return_next_url = TRUE)

currentBatch <- fullMovies[[1]]

save(currentBatch, file=(file.path("fullIMDbData_01.RData")))

url01 <- fullMovies[[2]]




fullMovies <- scrape_movies(n_start=25001, n_end=50000, min_number_votes = 0, print_progress = FALSE, return_next_url = TRUE, start_url = fullMovies[[2]])

currentBatch <- fullMovies[[1]]

save(currentBatch, file=(file.path("fullIMDbData_02.RData")))

url02 <- fullMovies[[2]]


fullMovies <- scrape_movies(n_start=50001, n_end=75000, min_number_votes = 0, print_progress = FALSE, return_next_url = TRUE, start_url = url02)

currentBatch <- fullMovies[[1]]

save(currentBatch, file=(file.path("fullIMDbData_03.RData")))

url03 <- fullMovies[[2]]


fullMovies <- scrape_movies(n_start=75001, n_end=100000, min_number_votes = 0, print_progress = FALSE, return_next_url = TRUE, start_url = url03)

currentBatch <- fullMovies[[1]]

save(currentBatch, file=(file.path("fullIMDbData_04.RData")))

url04 <- fullMovies[[2]]


fullMovies <- scrape_movies(n_start=100001, n_end=125000, min_number_votes = 0, print_progress = FALSE, return_next_url = TRUE, start_url = url04)

currentBatch <- fullMovies[[1]]

save(currentBatch, file=(file.path("fullIMDbData_05.RData")))

url05 <- fullMovies[[2]]


fullMovies <- scrape_movies(n_start=125001, n_end=150000, min_number_votes = 0, print_progress = FALSE, return_next_url = TRUE, start_url = url05)

currentBatch <- fullMovies[[1]]

save(currentBatch, file=(file.path("fullIMDbData_06.RData")))

url06 <- fullMovies[[2]]


fullMovies <- scrape_movies(n_start=150001, n_end=175000, min_number_votes = 0, print_progress = FALSE, return_next_url = TRUE, start_url = url06)

currentBatch <- fullMovies[[1]]

save(currentBatch, file=(file.path("fullIMDbData_07.RData")))

url07 <- fullMovies[[2]]


fullMovies <- scrape_movies(n_start=175001, n_end=200000, min_number_votes = 0, print_progress = FALSE, return_next_url = TRUE, start_url = url07)

currentBatch <- fullMovies[[1]]

save(currentBatch, file=(file.path("fullIMDbData_08.RData")))

url08 <- fullMovies[[2]]


fullMovies <- scrape_movies(n_start=200001, n_end=225000, min_number_votes = 0, print_progress = FALSE, return_next_url = TRUE, start_url = url08)

currentBatch <- fullMovies[[1]]

save(currentBatch, file=(file.path("fullIMDbData_09.RData")))

url09 <- fullMovies[[2]]


22750

fullMovies <- scrape_movies(n_start=225001, n_end=247750, min_number_votes = 0, print_progress = FALSE, return_next_url = TRUE, start_url = url09)

currentBatch <- fullMovies[[1]]

save(currentBatch, file=(file.path("fullIMDbData_10.RData")))

url10 <- fullMovies[[2]]



load("fullIMDbData_01.RData")
movies <- currentBatch

load("fullIMDbData_02.RData")
movies <- full_join(movies, currentBatch)

load("fullIMDbData_03.RData")
movies <- full_join(movies, currentBatch)

load("fullIMDbData_04.RData")
movies <- full_join(movies, currentBatch)

load("fullIMDbData_05.RData")
movies <- full_join(movies, currentBatch)

load("fullIMDbData_06.RData")
movies <- full_join(movies, currentBatch)

load("fullIMDbData_07.RData")
movies <- full_join(movies, currentBatch)

load("fullIMDbData_08.RData")
movies <- full_join(movies, currentBatch)

load("fullIMDbData_09.RData")
movies <- full_join(movies, currentBatch)

load("fullIMDbData_10.RData")
movies <- full_join(movies, currentBatch)




save(movies, file=(file.path("fullIMDbData.RData")))

load("fullIMDbData.RData")

p3 <- movies |> separate_rows(genre, sep =",\\s*") 

