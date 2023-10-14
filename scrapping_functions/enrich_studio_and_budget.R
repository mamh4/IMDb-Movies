enrich_studio_and_budget <- function(titles_and_year_df) {
  # Initialize an empty list to store results
  results <- list()
  
  for (title in seq_along(1:nrow(titles_and_year_df))) {
    # Create the search query based on the movie title
    search_query <- paste(titles_and_year_df[title,1],titles_and_year_df[title,2], "film")
    
    # Create the search URL
    search_url <- paste0("https://en.wikipedia.org/w/index.php?search=", URLencode(search_query))
    #browser()
    # Read the HTML content of the search results webpage
    
    
    # Initialize a variable for tracking retry attempts
    retry_attempt <- 1
    success <- FALSE
    
    while(!success){
      
      check_internet_connection <- function() {
        tryCatch({
          con <- url("http://www.google.com", "r")
          close(con)
          cat("Internet connection is available.\n")
          return(TRUE)
        }, error = function(e) {
          cat("Internet connection is not available.\n")
          return(FALSE)
        })
      }
      # Attempt to process the data batch
      #cat("Processing title ", title, ", Attempt ", retry_attempt, "\n")
      search_webpage <- try(read_html(search_url), silent = TRUE)
      
      if (!inherits(search_webpage, "try-error")) {
        # Processing was successful
        success <- TRUE
        
        # Extract the URL of the first search result (Wikipedia webpage)
        first_result_url <- search_webpage %>%
          html_node(".mw-search-result-heading a") %>%
          html_attr("href")
        
        # Create the full URL of the first search result
        full_url <- paste0("https://en.wikipedia.org", first_result_url)
        # Read the HTML content of the first search result webpage (movie's Wikipedia webpage)
        webpage <- tryCatch({
          read_html(full_url)
        }, error = function(e) {
          return(search_webpage)
        })
        
        # Initialize budget and studio as NA before tryCatch
        
        # Extract the budget
        budget <- tryCatch({
          webpage %>%
            html_node("th:contains('Budget') + td") %>%
            html_text()
        }, error = function(e) {
          return(NA)
        })
        
        # Extract the studio
        studio <- tryCatch({
          webpage %>%
            html_node("th:contains('Distributed by') + td") %>%
            html_text()
        }, error = function(e) {
          return(NA)
        })
        
        # Store the result in the list
        result <- tibble(title = titles_and_year_df[[title,1]], studio = as.character(studio), budget = as.character(budget))
        cat(title," ", titles_and_year_df[[title,1]]," ",studio," ", budget,"\n")
        results[[title]] <- result
      } 
      else if(inherits(search_webpage, "try-error") & check_internet_connection()){
        success <- TRUE
        result <- tibble(title = titles_and_year_df[[title,1]], studio = NA, budget = NA)
        cat(title," ", titles_and_year_df[[title,1]]," ",NA," ", NA,"\n")
        results[[title]] <- result
      } else {
        retry_attempt <- retry_attempt + 1
        Sys.sleep(10)
      }
      
      
    }
    
  }
  # Combine the results into a single tibble
  results_df <- do.call(rbind, results)
  
  return(results_df)
  
}