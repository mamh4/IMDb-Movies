find_dob <- function(star_or_director){
  dob_tibble <- tibble(star_or_director = character(), dob = character())
  for(i in seq_along(1:nrow(star_or_director))){
    dob_data <- find_single_dob(star_or_director[[i,1]])
    dob_tibble <- rbind(dob_tibble,dob_data)
  }
  return(dob_tibble)
}


find_single_dob <- function(star) {
  star_with_underscore <- str_replace_all(string = str_squish(star),pattern =  "\\s+",replacement =  "_")
  url <- paste0("https://en.wikipedia.org/wiki/", star_with_underscore)
  tryCatch({
    webpage <- read_html(url)
    dob <- webpage %>%
      html_node("span.bday") %>%
      html_text()
    
    cat("Date of Birth for ", star, ": ", dob, "\n")
    
    # Return a tibble with "title" and "dob" columns
    tibble(title = star, dob = dob)
  }, error = function(e) {
    cat("Error occurred for ", star, ": ", conditionMessage(e), "\n")
    # Return a tibble with "title" and "dob" columns, where "dob" is NA
    tibble(title = star, dob = NA)
  })
}
