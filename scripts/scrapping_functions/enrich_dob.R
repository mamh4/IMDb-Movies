find_dob <- function(star_or_director){
  dob_tibble <- tibble(star_or_director = character(), dob = character())
  for(i in seq_along(star_or_director)){
    dob_data <- find_single_dob(star_or_director[i])
    dob_tibble <- rbind(dob_tibble,dob_data)
  }
  return(dob_tibble)
}


find_single_dob <- function(star) {
  star_with_underscore <- str_replace_all(str_squish(star), "\\s+", "_")
  url <- paste0("https://en.wikipedia.org/wiki/", star_with_underscore)
  tryCatch({
    webpage <- read_html(url)
    dob <- webpage %>%
      html_node("span.bday") %>%
      html_text()
    
    cat("Date of Birth for ", star_with_underscore, ": ", dob, "\n")
    
    # Return a tibble with "title" and "dob" columns
    tibble(title = star_with_underscore, dob = dob)
  }, error = function(e) {
    cat("Error occurred for ", star_with_underscore, ": ", conditionMessage(e), "\n")
    # Return a tibble with "title" and "dob" columns, where "dob" is NA
    tibble(title = star_with_underscore, dob = NA)
  })
}
