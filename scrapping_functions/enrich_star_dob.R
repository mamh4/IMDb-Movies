find_dob <- function(star_with_underscore){
  dob_data <- sapply(star_with_underscore, find_single_dob)
  # Combine the results into a single tibble
  dob_df <- bind_rows(dob_data)
  return(dob_df)
}


find_single_dob <- function(star) {
  star_with_underscore <- str_replace_all(str_squish(star), "\\s+", "_")
  url <- paste0("https://en.wikipedia.org/wiki/", star_with_underscore)
  tryCatch({
    webpage <- read_html(url)
    dob <- webpage %>%
      html_node(".infobox.biography.vcard .bday") %>%
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

