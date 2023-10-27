library(stringr)


# This script runs some regex and algorithms to clean up the studio and budget columns retrieved from web scrapping on
# Wikipedia. For Studio after implementing a series of regex it proved best to arrange the studio by charachter size
# and replace existing "relatively small studio name" with a larger one. i.e Netflix will replace Netflix + country.
# It is not the most robust but after trying different regex and algorithms it provided the best data.


# Regarding budget, we exclude non-US$ as it poses a larger challenge for converting currency and identifying currency
# to begin with. Luckily Wikipedia is consistent when providing budget info in US$ by often stating $ at the beginning.
# For other currencies it is sometimes symbolic and sometime abbreviated. 


# Variable names here are irrelevant as they are only used to go from one version to a cleaner one.



remove_references <- function(text) {
  # Remove everything between "[" and "]"
  cleaned_text <- gsub("\\[[^]]*\\]", "", text)
  return(cleaned_text)
}

orig_studio <- studio_and_budget$studio

studio_and_budget2 <- studio_and_budget
a <- remove_references(studio_and_budget2$studio) |> gsub("^\\n", "", x = _) |> gsub("\\n", ", ", x = _)

studio_and_budget2$studio <- a

studio_and_budget_unique <- studio_and_budget2 |> arrange(nchar(studio)) |> select(studio) |>unique()
studio_and_budget_unique_adjusted <- studio_and_budget2 |> arrange(nchar(studio)) |> select(studio) |> unique()


for(i in 1:nrow(studio_and_budget_unique_adjusted)) {
  pattern = studio_and_budget_unique_adjusted[[i, "studio"]]
  for (k in i:nrow(studio_and_budget_unique_adjusted)) {
    #print(paste("outer loop", i, "inner loop", k))
    if(i==nrow(studio_and_budget_unique_adjusted)){break}#prevent warning with grepl when k is the end
    if (grepl(pattern, x = studio_and_budget_unique_adjusted[[k, "studio"]])) {
      studio_and_budget_unique_adjusted[[k, "studio"]] <- pattern
    }
  }
}
join_table <- tibble(studio_key = studio_and_budget_unique$studio,
                     studio = studio_and_budget_unique_adjusted)

studio_and_budget2 <- studio_and_budget |> left_join(join_table, join_by(studio ==studio_key))
#studio_and_budget$studio <- studio_and_budget2$studio.y$studio

studio_and_budget_cleaned <- studio_and_budget2 |> select(-studio.y) |> 
  mutate(studio = studio_and_budget2$studio.y$studio,
         studio = if_else(studio=='20th Century-Fox', '20th Century Fox', studio),
         studio = if_else(studio == 'Buena Vista International' | studio == 'Buena Vista Distribution' |
                            studio == 'Buena Vista Film Distribution',
                          'Buena Vista Pictures', studio),
         studio = if_else(studio == "Pathe Distribution", "Pathé", studio),
         studio = if_else(grepl("AFMD",studio),"AFMD",studio),
         studio = if_else(studio=="Universal International" | studio=="Universal-International" | studio=="Universal Studios",
                          "Universal Pictures",studio)
         )

# Clean up Budget

orig_budget <- studio_and_budget_cleaned$budget
  
a <- remove_references(studio_and_budget_cleaned$budget)

#exclude everything after opening brackets or or TODO

b <- gsub("\\(.*", "", a) # remove everything after opening bracket

c <- gsub("\\s(or|to|and).*$", "", b)

d <-  str_extract(c, "\\$.*") #take everything from $ until end (Drop Budget figures using different currencies)

e <- str_replace(d, "[-−–—–].*?\\d+(,\\d+)?\\s", " ") #if there is "-" followed by some number, remove them
f <- str_replace(pattern = "[-−–—–].*", replacement = "", e)#remaining cases $40-"$"50

# cases where million pops up twice: unique(g[str_count(g, pattern = "million")>1]), remove either occurance

replace_excess_million <- function(text_array) {
  for (i in seq_along(text_array)) {
    if (!is.na(str_count(text_array[i], pattern = "million"))) {
      if (str_count(text_array[i], pattern = "million") > 1) {
        text_array[i] <- sub(x = text_array[i], pattern = "million", replacement = "")
      }
    }
  }
  return(text_array)
}

g <- replace_excess_million(f)

h <- gsub("Post.*", "", g) #removes post production (only care about budget "pre-production")

#if there is or something remove the or something
extract_million <- function(text) {
  # Use regular expressions to identify numbers with million
  pattern <- "(\\$?[0-9,.]+\\s?million)"
  
  # Extract the first numeric value with million (if it exists)
  matches <- regmatches(text, gregexpr(pattern, text))
  
  if (length(matches[[1]]) > 0) {
    # Get the first match
    first_match <- matches[[1]][1]
    
    # Remove non-numeric characters
    cleaned_match <- gsub("[^0-9.,]", "", first_match)
    
    # Convert to numeric (replace commas with periods for decimal values)
    #cleaned_match <- as.numeric(gsub(",", "\\.", cleaned_match))
    
    # Return the cleaned numeric value
    result <- paste(cleaned_match, "million")
  } else {
    # If there's no match, return the original text
    result <- text
  }
  
  return(result)
}
i <- sapply(h, extract_million) #|> unlist()

j <- gsub(pattern = "\\$",replacement = "",i) |> str_squish()
## All good till here:

convert_to_numeric <- function(text_array) {
  result <- numeric(length(text_array))
  for (i in seq_along(text_array)) {
    # Replace any text following "million"
    text <- sub("\\s?million.*", "", tolower(text_array[i]), perl = TRUE)
    
    # Remove commas and special characters
    text <- gsub("[^0-9.]", "", text, perl = TRUE)
    
    # Convert to numeric
    if (grepl("[0-9.]+", text)) {
      result[i] <- as.numeric(text)
    } else {
      result[i] <- NA
    }
    if (grepl("million", tolower(text_array[i]))) {
      result[i] <- result[i] * 1000000
    }
  }
  return(result)
}

k <- convert_to_numeric(j)


studio_and_budget_cleaned$budget <- k



if(nrow(studio_and_budget_cleaned)>=10000) {
  #Manual adjustments: boundary cases range is from thousand to million. i.e 800,000- 1 million
  studio_and_budget_cleaned$budget[2967] <- studio_and_budget_cleaned$budget[2967] / 1e6
  studio_and_budget_cleaned$budget[85] <- studio_and_budget_cleaned$budget[85] / 1e6
  
  #Manual Adjustment thousand entry in text instead of million
  studio_and_budget_cleaned$budget[8440] <- studio_and_budget_cleaned$budget[8440] * 1e4
  
  
  studio_and_budget_cleaned$budget[4496] <- NA
  #d[2967]
  #d[85]
  
  #when searching It's a Mad Mad Mad Mad World 1963 film on Wikipedia you get Mad men and the budget is incorrect
  studio_and_budget_cleaned[studio_and_budget_cleaned$title=='It\'s a Mad Mad Mad Mad World','budget'] <- 9.4e6
}

studio_and_budget_cleaned$orig_studio <- orig_studio
studio_and_budget_cleaned$orig_budget <- orig_budget


# clean-up memory
rm(
  orig_studio,
  studio_and_budget_unique,
  studio_and_budget_unique_adjusted,
  studio_and_budget2,
  a,
  b,
  c,
  d,
  e,
  f,
  g,
  h,
  i,
  j,
  k,
  orig_budget,
  convert_to_numeric,
  extract_million,
  remove_references,
  replace_excess_million
)
