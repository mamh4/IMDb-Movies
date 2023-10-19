library(stringr)
# Clean up Studio
remove_references <- function(text) {
  # Remove everything between "[" and "]"
  cleaned_text <- gsub("\\[[^]]*\\]", "", text)
  return(cleaned_text)
}

a <- remove_references(studio_and_budget$studio) |> gsub("^\\n", "", x = _) |> gsub("\\n", ", ", x = _)
b <- gsub("([a-z])([A-Z])", "\\1, \\2", a)
c <- gsub(pattern = ", Inc",replacement = " Inc", b)
d <- str_squish(c)

studio_and_budget$studio <- d

# Clean up Budget

orig_budget <- studio_and_budget$budget
  
a <- remove_references(studio_and_budget$budget)

#exclude everything after opening brackets or or TODO

b <- gsub("\\(.*", "", a) # remove everything after opening bracket

c <- gsub("\\s(or|to|and).*$", "", b)

d <-  str_extract(c, "\\$.*") #take everything from $ until end (Drop Budget figures using different currencies)

e <- str_replace(d, "[-−–—–].*?\\d+(,\\d+)?\\s", " ")#if there is "-" followed by some number, remove them
f <- sub("[-−–—–].*", "", e)#remaining cases $40-"$"50

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


studio_and_budget$budget <- k




#Manual adjustments: boundary cases range is from thousand to million. i.e 800,000- 1 million
studio_and_budget$budget[2967] <- studio_and_budget$budget[2967]/1e6
studio_and_budget$budget[85] <- studio_and_budget$budget[85]/1e6

#Manual Adjustment thousand entry in text instead of million
studio_and_budget$budget[8440] <- studio_and_budget$budget[8440]*1e4


studio_and_budget$budget[4496] <- NA
#d[2967]
#d[85]



convert_to_numeric <- function(text) {
  
  cleaned_text <- gsub("[^0-9.,]", "", text)
  
  # Replace any commas inside numbers with periods
  cleaned_text <- gsub("(?<=\\d),(?=\\d)", ".", cleaned_text, perl = TRUE)
  
  # Replace periods only if they are followed by three digits (for thousands separators)
  cleaned_text <- gsub("\\.(?=\\d{3})", "", cleaned_text, perl = TRUE)
  
  # Replace any remaining commas with empty strings
  cleaned_text <- gsub(",", "", cleaned_text)
  
  # Convert to numeric
  numeric_value <- as.numeric(cleaned_text)
  
  
  if (grepl("million", tolower(text))) {
    numeric_value <- numeric_value * 1e6
  }
  
  return(numeric_value)
}

k <- sapply(j, convert_to_numeric)


studio_and_budget$budget <- k

studio_and_budget$orig_budget <- orig_budget


# clean-up memory
rm(
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
