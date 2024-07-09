library(dplyr)
library(lubridate)
library(stringr)
library(tidytext)
library(textstem)
library(textdata)

all_reviews <- do.call(rbind, lapply(all_results, function(x) {
  if (!is.null(x$reviews) && nrow(x$reviews) > 0) {
    cbind(restaurant_name = x$restaurant_name, x$reviews)
  }
}))

### Step 1: Remove rows with empty restaurant_name or text ###
all_reviews <- all_reviews %>%
  filter(restaurant_name != "", text != "")

### Step 2: Convert relative dates to actual dates ###
convert_relative_date <- function(relative_date) {
  # Convert the date string to lowercase and remove any leading or trailing whitespace
  relative_date <- tolower(trimws(relative_date))
  
  # Tokenize the date string
  parts <- str_split(relative_date, "\\s+", simplify = TRUE)
  
  if (length(parts) != 3) {
    stop("Unexpected date format: ", relative_date)
  }
  
  # Handle cases with "un", "una"
  if (parts[2] %in% c("un", "una")) {
    number <- 1
    unit <- parts[3]
  } else {
    number <- as.numeric(parts[2])
    unit <- parts[3]
  }
  
  # Create a dictionary for unit conversion
  unit_dict <- list(
    "hora" = "hours",
    "horas" = "hours",
    "día" = "days",
    "dias" = "days",
    "día" = "days",
    "días" = "days",
    "semana" = "weeks",
    "semanas" = "weeks",
    "mes" = "months",
    "meses" = "months",
    "año" = "years",
    "años" = "years",
    "ano" = "years",
    "anos" = "years",
    "dia" = "days",
    "dias" = "days"
  )
  
  # Convert the unit to plural form using the dictionary
  unit <- unit_dict[[unit]]
  
  # Check if unit conversion was successful
  if (is.null(unit)) {
    stop("Unexpected date format: ", relative_date) # Report unexpected formats
  }
  
  # Get the current date
  current_date <- Sys.Date()
  
  # Adjust the current date based on the relative description
  adjusted_date <- switch(unit,
                          "hours" = current_date - hours(number),
                          "days" = current_date - days(number),
                          "weeks" = current_date - weeks(number),
                          "months" = current_date - months(number),
                          "years" = current_date - years(number),
                          stop("Unexpected date format: ", relative_date)
  )
  
  return(as.Date(adjusted_date))
}

# Apply the function to the date column using lapply and unlist
all_reviews$date <- as.Date(unlist(lapply(all_reviews$date, convert_relative_date)))

### Step 3: Filter out dates older than 3 years ###
cutoff_date <- Sys.Date() - years(3)
all_reviews <- all_reviews %>%
  filter(date >= cutoff_date)

### Step 4: Add an ID to each review by restaurant ###
Reviews_clean <- all_reviews %>%
  group_by(restaurant_name) %>%
  mutate(ID = row_number(),
         document = paste(restaurant_name, ID, sep = "_")) %>%
  select(document, restaurant_name, ID, date, text)

### Step 5: Reviews by word ###
Reviews_by_word <- Reviews_clean %>% 
  group_by(document) %>% 
  unnest_tokens(word, text) 

# Creating additional stop words with correct quotation mark for the books
additional_stop_words <- stop_words
additional_stop_words$word <- gsub("'", "’", additional_stop_words$word)

### Step 6: excluding stop words ###
Reviews_by_word <- Reviews_by_word %>% 
  anti_join(stop_words, by = "word") %>% 
  anti_join(additional_stop_words, by = "word") %>% 
  filter(!is.na(word))

### Step 7: Lemmatize ### 
Reviews_by_word_l <- Reviews_by_word
Reviews_by_word_l$word <- lemmatize_words(Reviews_by_word_l$word)

### Step 8: Sentiment analysis ###
# Load sentiment lexicons
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")

# Join with tokenized words
Reviews_with_sentiment <- Reviews_by_word_l %>%
  inner_join(bing, by = "word") %>%
  group_by(document, sentiment) %>%
  summarize(sentiment_count = n()) %>%
  spread(sentiment, sentiment_count, fill = 0) %>%
  ungroup()

# Calculate overall sentiment score for each review
Reviews_with_sentiment <- Reviews_with_sentiment %>%
  mutate(sentiment_score = positive - negative)

