# Install necessary packages if not already installed
# install.packages("rvest")
# install.packages("RSelenium")
# install.packages("httr")
# install.packages("stringr")

# Load required libraries
library(rvest)
library(RSelenium)
library(httr)
library(stringr)

# Start a Selenium server and browser
rD <- rsDriver(browser = "chrome", port = 4444L, chromever = "latest")
remDr <- rD[["client"]]

# Function to scrape Google Maps reviews
scrape_google_maps <- function(url) {
  remDr$navigate(url)
  Sys.sleep(2)  # Wait for the page to load
  
  # Function to scroll the side panel and load more reviews
  scroll_to_load_reviews <- function() {
    side_panel <- remDr$findElement(using = 'css selector', 'div.m6QErb.DxyBCb.kA9KIf.dS8AEf.XiKgde')  # Adjust the selector as necessary
    last_height <- as.numeric(remDr$executeScript("return arguments[0].scrollHeight", list(side_panel)))
    
    while (TRUE) {
      remDr$executeScript("arguments[0].scrollTop = arguments[0].scrollHeight", list(side_panel))
      Sys.sleep(1.5)  # Wait for more reviews to load
      new_height <- as.numeric(remDr$executeScript("return arguments[0].scrollHeight", list(side_panel)))
      
      print(paste("last_height:", last_height, "new_height:", new_height))  # Debugging output
      
      if (new_height == last_height) {
        print("No more reviews to load.")
        break
      }
      last_height <- new_height
    }
  }
  
  scroll_to_load_reviews()
  
  # Click on all "Read more" buttons to expand long reviews
  expand_long_reviews <- function() {
    read_more_buttons <- remDr$findElements(using = 'css selector', 'button.w8nwRe.kyuRq')
    for (button in read_more_buttons) {
      button$clickElement()
      #Sys.sleep(0.2)  # Wait a bit after clicking to ensure the text expands
    }
  }
  
  expand_long_reviews()  # Ensure long reviews are expanded
  
  # Get the page source after loading all reviews
  page_source <- remDr$getPageSource()[[1]]
  write(page_source, "page_source.html")  # Save page source to file for inspection
  
  # Parse the page source
  page <- read_html(page_source)
  
  reviews <- data.frame(text = character(), date = character(), rating = character(), stringsAsFactors = FALSE)
  
  review_elements <- page %>% html_nodes(".jftiEf.fontBodyMedium")
  print(paste("Number of review elements found:", length(review_elements)))  # Debugging output
  
  for (element in review_elements) {
    review_text <- element %>% html_node(".wiI7pd") %>% html_text(trim = TRUE)
    review_date <- element %>% html_node(".rsqaWe") %>% html_text(trim = TRUE)
    review_rating <- element %>% html_node(".kvMYJc") %>% html_attr("aria-label") %>% str_extract("\\d+")
    
    reviews <- rbind(reviews, data.frame(text = review_text, date = review_date, rating = review_rating, stringsAsFactors = FALSE))
  }
  
  return(reviews)
}

# Example URL
url <- 'https://www.google.com/maps/place/The+Up+In+Arms/@51.7607238,-1.2353526,16z/data=!4m8!3m7!1s0x4876c1c80b006027:0x4ad26578740b9ce1!8m2!3d51.7595082!4d-1.2360883!9m1!1b1!16s%2Fg%2F11fk3n4q4t?entry=ttu'
reviews <- scrape_google_maps(url)

print(reviews)

# Close the browser and server
remDr$close()
rD[["server"]]$stop()
