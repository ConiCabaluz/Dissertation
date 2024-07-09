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
  Sys.sleep(10)  # Increased wait time for the page to load
  
  # Extract the restaurant name
  restaurant_name <- remDr$findElement(using = 'xpath', "//h1[contains(@class, 'DUwDvf')]")$getElementText()[[1]]
  
  # Extract the address using primary and alternative XPaths
  address <- NA
  address_xpaths <- c(
    "//*[@id='QA0Szd']/div/div/div[1]/div[2]/div/div[1]/div/div/div[11]/div[3]/button/div/div[2]",
    "//*[@id='QA0Szd']/div/div/div[1]/div[2]/div/div[1]/div/div/div[11]/div[2]/div/div[1]",
    "//*[@id='QA0Szd']/div/div/div[1]/div[2]/div/div[1]/div/div/div[9]/div[3]/button/div/div[2]/div[1]"
  )
  
  for (xpath in address_xpaths) {
    if (length(remDr$findElements(using = 'xpath', xpath)) > 0) {
      tryCatch({
        address_element <- remDr$findElement(using = 'xpath', xpath)
        address <- address_element$getElementText()[[1]]
        print(paste("Found Address using XPath:", xpath, address))  # Debugging output
        break
      }, error = function(e) {
        message(paste("Could not find the address element using XPath:", xpath))
      })
    }
  }
  
  if (is.na(address)) {
    message("Address element not present.")
  }
  
  # Get the webpage URL using primary and alternative XPaths
  webpage_url <- NA
  webpage_url_xpaths <- c(
    "//*[@id='QA0Szd']/div/div/div[1]/div[2]/div/div[1]/div/div/div[11]/div[7]/a/div/div[2]/div[1]",
    "//*[@id='QA0Szd']/div/div/div[1]/div[2]/div/div[1]/div/div/div[11]/div[6]/a/div/div[2]/div[1]",
    "//*[@id='QA0Szd']/div/div/div[1]/div[2]/div/div[1]/div/div/div[9]/div[5]/a/div/div[2]/div[1]"
  )
  
  for (xpath in webpage_url_xpaths) {
    if (length(remDr$findElements(using = 'xpath', xpath)) > 0) {
      tryCatch({
        webpage_url_element <- remDr$findElement(using = 'xpath', xpath)
        webpage_url <- webpage_url_element$getElementText()[[1]]
        print(paste("Found Webpage URL using XPath:", xpath, webpage_url))  # Debugging output
        break
      }, error = function(e) {
        message(paste("Could not find the webpage URL element using XPath:", xpath))
      })
    }
  }
  
  if (is.na(webpage_url)) {
    message("Webpage URL element not present.")
  }
  
  # Click on the hours button to get hours information using primary and alternative XPaths
  hours <- NA
  hours_button_xpaths <- c(
    "//*[@id='QA0Szd']/div/div/div[1]/div[2]/div/div[1]/div/div/div[11]/div[4]/button",
    "//*[@id='QA0Szd']/div/div/div[1]/div[2]/div/div[1]/div/div/div[11]/div[4]/div[1]",
    "//*[@id='QA0Szd']/div/div/div[1]/div[2]/div/div[1]/div/div/div[9]/div[4]/div[1]"
  )
  hours_info_xpaths <- c(
    "//*[@id='QA0Szd']/div/div/div[1]/div[2]/div/div[1]/div/div/div[2]/div[1]/div[2]",
    "//*[@id='QA0Szd']/div/div/div[1]/div[2]/div/div[1]/div/div/div[11]/div[4]/div[2]",
    "//*[@id='QA0Szd']/div/div/div[1]/div[2]/div/div[1]/div/div/div[9]/div[4]/div[2]"
  )
  
  for (i in seq_along(hours_button_xpaths)) {
    if (length(remDr$findElements(using = 'xpath', hours_button_xpaths[i])) > 0) {
      tryCatch({
        hours_button <- remDr$findElement(using = 'xpath', hours_button_xpaths[i])
        hours_button$clickElement()
        Sys.sleep(5)  # Wait for the hours information to load
        print(paste("Clicked on the hours button using XPath:", hours_button_xpaths[i]))  # Debugging output
        
        # Extract hours information
        hours_element <- remDr$findElement(using = 'xpath', hours_info_xpaths[i])
        hours_html <- hours_element$getElementAttribute("outerHTML")[[1]]
        hours <- read_html(hours_html) %>%
          html_nodes("tr") %>%
          html_text(trim = TRUE)
        print(paste("Found Opening Hours using XPath:", hours_info_xpaths[i], paste(hours, collapse = "\n")))  # Debugging output
        break
      }, error = function(e) {
        message(paste("Could not find the hours button or information using XPath:", hours_button_xpaths[i]))
      })
    }
  }
  
  if (length(hours) == 0 || is.na(hours[1])) {
    message("Hours information not present.")
  }
  
  # Navigate back to the original URL to access the reviews button
  remDr$navigate(url)
  Sys.sleep(5)  # Wait for the page to load again
  
  # Click on the "Reviews" tab to reveal reviews
  tryCatch({
    reviews_tab_button_xpath <- "//*[@id='QA0Szd']/div/div/div[1]/div[2]/div/div[1]/div/div/div[3]/div/div/button[2]"
    reviews_tab_button <- remDr$findElement(using = 'xpath', reviews_tab_button_xpath)
    reviews_tab_button$clickElement()
    Sys.sleep(5)  # Wait for the reviews to load
    print("Clicked on the reviews tab button")  # Debugging output
  }, error = function(e) {
    message("Could not find the reviews tab button. Skipping reviews extraction.")
    return(list(restaurant_name = restaurant_name, address = address, webpage_url = webpage_url, opening_hours = hours, reviews = NA))
  })
  
  # Function to scroll the side panel and load more reviews
  scroll_to_load_reviews <- function() {
    side_panel <- remDr$findElement(using = 'css selector', 'div.m6QErb.DxyBCb.kA9KIf.dS8AEf.XiKgde')
    last_height <- as.numeric(remDr$executeScript("return arguments[0].scrollHeight", list(side_panel)))
    
    while (TRUE) {
      remDr$executeScript("arguments[0].scrollTop = arguments[0].scrollHeight", list(side_panel))
      Sys.sleep(2)  # Wait for more reviews to load
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
      Sys.sleep(0.5)  # Wait a bit after clicking to ensure the text expands
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
  
  # Add restaurant name, address, webpage URL, and opening hours to the results
  result <- list(
    restaurant_name = restaurant_name,
    address = address,
    webpage_url = webpage_url,
    opening_hours = hours,
    reviews = reviews
  )
  
  return(result)
}

# Initialize an empty list to store results
all_results <- list()

# Loop through each restaurant URL and gather data
for (url in restaurant_urls) {
  result <- scrape_google_maps(url)
  all_results <- append(all_results, list(result))
}

# Combine results into a single data frame for easy viewing
all_reviews <- do.call(rbind, lapply(all_results, function(x) x$reviews))
all_restaurants <- do.call(rbind, lapply(all_results, function(x) {
  data.frame(
    restaurant_name = x$restaurant_name,
    address = x$address,
    webpage_url = x$webpage_url,
    opening_hours = paste(x$opening_hours, collapse = "\n")
  )
}))

saveRDS(all_results, "all_results.rds")

# Print all restaurant information
print(all_restaurants)
print(all_reviews)

# Close the browser and server
remDr$close()
rD[["server"]]$stop()

