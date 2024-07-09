# Load the saved results
loaded_results <- readRDS("all_results.rds")

# Combine results into a single data frame for easy viewing
all_reviews <- do.call(rbind, lapply(all_results, function(x) {
  if (!is.null(x$reviews) && nrow(x$reviews) > 0) {
    cbind(restaurant_name = x$restaurant_name, x$reviews)
  }
}))

all_restaurants <- do.call(rbind, lapply(all_results, function(x) {
  data.frame(
    restaurant_name = x$restaurant_name,
    address = x$address,
    webpage_url = x$webpage_url,
    opening_hours = paste(x$opening_hours, collapse = "\n"),
    stringsAsFactors = FALSE
  )
}))

# Print loaded restaurant information and reviews
print(all_restaurants_loaded)
print(all_reviews)
