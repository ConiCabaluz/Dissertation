library("RSelenium")
library("rvest")
library("tidyverse")
library("sys")

url <- "https://www.tripadvisor.com/Hotel_Review-g60982-d209422-Reviews-Hilton_Waikiki_Beach-Honolulu_Oahu_Hawaii.html"
rD <- rsDriver(browser="firefox", port=45354L, verbose=F)
remDr <- rD[["client"]]
remDr$navigate(url)