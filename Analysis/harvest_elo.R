library(tidyverse)
library(rvest)
library(RSelenium)

url <- "https://www.eloratings.net/2018_World_Cup"

rsd <- rsDriver(browser = "chrome")
remDr <- rsd$client

#Go to your url
remDr$navigate(url)
page <- read_html(remDr$getPageSource()[[1]])


read_html("https://www.eloratings.net/2018_World_Cup") %>%
  html_node(xpath = '//*[@id="maintable_2018_World_Cup"]')
