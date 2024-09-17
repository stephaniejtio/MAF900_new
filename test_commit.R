# Source: Risk Return, and Equilibrium 1973 

#Libraries
library(RPostgres)
library(dplyr)
library(lubridate)
library(broom)
library(tidyverse)

#Connection to WRDS
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='stephaniejtio',
                  password='Stefanitio88')

crsp_data <- dbGetQuery(wrds, "
  SELECT permno, date, ret
  FROM crsp.msf
  WHERE date >= '1926-01-01' AND date <= '1968-12-31'
")

# Convert date to Date object
crsp_data$date <- ymd(crsp_data$date)

# Define function to calculate beta for each stock
calculate_beta <- function(data) {
  market_model <- lm(ret ~ vwretd, data = data)
  tidy(market_model) %>%
    filter(term == "vwretd") %>%
    select(estimate) %>%
    pull()
}
crsp_data <- crsp_data %>%
  mutate(year = year(date), month = month(date))
