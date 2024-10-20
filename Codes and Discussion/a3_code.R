# MAF900 Assignment 3 - T2 2024
# By Stephanie and Kristina
# You can find detailed work of each person in the following
# Fama Macbeth (1973) Replication
# Note: Based on the requirement of this project, Dr Saikat requests the students to collect the data 
# until Dec 2023. After checking the rules of the three periods formation (Portfolio formation, Initial Estimation,
# Testing Period) and data appropriateness, we find it more appropriate to use December 2022 as the end date, 
# which makes the report clearer and more understandable.

#The following codes are for MAF900 Replication Assignment 3 in T3 2024. 


#########################
#Start by Kristina
#Install some necessary packages
#install and add required packages 
#install.packages("broom")
#install.packages("slider")
install.packages("officer")
install.packages("flextable")
update.packages(c("flextable", "systemfonts"))
install.packages("systemfonts", dependencies = TRUE)
install.packages("flextable", dependencies = TRUE)
install.packages("kableExtra")
install.packages("xtable")
install.packages("openxlsx")

#Load required libraries 
library(broom)
library(RPostgres)
library(tidyverse)
library(RSQLite)
library(furrr)
library(lubridate)
library(dplyr)
library(slider)
library(purrr)
library(modelsummary)
library(tseries)
library(officer)
library(kableExtra)
library(xtable)
library(openxlsx)
library(tidyr)

#Connect to Steph wrds
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='stephaniejtio')

#Connect to kristina's wrds
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='s224294027')

#Collect CRSP monthly stock return data 
#Filtering data between the start of 1926 to the end of 2023
msf_db <- tbl(wrds, sql("select * from crsp.msf"))
start_date <- ymd("1926-01-01")
end_date <- ymd("2023-12-31")

#Select the relevant columns from the CRSP data for the analysis
crsp_monthly <- msf_db |>
  filter(date >= start_date & date <= end_date) |>
  select(
    permno, # security identifier
    date, # date of the observation
    ret, # return
  ) |> collect()

#Stock and exchange identifier
msenames_db <- tbl(wrds, sql("select * from crsp.msenames"))

#Filter and collect stock identifiers
crsp_stockids <- msenames_db |>
  select (permno, primexch)|> collect()|>unique()|> filter(primexch == 'N')

capm_data <- crsp_monthly |> inner_join(
  crsp_stockids |>
    select(permno, primexch), by = c("permno"))

#Create Fisher Index - for Rm
fi_mkt <- capm_data|> group_by(date)|> summarise(raw_mkt = mean(ret, na.rm = TRUE))

#Combine stock return and market return i.e. fisher index return
capm_data <- capm_data %>%
  select(permno, date, ret) %>%
  rename(raw_ret = ret) %>%
  inner_join(fi_mkt, by = c("date")) %>%
  rename(month = date) %>%
  arrange(permno, month) %>%
  drop_na(raw_ret, raw_mkt)

#Finding the securities meeting the data requirements
#The logic of this part is, first using the period 1 to check the consistency of the codes, then run function to loop
#Finding the securities during Jan 1935 (first month of a testing period)
permno_first_month <- capm_data %>%
  filter(month >= as.Date("1935-01-01") & month < as.Date("1935-02-01")) %>%
  select(permno) %>%
  distinct()%>%
  pull(permno)

#Calculate the number of permno in January 1935
count_first_month <- length(permno_first_month)

#Count the number of permnos that have data for each month from 1926 to 1934
count_valid_permno <- capm_data %>%
  filter(month >= as.Date("1926-01-01") & month <= as.Date("1934-12-31")) %>%
  filter(permno %in% permno_first_month) %>%
  group_by(permno) %>%
  summarize(months_count = n_distinct(format(month, "%Y-%m"))) %>%
  filter(months_count == 108)

#The amount, count the number of securities
count_in_all_months <- nrow(count_valid_permno)
#Finish by Kristina 
#########################


#########################
#Start by Stephanie
#Portfolio formation 
#This approach is based and aligned with Fama's paper (referring to page 615 - 618)
#Calculate BETA for period 1926-1929

# Portfolio formation using estimated betas for the period 1926-1929
capm_data1 <- capm_data %>%
  filter(month >= as.Date("1926-01-01") & month <= as.Date("1929-12-31"))
capm_data1<- capm_data1 %>%
  inner_join(count_valid_permno, by = "permno")

#Calculate beta for each security 
beta_results <- capm_data1 %>%
  group_by(permno) %>%
  do(tidy(lm(raw_ret ~ raw_mkt, data = .)))

#Print the results for beta
print(beta_results)

#Filter for the beta results and extract the necessary statistics (beta, standard error, t-statistic, and p-value)
beta_results_only <- beta_results %>%
  filter(term == "raw_mkt") %>%
  select(permno, beta = estimate, std_error = std.error, t_statistic = statistic, p_value = p.value)

#Beta results for each of the security 
print(beta_results_only) 

#Check for any missing values (NA) in the beta estimates
sum(is.na(beta_results_only$beta)) 

#Remove NA values from beta
beta_results_only <- beta_results_only %>%
  filter(!is.na(beta))

#Sort the securities by their estimated beta values
beta_results_only <- beta_results_only %>%
  arrange(beta) 

#Based on the paper Page 615 section B.Details 
#Allocating securities into 20 portfolios 

#Calculate the total number of securities 
N <- nrow(beta_results_only)

#Calculate the number of securities, middle portfolios
securities_per_portfolio <- floor(N / 20)

#Calculate the remainder, allocate to first and last portfolios
remainder <- N - 20 * securities_per_portfolio

#Determine number of securities, first and last portfolios (this is based on Fama's approach)
first_last_extra <- floor(remainder / 2)
last_portfolio_extra <- remainder %% 2  # If odd, last portfolio gets an extra security

#Calculating the size of each porfolio
portfolio_sizes <- c(
  securities_per_portfolio + first_last_extra,        # first portfolio
  rep(securities_per_portfolio, 18),                 # 2nd to 19th portfolio
  securities_per_portfolio + first_last_extra + last_portfolio_extra  # 20th portfolio
)

#Create a variable named portfolio
beta_results_only$portfolio <- rep(1:20, times = portfolio_sizes)
#Finish by Stephanie
#########################


#########################
#Start from Kristina
#This approach is based and aligned with Fama's paper (referring to page 615 - 618)
#Calculate BETA for period 1930-1934 using CAPM model
capm_data2 <- capm_data %>%
  filter(month >= as.Date("1930-01-01") & month <= as.Date("1934-12-31")) 

#Finding those securities meeting the data requirement
capm_data2<- capm_data2 %>%
  inner_join(count_valid_permno, by = "permno")

#Compute the regression model for each permno and extract the standard deviations of residuals
beta_results2 <- capm_data2 %>%
  group_by(permno) %>%
  do({
    m1 <- lm(raw_ret ~ raw_mkt, data = .)
    residuals <- resid(m1)  
    idsr <- sd(residuals) 
    tidy(m1)%>%
      mutate(idsr = idsr)
  })

#Getting the value of beta and extract relevant statistics
beta_results_only2 <- beta_results2 %>%
  filter(term == "raw_mkt") %>%
  select(permno, beta = estimate, std_error = std.error, t_statistic = statistic, p_value = p.value, idsr = idsr)

#Remove any missing beta values
beta_results_only2 <- beta_results_only2 %>%
  filter(!is.na(beta))

#Calculate the portfolio return during 1935-1938
#Set a new dataset for period 1935-1938
capm_data3 <- capm_data %>%
  filter(month >= as.Date("1935-01-01") & month <= as.Date("1938-12-31"))
capm_data3<- capm_data3 %>%
  inner_join(count_valid_permno, by = "permno")

#Portfolio
capm_data3<- capm_data3 %>%
  inner_join(beta_results_only %>% select(permno, portfolio), by = "permno")

#Join beta_results_only2 with portfolio number
beta_results_only2 <- beta_results_only2 %>%
  inner_join(beta_results_only %>% select(permno, portfolio), by = "permno")

#Calculate the portfolio beta and standard errors of beta during 1930-1934.
beta_means_by_portfolio2 <- beta_results_only2 %>%
  group_by(portfolio) %>%              
  summarise(mean_beta = mean(beta, na.rm = TRUE),
            mean_std_beta = mean(std_error, na.rm = TRUE),
            mean_idsr = mean(idsr, na.rm = TRUE))

#Calculate the portfolio beta and standard errors of beta during 1930-1935.
#Filter the data by date range and group by permno, run the regression.
beta_means_by_portfolio2 <- capm_data %>%
  filter(month >= as.Date("1930-01-01") & month <= as.Date("1935-12-31")) %>%
  inner_join(count_valid_permno, by = "permno") %>%
  group_by(permno) %>%
  do({
    m1 <- lm(raw_ret ~ raw_mkt, data = .)   
    residuals <- resid(m1)                 
    idsr <- sd(residuals)                  
    tidy_m1 <- tidy(m1)                    
    data.frame(
      beta = tidy_m1$estimate[2],            
      std_error = tidy_m1$std.error[2],     
      idsr = idsr                           
    )
  }) %>%
  ungroup()%>%
  inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%  
  group_by(portfolio) %>%
  summarise(
    mean_beta1 = mean(beta, na.rm = TRUE),        
    mean_std_beta1 = mean(std_error, na.rm = TRUE),
    mean_idsr1 = mean(idsr, na.rm = TRUE)  
  ) %>%
  ungroup()%>%
  inner_join(beta_means_by_portfolio2, by = "portfolio")

#Calculate the porfolio beta and standard errors of beta during 1930-1936.
#Filter the data by date range and group by permno, run the regression.
beta_means_by_portfolio2 <- capm_data %>%
  filter(month >= as.Date("1930-01-01") & month <= as.Date("1936-12-31")) %>%
  inner_join(count_valid_permno, by = "permno") %>%
  group_by(permno) %>%
  do({
    m1 <- lm(raw_ret ~ raw_mkt, data = .)   
    residuals <- resid(m1)                 
    idsr <- sd(residuals)                  
    tidy_m1 <- tidy(m1)                    
    data.frame(
      beta = tidy_m1$estimate[2],            
      std_error = tidy_m1$std.error[2],     
      idsr = idsr                           
    )
  }) %>%
  ungroup()%>%
  inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%  
  group_by(portfolio) %>%
  summarise(
    mean_beta2 = mean(beta, na.rm = TRUE),        
    mean_std_beta2 = mean(std_error, na.rm = TRUE),
    mean_idsr2 = mean(idsr, na.rm = TRUE)  
  ) %>%
  ungroup()%>%
  inner_join(beta_means_by_portfolio2, by = "portfolio")

#Calculate the porfolio beta and standard errors of beta during 1930-1937.
#Filter the data by date range and group by permno, run the regression.
beta_means_by_portfolio2 <- capm_data %>%
  filter(month >= as.Date("1930-01-01") & month <= as.Date("1937-12-31")) %>%
  inner_join(count_valid_permno, by = "permno") %>%
  group_by(permno) %>%
  do({
    m1 <- lm(raw_ret ~ raw_mkt, data = .)   
    residuals <- resid(m1)                 
    idsr <- sd(residuals)                  
    tidy_m1 <- tidy(m1)                    
    data.frame(
      beta = tidy_m1$estimate[2],            
      std_error = tidy_m1$std.error[2],     
      idsr = idsr                           
    )
  }) %>%
  ungroup()%>%
  inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%  
  group_by(portfolio) %>%
  summarise(
    mean_beta3 = mean(beta, na.rm = TRUE),        
    mean_std_beta3 = mean(std_error, na.rm = TRUE),
    mean_idsr3 = mean(idsr, na.rm = TRUE)  
  ) %>%
  ungroup()%>%
  inner_join(beta_means_by_portfolio2, by = "portfolio")

#Calculate mean beta and std. 
beta_means_by_portfolio2 <- beta_means_by_portfolio2 %>%
  group_by(portfolio) %>%
  summarise(
    mean_beta_avg = mean(c(mean_beta, mean_beta1, mean_beta2, mean_beta3, na.rm = TRUE)),
    mean_std_beta_avg = mean(c(mean_std_beta, mean_std_beta1, mean_std_beta2, mean_std_beta3, na.rm = TRUE)),
    mean_idsr_avg = mean(c(mean_idsr, mean_idsr1, mean_idsr2, mean_idsr3, na.rm = TRUE)),
  )

#Calculate the monthly portfolio return
average_by_portfolio_month3 <- capm_data3 %>%
  group_by(portfolio, month) %>%        
  summarize(avg_ret = mean(raw_ret, na.rm = TRUE), 
            avg_mkt = mean(raw_mkt, na.rm = TRUE)) %>%
  ungroup()

#Linear regression calculates the R^2 between avg_ret and avg_mkt
r_squared_by_portfolio3 <- average_by_portfolio_month3 %>%
  group_by(portfolio) %>% 
  do({
    model <- lm(avg_ret ~ avg_mkt, data = .)
    r_squared <- summary(model)$r.squared  
    data.frame(r_squared = r_squared) 
  }) %>%
  ungroup() 

#Calculate the standard deviation of avg_ret by portfolio classification
stddev_by_portfolio3 <- average_by_portfolio_month3 %>%
  group_by(portfolio) %>% 
  summarise(
    stddev_avg_ret = sd(avg_ret, na.rm = TRUE)
  ) %>%
  ungroup() 

#Create a new file named table2 to merge together
table2 <- stddev_by_portfolio3 %>%
  inner_join(r_squared_by_portfolio3, by = "portfolio") %>%
  inner_join(beta_means_by_portfolio2, by = "portfolio") 
#Finished by Kristina 
#########################


#########################
#Start by Stephanie
#Standard deviation of the portfolio residuals

#Regression for each portfolio to get the residuals and calculate their standard deviation
portfolio_residuals3 <- average_by_portfolio_month3 %>%
  group_by(portfolio) %>%
  do({
    model <- lm(avg_ret ~ avg_mkt, data = .)  
    residuals <- resid(model)  
    data.frame(month = .$month, residuals = residuals)
  })

#Calculate the standard deviation of residuals for each portfolio 
#Standard deviation of residuals
stddev_residuals_by_portfolio3 <- portfolio_residuals3 %>%
  group_by(portfolio) %>%
  summarise(stddev_residuals = sd(residuals, na.rm = TRUE))

#Perform join to Table 2
table2 <- table2 %>%
  inner_join(stddev_residuals_by_portfolio3, by = "portfolio")

#Finish by Stephanie
#########################


#########################
#Start by Stephanie and Kristina
#After confirming the accuracy of codes using the period 1. 
#We generate a function to repeat the above steps by using different period
#Generalised function to perform Fama-MacBeth type analysis for a given period
#Function input includes the capm data, portfolio, estimation, and testing start end end date of the period

run_fama_macbeth_analysis <- function(capm_data, portfolio_start, portfolio_end, 
                                      estimation_start, estimation_end, estimation_end1, 
                                      estimation_end2, estimation_end3, testing_start,
                                      testing_start1, testing_end) {
  
  #Finding the securities meeting the data requirements
  permno_first_month <- capm_data %>%
    filter(month >= as.Date(testing_start) & month < as.Date(testing_start1)) %>%
    select(permno) %>%
    distinct()%>%
    pull(permno)
  
  #Calculate the number of permno in first month of testing period
  count_first_month <- length(permno_first_month)
  
  #Count the number of permnos that meeting the data requirement
  #Portfolio formation period
  valid_permnos_formation <- capm_data %>%
    filter(month >= as.Date(portfolio_start) & month <= as.Date(portfolio_end)) %>%
    mutate(year = format(month, "%Y"), month = format(month, "%m")) %>%
    group_by(permno, year) %>%
    summarise(month_count = n_distinct(month), .groups = 'drop') %>%
    filter(month_count == 12) %>% 
    group_by(permno) %>%
    summarise(years_count = n()) %>%
    filter(years_count >= 4) %>%  
    pull(permno)
  
  #Estimation period
  count_valid_permno  <- capm_data %>%
    filter(permno %in% valid_permnos_formation) %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end)) %>%
    mutate(year = format(month, "%Y"), month = format(month, "%m")) %>%
    group_by(permno, year) %>%
    summarise(month_count = n_distinct(month), .groups = 'drop') %>%
    filter(month_count == 12) %>%  
    group_by(permno) %>%
    summarise(years_count = n()) %>%
    filter(years_count == 5) 
  
  #The amount
  count_in_all_months <- nrow(count_valid_permno)
  
  #Portfolio formation period: calculate BETA for portfolio_start to portfolio_end
  portfolio_data <- capm_data %>%
    filter(month >= as.Date(portfolio_start) & month <= as.Date(portfolio_end))
  portfolio_data<- portfolio_data %>%
    inner_join(count_valid_permno, by = "permno")
  
  #Calculate beta
  beta_results <- portfolio_data %>%
    group_by(permno) %>%
    do(tidy(lm(raw_ret ~ raw_mkt, data = .)))
  
  #Filter beta results and assign portfolios
  beta_results_only <- beta_results %>%
    filter(term == "raw_mkt") %>%
    select(permno, beta = estimate, std_error = std.error, t_statistic = statistic, p_value = p.value) %>%
    filter(!is.na(beta))
  
  #Calculate the number of securities and assign portfolios based on ranked betas
  beta_results_only <- beta_results_only %>%
    arrange(beta)  #Ranking
  
  N <- nrow(beta_results_only)
  # 20 portfolios based on the Fama paper (referring to page 615 of the paper)
  securities_per_portfolio <- floor(N / 20)
  remainder <- N - 20 * securities_per_portfolio
  first_last_extra <- floor(remainder / 2)
  last_portfolio_extra <- remainder %% 2
  
  portfolio_sizes <- c(
    securities_per_portfolio + first_last_extra,        # First portfolio
    rep(securities_per_portfolio, 18),                 # Middle portfolios
    securities_per_portfolio + first_last_extra + last_portfolio_extra  # Last portfolio
  )
  
  beta_results_only$portfolio <- rep(1:20, times = portfolio_sizes)
  
  #Initial Estimation Period: calculate BETA year by year
  estimation_period_data <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end))
  estimation_period_data<- estimation_period_data %>%
    inner_join(count_valid_permno, by = "permno")
  
  #Beta calculations
  beta_results2 <- estimation_period_data %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)
      residuals <- resid(m1)  
      idsr <- sd(residuals) 
      tidy(m1)%>%
        mutate(idsr = idsr)
    })
  
  beta_results_only2 <- beta_results2 %>%
    filter(term == "raw_mkt") %>%
    select(permno, beta = estimate, std_error = std.error, t_statistic = statistic, p_value = p.value, idsr = idsr)
  
  beta_results_only2 <- beta_results_only2 %>%
    filter(!is.na(beta))
  
  #Testing Period: Calculate returns for each portfolio
  testing_period_data <- capm_data %>%
    filter(month >= as.Date(testing_start) & month <= as.Date(testing_end))
  testing_period_data<- testing_period_data %>%
    inner_join(count_valid_permno, by = "permno")
  
  #Portfolio
  testing_period_data<- testing_period_data %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno")
  
  #Join beta_results_only2 with portfolio number
  beta_results_only2 <- beta_results_only2 %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno")
  
  #Calculate the porfolio beta and standard errors of beta.
  beta_means_by_portfolio2 <- beta_results_only2 %>%
    group_by(portfolio) %>%              
    summarise(mean_beta = mean(beta, na.rm = TRUE),
              mean_std_beta = mean(std_error, na.rm = TRUE),
              mean_idsr = mean(idsr, na.rm = TRUE))
  
  #Calculate the porfolio beta and standard errors of beta by updating one year.
  #Filter the data by date range and group by permno, run the regression.
  beta_means_by_portfolio2 <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end1)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)   
      residuals <- resid(m1)                 
      idsr <- sd(residuals)                  
      tidy_m1 <- tidy(m1)                    
      data.frame(
        beta = tidy_m1$estimate[2],            
        std_error = tidy_m1$std.error[2],     
        idsr = idsr                           
      )
    }) %>%
    ungroup()%>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%  
    group_by(portfolio) %>%
    summarise(
      mean_beta1 = mean(beta, na.rm = TRUE),        
      mean_std_beta1 = mean(std_error, na.rm = TRUE),
      mean_idsr1 = mean(idsr, na.rm = TRUE)  
    ) %>%
    ungroup()%>%
    inner_join(beta_means_by_portfolio2, by = "portfolio")
  
  #Calculate the porfolio beta and standard errors of beta by updating one year.
  #Filter the data by date range and group by permno, run the regression.
  beta_means_by_portfolio2 <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end2)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)   
      residuals <- resid(m1)                 
      idsr <- sd(residuals)                  
      tidy_m1 <- tidy(m1)                    
      data.frame(
        beta = tidy_m1$estimate[2],            
        std_error = tidy_m1$std.error[2],     
        idsr = idsr                           
      )
    }) %>%
    ungroup()%>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%  
    group_by(portfolio) %>%
    summarise(
      mean_beta2 = mean(beta, na.rm = TRUE),        
      mean_std_beta2 = mean(std_error, na.rm = TRUE),
      mean_idsr2 = mean(idsr, na.rm = TRUE)  
    ) %>%
    ungroup()%>%
    inner_join(beta_means_by_portfolio2, by = "portfolio")
  
  #Calculate the porfolio beta and standard errors of beta by updating one year.
  #Filter the data by date range and group by permno, run the regression.
  beta_means_by_portfolio2 <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end3)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)   
      residuals <- resid(m1)                 
      idsr <- sd(residuals)                  
      tidy_m1 <- tidy(m1)                    
      data.frame(
        beta = tidy_m1$estimate[2],            
        std_error = tidy_m1$std.error[2],     
        idsr = idsr                           
      )
    }) %>%
    ungroup()%>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%  
    group_by(portfolio) %>%
    summarise(
      mean_beta3 = mean(beta, na.rm = TRUE),        
      mean_std_beta3 = mean(std_error, na.rm = TRUE),
      mean_idsr3 = mean(idsr, na.rm = TRUE)  
    ) %>%
    ungroup()%>%
    inner_join(beta_means_by_portfolio2, by = "portfolio")
  
  #Calculate mean beta and standard deviation
  beta_means_by_portfolio2 <- beta_means_by_portfolio2 %>%
    group_by(portfolio) %>%
    summarise(
      mean_beta_avg = mean(c(mean_beta, mean_beta1, mean_beta2, mean_beta3, na.rm = TRUE)),
      mean_std_beta_avg = mean(c(mean_std_beta, mean_std_beta1, mean_std_beta2, mean_std_beta3, na.rm = TRUE)),
      mean_idsr_avg = mean(c(mean_idsr, mean_idsr1, mean_idsr2, mean_idsr3, na.rm = TRUE)),
    )
  
  #Calculate average returns by portfolio
  average_by_portfolio_month <- testing_period_data %>%
    group_by(portfolio, month) %>%
    summarize(avg_ret = mean(raw_ret, na.rm = TRUE), avg_mkt = mean(raw_mkt, na.rm = TRUE)) %>%
    ungroup()
  
  #Calculate R-squared values
  r_squared_by_portfolio <- average_by_portfolio_month %>%
    group_by(portfolio) %>%
    do({
      model <- lm(avg_ret ~ avg_mkt, data = .)
      r_squared <- summary(model)$r.squared
      data.frame(r_squared = r_squared)
    }) %>%
    ungroup()
  
  #Calculate standard deviation of returns
  stddev_by_portfolio <- average_by_portfolio_month %>%
    group_by(portfolio) %>%
    summarise(stddev_avg_ret = sd(avg_ret, na.rm = TRUE)) %>%
    ungroup()
  
  #Residuals
  portfolio_residuals3 <- average_by_portfolio_month %>%
    group_by(portfolio) %>%
    do({
      model <- lm(avg_ret ~ avg_mkt, data = .)  
      residuals <- resid(model)  
      data.frame(month = .$month, residuals = residuals)
    })
  
  #Standard Deviations of residuals
  stddev_residuals_by_portfolio3 <- portfolio_residuals3 %>%
    group_by(portfolio) %>%
    summarise(stddev_residuals = sd(residuals, na.rm = TRUE)) 
  
  #Calculate the final table merging all information
  final_table <- stddev_by_portfolio %>%
    inner_join(r_squared_by_portfolio, by = "portfolio") %>%
    inner_join(beta_means_by_portfolio2, by = "portfolio")%>%
    inner_join(stddev_residuals_by_portfolio3, by = "portfolio")%>%
    mutate(count_first_month = count_first_month,
           count_in_all_months = count_in_all_months,
           ratio = stddev_residuals / mean_idsr_avg,
           date1 = as.Date(portfolio_start),
           date2 = as.Date(estimation_start),
           date3 = as.Date(testing_start),
           date4 = as.Date(portfolio_end),
           date5 = as.Date(estimation_end),
           date6 = as.Date(testing_end))
  
  return(final_table)
}
#Finish by Kristina and Stephanie
#########################


#########################
#Start by Kristina
#Define periods from Table 1
#Initialise an empty list for periods
periods <- list()

#Define the start and end year
start_year <- 1927
end_year <- 2007

#Loop to generate each time period
for (year in seq(start_year, end_year, by = 4)) {
  portfolio_start <- paste0(year, "-01-01")
  portfolio_end <- paste0(year + 7 - 1, "-12-31")
  estimation_start <- paste0(year + 7, "-01-01")
  estimation_end <- paste0(year + 12 - 1, "-12-31")
  estimation_end1 <- paste0(year + 12, "-12-31")
  estimation_end2 <- paste0(year + 13, "-12-31")
  estimation_end3 <- paste0(year + 14, "-12-31")
  testing_start <- paste0(year + 12, "-01-01")
  testing_start1 <- paste0(year + 12, "-02-01")
  testing_end <- paste0(year + 16 - 1, "-12-31")
  
  #Add the generated time period to the list
  periods[[length(periods) + 1]] <- list(
    portfolio_start = portfolio_start,
    portfolio_end = portfolio_end,
    estimation_start = estimation_start,
    estimation_end = estimation_end,
    estimation_end1 = estimation_end1,
    estimation_end2 = estimation_end2,
    estimation_end3 = estimation_end3,
    testing_start = testing_start,
    testing_start1 = testing_start1,
    testing_end = testing_end
  )
}

#Considering the first one
extra_period <- list(
  portfolio_start = "1926-01-01",
  portfolio_end = "1929-12-31",
  estimation_start = "1930-01-01",
  estimation_end = "1934-12-31",
  estimation_end1 = "1935-12-31",
  estimation_end2 = "1936-12-31",
  estimation_end3 = "1937-12-31",
  testing_start = "1935-01-01",
  testing_start1 = "1935-02-01",
  testing_end = "1938-12-31"
)

#Add time periods to the periods list
combined_periods <- c(list(extra_period), periods)

#Loop through each period and calculate results
results <- lapply(combined_periods, function(combined_periods) {
  run_fama_macbeth_analysis(capm_data, combined_periods$portfolio_start, combined_periods$portfolio_end, combined_periods$estimation_start, combined_periods$estimation_end, combined_periods$estimation_end1, combined_periods$estimation_end2, combined_periods$estimation_end3, combined_periods$testing_start, combined_periods$testing_start1, combined_periods$testing_end)
})

#Combine results into a data frame
combined_results_table1_2 <- do.call(rbind, lapply(results, as.data.frame))

#Create Table 1
table1 <- combined_results_table1_2 %>%
  filter(portfolio == 1) %>%
  mutate(
    date1 = as.Date(date1, format = "%Y-%m-%d"),
    date2 = as.Date(date2, format = "%Y-%m-%d"),
    date3 = as.Date(date3, format = "%Y-%m-%d"),
    date4 = as.Date(date4, format = "%Y-%m-%d"),
    date5 = as.Date(date5, format = "%Y-%m-%d"),
    date6 = as.Date(date6, format = "%Y-%m-%d"),
    `Portfolio formation period` = paste(year(date1), year(date4), sep = " - "),
    `Initial estimation period` = paste(year(date2), year(date5), sep = " - "),
    `Testing period` = paste(year(date3), year(date6), sep = " - "),
    `No. of securities available` = as.character(count_first_month),
    `No. of securities meeting data requirement` = as.character(count_in_all_months)
  ) %>%
  select(
    `Portfolio formation period`,
    `Initial estimation period`,
    `Testing period`,
    `No. of securities available`,
    `No. of securities meeting data requirement`
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  group_by(Variable) %>%
  summarise(Value = list(Value)) %>%
  unnest_wider(Value, names_sep = "_") %>%
  mutate(Variable = factor(Variable, levels = c(
    "Portfolio formation period",
    "Initial estimation period",
    "Testing period",
    "No. of securities available",
    "No. of securities meeting data requirement"
  ))) %>%
  arrange(Variable)


colnames(table1)[-1] <- paste0("Period", 1:(ncol(table1) - 1))

#Output Table 1
write.xlsx(table1, "table1.xlsx", rowNames = FALSE)

#Organising table2
table2 <- combined_results_table1_2 %>%
  filter(date3 == "1935-01-01" 
         | date3 == "1943-01-01"
         | date3 == "1951-01-01"
         | date3 == "1959-01-01"
         | date3 == "1967-01-01"
         | date3 == "1975-01-01"
         | date3 == "1983-01-01"
         | date3 == "1991-01-01"
         | date3 == "1999-01-01"
         | date3 == "2007-01-01"
         | date3 == "2015-01-01") %>%
  mutate(
    date1 = as.Date(date1, format = "%Y-%m-%d"),
    date2 = as.Date(date2, format = "%Y-%m-%d"),
    date3 = as.Date(date3, format = "%Y-%m-%d"), #make sure the format of date
    `Portfolio` = as.integer(portfolio),
    `Portfolio for Estimation period` = as.integer(year(date3)-1),
    `Beta_p,t-1` = mean_beta_avg,  
    `s(Beta_p,t-1)` = mean_std_beta_avg,
    `r(Rp, Rm)^2` = r_squared,
    `s(Rp)` = stddev_avg_ret,
    `s(Epsilon_p)` = mean_idsr_avg,
    `s_p,t-1(Epsilon_i)` = stddev_residuals,
    `s(Epsilon_p)/s_p,t-1(Epsilon_i)` = ratio
  ) %>%
  select(
    `Portfolio`,
    `Portfolio for Estimation period`,
    `Beta_p,t-1`,  
    `s(Beta_p,t-1)`,
    `r(Rp, Rm)^2`,
    `s(Rp)`,
    `s(Epsilon_p)`,
    `s_p,t-1(Epsilon_i)`,
    `s(Epsilon_p)/s_p,t-1(Epsilon_i)`
  ) %>% 
  mutate(across(-c(Portfolio, `Portfolio for Estimation period`), 
                ~ round(., 3)))

#Spliting table2 into different periods
tables_split <- split(table2, table2$`Portfolio for Estimation period`)
for (value in names(tables_split)) {
  assign(paste0("table2_", value), tables_split[[value]])
}

tables <- list(table2_1934, table2_1942, table2_1950, table2_1958, table2_1966,table2_1974,
               table2_1982,table2_1990,table2_1998,table2_2006,table2_2014)

#Integrating together
tables_processed <- lapply(tables, function(df) {
  df %>% 
    select(-Portfolio) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    group_by(Variable) %>%
    summarise(Value = list(Value)) %>%
    unnest_wider(Value, names_sep = "_") %>%
    mutate(Variable = factor(Variable, levels = c(
      "Portfolio for Estimation period",
      "Beta_p,t-1",
      "s(Beta_p,t-1)",
      "r(Rp, Rm)^2",
      "s(Rp)",
      "s(Epsilon_p)",
      "s_p,t-1(Epsilon_i)",
      "s(Epsilon_p)/s_p,t-1(Epsilon_i)"
    ))) %>%
    arrange(Variable) %>%
    rename(Statistic = Variable) %>%
    rename_with(~ paste0("Portfolio", seq_along(.)), starts_with("Value_"))
})

table2_combined <- bind_rows(tables_processed)

#Outputing table2
write.xlsx(table2_combined, "table2.xlsx", rowNames = FALSE)

#Finish by Kristina
#########################


#########################
#Start by Stephanie and Kristina
#TABLE 3
###Collect FF 3 factor data to get Rf (based on Dr. Saikat's lecture)
#Fama-French 3-factor data from Kenneth French's
#Based on Dr. Saikat's lecture Topic 6
#We focus on extracting the risk-free rate (Rf) for use in cross-sectional regression models.

temp <- tempfile(fileext = ".zip")
download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip", temp)
temp1 <- unzip(temp, exdir = ".")
ff_3factors_monthly <- read_csv(temp1, skip = 5) 
names(ff_3factors_monthly) <- c('dt', 'rmrf', 'smb', 'hml', 'rf')
unlink(temp)
unlink(temp1)

# Convert data and cleaning the data from FF 3-factor data
# Get date, RF, and month from the FF 3 factor data 
ff_3factors_mon <- ff_3factors_monthly |> 
  filter(nchar(dt) == 6) |> 
  mutate(
    yr = str_sub(dt, 1, 4),
    mon = str_sub(dt, -2, -1),
    date = make_date(year = yr, month = mon, day = 1),
    mkt_excess = as.numeric(rmrf),
    smb = as.numeric(smb),
    hml = as.numeric(hml),
    rf = as.numeric(rf),
    month_label = format(date, "%Y-%m") 
  ) |> 
  select(c('date', 'rf', 'month_label')) 


#Function to get all betas used in Table 3 to calculate gamma coefficients
#For Table 3, it requires us to run several multiple periods. 
#For convenience, creating a function to collect all betas and other statistics in different time point for Table 3. 
calculate_gamma_table3_with_yearly_update <- function(capm_data, portfolio_start, portfolio_end, estimation_start, estimation_end, 
                                                      estimation_end1, estimation_end2, estimation_end3, testing_start, testing_start1, testing_end, 
                                                      testing_start_m1, testing_start_m2, testing_start_m3, testing_start_m4, testing_end_m1, 
                                                      testing_end_m2, testing_end_m3, testing_end_m4) {
  
  
  #Identify the permno (unique stock identifier) available in the first testing month
  #This ensures that the securities analyzed are present throughout the testing period.
  permno_first_month <- capm_data %>%
    filter(month >= as.Date(testing_start)& month < as.Date(testing_start1)) %>%
    select(permno) %>%
    distinct() %>%
    pull(permno)
  
  #Filter permno for valid periods and data requirements (referring to Fama Macbeth paper, we should use data requirements)
  valid_permnos_formation <- capm_data %>%
    filter(month >= as.Date(portfolio_start) & month <= as.Date(portfolio_end)) %>%
    mutate(year = format(month, "%Y"), month = format(month, "%m")) %>%
    group_by(permno, year) %>%
    summarise(month_count = n_distinct(month), .groups = 'drop') %>%
    filter(month_count == 12) %>% 
    group_by(permno) %>%
    summarise(years_count = n()) %>%
    filter(years_count >= 4) %>%  
    pull(permno)
  
  #Count the valid permno from the CAPM data for the estimation period.
  count_valid_permno  <- capm_data %>%
    filter(permno %in% valid_permnos_formation) %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end)) %>%
    mutate(year = format(month, "%Y"), month = format(month, "%m")) %>%
    group_by(permno, year) %>%
    summarise(month_count = n_distinct(month), .groups = 'drop') %>%
    filter(month_count == 12) %>%  
    group_by(permno) %>%
    summarise(years_count = n()) %>%
    filter(years_count == 5) 
  
  #Assign portfolios based on ranked betas
  #Following Fama-MacBeth (1973), the securities are sorted into 20 portfolios
  portfolio_data <- capm_data %>%
    filter(month >= as.Date(testing_start) & month <= as.Date(testing_end)) %>%
    inner_join(count_valid_permno, by = "permno")
  
  #Estimate betas for each security using CAPM regression of security returns
  beta_results <- portfolio_data %>%
    group_by(permno) %>%
    do(tidy(lm(raw_ret ~ raw_mkt, data = .)))
  
  #Filter only the beta results, assigned portfolios, permno, std error
  beta_results_only <- beta_results %>%
    filter(term == "raw_mkt") %>%
    select(permno, beta = estimate, std_error = std.error) %>%
    filter(!is.na(beta))
  
  #Arrange securities by beta for portfolio assignment
  beta_results_only <- beta_results_only %>%
    arrange(beta) 
  
  #Securities per portfolio, we use the number of row of the previous data we got / 20
  #calculated as 1/20th of the total securities following Fama-MacBeth (1973)
  N <- nrow(beta_results_only)
  securities_per_portfolio <- floor(N / 20)
  remainder <- N - 20 * securities_per_portfolio
  first_last_extra <- floor(remainder / 2)
  last_portfolio_extra <- remainder %% 2
  
  #Define portfolio sizes to distribute securities evenly
  portfolio_sizes <- c(
    securities_per_portfolio + first_last_extra,        # First portfolio
    rep(securities_per_portfolio, 18),                 # Middle portfolios
    securities_per_portfolio + first_last_extra + last_portfolio_extra  # Last portfolio
  )
  
  #Assign portfolios to securities based on their beta values
  beta_results_only$portfolio <- rep(1:20, times = portfolio_sizes)
  
  #Update yearly, calculate portfolio beta and standard errors for the estimation periods
  beta_0 <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)
      tidy(m1) %>%
        filter(term == "raw_mkt") %>%
        select(beta = estimate, std_error = std.error) %>%
        mutate(residual_sd = sd(residuals(m1), na.rm = TRUE)) 
    }) %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
    group_by(portfolio) %>%
    summarise(mean_beta = mean(beta, na.rm = TRUE), 
              mean_std_beta = mean(std_error, na.rm = TRUE),
              mean_residual_sd = mean(residual_sd, na.rm = TRUE))  
  
#Finish by Kristina and Stephanie. 
#########################
  
  
#########################
  #Start by Kristina
  #Calculate betas by updating one year at a time
  beta_1 <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end1)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)
      tidy(m1) %>%
        filter(term == "raw_mkt") %>%
        select(beta = estimate, std_error = std.error)%>%
        mutate(residual_sd = sd(residuals(m1), na.rm = TRUE)) 
    }) %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
    group_by(portfolio) %>%
    summarise(mean_beta = mean(beta, na.rm = TRUE), mean_std_beta = mean(std_error, na.rm = TRUE),
              mean_residual_sd = mean(residual_sd, na.rm = TRUE))
  
  #Calculate betas by updating one year at a time
  beta_2 <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end2)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)
      tidy(m1) %>%
        filter(term == "raw_mkt") %>%
        select(beta = estimate, std_error = std.error)%>%
        mutate(residual_sd = sd(residuals(m1), na.rm = TRUE)) 
    }) %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
    group_by(portfolio) %>%
    summarise(mean_beta = mean(beta, na.rm = TRUE), mean_std_beta = mean(std_error, na.rm = TRUE),
              mean_residual_sd = mean(residual_sd, na.rm = TRUE))
  
  #Calculate betas by updating one year at a time
  beta_3 <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end3)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)
      tidy(m1) %>%
        filter(term == "raw_mkt") %>%
        select(beta = estimate, std_error = std.error)%>%
        mutate(residual_sd = sd(residuals(m1), na.rm = TRUE)) 
    }) %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
    group_by(portfolio) %>%
    summarise(mean_beta = mean(beta, na.rm = TRUE), mean_std_beta = mean(std_error, na.rm = TRUE),
              mean_residual_sd = mean(residual_sd, na.rm = TRUE))
  
  
  #Calculate average returns by portfolio and month
  #This section estimates average returns for each portfolios to prepare the data for the Fama-MacBeth cross-sectional regression.
  avg_returns_by_portfolio <- capm_data %>%
    filter(month >= as.Date(testing_start) & month <= as.Date(testing_end)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%  
    group_by(portfolio, month, month_label) %>%  
    summarise(avg_ret = mean(raw_ret, na.rm = TRUE),
              avg_mkt = mean(raw_mkt, na.rm = TRUE)) %>%
    ungroup()
  
  #Creating a data frames to including all months in each year for betas
  months_0 <- tibble(month = seq(ymd(testing_start_m1), ymd(testing_end_m1), by = "month")) %>%
    mutate(year = year(month), 
           month_label = format(month, "%Y-%m"))
  months_1 <- tibble(month = seq(ymd(testing_start_m2), ymd(testing_end_m2), by = "month")) %>%
    mutate(year = year(month), 
           month_label = format(month, "%Y-%m"))
  months_2 <- tibble(month = seq(ymd(testing_start_m3), ymd(testing_end_m3), by = "month")) %>%
    mutate(year = year(month), 
           month_label = format(month, "%Y-%m"))
  months_3 <- tibble(month = seq(ymd(testing_start_m4), ymd(testing_end_m4), by = "month")) %>%
    mutate(year = year(month), 
           month_label = format(month, "%Y-%m"))
  
  #Putting beta_0 to month 0
  beta_0 <- beta_0 %>%
    select(portfolio, mean_beta, mean_std_beta,mean_residual_sd) %>%
    expand_grid(months_0) %>%
    left_join(beta_0, by = "portfolio") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%
    select(-mean_beta.y, -mean_std_beta.y, -mean_residual_sd.y) %>%  
    rename(mean_beta = mean_beta.x, mean_std_beta = mean_std_beta.x, mean_residual_sd=mean_residual_sd.x) 
  
  #Putting beta_1 to month 1
  #Repeat for beta_1, beta_2, and beta_3
  beta_1 <- beta_1 %>%
    select(portfolio, mean_beta, mean_std_beta,mean_residual_sd) %>%
    expand_grid(months_1) %>%
    left_join(beta_1, by = "portfolio") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%
    select(-mean_beta.y, -mean_std_beta.y, -mean_residual_sd.y) %>%  
    rename(mean_beta = mean_beta.x, mean_std_beta = mean_std_beta.x, mean_residual_sd = mean_residual_sd.x) 
  
  #Putting beta_2 to month 2
  beta_2 <- beta_2 %>%
    select(portfolio, mean_beta, mean_std_beta, mean_residual_sd) %>%
    expand_grid(months_2) %>%
    left_join(beta_2, by = "portfolio") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%
    select(-mean_beta.y, -mean_std_beta.y, -mean_residual_sd.y) %>%  
    rename(mean_beta = mean_beta.x, mean_std_beta = mean_std_beta.x, mean_residual_sd=mean_residual_sd.x) 
  
  #Putting beta_3 to month 3
  beta_3 <- beta_3 %>%
    select(portfolio, mean_beta, mean_std_beta, mean_residual_sd) %>%
    expand_grid(months_3) %>%
    left_join(beta_3, by = "portfolio") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%
    select(-mean_beta.y, -mean_std_beta.y, -mean_residual_sd.y) %>%  
    rename(mean_beta = mean_beta.x, mean_std_beta = mean_std_beta.x, mean_residual_sd= mean_residual_sd.x) 
  
  #Combine all beta calculations into one data frame for further analysis
  #This step prepares the combined dataset for cross-sectional regression analysis as per the Fama-MacBeth approach.
  combined <- bind_rows(beta_0, beta_1, beta_2, beta_3)
  
  #Merging betas with average portfolio return
  avg_returns_by_portfolio <- avg_returns_by_portfolio %>%
    left_join(combined %>% select(portfolio, mean_beta, mean_std_beta, mean_residual_sd, month_label), 
              by = c("month_label" = "month_label", "portfolio" = "portfolio"))%>%
    mutate(mean_beta_square = mean_beta * mean_beta)

  return(avg_returns_by_portfolio)
}

#Begin to define different period
#Period
periods <- list()

#Define the start and end year
start_year <- 1927
end_year <- 2007

#Loop to generate each time period
for (year in seq(start_year, end_year, by = 4)) {
  portfolio_start <- paste0(year, "-01-01")
  portfolio_end <- paste0(year + 7 - 1, "-12-31")
  estimation_start <- paste0(year + 7, "-01-01")
  estimation_end <- paste0(year + 12 - 1, "-12-31")
  estimation_end1 <- paste0(year + 12, "-12-31")
  estimation_end2 <- paste0(year + 13, "-12-31")
  estimation_end3 <- paste0(year + 14, "-12-31")
  testing_start <- paste0(year + 12, "-01-01")
  testing_start1 <- paste0(year + 12, "-02-01")
  testing_end <- paste0(year + 16 - 1, "-12-31")
  testing_start_m1 <- paste0(year + 12, "-01-01")
  testing_start_m2 <- paste0(year + 13, "-01-01")
  testing_start_m3 <- paste0(year + 14, "-01-01")
  testing_start_m4 <- paste0(year + 15, "-01-01")
  testing_end_m1 <- paste0(year + 12, "-12-31")
  testing_end_m2 <- paste0(year + 13, "-12-31")
  testing_end_m3 <- paste0(year + 14, "-12-31")
  testing_end_m4 <- paste0(year + 15, "-12-31")
  
  #Add the generated time period to the list
  periods[[length(periods) + 1]] <- list(
    portfolio_start = portfolio_start,
    portfolio_end = portfolio_end,
    estimation_start = estimation_start,
    estimation_end = estimation_end,
    estimation_end1 = estimation_end1,
    estimation_end2 = estimation_end2,
    estimation_end3 = estimation_end3,
    testing_start = testing_start,
    testing_start1 = testing_start1,
    testing_end = testing_end,
    testing_start_m1 = testing_start_m1,
    testing_start_m2 = testing_start_m2,
    testing_start_m3 = testing_start_m3,
    testing_start_m4 = testing_start_m4,
    testing_end_m1 = testing_end_m1,
    testing_end_m2 = testing_end_m2,
    testing_end_m3 = testing_end_m3,
    testing_end_m4 = testing_end_m4
  )
}

#Considering the first one
extra_period <- list(
  portfolio_start = "1926-01-01",
  portfolio_end = "1929-12-31",
  estimation_start = "1930-01-01",
  estimation_end = "1934-12-31",
  estimation_end1 = "1935-12-31",
  estimation_end2 = "1936-12-31",
  estimation_end3 = "1937-12-31",
  testing_start = "1935-01-01",
  testing_start1 = "1935-02-01",
  testing_end = "1938-12-31",
  testing_start_m1 = "1935-01-01",
  testing_start_m2 = "1936-01-01",
  testing_start_m3 = "1937-01-01",
  testing_start_m4 = "1938-01-01",
  testing_end_m1 = "1935-12-31",
  testing_end_m2 = "1936-12-31",
  testing_end_m3 = "1937-12-31",
  testing_end_m4 = "1938-12-31"
)

#Add time periods to the periods list
combined_periods <- c(list(extra_period), periods)

#Loop through each period and calculate results
results <- lapply(combined_periods, function(combined_periods) {
  calculate_gamma_table3_with_yearly_update(capm_data, combined_periods$portfolio_start, combined_periods$portfolio_end, combined_periods$estimation_start, 
                            combined_periods$estimation_end, combined_periods$estimation_end1, combined_periods$estimation_end2, 
                            combined_periods$estimation_end3, combined_periods$testing_start, combined_periods$testing_start1, 
                            combined_periods$testing_end, combined_periods$testing_start_m1, combined_periods$testing_start_m2,
                            combined_periods$testing_start_m3, combined_periods$testing_start_m4, combined_periods$testing_end_m1,
                            combined_periods$testing_end_m2, combined_periods$testing_end_m3, combined_periods$testing_end_m4)
})

#Combine results into a data frame
combined_results <- do.call(rbind, lapply(results, as.data.frame))

#Look for monthly rf
ff_3factors_mon <- ff_3factors_monthly |> 
  filter(nchar(dt) == 6) |> 
  mutate(
    yr = str_sub(dt, 1, 4),
    mon = str_sub(dt, -2, -1),
    date = make_date(year = yr, month = mon, day = 1),
    mkt_excess = as.numeric(rmrf),
    smb = as.numeric(smb),
    hml = as.numeric(hml),
    rf = as.numeric(rf),
    month_label = format(date, "%Y-%m") 
  ) |> 
  select(c('date', 'rf', 'month_label')) |> 
  filter(month_label >= "1935-01" & month_label <= "2022-12")

#Merge with combined_result
combined_results <- combined_results |> 
  left_join(ff_3factors_mon, by = "month_label")

#Begin with Table 3
#First, looking for panel a
#A function to create table 3 panela
table3_panela_function <- function(combined_results, start, end) {
  
  #Calculate gamma0 and gamma1 using Fama-MacBeth regression (cross-sectional regression)
  table3 <- combined_results %>% 
    filter(month >= as.Date(start) & month <= as.Date(end)) %>%     
    group_by(month) %>%       
    do({       model <- lm(avg_ret ~ mean_beta, data = .)       
    tidy(model)       }) %>%     
    ungroup()%>%     
    summarise(       
      gamma0 = mean(estimate[term == "(Intercept)"], na.rm = TRUE),       
      gamma1 = mean(estimate[term == "mean_beta"], na.rm = TRUE),       
      s_gamma0 = mean(std.error[term == "(Intercept)"], na.rm = TRUE),       
      s_gamma1 = mean(std.error[term == "mean_beta"], na.rm = TRUE),
      .groups = 'drop'     )
  
  #Calculate adj.R2
  adjusted_r2 <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>% 
    group_by(month) %>%
    do({
      model <- lm(avg_ret ~ mean_beta, data = .)
      model_summary <- summary(model)
      tibble(
        month = unique(.$month),
        adj_r_squared = model_summary$adj.r.squared
      )
    }) %>%
    ungroup()
  
  #Mean value
  mean_adjusted_r2 <- adjusted_r2 %>%
    summarise(mean_adj_r_squared = mean(adj_r_squared, na.rm = TRUE),
              sd_adj_r_squared = sd(adj_r_squared, na.rm = TRUE))
  
  #Merge together
  table3 <- table3 %>%
    left_join(mean_adjusted_r2, by = character())
  
  #A regression
  regressions <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>% 
    group_by(month) %>%
    do({
      model <- lm(avg_ret ~ mean_beta, data = .)
      tidy(model)  
    }) %>%
    ungroup()
  
  #Getting monthly gamma0, gamma1, etc
  regression_summary <- regressions %>%
    filter(term %in% c("(Intercept)", "mean_beta")) %>%
    pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic)) %>%
    rename(
      gama0_monthly = `estimate_(Intercept)`,
      s_gama0_monthly = `std.error_(Intercept)`,
      t_gama0_monthly = `statistic_(Intercept)`,
      gama1_monthly = `estimate_mean_beta`,
      s_gama1_monthly = `std.error_mean_beta`,
      t_gama1_monthly = `statistic_mean_beta`
    )%>%
    select(-contains("p.value"))
  
  #calculate the sample mean
  mean_gama0 <- mean(regression_summary$gama0_monthly, na.rm = TRUE)
  mean_gama1 <- mean(regression_summary$gama1_monthly, na.rm = TRUE)
  
  #As there are some N/A values, I aim to combine all efficient values into one dataframe
  regression_summary1 <- regression_summary %>%
    filter(!is.na(gama0_monthly))%>%
    select(
      month,
      gama0_monthly,
      s_gama0_monthly,
      t_gama0_monthly
    )
  
  regression_summary2 <- regression_summary %>%
    filter(!is.na(gama1_monthly))%>%
    select(
      month,
      gama1_monthly,
      s_gama1_monthly,
      t_gama1_monthly
    )

  regression_summary <- regression_summary1 %>%
    left_join(regression_summary2, by = "month")
  
  #Merging all together
  regression_summary <- regression_summary %>%
    mutate(month_label = format(month, "%Y-%m"))
  
  #Get the value of gamma0-rf
  regression_summary <- regression_summary %>%
    left_join(ff_3factors_mon, by = "month_label") %>%
    mutate(gama0_minus_rf = gama0_monthly - rf)
  
  #Mean
  mean_gama0_minus_rf <- mean(regression_summary$gama0_minus_rf, na.rm = TRUE)
  
  #Calculate first-order serial correlation
  corr_m0 <- cor(regression_summary$gama0_monthly, lag(regression_summary$gama0_monthly), use = "complete.obs")
  corr_m1 <- cor(regression_summary$gama1_monthly, lag(regression_summary$gama1_monthly), use = "complete.obs")
  
  #Assuming a correlation with a mean of 0
  corr0_0 <- cor(regression_summary$gama0_monthly, lag(regression_summary$gama0_monthly) - mean_gama0, use = "complete.obs")
  corr0_1 <- cor(regression_summary$gama1_monthly, lag(regression_summary$gama1_monthly) - mean_gama1, use = "complete.obs")
  
  #Merge the results into table3
  corr0_gamma0_minus_rf <- cor(regression_summary$gama0_minus_rf, lag(regression_summary$gama0_minus_rf) - mean_gama0_minus_rf, use = "complete.obs")
  
  #t statistics of all
  t_gamma0_minus_rf <- regression_summary %>%
    summarise(
      t_gamma0_minus_rf = mean(gama0_minus_rf, na.rm = TRUE) / (sd(gama0_minus_rf, na.rm = TRUE) / sqrt(sum(!is.na(gama0_minus_rf))))
    )
  
  t_gamma0 <- regression_summary %>%
    summarise(
      t_gamma0 = mean(gama0_monthly, na.rm = TRUE) / (sd(gama0_monthly, na.rm = TRUE) / sqrt(sum(!is.na(gama0_monthly))))
    )
  
  t_gamma1 <- regression_summary %>%
    summarise(
      t_gamma1 = mean(gama1_monthly, na.rm = TRUE) / (sd(gama1_monthly, na.rm = TRUE) / sqrt(sum(!is.na(gama1_monthly))))
    )
  
  #Merge together
  table3 <- table3 %>%
    cross_join(t_gamma0_minus_rf) %>%
    cross_join(t_gamma0) %>%
    cross_join(t_gamma1)
  
  corr_m1 <- tibble(corr_m1 = corr_m1)
  corr0_gamma0_minus_rf <- tibble(corr0_gamma0_minus_rf = corr0_gamma0_minus_rf)
  mean_gama0_minus_rf <- tibble(mean_gama0_minus_rf = mean_gama0_minus_rf)
  
  table3 <- table3 %>%
    cross_join(corr_m1) %>%
    cross_join(corr0_gamma0_minus_rf) %>%
    cross_join(mean_gama0_minus_rf)
}

#Begin to run panel a
periods1 <- list()

#Define the start and end year
start_year <- 1946
end_year <- 2015

#Loop to generate each time period
for (year in seq(start_year, end_year, by = 10)) {
  start <- paste0(year, "-01-01")
  end <- paste0(year + 9, "-12-31")
  
  #Add the generated time period to the list
  periods1[[length(periods1) + 1]] <- list(
    start = start,
    end = end)
}

periods2 <- list()

#Define the start and end year
start_year <- 1941
end_year <- 2020

#Loop to generate each time period
for (year in seq(start_year, end_year, by = 5)) {
  start <- paste0(year, "-01-01")
  end <- paste0(year + 4, "-12-31")
  
  #Add the generated time period to the list
  periods2[[length(periods2) + 1]] <- list(
    start = start,
    end = end)
}

#Some periods do not have a foundable rule, thus manually define
custom_periods1 <- list(start = "1935-01-01", end = "2022-12-31")
custom_periods2 <- list(start = "1935-01-01", end = "1945-12-31")
custom_periods3 <- list(start = "1935-01-01", end = "1940-12-31")
custom_periods4 <- list(start = "2016-01-01", end = "2022-12-31")
custom_periods5 <- list(start = "2021-01-01", end = "2022-12-31")

#Add time periods to the periods list
combined_periods <- c(list(custom_periods1), list(custom_periods2), periods1, list(custom_periods4), list(custom_periods3), periods2, list(custom_periods5))

#Loop through each period and calculate results
panela <- lapply(combined_periods, function(combined_periods) {
  result <- table3_panela_function(combined_results, combined_periods$start, combined_periods$end)
  result$Period <- paste(combined_periods$start, "to", combined_periods$end)
  return(result)
})

#Combine results into a data frame
t3_panela <- do.call(rbind, lapply(panela, as.data.frame))

#Finish by kristina
#########################


#########################
#Start by Stephanie, improved by Kristina
#Table 3 Panel B
#The goal here is to estimate gamma coefficients (γ0, γ1, and γ2) using the cross-sectional regressions proposed by Fama-MacBeth (1973).
#Panel B have more complexity by using an additional squared term for beta (β²) in the regression
#Regression PANEL B should include quadratic term 
table3_panelb_function <- function(combined_results, start, end) {
  
  #Calculate gamma0, gamma1, and gamma2 using Fama-MacBeth regression (cross-sectional regression)
  #The squared term allows capturing non-linear relationships between beta and returns.
  table3 <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>%
    group_by(month) %>%
    do({
      #Panel B: Include the beta^2 term in the regression
      #and add gamma2 (based on Table 3 Panel B of the Fama Paper)
      model <- lm(avg_ret ~ mean_beta + mean_beta_square, data = .)
      tidy(model)
    }) %>%
    ungroup() %>%
    summarise(
      gamma0 = mean(estimate[term == "(Intercept)"], na.rm = TRUE),
      gamma1 = mean(estimate[term == "mean_beta"], na.rm = TRUE),
      gamma2 = mean(estimate[term == "mean_beta_square"], na.rm = TRUE),
      s_gamma0 = mean(std.error[term == "(Intercept)"], na.rm = TRUE),
      s_gamma1 = mean(std.error[term == "mean_beta"], na.rm = TRUE),
      s_gamma2 = mean(std.error[term == "mean_beta_square"], na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Calculate adjusted R-squared calculation with mean_beta^2
  adjusted_r2 <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>%
    group_by(month) %>%
    do({
      model <- lm(avg_ret ~ mean_beta + mean_beta_square, data = .)
      model_summary <- summary(model)
      tibble(
        month = unique(.$month),
        adj_r_squared = model_summary$adj.r.squared
      )
    }) %>%
    ungroup()
  
  #Calculate the average and standard deviation of adjusted R-squared
  mean_adjusted_r2 <- adjusted_r2 %>%
    summarise(mean_adj_r_squared = mean(adj_r_squared, na.rm = TRUE),
              sd_adj_r_squared = sd(adj_r_squared, na.rm = TRUE))
  
  table3 <- table3 %>%
    left_join(mean_adjusted_r2, by = character())
  
  #Extract regression coefficients
  regressions <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>% 
    group_by(month) %>%
    do({
      model <- lm(avg_ret ~ mean_beta + mean_beta_square, data = .)
      tidy(model)  
    }) %>%
    ungroup()
  
  #Pivot the regression results to analyse coefficients
  regression_summary <- regressions %>%
    filter(term %in% c("(Intercept)", "mean_beta","mean_beta_square")) %>%
    pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic)) %>%
    rename(
      gama0_monthly = `estimate_(Intercept)`,
      s_gama0_monthly = `std.error_(Intercept)`,
      t_gama0_monthly = `statistic_(Intercept)`,
      gama1_monthly = `estimate_mean_beta`,
      s_gama1_monthly = `std.error_mean_beta`,
      t_gama1_monthly = `statistic_mean_beta`,
      gama2_monthly = `estimate_mean_beta_square`,
      s_gama2_monthly = `std.error_mean_beta_square`,
      t_gama2_monthly = `statistic_mean_beta_square`
    )%>%
    select(-contains("p.value"))
  
  #Calculate the mean gamma values for Panel B
  mean_gama0 <- mean(regression_summary$gama0_monthly, na.rm = TRUE)
  mean_gama1 <- mean(regression_summary$gama1_monthly, na.rm = TRUE)
  mean_gama2 <- mean(regression_summary$gama2_monthly, na.rm = TRUE)
  
  #Regression summary 
  regression_summary1 <- regression_summary %>%
    filter(!is.na(gama0_monthly))%>%
    select(
      month,
      gama0_monthly,
      s_gama0_monthly,
      t_gama0_monthly
    )
  
  regression_summary2 <- regression_summary %>%
    filter(!is.na(gama1_monthly))%>%
    select(
      month,
      gama1_monthly,
      s_gama1_monthly,
      t_gama1_monthly
    )
  
  regression_summary3 <- regression_summary %>%
    filter(!is.na(gama2_monthly))%>%
    select(
      month,
      gama2_monthly,
      s_gama2_monthly,
      t_gama2_monthly
    )
  
  #Merge regression summaries
  regression_summary <- regression_summary1 %>%
    left_join(regression_summary2, by = "month")%>%
    left_join(regression_summary3, by = "month")
  
  regression_summary <- regression_summary %>%
    mutate(month_label = format(month, "%Y-%m"))
  
  #Adjust gamma0 by subtracting risk-free rate (rf)
  regression_summary <- regression_summary %>%
    left_join(ff_3factors_mon, by = "month_label") %>%
    mutate(gama0_minus_rf = gama0_monthly - rf)
  
  mean_gama0_minus_rf <- mean(regression_summary$gama0_minus_rf, na.rm = TRUE)
  
  #Calculate first-order serial correlation of gamma coefficients
  corr_m0 <- cor(regression_summary$gama0_monthly, lag(regression_summary$gama0_monthly), use = "complete.obs")
  corr_m1 <- cor(regression_summary$gama1_monthly, lag(regression_summary$gama1_monthly), use = "complete.obs")
  
  #Assuming a correlation with a mean of 0 for gamma coefficients 
  corr0_0 <- cor(regression_summary$gama0_monthly, lag(regression_summary$gama0_monthly) - mean_gama0, use = "complete.obs")
  corr0_1 <- cor(regression_summary$gama1_monthly, lag(regression_summary$gama1_monthly) - mean_gama1, use = "complete.obs")
  corr0_2 <- cor(regression_summary$gama2_monthly, lag(regression_summary$gama2_monthly) - mean_gama2, use = "complete.obs")
  
  #Merge the results into table3
  corr0_gamma0_minus_rf <- cor(regression_summary$gama0_minus_rf, lag(regression_summary$gama0_minus_rf) - mean_gama0_minus_rf, use = "complete.obs")
  
  t_gamma0_minus_rf <- regression_summary %>%
    summarise(
      t_gamma0_minus_rf = mean(gama0_minus_rf, na.rm = TRUE) / (sd(gama0_minus_rf, na.rm = TRUE) / sqrt(sum(!is.na(gama0_minus_rf))))
    )
  
  t_gamma0 <- regression_summary %>%
    summarise(
      t_gamma0 = mean(gama0_monthly, na.rm = TRUE) / (sd(gama0_monthly, na.rm = TRUE) / sqrt(sum(!is.na(gama0_monthly))))
    )
  
  t_gamma1 <- regression_summary %>%
    summarise(
      t_gamma1 = mean(gama1_monthly, na.rm = TRUE) / (sd(gama1_monthly, na.rm = TRUE) / sqrt(sum(!is.na(gama1_monthly))))
    )
  
  t_gamma2 <- regression_summary %>%
    summarise(
      t_gamma2 = mean(gama2_monthly, na.rm = TRUE) / (sd(gama2_monthly, na.rm = TRUE) / sqrt(sum(!is.na(gama2_monthly))))
    )
  
  table3 <- table3 %>%
    cross_join(t_gamma0_minus_rf) %>%
    cross_join(t_gamma0) %>%
    cross_join(t_gamma1)%>%
    cross_join(t_gamma2)
  
  corr_m1 <- tibble(corr_m1 = corr_m1)
  corr0_gamma0_minus_rf <- tibble(corr0_gamma0_minus_rf = corr0_gamma0_minus_rf)
  mean_gama0_minus_rf <- tibble(mean_gama0_minus_rf = mean_gama0_minus_rf)
  corr0_2 <- tibble(corr0_2 = corr0_2)
  
  table3 <- table3 %>%
    cross_join(corr_m1) %>%
    cross_join(corr0_gamma0_minus_rf) %>%
    cross_join(mean_gama0_minus_rf)%>%
    cross_join(corr0_2)
}


#Panel B
periods1 <- list()

#Define the start and end year
start_year <- 1946
end_year <- 2015

#Loop to for each time period
for (year in seq(start_year, end_year, by = 10)) {
  start <- paste0(year, "-01-01")
  end <- paste0(year + 9, "-12-31")
  
  #add the generated time period to the list
  periods1[[length(periods1) + 1]] <- list(
    start = start,
    end = end)
}

periods2 <- list()

#define the start and end year
start_year <- 1941
end_year <- 2020

#loop to generate each time period
for (year in seq(start_year, end_year, by = 5)) {
  start <- paste0(year, "-01-01")
  end <- paste0(year + 4, "-12-31")
  
  #add the generated time period to the list
  periods2[[length(periods2) + 1]] <- list(
    start = start,
    end = end)
}

custom_periods1 <- list(start = "1935-01-01", end = "2022-12-31")
custom_periods2 <- list(start = "1935-01-01", end = "1945-12-31")
custom_periods3 <- list(start = "1935-01-01", end = "1940-12-31")
custom_periods4 <- list(start = "2016-01-01", end = "2022-12-31")
custom_periods5 <- list(start = "2021-01-01", end = "2022-12-31")

#add time periods to the periods list
combined_periods <- c(list(custom_periods1), list(custom_periods2), periods1, list(custom_periods4), list(custom_periods3), periods2, list(custom_periods5))

#loop through each period and calculate results
panelb <- lapply(combined_periods, function(combined_periods) {
  result <- table3_panelb_function(combined_results, combined_periods$start, combined_periods$end)
  result$Period <- paste(combined_periods$start, "to", combined_periods$end)
  return(result)
})

#combine results into a data frame
# in Panel B, mean_beta^ is included in the regression part 
# to capture the non-linear effect of beta on the returns
# gamma 2 is the additional gamma coefficient, calculated from the quadratic term 
t3_panelb <- do.call(rbind, lapply(panelb, as.data.frame))
#finished by Stephanie, improved by Kristina
#########################


#########################
#Table 3 PANEL C
#Start by Stephanie 

#Panel C includes gamma3 (mean residual sd)
#Panel C get estimates gamma0, gamma1, gamma3 (residual risk), without quadratic term for this panel
#based on the Fama paper, aligned with the original paper's approach 

#Gamma3 corresponds to the residual variance (idiosyncratic risk)
table3_panelc_function <- function(combined_results, start, end) {
  
  #Calculate gamma0, gamma1, and gamma3 using Fama-MacBeth regression (cross-sectional regression)
  table3 <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>%
    group_by(month) %>%
    do({
      # Panel C: Include the beta term and residual standard deviation in the regression
      model <- lm(avg_ret ~ mean_beta + mean_residual_sd, data = .)
      tidy(model)
    }) %>%
    ungroup() %>%
    summarise(
      gamma0 = mean(estimate[term == "(Intercept)"], na.rm = TRUE),
      gamma1 = mean(estimate[term == "mean_beta"], na.rm = TRUE),
      gamma3 = mean(estimate[term == "mean_residual_sd"], na.rm = TRUE),
      s_gamma0 = mean(std.error[term == "(Intercept)"], na.rm = TRUE),
      s_gamma1 = mean(std.error[term == "mean_beta"], na.rm = TRUE),
      s_gamma3 = mean(std.error[term == "mean_residual_sd"], na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Adjusted R-squared calculation without mean_beta_square
  adjusted_r2 <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>%
    group_by(month) %>%
    do({
      model <- lm(avg_ret ~ mean_beta + mean_residual_sd, data = .)
      model_summary <- summary(model)
      tibble(
        month = unique(.$month),
        adj_r_squared = model_summary$adj.r.squared
      )
    }) %>%
    ungroup()
  
  mean_adjusted_r2 <- adjusted_r2 %>%
    summarise(mean_adj_r_squared = mean(adj_r_squared, na.rm = TRUE),
              sd_adj_r_squared = sd(adj_r_squared, na.rm = TRUE))
  
  table3 <- table3 %>%
    left_join(mean_adjusted_r2, by = character())
  
  regressions <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>%
    group_by(month) %>%
    do({
      model <- lm(avg_ret ~ mean_beta + mean_residual_sd, data = .)
      tidy(model)
    }) %>%
    ungroup()
  
  regression_summary <- regressions %>%
    filter(term %in% c("(Intercept)", "mean_beta", "mean_residual_sd")) %>%
    pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic)) %>%
    rename(
      gama0_monthly = `estimate_(Intercept)`,
      s_gama0_monthly = `std.error_(Intercept)`,
      t_gama0_monthly = `statistic_(Intercept)`,
      gama1_monthly = `estimate_mean_beta`,
      s_gama1_monthly = `std.error_mean_beta`,
      t_gama1_monthly = `statistic_mean_beta`,
      gama3_monthly = `estimate_mean_residual_sd`,
      s_gama3_monthly = `std.error_mean_residual_sd`,
      t_gama3_monthly = `statistic_mean_residual_sd`
    ) %>%
    select(-contains("p.value"))
  
  # Calculate the sample means
  mean_gama0 <- mean(regression_summary$gama0_monthly, na.rm = TRUE)
  mean_gama1 <- mean(regression_summary$gama1_monthly, na.rm = TRUE)
  mean_gama3 <- mean(regression_summary$gama3_monthly, na.rm = TRUE)
  
  regression_summary1 <- regression_summary %>%
    filter(!is.na(gama0_monthly)) %>%
    select(
      month,
      gama0_monthly,
      s_gama0_monthly,
      t_gama0_monthly
    )
  
  regression_summary2 <- regression_summary %>%
    filter(!is.na(gama1_monthly)) %>%
    select(
      month,
      gama1_monthly,
      s_gama1_monthly,
      t_gama1_monthly
    )
  
  regression_summary3 <- regression_summary %>%
    filter(!is.na(gama3_monthly)) %>%
    select(
      month,
      gama3_monthly,
      s_gama3_monthly,
      t_gama3_monthly
    )
  
  regression_summary <- regression_summary1 %>%
    left_join(regression_summary2, by = "month") %>%
    left_join(regression_summary3, by = "month")
  
  regression_summary <- regression_summary %>%
    mutate(month_label = format(month, "%Y-%m"))
  
  regression_summary <- regression_summary %>%
    left_join(ff_3factors_mon, by = "month_label") %>%
    mutate(gama0_minus_rf = gama0_monthly - rf)
  
  mean_gama0_minus_rf <- mean(regression_summary$gama0_minus_rf, na.rm = TRUE)
  
  # Calculate first-order serial correlation
  corr_m0 <- cor(regression_summary$gama0_monthly, lag(regression_summary$gama0_monthly), use = "complete.obs")
  corr_m1 <- cor(regression_summary$gama1_monthly, lag(regression_summary$gama1_monthly), use = "complete.obs")
  
  # We assume a correlation with a mean of 0
  corr0_0 <- cor(regression_summary$gama0_monthly, lag(regression_summary$gama0_monthly) - mean_gama0, use = "complete.obs")
  corr0_1 <- cor(regression_summary$gama1_monthly, lag(regression_summary$gama1_monthly) - mean_gama1, use = "complete.obs")
  corr0_3 <- cor(regression_summary$gama3_monthly, lag(regression_summary$gama3_monthly) - mean_gama3, use = "complete.obs")
  
  # Merge the results into Table3
  corr0_gamma0_minus_rf <- cor(regression_summary$gama0_minus_rf, lag(regression_summary$gama0_minus_rf) - mean_gama0_minus_rf, use = "complete.obs")
  
  t_gamma0_minus_rf <- regression_summary %>%
    summarise(
      t_gamma0_minus_rf = mean(gama0_minus_rf, na.rm = TRUE) / (sd(gama0_minus_rf, na.rm = TRUE) / sqrt(sum(!is.na(gama0_minus_rf))))
    )
  
  t_gamma0 <- regression_summary %>%
    summarise(
      t_gamma0 = mean(gama0_monthly, na.rm = TRUE) / (sd(gama0_monthly, na.rm = TRUE) / sqrt(sum(!is.na(gama0_monthly))))
    )
  
  t_gamma1 <- regression_summary %>%
    summarise(
      t_gamma1 = mean(gama1_monthly, na.rm = TRUE) / (sd(gama1_monthly, na.rm = TRUE) / sqrt(sum(!is.na(gama1_monthly))))
    )
  
  t_gamma3 <- regression_summary %>%
    summarise(
      t_gamma3 = mean(gama3_monthly, na.rm = TRUE) / (sd(gama3_monthly, na.rm = TRUE) / sqrt(sum(!is.na(gama3_monthly))))
    )
  
  table3 <- table3 %>%
    cross_join(t_gamma0_minus_rf) %>%
    cross_join(t_gamma0) %>%
    cross_join(t_gamma1) %>%
    cross_join(t_gamma3)
  
  corr_m1 <- tibble(corr_m1 = corr_m1)
  corr0_gamma0_minus_rf <- tibble(corr0_gamma0_minus_rf = corr0_gamma0_minus_rf)
  mean_gama0_minus_rf <- tibble(mean_gama0_minus_rf = mean_gama0_minus_rf)
  corr0_3 <- tibble(corr0_3 = corr0_3)
  
  table3 <- table3 %>%
    cross_join(corr_m1) %>%
    cross_join(corr0_gamma0_minus_rf) %>%
    cross_join(mean_gama0_minus_rf) %>%
    cross_join(corr0_3)
}


# Assign list for each periods 
periods1 <- list()

#Define the start and end year
start_year <- 1946
end_year <- 2015

#Loop to for each time period
for (year in seq(start_year, end_year, by = 10)) {
  start <- paste0(year, "-01-01")
  end <- paste0(year + 9, "-12-31")
  
  #add the generated time period to the list
  periods1[[length(periods1) + 1]] <- list(
    start = start,
    end = end)
}

periods2 <- list()

#define the start and end year
start_year <- 1941
end_year <- 2020

#loop to generate each time period
for (year in seq(start_year, end_year, by = 5)) {
  start <- paste0(year, "-01-01")
  end <- paste0(year + 4, "-12-31")
  
  #add the generated time period to the list
  periods2[[length(periods2) + 1]] <- list(
    start = start,
    end = end)
}

custom_periods1 <- list(start = "1935-01-01", end = "2022-12-31")
custom_periods2 <- list(start = "1935-01-01", end = "1945-12-31")
custom_periods3 <- list(start = "1935-01-01", end = "1940-12-31")
custom_periods4 <- list(start = "2016-01-01", end = "2022-12-31")
custom_periods5 <- list(start = "2021-01-01", end = "2022-12-31")

#add time periods to the periods list
combined_periods <- c(list(custom_periods1), list(custom_periods2), periods1, list(custom_periods4), list(custom_periods3), periods2, list(custom_periods5))

#loop through each period and calculate results
panelc <- lapply(combined_periods, function(combined_periods) {
  result <- table3_panelc_function(combined_results, combined_periods$start, combined_periods$end)
  result$Period <- paste(combined_periods$start, "to", combined_periods$end)
  return(result)
})

#combine results into a data frame
t3_panelc <- do.call(rbind, lapply(panelc, as.data.frame))
#finish by Stephanie
#########################


#########################
#Start by Kristina
#Panel d begins
#A function for panel d
table3_paneld_function <- function(combined_results, start, end) {
  
  #Calculate gamma0, gamma1, gamma2 and gamma3 using Fama-MacBeth regression (cross-sectional regression)
  table3 <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>%
    group_by(month) %>%
    do({
      model <- lm(avg_ret ~ mean_beta + mean_beta_square + mean_residual_sd, data = .)
      tidy(model)
    }) %>%
    ungroup() %>%
    summarise(
      gamma0 = mean(estimate[term == "(Intercept)"], na.rm = TRUE),
      gamma1 = mean(estimate[term == "mean_beta"], na.rm = TRUE),
      gamma2 = mean(estimate[term == "mean_beta_square"], na.rm = TRUE),
      gamma3 = mean(estimate[term == "mean_residual_sd"], na.rm = TRUE),
      s_gamma0 = mean(std.error[term == "(Intercept)"], na.rm = TRUE),
      s_gamma1 = mean(std.error[term == "mean_beta"], na.rm = TRUE),
      s_gamma2 = mean(std.error[term == "mean_beta_square"], na.rm = TRUE),
      s_gamma3 = mean(std.error[term == "mean_residual_sd"], na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Adjusted R-squared calculation with mean_beta^2
  adjusted_r2 <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>%
    group_by(month) %>%
    do({
      model <- lm(avg_ret ~ mean_beta + mean_beta_square + mean_residual_sd, data = .)
      model_summary <- summary(model)
      tibble(
        month = unique(.$month),
        adj_r_squared = model_summary$adj.r.squared
      )
    }) %>%
    ungroup()
  
  mean_adjusted_r2 <- adjusted_r2 %>%
    summarise(mean_adj_r_squared = mean(adj_r_squared, na.rm = TRUE),
              sd_adj_r_squared = sd(adj_r_squared, na.rm = TRUE))
  
  table3 <- table3 %>%
    left_join(mean_adjusted_r2, by = character())
  
  regressions <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>% 
    group_by(month) %>%
    do({
      model <- lm(avg_ret ~ mean_beta + mean_beta_square + mean_residual_sd, data = .)
      tidy(model)  
    }) %>%
    ungroup()
  
  regression_summary <- regressions %>%
    filter(term %in% c("(Intercept)", "mean_beta","mean_beta_square", "mean_residual_sd")) %>%
    pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic)) %>%
    rename(
      gama0_monthly = `estimate_(Intercept)`,
      s_gama0_monthly = `std.error_(Intercept)`,
      t_gama0_monthly = `statistic_(Intercept)`,
      gama1_monthly = `estimate_mean_beta`,
      s_gama1_monthly = `std.error_mean_beta`,
      t_gama1_monthly = `statistic_mean_beta`,
      gama2_monthly = `estimate_mean_beta_square`,
      s_gama2_monthly = `std.error_mean_beta_square`,
      t_gama2_monthly = `statistic_mean_beta_square`,
      gama3_monthly = `estimate_mean_residual_sd`,
      s_gama3_monthly = `std.error_mean_residual_sd`,
      t_gama3_monthly = `statistic_mean_residual_sd`
    )%>%
    select(-contains("p.value"))
  
  #Calculate the sample mean
  #For each of mean gamma
  mean_gama0 <- mean(regression_summary$gama0_monthly, na.rm = TRUE)
  mean_gama1 <- mean(regression_summary$gama1_monthly, na.rm = TRUE)
  mean_gama2 <- mean(regression_summary$gama2_monthly, na.rm = TRUE)
  mean_gama3 <- mean(regression_summary$gama3_monthly, na.rm = TRUE)
  
  #Calculate the regression summary for each
  regression_summary1 <- regression_summary %>%
    filter(!is.na(gama0_monthly))%>%
    select(
      month,
      gama0_monthly,
      s_gama0_monthly,
      t_gama0_monthly
    )
  
  regression_summary2 <- regression_summary %>%
    filter(!is.na(gama1_monthly))%>%
    select(
      month,
      gama1_monthly,
      s_gama1_monthly,
      t_gama1_monthly
    )
  
  regression_summary3 <- regression_summary %>%
    filter(!is.na(gama2_monthly))%>%
    select(
      month,
      gama2_monthly,
      s_gama2_monthly,
      t_gama2_monthly
    )
  
  regression_summary4 <- regression_summary %>%
    filter(!is.na(gama3_monthly))%>%
    select(
      month,
      gama3_monthly,
      s_gama3_monthly,
      t_gama3_monthly
    )
  
  regression_summary <- regression_summary1 %>%
    left_join(regression_summary2, by = "month")%>%
    left_join(regression_summary3, by = "month")%>%
    left_join(regression_summary4, by = "month")
  
  #Modify the format of the year and month
  regression_summary <- regression_summary %>%
    mutate(month_label = format(month, "%Y-%m"))
  
  regression_summary <- regression_summary %>%
    left_join(ff_3factors_mon, by = "month_label") %>%
    mutate(gama0_minus_rf = gama0_monthly - rf)
  
  mean_gama0_minus_rf <- mean(regression_summary$gama0_minus_rf, na.rm = TRUE)
  
  #Calculate first-order serial correlation
  corr_m0 <- cor(regression_summary$gama0_monthly, lag(regression_summary$gama0_monthly), use = "complete.obs")
  corr_m1 <- cor(regression_summary$gama1_monthly, lag(regression_summary$gama1_monthly), use = "complete.obs")
  
  #Assuming a correlation with a mean of 0
  corr0_0 <- cor(regression_summary$gama0_monthly, lag(regression_summary$gama0_monthly) - mean_gama0, use = "complete.obs")
  corr0_1 <- cor(regression_summary$gama1_monthly, lag(regression_summary$gama1_monthly) - mean_gama1, use = "complete.obs")
  corr0_2 <- cor(regression_summary$gama2_monthly, lag(regression_summary$gama2_monthly) - mean_gama2, use = "complete.obs")
  corr0_3 <- cor(regression_summary$gama3_monthly, lag(regression_summary$gama3_monthly) - mean_gama3, use = "complete.obs")
  
  #Merge the results into table3
  corr0_gamma0_minus_rf <- cor(regression_summary$gama0_minus_rf, lag(regression_summary$gama0_minus_rf) - mean_gama0_minus_rf, use = "complete.obs")
  
  t_gamma0_minus_rf <- regression_summary %>%
    summarise(
      t_gamma0_minus_rf = mean(gama0_minus_rf, na.rm = TRUE) / (sd(gama0_minus_rf, na.rm = TRUE) / sqrt(sum(!is.na(gama0_minus_rf))))
    )
  
  t_gamma0 <- regression_summary %>%
    summarise(
      t_gamma0 = mean(gama0_monthly, na.rm = TRUE) / (sd(gama0_monthly, na.rm = TRUE) / sqrt(sum(!is.na(gama0_monthly))))
    )
  
  t_gamma1 <- regression_summary %>%
    summarise(
      t_gamma1 = mean(gama1_monthly, na.rm = TRUE) / (sd(gama1_monthly, na.rm = TRUE) / sqrt(sum(!is.na(gama1_monthly))))
    )
  
  t_gamma2 <- regression_summary %>%
    summarise(
      t_gamma2 = mean(gama2_monthly, na.rm = TRUE) / (sd(gama2_monthly, na.rm = TRUE) / sqrt(sum(!is.na(gama2_monthly))))
    )
  
  t_gamma3 <- regression_summary %>%
    summarise(
      t_gamma3 = mean(gama3_monthly, na.rm = TRUE) / (sd(gama3_monthly, na.rm = TRUE) / sqrt(sum(!is.na(gama3_monthly))))
    )
  
  table3 <- table3 %>%
    cross_join(t_gamma0_minus_rf) %>%
    cross_join(t_gamma0) %>%
    cross_join(t_gamma1)%>%
    cross_join(t_gamma2)%>%
    cross_join(t_gamma3)
  
  corr_m1 <- tibble(corr_m1 = corr_m1)
  corr0_gamma0_minus_rf <- tibble(corr0_gamma0_minus_rf = corr0_gamma0_minus_rf)
  mean_gama0_minus_rf <- tibble(mean_gama0_minus_rf = mean_gama0_minus_rf)
  corr0_2 <- tibble(corr0_2 = corr0_2)
  corr0_3 <- tibble(corr0_3 = corr0_3)
  
  table3 <- table3 %>%
    cross_join(corr_m1) %>%
    cross_join(corr0_gamma0_minus_rf) %>%
    cross_join(mean_gama0_minus_rf)%>%
    cross_join(corr0_2)%>%
    cross_join(corr0_3)
}


#Panel D, assign to list for each period
periods1 <- list()

#Define the start and end year
start_year <- 1946
end_year <- 2015

#Loop to for each time period
for (year in seq(start_year, end_year, by = 10)) {
  start <- paste0(year, "-01-01")
  end <- paste0(year + 9, "-12-31")
  
  #add the generated time period to the list
  periods1[[length(periods1) + 1]] <- list(
    start = start,
    end = end)
}

periods2 <- list()

#Define the start and end year
start_year <- 1941
end_year <- 2020

#Loop to generate each time period
for (year in seq(start_year, end_year, by = 5)) {
  start <- paste0(year, "-01-01")
  end <- paste0(year + 4, "-12-31")
  
  #Add the generated time period to the list
  periods2[[length(periods2) + 1]] <- list(
    start = start,
    end = end)
}

custom_periods1 <- list(start = "1935-01-01", end = "2022-12-31")
custom_periods2 <- list(start = "1935-01-01", end = "1945-12-31")
custom_periods3 <- list(start = "1935-01-01", end = "1940-12-31")
custom_periods4 <- list(start = "2016-01-01", end = "2022-12-31")
custom_periods5 <- list(start = "2021-01-01", end = "2022-12-31")

#Add time periods to the periods list
combined_periods <- c(list(custom_periods1), list(custom_periods2), periods1, list(custom_periods4), list(custom_periods3), periods2, list(custom_periods5))

#Loop through each period and calculate results
paneld <- lapply(combined_periods, function(combined_periods) {
  result <- table3_paneld_function(combined_results, combined_periods$start, combined_periods$end)
  result$Period <- paste(combined_periods$start, "to", combined_periods$end)
  return(result)
})


#Combine results into a data frame
t3_paneld <- do.call(rbind, lapply(paneld, as.data.frame))

#Distinguish each panel
t3_panela <- t3_panela %>% mutate(panel = "panel_a")
t3_panelb <- t3_panelb %>% mutate(panel = "panel_b")
t3_panelc <- t3_panelc %>% mutate(panel = "panel_c")
t3_paneld <- t3_paneld %>% mutate(panel = "panel_d")

#Integrating all four panels together
combined_table3 <- bind_rows(t3_panela, t3_panelb, t3_panelc, t3_paneld)

#Rearrange the order of the columns of combined_table3
combined_table3 <- combined_table3 %>%
  select(
    panel,
    Period,
    gamma0,
    gamma1,
    gamma2,
    gamma3,
    mean_gama0_minus_rf,
    s_gamma0,
    s_gamma1,
    s_gamma2,
    s_gamma3,
    corr0_gamma0_minus_rf,
    corr_m1,
    corr0_2,
    corr0_3,
    t_gamma0,
    t_gamma1,
    t_gamma2,
    t_gamma3,
    t_gamma0_minus_rf,
    mean_adj_r_squared,
    sd_adj_r_squared
  )

#Rename variables for Table 3
combined_table3 <- combined_table3 %>%
  rename(
    Panel = panel,
    `gamma0-Rf` = mean_gama0_minus_rf,
    `s(gamma0)` = s_gamma0,
    `s(gamma1)` = s_gamma1,
    `s(gamma2)` = s_gamma2,
    `s(gamma3)` = s_gamma3,
    `Rho_0(gamma0-Rf)` = corr0_gamma0_minus_rf,
    `Rho_m(gamma1)` = corr_m1,
    `Rho_0(gamma2)` = corr0_2,
    `Rho_0(gamma3)` = corr0_3,
    `t(gamma0)` = t_gamma0,
    `t(gamma1)` = t_gamma1,
    `t(gamma2)` = t_gamma2,
    `t(gamma3)` = t_gamma3,
    `t(gamma0-Rf)` = t_gamma0_minus_rf,
    `r^2` = mean_adj_r_squared,
    `s(r^2)` = sd_adj_r_squared
  )%>%
  mutate(across(-c(Period, Panel), ~ round(as.numeric(.), 3))) %>%
  mutate(Period = sub("^(\\d{4})-\\d{2}-\\d{2} to (\\d{4})-\\d{2}-\\d{2}$", "\\1-\\2", Period))

# Outputting Table 3
write.xlsx(combined_table3, "table3.xlsx", rowNames = FALSE)

#Finish by Kristina
#########################


#########################
#Start by Kristina
#Collecting Rm/Rf and calculate some statistics of Table 4
fi_mkt_rf <- fi_mkt %>%
  mutate(month_label = format(date, "%Y-%m")) %>%  #extract year and month
  filter(month_label >= "1935-01" & month_label <= "2022-12")%>% #keep the specified range of months
  left_join(ff_3factors_mon, by = "month_label")  %>% #join with ff_3factors_mon (including rf)
  mutate(Rm_Rf = raw_mkt - rf) %>% #market excess return
  mutate(Rm_Rf = format(round(Rm_Rf, 10), nsmall = 10, scientific = FALSE)) %>%#keep ten decimal places
  mutate(Rm_Rf = as.numeric(raw_mkt - rf))%>%  
  select(-date.y, date = date.x)  #delete date.y and rename date.x to date

# Function to calculate the new statistics in table 4 compared to panel a of table 3
table4_rm_rf_function <- function(fi_mkt_rf, start, end) {
  
  table4_1 <- fi_mkt_rf %>%
    filter(date >= as.Date(start) & date <= as.Date(end)) %>%
    summarise(
      mean_raw_mkt = mean(raw_mkt, na.rm = TRUE),
      sd_raw_mkt = sd(raw_mkt, na.rm = TRUE),
      mean_Rm_Rf = mean(Rm_Rf, na.rm = TRUE),
      sd_Rm_Rf = sd(Rm_Rf, na.rm = TRUE),
      mean_rf = mean(rf, na.rm = TRUE),
      sd_rf = sd(rf, na.rm = TRUE),
      #calculate the first-order serial correlation
      serial_corr_raw_mkt = cor(raw_mkt[-length(raw_mkt)], raw_mkt[-1], use = "complete.obs"),
      serial_corr_Rm_Rf = cor(as.numeric(Rm_Rf)[-length(Rm_Rf)], as.numeric(Rm_Rf)[-1], use = "complete.obs"),
      serial_corr_rf = cor(rf[-length(rf)], rf[-1], use = "complete.obs"),
      #t-statistics
      t_stat_raw_mkt = mean_raw_mkt / (sd_raw_mkt / sqrt(sum(!is.na(raw_mkt)))),
      t_stat_Rm_Rf = mean_Rm_Rf / (sd_Rm_Rf / sqrt(sum(!is.na(Rm_Rf))))
      
    )

}

# Define period
periods1 <- list()

#Define the start and end year
start_year <- 1946
end_year <- 2015

# For Loop to for each time period
for (year in seq(start_year, end_year, by = 10)) {
  start <- paste0(year, "-01-01")
  end <- paste0(year + 9, "-12-31")
  
  #add the generated time period to the list
  periods1[[length(periods1) + 1]] <- list(
    start = start,
    end = end)
}

periods2 <- list()

#Define the start and end year
start_year <- 1941
end_year <- 2020

#Loop to generate each time period
for (year in seq(start_year, end_year, by = 5)) {
  start <- paste0(year, "-01-01")
  end <- paste0(year + 4, "-12-31")
  
  #Add the generated time period to the list
  periods2[[length(periods2) + 1]] <- list(
    start = start,
    end = end)
}

custom_periods1 <- list(start = "1935-01-01", end = "2022-12-31")
custom_periods2 <- list(start = "1935-01-01", end = "1945-12-31")
custom_periods3 <- list(start = "1935-01-01", end = "1940-12-31")
custom_periods4 <- list(start = "2016-01-01", end = "2022-12-31")
custom_periods5 <- list(start = "2021-01-01", end = "2022-12-31")

#Add time periods to the periods list
combined_periods <- c(list(custom_periods1), list(custom_periods2), periods1, list(custom_periods4), list(custom_periods3), periods2, list(custom_periods5))

#Loop through each period and calculate results
table4_rm_rf <- lapply(combined_periods, function(combined_periods) {
  result <- table4_rm_rf_function(fi_mkt_rf, combined_periods$start, combined_periods$end)
  result$Period <- paste(combined_periods$start, "to", combined_periods$end)
  return(result)
})

#Combine results into a data frame
t4_rm_rf <- do.call(rbind, lapply(table4_rm_rf, as.data.frame))

#A function to rerun table 3 panela, as in table 4, Rho_m_gamma0 is added
table3_panela_for_table4_function <- function(combined_results, start, end) {
  
  #calculate gamma0 and gamma1 using Fama-MacBeth regression (cross-sectional regression)
  table3 <- combined_results %>% 
    filter(month >= as.Date(start) & month <= as.Date(end)) %>%     
    group_by(month) %>%       
    do({       model <- lm(avg_ret ~ mean_beta, data = .)       
    tidy(model)       }) %>%     
    ungroup()%>%     
    summarise(       
      gamma0 = mean(estimate[term == "(Intercept)"], na.rm = TRUE),       
      gamma1 = mean(estimate[term == "mean_beta"], na.rm = TRUE),       
      s_gamma0 = mean(std.error[term == "(Intercept)"], na.rm = TRUE),       
      s_gamma1 = mean(std.error[term == "mean_beta"], na.rm = TRUE),
      .groups = 'drop'     )
  
  adjusted_r2 <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>% 
    group_by(month) %>%
    do({
      model <- lm(avg_ret ~ mean_beta, data = .)
      model_summary <- summary(model)
      tibble(
        month = unique(.$month),
        adj_r_squared = model_summary$adj.r.squared
      )
    }) %>%
    ungroup()
  
  mean_adjusted_r2 <- adjusted_r2 %>%
    summarise(mean_adj_r_squared = mean(adj_r_squared, na.rm = TRUE),
              sd_adj_r_squared = sd(adj_r_squared, na.rm = TRUE))
  
  table3 <- table3 %>%
    left_join(mean_adjusted_r2, by = character())
  
  regressions <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>% 
    group_by(month) %>%
    do({
      model <- lm(avg_ret ~ mean_beta, data = .)
      tidy(model)  
    }) %>%
    ungroup()
  
  regression_summary <- regressions %>%
    filter(term %in% c("(Intercept)", "mean_beta")) %>%
    pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic)) %>%
    rename(
      gama0_monthly = `estimate_(Intercept)`,
      s_gama0_monthly = `std.error_(Intercept)`,
      t_gama0_monthly = `statistic_(Intercept)`,
      gama1_monthly = `estimate_mean_beta`,
      s_gama1_monthly = `std.error_mean_beta`,
      t_gama1_monthly = `statistic_mean_beta`
    )%>%
    select(-contains("p.value"))
  
  #calculate the sample mean
  mean_gama0 <- mean(regression_summary$gama0_monthly, na.rm = TRUE)
  mean_gama1 <- mean(regression_summary$gama1_monthly, na.rm = TRUE)
  
  regression_summary1 <- regression_summary %>%
    filter(!is.na(gama0_monthly))%>%
    select(
      month,
      gama0_monthly,
      s_gama0_monthly,
      t_gama0_monthly
    )
  
  regression_summary2 <- regression_summary %>%
    filter(!is.na(gama1_monthly))%>%
    select(
      month,
      gama1_monthly,
      s_gama1_monthly,
      t_gama1_monthly
    )
  
  regression_summary <- regression_summary1 %>%
    left_join(regression_summary2, by = "month")
  
  regression_summary <- regression_summary %>%
    mutate(month_label = format(month, "%Y-%m"))
  
  regression_summary <- regression_summary %>%
    left_join(ff_3factors_mon, by = "month_label") %>%
    mutate(gama0_minus_rf = gama0_monthly - rf)
  
  mean_gama0_minus_rf <- mean(regression_summary$gama0_minus_rf, na.rm = TRUE)
  
  #calculate first-order serial correlation
  corr_m0 <- cor(regression_summary$gama0_monthly, lag(regression_summary$gama0_monthly), use = "complete.obs")
  corr_m1 <- cor(regression_summary$gama1_monthly, lag(regression_summary$gama1_monthly), use = "complete.obs")
  
  #assuming a correlation with a mean of 0
  corr0_0 <- cor(regression_summary$gama0_monthly, lag(regression_summary$gama0_monthly) - mean_gama0, use = "complete.obs")
  corr0_1 <- cor(regression_summary$gama1_monthly, lag(regression_summary$gama1_monthly) - mean_gama1, use = "complete.obs")
  
  #merge the results into table3
  corr0_gamma0_minus_rf <- cor(regression_summary$gama0_minus_rf, lag(regression_summary$gama0_minus_rf) - mean_gama0_minus_rf, use = "complete.obs")
  
  t_gamma0_minus_rf <- regression_summary %>%
    summarise(
      t_gamma0_minus_rf = mean(gama0_minus_rf, na.rm = TRUE) / (sd(gama0_minus_rf, na.rm = TRUE) / sqrt(sum(!is.na(gama0_minus_rf))))
    )
  
  t_gamma0 <- regression_summary %>%
    summarise(
      t_gamma0 = mean(gama0_monthly, na.rm = TRUE) / (sd(gama0_monthly, na.rm = TRUE) / sqrt(sum(!is.na(gama0_monthly))))
    )
  
  t_gamma1 <- regression_summary %>%
    summarise(
      t_gamma1 = mean(gama1_monthly, na.rm = TRUE) / (sd(gama1_monthly, na.rm = TRUE) / sqrt(sum(!is.na(gama1_monthly))))
    )
  
  table3 <- table3 %>%
    cross_join(t_gamma0) %>%
    cross_join(t_gamma1)
  
  corr_m1 <- tibble(corr_m1 = corr_m1)
  corr_m0 <- tibble(corr_m0 = corr_m0)
  
  table3 <- table3 %>%
    cross_join(corr_m1) %>%
    cross_join(corr_m0) 
}

periods1 <- list()

#define the start and end year
start_year <- 1946
end_year <- 2015

#Loop to generate each time period
for (year in seq(start_year, end_year, by = 10)) {
  start <- paste0(year, "-01-01")
  end <- paste0(year + 9, "-12-31")
  
  #Add the generated time period to the list
  periods1[[length(periods1) + 1]] <- list(
    start = start,
    end = end)
}

periods2 <- list()

#Define the start and end year
start_year <- 1941
end_year <- 2020

#Loop to generate each time period
for (year in seq(start_year, end_year, by = 5)) {
  start <- paste0(year, "-01-01")
  end <- paste0(year + 4, "-12-31")
  
  #Add the generated time period to the list
  periods2[[length(periods2) + 1]] <- list(
    start = start,
    end = end)
}

custom_periods1 <- list(start = "1935-01-01", end = "2022-12-31")
custom_periods2 <- list(start = "1935-01-01", end = "1945-12-31")
custom_periods3 <- list(start = "1935-01-01", end = "1940-12-31")
custom_periods4 <- list(start = "2016-01-01", end = "2022-12-31")
custom_periods5 <- list(start = "2021-01-01", end = "2022-12-31")

#Add time periods to the periods list
combined_periods <- c(list(custom_periods1), list(custom_periods2), periods1, list(custom_periods4), list(custom_periods3), periods2, list(custom_periods5))

#Loop through each period and calculate results
panela_for_table4 <- lapply(combined_periods, function(combined_periods) {
  result <- table3_panela_for_table4_function(combined_results, combined_periods$start, combined_periods$end)
  result$Period <- paste(combined_periods$start, "to", combined_periods$end)
  return(result)
})

#Combine results into a data frame
t3_panela_for_table4 <- do.call(rbind, lapply(panela_for_table4, as.data.frame))

#Merge together
table4 <- t4_rm_rf %>%
  left_join(t3_panela_for_table4, by = "Period")%>%
  mutate(
    Rm_Rf_sd_rm = mean_Rm_Rf / sd_raw_mkt, 
    gamma1_sd_rm = gamma1 / sd_raw_mkt 
  )%>%
  select(Period, mean_raw_mkt, mean_Rm_Rf, gamma1, gamma0, mean_rf, Rm_Rf_sd_rm, gamma1_sd_rm, 
         sd_raw_mkt, sd_Rm_Rf, s_gamma0, sd_rf, t_stat_raw_mkt, t_stat_Rm_Rf, t_gamma1, t_gamma0,
         serial_corr_raw_mkt, serial_corr_Rm_Rf, corr_m1, corr_m0, serial_corr_rf)%>%
  rename(
    Rm = mean_raw_mkt,
    `Rm-Rf` = mean_Rm_Rf,
    Rf = mean_rf,
    `Rm-Rf / s(Rm)`= Rm_Rf_sd_rm,
    `gamma1 / s(Rm)`= gamma1_sd_rm,
    `s(Rm)` = sd_raw_mkt,
    `s(Rm-Rf)` = sd_Rm_Rf,
    `s(gamma0)` = s_gamma0,
    `s(Rf)` = sd_rf,
    `t(Rm)` = t_stat_raw_mkt,
    `t(Rm-Rf)` = t_stat_Rm_Rf,
    `t(gamma1)` = t_gamma1,
    `t(gamma0)` = t_gamma0,
    `Rho_m(Rm)` = serial_corr_raw_mkt,
    `Rho_m(Rm-Rf)` = serial_corr_Rm_Rf,
    `Rho_m(gamma1)` = corr_m1,
    `Rho_m(gamma0)` = corr_m0,
    `Rho_m(Rf)` = serial_corr_rf
  )%>%
  mutate(across(-Period, ~ round(as.numeric(.), 3)))%>%
  mutate(Period = sub("^(\\d{4})-\\d{2}-\\d{2} to (\\d{4})-\\d{2}-\\d{2}$", "\\1-\\2", Period))


#Outputting table4
write.xlsx(table4, "table4.xlsx", rowNames = FALSE)

#Finish by Kristina
#########################
