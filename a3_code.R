#the following codes are for MAF900 assignment 3 in T3 2024. 
#start from kristina
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

#connect to our wrds
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='replace',
                  password='replace')

#connect to kristina's wrds
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='s224294027')

#collect CRSP monthly stock return data 
msf_db <- tbl(wrds, sql("select * from crsp.msf"))
start_date <- ymd("1926-01-01")
end_date <- ymd("2023-12-31")

crsp_monthly <- msf_db |>
  filter(date >= start_date & date <= end_date) |>
  select(
    permno, # Security identifier
    date, # Date of the observation
    ret, # Return
  ) |> collect()

#stock and exchange identifier
msenames_db <- tbl(wrds, sql("select * from crsp.msenames"))

crsp_stockids <- msenames_db |>
  select (permno, primexch)|> collect()|>unique()|> filter(primexch == 'N')

capm_data <- crsp_monthly |> inner_join(
  crsp_stockids |>
    select(permno, primexch), by = c("permno"))

#create Fisher Index - for Rm
fi_mkt <- capm_data|> group_by(date)|> summarise(raw_mkt = mean(ret, na.rm = TRUE))

#combine stock return and market return i.e. fisher index return
capm_data <- capm_data %>%
  select(permno, date, ret) %>%
  rename(raw_ret = ret) %>%
  inner_join(fi_mkt, by = c("date")) %>%
  rename(month = date) %>%
  arrange(permno, month) %>%
  drop_na(raw_ret, raw_mkt)

#finding the securities meeting the data requirements
#finding the securies during Jan 1935 (first month of a tesing period)
permno_first_month <- capm_data %>%
  filter(month >= as.Date("1935-01-01") & month < as.Date("1935-02-01")) %>%
  select(permno) %>%
  distinct()%>%
  pull(permno)

#calculate the number of permno in January 1935
count_first_month <- length(permno_first_month)

#count the number of permnos that have data for each month from 1926 to 1934
count_valid_permno <- capm_data %>%
  filter(month >= as.Date("1926-01-01") & month <= as.Date("1934-12-31")) %>%
  filter(permno %in% permno_first_month) %>%
  group_by(permno) %>%
  summarize(months_count = n_distinct(format(month, "%Y-%m"))) %>%
  filter(months_count == 108)

#the amount
count_in_all_months <- nrow(count_valid_permno)
#finished by kristina 

#start by Stephanie
#portfolio formation 
#calculate BETA for period 1926-1929
capm_data1 <- capm_data %>%
  filter(month >= as.Date("1926-01-01") & month <= as.Date("1929-12-31"))
capm_data1<- capm_data1 %>%
  inner_join(count_valid_permno, by = "permno")

#calculate beta for each company
beta_results <- capm_data1 %>%
  group_by(permno) %>%
  do(tidy(lm(raw_ret ~ raw_mkt, data = .)))

#print the results for beta
print(beta_results)

#filter for the beta results
beta_results_only <- beta_results %>%
  filter(term == "raw_mkt") %>%
  select(permno, beta = estimate, std_error = std.error, t_statistic = statistic, p_value = p.value)

#beta results for each of the company 
print(beta_results_only) 

#check for NA values 
sum(is.na(beta_results_only$beta)) 

#remove NA values from beta
beta_results_only <- beta_results_only %>%
  filter(!is.na(beta))

beta_results_only <- beta_results_only %>%
  arrange(beta) 

#based on the paper Page 615 section B.Details 
#allocating securities into 20 portfolios 

#calculate the total number of securities 
N <- nrow(beta_results_only)

#calculate the number of securities, middle portfolios
securities_per_portfolio <- floor(N / 20)

#calculate the remainder, allocate to first and last portfolios
remainder <- N - 20 * securities_per_portfolio

#determine number of securities, first and last portfolios
first_last_extra <- floor(remainder / 2)
last_portfolio_extra <- remainder %% 2  # If odd, last portfolio gets an extra security

#calculating the size of each porfolio
portfolio_sizes <- c(
  securities_per_portfolio + first_last_extra,        # first portfolio
  rep(securities_per_portfolio, 18),                 # 2nd to 19th portfolio
  securities_per_portfolio + first_last_extra + last_portfolio_extra  # 20th portfolio
)

#create a variable named portfolio
beta_results_only$portfolio <- rep(1:20, times = portfolio_sizes)

#start from Kristina
#calculate BETA for period 1930-1934
capm_data2 <- capm_data %>%
  filter(month >= as.Date("1930-01-01") & month <= as.Date("1934-12-31")) 

capm_data2<- capm_data2 %>%
  inner_join(count_valid_permno, by = "permno")

#compute the regression model for each permno and extract the standard deviations of residuals
beta_results2 <- capm_data2 %>%
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

#remove for NA beta
beta_results_only2 <- beta_results_only2 %>%
  filter(!is.na(beta))

#calculate the portfolio return during 1935-1938
#set a new dataset for period 1935-1938
capm_data3 <- capm_data %>%
  filter(month >= as.Date("1935-01-01") & month <= as.Date("1938-12-31"))
capm_data3<- capm_data3 %>%
  inner_join(count_valid_permno, by = "permno")

#portfolio
capm_data3<- capm_data3 %>%
  inner_join(beta_results_only %>% select(permno, portfolio), by = "permno")

#join beta_results_only2 with portfolio number
beta_results_only2 <- beta_results_only2 %>%
  inner_join(beta_results_only %>% select(permno, portfolio), by = "permno")

#calculate the porfolio beta and standard errors of beta during 1930-1934.
beta_means_by_portfolio2 <- beta_results_only2 %>%
  group_by(portfolio) %>%              
  summarise(mean_beta = mean(beta, na.rm = TRUE),
            mean_std_beta = mean(std_error, na.rm = TRUE),
            mean_idsr = mean(idsr, na.rm = TRUE))

#calculate the porfolio beta and standard errors of beta during 1930-1935.
#filter the data by date range and group by permno, run the regression.
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

#calculate the porfolio beta and standard errors of beta during 1930-1936.
#filter the data by date range and group by permno, run the regression.
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

#calculate the porfolio beta and standard errors of beta during 1930-1937.
#filter the data by date range and group by permno, run the regression.
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

#calculate mean beta and std. 
beta_means_by_portfolio2 <- beta_means_by_portfolio2 %>%
  group_by(portfolio) %>%
  summarise(
    mean_beta_avg = mean(c(mean_beta, mean_beta1, mean_beta2, mean_beta3, na.rm = TRUE)),
    mean_std_beta_avg = mean(c(mean_std_beta, mean_std_beta1, mean_std_beta2, mean_std_beta3, na.rm = TRUE)),
    mean_idsr_avg = mean(c(mean_idsr, mean_idsr1, mean_idsr2, mean_idsr3, na.rm = TRUE)),
  )

#calculate the monthly portfolio return
average_by_portfolio_month3 <- capm_data3 %>%
  group_by(portfolio, month) %>%        
  summarize(avg_ret = mean(raw_ret, na.rm = TRUE), 
            avg_mkt = mean(raw_mkt, na.rm = TRUE)) %>%
  ungroup()

#linear regression calculates the R^2 between avg_ret and avg_mkt
r_squared_by_portfolio3 <- average_by_portfolio_month3 %>%
  group_by(portfolio) %>% 
  do({
    model <- lm(avg_ret ~ avg_mkt, data = .)
    r_squared <- summary(model)$r.squared  
    data.frame(r_squared = r_squared) 
  }) %>%
  ungroup() 

#calculate the standard deviation of avg_ret by portfolio classification
stddev_by_portfolio3 <- average_by_portfolio_month3 %>%
  group_by(portfolio) %>% 
  summarise(
    stddev_avg_ret = sd(avg_ret, na.rm = TRUE)
  ) %>%
  ungroup() 

#create a new file named table2 to merge together
table2 <- stddev_by_portfolio3 %>%
  inner_join(r_squared_by_portfolio3, by = "portfolio") %>%
  inner_join(beta_means_by_portfolio2, by = "portfolio") 
#finished by Kristina 


#Start by Stephanie 
#updated with sample size 1935 to 1938
#Row 5
#standard deviation of the portfolio residuals

#regression for each portfolio to get the residuals
portfolio_residuals3 <- average_by_portfolio_month3 %>%
  group_by(portfolio) %>%
  do({
    model <- lm(avg_ret ~ avg_mkt, data = .)  
    residuals <- resid(model)  
    data.frame(month = .$month, residuals = residuals)
  })

#calculate the standard deviation of residuals for each portfolio 
#standard deviation of residuals
stddev_residuals_by_portfolio3 <- portfolio_residuals3 %>%
  group_by(portfolio) %>%
  summarise(stddev_residuals = sd(residuals, na.rm = TRUE))

#join to table2
table2 <- table2 %>%
  inner_join(stddev_residuals_by_portfolio3, by = "portfolio")

#start by Stephanie and Kristina
#we generate a function to repeat the above steps by using different period.
#generalized function to perform Fama-MacBeth type analysis for a given period
run_fama_macbeth_analysis <- function(capm_data, portfolio_start, portfolio_end, estimation_start, estimation_end, estimation_end1, estimation_end2, estimation_end3, testing_start,testing_start1, testing_end) {
  
  #finding the securities meeting the data requirements
  permno_first_month <- capm_data %>%
    filter(month >= as.Date(testing_start) & month < as.Date(testing_start1)) %>%
    select(permno) %>%
    distinct()%>%
    pull(permno)
  
  #calculate the number of permno in first month of testing period
  count_first_month <- length(permno_first_month)
  
  #count the number of permnos that meeting the data requirement
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
  
  #the amount
  count_in_all_months <- nrow(count_valid_permno)
  
  #portfolio formation period: calculate BETA for portfolio_start to portfolio_end
  portfolio_data <- capm_data %>%
    filter(month >= as.Date(portfolio_start) & month <= as.Date(portfolio_end))
  portfolio_data<- portfolio_data %>%
    inner_join(count_valid_permno, by = "permno")
  
  beta_results <- portfolio_data %>%
    group_by(permno) %>%
    do(tidy(lm(raw_ret ~ raw_mkt, data = .)))
  
  #filter beta results and assign portfolios
  beta_results_only <- beta_results %>%
    filter(term == "raw_mkt") %>%
    select(permno, beta = estimate, std_error = std.error, t_statistic = statistic, p_value = p.value) %>%
    filter(!is.na(beta))
  
  #calculate the number of securities and assign portfolios based on ranked betas
  beta_results_only <- beta_results_only %>%
    arrange(beta) 
  
  N <- nrow(beta_results_only)
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
  
  #initial Estimation Period: Calculate BETA year by year
  estimation_period_data <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end))
  estimation_period_data<- estimation_period_data %>%
    inner_join(count_valid_permno, by = "permno")
  
  #beta calculations
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
  
  #testing Period: Calculate returns for each portfolio
  testing_period_data <- capm_data %>%
    filter(month >= as.Date(testing_start) & month <= as.Date(testing_end))
  testing_period_data<- testing_period_data %>%
    inner_join(count_valid_permno, by = "permno")
  
  #portfolio
  testing_period_data<- testing_period_data %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno")
  
  #join beta_results_only2 with portfolio number
  beta_results_only2 <- beta_results_only2 %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno")
  
  #calculate the porfolio beta and standard errors of beta.
  beta_means_by_portfolio2 <- beta_results_only2 %>%
    group_by(portfolio) %>%              
    summarise(mean_beta = mean(beta, na.rm = TRUE),
              mean_std_beta = mean(std_error, na.rm = TRUE),
              mean_idsr = mean(idsr, na.rm = TRUE))
  
  #calculate the porfolio beta and standard errors of beta by updating one year.
  #filter the data by date range and group by permno, run the regression.
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
  
  #calculate the porfolio beta and standard errors of beta by updating one year.
  #filter the data by date range and group by permno, run the regression.
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
  
  #calculate the porfolio beta and standard errors of beta by updating one year.
  #filter the data by date range and group by permno, run the regression.
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
  
  #calculate mean beta and std. 
  beta_means_by_portfolio2 <- beta_means_by_portfolio2 %>%
    group_by(portfolio) %>%
    summarise(
      mean_beta_avg = mean(c(mean_beta, mean_beta1, mean_beta2, mean_beta3, na.rm = TRUE)),
      mean_std_beta_avg = mean(c(mean_std_beta, mean_std_beta1, mean_std_beta2, mean_std_beta3, na.rm = TRUE)),
      mean_idsr_avg = mean(c(mean_idsr, mean_idsr1, mean_idsr2, mean_idsr3, na.rm = TRUE)),
    )
  
  #calculate average returns by portfolio
  average_by_portfolio_month <- testing_period_data %>%
    group_by(portfolio, month) %>%
    summarize(avg_ret = mean(raw_ret, na.rm = TRUE), avg_mkt = mean(raw_mkt, na.rm = TRUE)) %>%
    ungroup()
  
  #calculate R-squared values
  r_squared_by_portfolio <- average_by_portfolio_month %>%
    group_by(portfolio) %>%
    do({
      model <- lm(avg_ret ~ avg_mkt, data = .)
      r_squared <- summary(model)$r.squared
      data.frame(r_squared = r_squared)
    }) %>%
    ungroup()
  
  #calculate standard deviation of returns
  stddev_by_portfolio <- average_by_portfolio_month %>%
    group_by(portfolio) %>%
    summarise(stddev_avg_ret = sd(avg_ret, na.rm = TRUE)) %>%
    ungroup()
  
  #residuals
  portfolio_residuals3 <- average_by_portfolio_month %>%
    group_by(portfolio) %>%
    do({
      model <- lm(avg_ret ~ avg_mkt, data = .)  
      residuals <- resid(model)  
      data.frame(month = .$month, residuals = residuals)
    })
  
  #standard deviation of residuals
  stddev_residuals_by_portfolio3 <- portfolio_residuals3 %>%
    group_by(portfolio) %>%
    summarise(stddev_residuals = sd(residuals, na.rm = TRUE)) 
  
  #calculate the final table merging all information
  final_table <- stddev_by_portfolio %>%
    inner_join(r_squared_by_portfolio, by = "portfolio") %>%
    inner_join(beta_means_by_portfolio2, by = "portfolio")%>%
    inner_join(stddev_residuals_by_portfolio3, by = "portfolio")%>%
    mutate(count_first_month = count_first_month,
           count_in_all_months = count_in_all_months,
           ratio = stddev_residuals / mean_idsr_avg)
  
  return(final_table)
}
#finish by Kristina and Stephanie

#start by Kristina
#define periods from Table 1
#initialize an empty list
periods <- list()

#define the start and end year
start_year <- 1927
end_year <- 2007

#loop to generate each time period
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
  
  #add the generated time period to the list
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

#considering the first one
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

#add time periods to the periods list
combined_periods <- c(list(extra_period), periods)

#loop through each period and calculate results
results <- lapply(combined_periods, function(combined_periods) {
  run_fama_macbeth_analysis(capm_data, combined_periods$portfolio_start, combined_periods$portfolio_end, combined_periods$estimation_start, combined_periods$estimation_end, combined_periods$estimation_end1, combined_periods$estimation_end2, combined_periods$estimation_end3, combined_periods$testing_start, combined_periods$testing_start1, combined_periods$testing_end)
})

#combine results into a data frame
combined_results <- do.call(rbind, lapply(results, as.data.frame))

#install.packages("openxlsx")
library(openxlsx)
#create an Excel file
write.xlsx(combined_results, "TABLE 2.xlsx")

#finish by Kristina

#start by Stephanie and Kristina
#TABLE 3
### Collect FF 3 factor data ####
# Based on Dr. Saikat's lecture Topic 6
temp <- tempfile(fileext = ".zip")
download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip", temp)
temp1 <- unzip(temp, exdir = ".")
ff_3factors_monthly <- read_csv(temp1, skip = 5) 
names(ff_3factors_monthly) <- c('dt', 'rmrf', 'smb', 'hml', 'rf')
unlink(temp)
unlink(temp1)

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


# Function to calculate gamma for Table 3 Panel A
calculate_gamma_table3_with_yearly_update <- function(capm_data, portfolio_start, portfolio_end, estimation_start, estimation_end, 
                                                      estimation_end1, estimation_end2, estimation_end3, testing_start, testing_start1, testing_end, 
                                                      testing_start_m1, testing_start_m2, testing_start_m3, testing_start_m4, testing_end_m1, 
                                                      testing_end_m2, testing_end_m3, testing_end_m4) {
  
  
  # Identify the permno available in the first testing month
  permno_first_month <- capm_data %>%
    filter(month >= as.Date(testing_start)& month < as.Date(testing_start1)) %>%
    select(permno) %>%
    distinct() %>%
    pull(permno)
  
  # Filter permno for valid periods and data requirements
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
  
  # Count the valid permno from capm_data
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
  
  # Assign portfolios based on ranked betas
  portfolio_data <- capm_data %>%
    filter(month >= as.Date(testing_start) & month <= as.Date(testing_end)) %>%
    inner_join(count_valid_permno, by = "permno")
  
  beta_results <- portfolio_data %>%
    group_by(permno) %>%
    do(tidy(lm(raw_ret ~ raw_mkt, data = .)))
  
  # Filter only the beta results, assigned portfolios, permno, std error
  beta_results_only <- beta_results %>%
    filter(term == "raw_mkt") %>%
    select(permno, beta = estimate, std_error = std.error) %>%
    filter(!is.na(beta))
  
  # Arrange 
  beta_results_only <- beta_results_only %>%
    arrange(beta) 
  
  # Securities per portfolio, we use the number of row of the previous data we got / 20
  # 20 is the number of portfolio - based on the Fama paper
  N <- nrow(beta_results_only)
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
  
  # Update yearly, calculate portfolio beta and standard errors for the estimation periods
  beta_0 <- capm_data %>%
      filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end)) %>%
      inner_join(count_valid_permno, by = "permno") %>%
      group_by(permno) %>%
      do({
        m1 <- lm(raw_ret ~ raw_mkt, data = .)
        tidy(m1) %>%
          filter(term == "raw_mkt") %>%
          select(beta = estimate, std_error = std.error)
      }) %>%
      inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
      group_by(portfolio) %>%
      summarise(mean_beta = mean(beta, na.rm = TRUE), mean_std_beta = mean(std_error, na.rm = TRUE))
  
  #finish by Kristina and Stephanie. 
  
  
  #start by Kristina
  # calculate betas by updating one year at a time
  beta_1 <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end1)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)
      tidy(m1) %>%
        filter(term == "raw_mkt") %>%
        select(beta = estimate, std_error = std.error)
    }) %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
    group_by(portfolio) %>%
    summarise(mean_beta = mean(beta, na.rm = TRUE), mean_std_beta = mean(std_error, na.rm = TRUE))
  
  beta_2 <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end2)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)
      tidy(m1) %>%
        filter(term == "raw_mkt") %>%
        select(beta = estimate, std_error = std.error)
    }) %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
    group_by(portfolio) %>%
    summarise(mean_beta = mean(beta, na.rm = TRUE), mean_std_beta = mean(std_error, na.rm = TRUE))
  
  beta_3 <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end3)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)
      tidy(m1) %>%
        filter(term == "raw_mkt") %>%
        select(beta = estimate, std_error = std.error)
    }) %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
    group_by(portfolio) %>%
    summarise(mean_beta = mean(beta, na.rm = TRUE), mean_std_beta = mean(std_error, na.rm = TRUE))
  
  
  # Calculate average returns by portfolio and month
  avg_returns_by_portfolio <- capm_data %>%
    filter(month >= as.Date(testing_start) & month <= as.Date(testing_end)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%  
    group_by(portfolio, month, month_label) %>%  
    summarise(avg_ret = mean(raw_ret, na.rm = TRUE),
              avg_mkt = mean(raw_mkt, na.rm = TRUE)) %>%
    ungroup()
  
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
  
  beta_0 <- beta_0 %>%
    select(portfolio, mean_beta, mean_std_beta) %>%
    expand_grid(months_0) %>%
    left_join(beta_0, by = "portfolio") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%
    select(-mean_beta.y, -mean_std_beta.y) %>%  
    rename(mean_beta = mean_beta.x, mean_std_beta = mean_std_beta.x) 
  
  beta_1 <- beta_1 %>%
    select(portfolio, mean_beta, mean_std_beta) %>%
    expand_grid(months_1) %>%
    left_join(beta_1, by = "portfolio") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%
    select(-mean_beta.y, -mean_std_beta.y) %>%  
    rename(mean_beta = mean_beta.x, mean_std_beta = mean_std_beta.x) 
  
  beta_2 <- beta_2 %>%
    select(portfolio, mean_beta, mean_std_beta) %>%
    expand_grid(months_2) %>%
    left_join(beta_2, by = "portfolio") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%
    select(-mean_beta.y, -mean_std_beta.y) %>%  
    rename(mean_beta = mean_beta.x, mean_std_beta = mean_std_beta.x) 
  
  beta_3 <- beta_3 %>%
    select(portfolio, mean_beta, mean_std_beta) %>%
    expand_grid(months_3) %>%
    left_join(beta_3, by = "portfolio") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%
    select(-mean_beta.y, -mean_std_beta.y) %>%  
    rename(mean_beta = mean_beta.x, mean_std_beta = mean_std_beta.x) 
  
  combined <- bind_rows(beta_0, beta_1, beta_2, beta_3)
  
  avg_returns_by_portfolio <- avg_returns_by_portfolio %>%
    left_join(combined %>% select(portfolio, mean_beta, mean_std_beta, month_label), 
              by = c("month_label" = "month_label", "portfolio" = "portfolio"))

  return(avg_returns_by_portfolio)
}



periods <- list()

#define the start and end year
start_year <- 1927
end_year <- 2007

#loop to generate each time period
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
  
  #add the generated time period to the list
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

#considering the first one
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

#add time periods to the periods list
combined_periods <- c(list(extra_period), periods)

#loop through each period and calculate results
results <- lapply(combined_periods, function(combined_periods) {
  calculate_gamma_table3_with_yearly_update(capm_data, combined_periods$portfolio_start, combined_periods$portfolio_end, combined_periods$estimation_start, 
                            combined_periods$estimation_end, combined_periods$estimation_end1, combined_periods$estimation_end2, 
                            combined_periods$estimation_end3, combined_periods$testing_start, combined_periods$testing_start1, 
                            combined_periods$testing_end, combined_periods$testing_start_m1, combined_periods$testing_start_m2,
                            combined_periods$testing_start_m3, combined_periods$testing_start_m4, combined_periods$testing_end_m1,
                            combined_periods$testing_end_m2, combined_periods$testing_end_m3, combined_periods$testing_end_m4)
})

#combine results into a data frame
combined_results <- do.call(rbind, lapply(results, as.data.frame))

#look for monthly rf
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

#merge with combined_result
combined_results <- combined_results |> 
  left_join(ff_3factors_mon, by = "month_label")

#a function to create table 3
table3_function <- function(combined_results, start, end) {
  
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
      s_gama0 = mean(std.error[term == "(Intercept)"], na.rm = TRUE),       
      s_gama1 = mean(std.error[term == "mean_beta"], na.rm = TRUE),
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

periods1 <- list()

#define the start and end year
start_year <- 1946
end_year <- 2015

#loop to generate each time period
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
panela <- lapply(combined_periods, function(combined_periods) {
  table3_function(combined_results, combined_periods$start, combined_periods$end)
})

#combine results into a data frame
t3_panela <- do.call(rbind, lapply(panela, as.data.frame))

#finish by kristina




# Table 3 Panel B
# Panel B have more complexity by using an additional squared term for beta (β²) in the regression

# Function to calculate gamma for Table 3 Panel B
# Differences from Panel A: include BETA SQUARED term in the regression 
# Start by Stephanie 

calculate_gamma_table3_with_yearly_PanelB <- function(capm_data, portfolio_start, portfolio_end, estimation_start, estimation_end, 
                                                      estimation_end1, estimation_end2, estimation_end3, testing_start, testing_start1, testing_end, 
                                                      testing_start_m1, testing_start_m2, testing_start_m3, testing_start_m4, testing_end_m1, 
                                                      testing_end_m2, testing_end_m3, testing_end_m4) {
  
  
  # Identify the permno available in the first testing month
  permno_first_month <- capm_data %>%
    filter(month >= as.Date(testing_start)& month < as.Date(testing_start1)) %>%
    select(permno) %>%
    distinct() %>%
    pull(permno)
  
  # Filter permno for valid periods and data requirements
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
  
  # Count the valid permno from capm_data
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
  
  # Assign portfolios based on ranked betas
  portfolio_data <- capm_data %>%
    filter(month >= as.Date(testing_start) & month <= as.Date(testing_end)) %>%
    inner_join(count_valid_permno, by = "permno")
  
  beta_results <- portfolio_data %>%
    group_by(permno) %>%
    do(tidy(lm(raw_ret ~ raw_mkt, data = .)))
  
  # Filter only the beta results, assigned portfolios, permno, std error
  beta_results_only <- beta_results %>%
    filter(term == "raw_mkt") %>%
    select(permno, beta = estimate, std_error = std.error) %>%
    filter(!is.na(beta))
  
  # Arrange 
  beta_results_only <- beta_results_only %>%
    arrange(beta) 
  
  # Securities per portfolio, we use the number of row of the previous data we got / 20
  # 20 is the number of portfolio - based on the Fama paper
  N <- nrow(beta_results_only)
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
  
  # Update yearly, calculate portfolio beta and standard errors for the estimation periods
  beta_0 <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)
      tidy(m1) %>%
        filter(term == "raw_mkt") %>%
        select(beta = estimate, std_error = std.error)
    }) %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
    group_by(portfolio) %>%
    summarise(mean_beta = mean(beta, na.rm = TRUE), mean_std_beta = mean(std_error, na.rm = TRUE))
  
  # calculate betas by updating one year at a time
  beta_1 <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end1)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)
      tidy(m1) %>%
        filter(term == "raw_mkt") %>%
        select(beta = estimate, std_error = std.error)
    }) %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
    group_by(portfolio) %>%
    summarise(mean_beta = mean(beta, na.rm = TRUE), mean_std_beta = mean(std_error, na.rm = TRUE))
  
  beta_2 <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end2)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)
      tidy(m1) %>%
        filter(term == "raw_mkt") %>%
        select(beta = estimate, std_error = std.error)
    }) %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
    group_by(portfolio) %>%
    summarise(mean_beta = mean(beta, na.rm = TRUE), mean_std_beta = mean(std_error, na.rm = TRUE))
  
  beta_3 <- capm_data %>%
    filter(month >= as.Date(estimation_start) & month <= as.Date(estimation_end3)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    group_by(permno) %>%
    do({
      m1 <- lm(raw_ret ~ raw_mkt, data = .)
      tidy(m1) %>%
        filter(term == "raw_mkt") %>%
        select(beta = estimate, std_error = std.error)
    }) %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
    group_by(portfolio) %>%
    summarise(mean_beta = mean(beta, na.rm = TRUE), mean_std_beta = mean(std_error, na.rm = TRUE))
  
  
  # Calculate average returns by portfolio and month
  avg_returns_by_portfolio <- capm_data %>%
    filter(month >= as.Date(testing_start) & month <= as.Date(testing_end)) %>%
    inner_join(count_valid_permno, by = "permno") %>%
    inner_join(beta_results_only %>% select(permno, portfolio), by = "permno") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%  
    group_by(portfolio, month, month_label) %>%  
    summarise(avg_ret = mean(raw_ret, na.rm = TRUE),
              avg_mkt = mean(raw_mkt, na.rm = TRUE)) %>%
    ungroup()
  
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
  
  beta_0 <- beta_0 %>%
    select(portfolio, mean_beta, mean_std_beta) %>%
    expand_grid(months_0) %>%
    left_join(beta_0, by = "portfolio") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%
    select(-mean_beta.y, -mean_std_beta.y) %>%  
    rename(mean_beta = mean_beta.x, mean_std_beta = mean_std_beta.x) 
  
  beta_1 <- beta_1 %>%
    select(portfolio, mean_beta, mean_std_beta) %>%
    expand_grid(months_1) %>%
    left_join(beta_1, by = "portfolio") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%
    select(-mean_beta.y, -mean_std_beta.y) %>%  
    rename(mean_beta = mean_beta.x, mean_std_beta = mean_std_beta.x) 
  
  beta_2 <- beta_2 %>%
    select(portfolio, mean_beta, mean_std_beta) %>%
    expand_grid(months_2) %>%
    left_join(beta_2, by = "portfolio") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%
    select(-mean_beta.y, -mean_std_beta.y) %>%  
    rename(mean_beta = mean_beta.x, mean_std_beta = mean_std_beta.x) 
  
  beta_3 <- beta_3 %>%
    select(portfolio, mean_beta, mean_std_beta) %>%
    expand_grid(months_3) %>%
    left_join(beta_3, by = "portfolio") %>%
    mutate(month_label = format(month, "%Y-%m")) %>%
    select(-mean_beta.y, -mean_std_beta.y) %>%  
    rename(mean_beta = mean_beta.x, mean_std_beta = mean_std_beta.x) 
  
  combined <- bind_rows(beta_0, beta_1, beta_2, beta_3)
  
  # Combine data with average returns and betas 
  avg_returns_by_portfolio <- avg_returns_by_portfolio %>%
    left_join(combined %>% select(portfolio, mean_beta, mean_std_beta, month_label), 
              by = c("month_label" = "month_label", "portfolio" = "portfolio"))
  
  return(avg_returns_by_portfolio)
}




#PANEL B
periods <- list()

#define the start and end year for Panel B
start_year <- 1927
end_year <- 2007

#loop to generate each time period
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
  
  #add the generated time period to the list
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

#considering the first one
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

#add time periods to the periods list
combined_periods <- c(list(extra_period), periods)

#loop through each period and calculate results
results <- lapply(combined_periods, function(combined_periods) {
  calculate_gamma_table3_with_yearly_PanelB(capm_data, combined_periods$portfolio_start, combined_periods$portfolio_end, combined_periods$estimation_start, 
                                            combined_periods$estimation_end, combined_periods$estimation_end1, combined_periods$estimation_end2, 
                                            combined_periods$estimation_end3, combined_periods$testing_start, combined_periods$testing_start1, 
                                            combined_periods$testing_end, combined_periods$testing_start_m1, combined_periods$testing_start_m2,
                                            combined_periods$testing_start_m3, combined_periods$testing_start_m4, combined_periods$testing_end_m1,
                                            combined_periods$testing_end_m2, combined_periods$testing_end_m3, combined_periods$testing_end_m4)
})

# Combine results into a data frame
combined_results <- do.call(rbind, lapply(results, as.data.frame))

# Monthly rf
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

# Combine and merge with combined_result
combined_results <- combined_results |> 
  left_join(ff_3factors_mon, by = "month_label")






# Regression PANEL B should include quadratic term 
table3_function_panel_b <- function(combined_results, start, end) {
  
  # Calculate gamma0, gamma1, and gamma2 using Fama-MacBeth regression (cross-sectional regression)
  table3 <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>%
    group_by(month) %>%
    do({
      # Panel B: Include the beta^2 term in the regression
      # and add gamma2 (based on Table 3 Panel B of the Fama Paper)
      model <- lm(avg_ret ~ mean_beta + I(mean_beta^2), data = .)
      tidy(model)
    }) %>%
    ungroup() %>%
    summarise(
      gamma0 = mean(estimate[term == "(Intercept)"], na.rm = TRUE),
      gamma1 = mean(estimate[term == "mean_beta"], na.rm = TRUE),
      gamma2 = mean(estimate[term == "I(mean_beta^2)"], na.rm = TRUE),
      s_gamma0 = mean(std.error[term == "(Intercept)"], na.rm = TRUE),
      s_gamma1 = mean(std.error[term == "mean_beta"], na.rm = TRUE),
      s_gamma2 = mean(std.error[term == "I(mean_beta^2)"], na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Adjusted R-squared calculation with mean_beta^2
  adjusted_r2 <- combined_results %>%
    filter(month >= as.Date(start) & month <= as.Date(end)) %>%
    group_by(month) %>%
    do({
      model <- lm(avg_ret ~ mean_beta + I(mean_beta^2), data = .)
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
panelB <- lapply(combined_periods, function(combined_periods) {
  table3_function_panel_b(combined_results, combined_periods$start, combined_periods$end)
})

#combine results into a data frame
# in Panel B, mean_beta^ is included in the regreesion part 
# to capture the non-linear effect of beta on the returns
# gamma 2 is the additional gamma coefficient, calculated from the quadratic term 
t3_panelB <- do.call(rbind, lapply(panelB, as.data.frame))

# Finished by Stephanie 









