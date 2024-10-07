#the following codes are for MAF900 assignment 3 in T3 2024. 
#start from kristina
#install.packages("broom")
#install.packages("slider")
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
#finished by kristina 

#Stephanie Start
#Portfolio formation 
#Calculate BETA for period 1926-1929
capm_data1 <- capm_data %>%
  filter(month >= as.Date("1926-01-01") & month <= as.Date("1929-12-31"))

# Calculate beta for each company
beta_results <- capm_data1 %>%
  group_by(permno) %>%
  do(tidy(lm(raw_ret ~ raw_mkt, data = .)))
# print the results for beta
print(beta_results)

# filter for the beta results
beta_results_only <- beta_results %>%
  filter(term == "raw_mkt") %>%
  select(permno, beta = estimate, std_error = std.error, t_statistic = statistic, p_value = p.value)

# beta results for each of the company 
print(beta_results_only) 

#check for NA values 
sum(is.na(beta_results_only$beta)) 

#remove NA values from beta
beta_results_only <- beta_results_only %>%
  filter(!is.na(beta))


# Based on the paper Page 615 section B.Details 
# allocating securities into 20 portfolios 

#calculate the total number of securities 
N <- nrow(beta_results_only)

#Calculate the number of securities, middle portfolios
securities_per_portfolio <- floor(N / 20)

#Calculate the remainder, allocate to first and last portfolios
remainder <- N - 20 * securities_per_portfolio

#Determine number of securities, first and last portfolios
first_last_extra <- floor(remainder / 2)
last_portfolio_extra <- remainder %% 2  # If odd, last portfolio gets an extra security

# Assign portfolios using the beta values (suffix 1)
breaks <- quantile(beta_results_only$beta, probs = seq(0, 1, length.out = 21))

# ranked 
beta_results_ranked <- beta_results_only %>%
  mutate(portfolio = cut(beta, breaks = breaks, labels = FALSE, include.lowest = TRUE))

# group by portfolio
portfolio_distribution <- beta_results_ranked %>%
  group_by(portfolio) %>%
  summarise(count = n(), .groups = 'drop')

# distribution of the securities for each portfolio 
print(portfolio_distribution)

# results based on the paper 
#Total number of securities / stocks
cat("Total securities (N):", N, "\n")
#Number of securities in each portfolio
cat("Securities per portfolio (middle 18):", securities_per_portfolio, "\n")
cat("Extra securities for first and last portfolios:", first_last_extra, "\n")
cat("Extra security for last portfolio (if N is odd):", last_portfolio_extra, "\n")

# portfolio assigned by using paper method
# rank based on beta
beta_results_only <- beta_results_only %>%
  arrange(beta)

#calculating the size of each porfolio
portfolio_sizes <- c(
  securities_per_portfolio + first_last_extra,        # first portfolio
  rep(securities_per_portfolio, 18),                 # 2nd to 19th portfolio
  securities_per_portfolio + first_last_extra + last_portfolio_extra  # 20th portfolio
)

#create a variable named portfolio
beta_results_only$portfolio <- rep(1:20, times = portfolio_sizes)

#Assign Securities to Portfolios Based on Ranked Betas
beta_results_sorted <- beta_results_ranked %>%
  arrange(beta)



#start from Kristina
#calculate BETA for period 1930-1934
capm_data2 <- capm_data %>%
  filter(month >= as.Date("1930-01-01") & month <= as.Date("1934-12-31"))

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

#sort by portfolio in matched_results and extract unique permno
permno_by_portfolio1 <- beta_results_only %>%
  arrange(portfolio) %>%            
  group_by(portfolio) %>%          
  distinct(permno) %>%      
  ungroup() 

#calculate the portfolio return during 1935-1938
#set a new dataset for period 1935-1938
capm_data3 <- capm_data %>%
  filter(month >= as.Date("1935-01-01") & month <= as.Date("1938-12-31"))

#find stocks still listed
#use inner_join to find matching permno
#merge by permno and automatically discard unmatched lines
capm_data3 <- capm_data3 %>%
  inner_join(permno_by_portfolio1, by = "permno")

#sort by portfolio in matched_results and extract unique permno
permno_by_portfolio3 <- capm_data3 %>%
  arrange(portfolio) %>%            
  group_by(portfolio) %>%          
  distinct(permno) %>%      
  ungroup() 

#use inner_join to find matching permno
beta_results_only2 <- beta_results_only2 %>%
  inner_join(permno_by_portfolio3, by = "permno")

#see how many permno match
matched_count <- n_distinct(beta_results_only2$permno)

#calculate the porfolio beta and standard errors of beta during 1930-1934.
beta_means_by_portfolio2 <- beta_results_only2 %>%
  group_by(portfolio) %>%              
  summarise(mean_beta = mean(beta, na.rm = TRUE),
            mean_std_beta = mean(std_error, na.rm = TRUE))

#calculate the porfolio beta and standard errors of beta during 1930-1935.
#filter the data by date range and group by permno, run the regression.
beta_means_by_portfolio2 <- capm_data %>%
  filter(month >= as.Date("1930-01-01") & month <= as.Date("1935-12-31")) %>% 
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
  inner_join(permno_by_portfolio3, by = "permno") %>%  
  group_by(portfolio) %>%
  summarise(
    mean_beta = mean(beta, na.rm = TRUE),        
    mean_std_beta = mean(std_error, na.rm = TRUE)  
  ) %>%
  ungroup()%>%
  inner_join(beta_means_by_portfolio2, by = "portfolio")

#calculate the porfolio beta and standard errors of beta during 1930-1936.
#filter the data by date range and group by permno, run the regression.
beta_means_by_portfolio2 <- capm_data %>%
  filter(month >= as.Date("1930-01-01") & month <= as.Date("1936-12-31")) %>% 
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
  inner_join(permno_by_portfolio3, by = "permno") %>%  
  group_by(portfolio) %>%
  summarise(
    mean_beta = mean(beta, na.rm = TRUE),        
    mean_std_beta = mean(std_error, na.rm = TRUE)  
  ) %>%
  ungroup()%>%
  inner_join(beta_means_by_portfolio2, by = "portfolio")

#calculate the porfolio beta and standard errors of beta during 1930-1937.
#filter the data by date range and group by permno, run the regression.
beta_means_by_portfolio2 <- capm_data %>%
  filter(month >= as.Date("1930-01-01") & month <= as.Date("1937-12-31")) %>% 
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
  inner_join(permno_by_portfolio3, by = "permno") %>%  
  group_by(portfolio) %>%
  summarise(
    mean_beta = mean(beta, na.rm = TRUE),        
    mean_std_beta = mean(std_error, na.rm = TRUE)  
  ) %>%
  ungroup()%>%
  inner_join(beta_means_by_portfolio2, by = "portfolio")

#
beta_means_by_portfolio2 <- beta_means_by_portfolio2 %>%
  group_by(portfolio) %>%
  summarise(
    mean_beta_avg = across(starts_with("mean_beta"), mean, na.rm = TRUE) %>% rowMeans(na.rm = TRUE),
    mean_std_beta_avg = across(starts_with("mean_std_beta"), mean, na.rm = TRUE) %>% rowMeans(na.rm = TRUE)
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


# Start by Stephanie 
# updated with sample size 1935 to 1938
# Row 5
# standard deviation of the portfolio residuals
# idiosyncratic risk at portfolio

# regression for each portfolio to get the residuals
# suffix 3 applied (testing period)
portfolio_residuals3 <- average_by_portfolio_month3 %>%
  group_by(portfolio) %>%
  do({
    model <- lm(avg_ret ~ avg_mkt, data = .)  
    residuals <- resid(model)  
    data.frame(month = .$month, residuals = residuals)
  })

#calculate the standard deviation of residuals for each portfolio 
#Suffix 3
# Standard deviation of residuals
stddev_residuals_by_portfolio3 <- portfolio_residuals3 %>%
  group_by(portfolio) %>%
  summarise(stddev_residuals = sd(residuals, na.rm = TRUE))  
print(stddev_residuals_by_portfolio)




# Row 6, average standard deviation of residuals (idsr) for each security in t-1 period (1930-1937)
# this will return specifically idsr and permno
#Updated
#calculate beta and idsr for each year range 
#Function: calculate idsr (avg standard deviation of residuals) for each security in t-1
calculate_avg_idsr_t1 <- function(end_date, permno_by_portfolio) {
  # t-1 start date by subtracting one year from end_date
  start_date <- as.Date(paste0(as.numeric(format(as.Date(end_date), "%Y")) - 1, "-01-01"))
  
  capm_data %>%
    filter(month >= start_date & month <= as.Date(end_date)) %>%  # t-1 period (previous year)
    group_by(permno) %>%
    do({
      model <- lm(raw_ret ~ raw_mkt, data = .)  
      residuals <- resid(model) 
      idsr <- sd(residuals)  
      data.frame(permno = unique(.$permno), idsr = idsr)  
    }) %>%
    ungroup() %>%
    inner_join(permno_by_portfolio, by = "permno")  
}

# idsr for t-1 periods
avg_idsr_1935_t1 <- calculate_avg_idsr_t1("1935-12-31", permno_by_portfolio3) 
avg_idsr_1936_t1 <- calculate_avg_idsr_t1("1936-12-31", permno_by_portfolio3) 
avg_idsr_1937_t1 <- calculate_avg_idsr_t1("1937-12-31", permno_by_portfolio3) 

# t-1 results for different years
# Calculate average standard deviation of residuals
avg_stddev_residuals_by_portfolio_t1 <- list(
  avg_idsr_1935_t1,
  avg_idsr_1936_t1,
  avg_idsr_1937_t1
) %>%
  bind_rows() %>%
  group_by(portfolio) %>%
  summarise(avg_idsr = mean(idsr, na.rm = TRUE))  

print(avg_stddev_residuals_by_portfolio_t1)
#######




# Row 7 
# SD of the portfolio residuals 
# avg SD of individual security residuals 
#Merge data for SD residuals (row 5) and avg SD of reisduals (row 6)
residual_risk <- stddev_residuals_by_portfolio3 %>%
  inner_join(avg_stddev_residuals_by_portfolio_t1, by = "portfolio")

# Calculate the ratio (s(ε_p) / s̅_p,t-1(ε_i)), suffix 3
residual_risk <- residual_risk %>%
  mutate(ratio = stddev_residuals / avg_idsr)
 




#########################################
# Stephanie starts, work in progress 
# Repeat the calculation for all periods 
# Function to calculate portfolio betas and residuals 
calculate_portfolio_betas <- function(data, start_date, end_date) {
  capm_data_filtered <- data %>%
    filter(month >= as.Date(start_date) & month <= as.Date(end_date))
  
  beta_results <- capm_data_filtered %>%
    group_by(permno) %>%
    do({
      model <- lm(raw_ret ~ raw_mkt, data = .)
      residuals <- resid(model)
      idsr <- sd(residuals)
      tidy_model <- tidy(model)
      data.frame(
        permno = unique(.$permno),
        beta = tidy_model$estimate[2],
        idsr = idsr,
        std_error = tidy_model$std.error[2]
      )
    }) %>%
    ungroup()
  
  return(beta_results)
}

# Function assigning securities to portfolios based on beta rankings
assign_to_portfolios <- function(beta_data, num_portfolios = 20) {
  # remove rows with NA values in beta column
  beta_data <- beta_data %>%
    filter(!is.na(beta) & !is.nan(beta))
  
  breaks <- quantile(beta_data$beta, probs = seq(0, 1, length.out = num_portfolios + 1))
  
  beta_data <- beta_data %>%
    mutate(portfolio = cut(beta, breaks = breaks, labels = FALSE, include.lowest = TRUE))
  
  return(beta_data)
}

# Function calculating portfolio returns
calculate_portfolio_returns <- function(data, portfolios) {
  data %>%
    inner_join(portfolios, by = "permno") %>%
    group_by(portfolio, month) %>%
    summarise(
      avg_ret = mean(raw_ret, na.rm = TRUE),
      avg_mkt = mean(raw_mkt, na.rm = TRUE)
    ) %>%
    ungroup()
}

# function to automate the tests
perform_fama_macbeth_tests <- function(data, periods, portfolios, num_portfolios = 20) {
  results <- list()
  
  for (period in periods) {
    #Calculate betas for the given period
    beta_results <- calculate_portfolio_betas(data, period$start, period$end)
    
    #Assign securities to portfolios based on beta
    portfolio_assignments <- assign_to_portfolios(beta_results, num_portfolios)
    
    #Calculate portfolio returns for the next period
    portfolio_returns <- calculate_portfolio_returns(data, portfolio_assignments)
    
    #Store results for the period
    results[[paste0("Period_", period$start, "_", period$end)]] <- list(
      betas = beta_results,
      portfolios = portfolio_assignments,
      returns = portfolio_returns
    )
  }
  return(results)
}

# Define periods (years) 
periods <- list(
  list(start = "1926-01-01", end = "1929-12-31"),
  list(start = "1930-01-01", end = "1934-12-31"),
  list(start = "1935-01-01", end = "1938-12-31")
  # Add portfolios (continue)
)

results <- perform_fama_macbeth_tests(capm_data, periods, permno_by_portfolio1)
