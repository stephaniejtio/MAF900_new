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

#calculate the porfolio beta and standard errors of beta
beta_means_by_portfolio2 <- beta_results_only2 %>%
  group_by(portfolio) %>%              
  summarise(mean_beta = mean(beta, na.rm = TRUE),
            mean_std_beta = mean(std_error, na.rm = TRUE))

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

# Row 5
# standard deviation of the portfolio residuals
# idiosyncratic risk at portfolio

#regression for each portfolio to get the residuals
portfolio_residuals <- average_by_portfolio_month %>%
  group_by(portfolio) %>%
  do({
    model <- lm(avg_ret ~ avg_mkt, data = .)  # Regress avg_ret on avg_mkt for each portfolio
    residuals <- resid(model)  # Extract the residuals from the model
    data.frame(month = .$month, residuals = residuals)  # Return the residuals and month
  })

#calculate the standard deviation of residuals for each portfolio 
stddev_residuals_by_portfolio <- portfolio_residuals %>%
  group_by(portfolio) %>%
  summarise(stddev_residuals = sd(residuals, na.rm = TRUE))  # Standard deviation of residuals
print(stddev_residuals_by_portfolio)




# Row 6
# we can reuse the regression model from previous ones (beta_results2)
# however we need to assign each security to its portfolio
# this will return specifically idsr and permno
beta_results3 <- capm_data2 %>%
  group_by(permno) %>%
  do({
    model <- lm(raw_ret ~ raw_mkt, data = .)
    residuals <- resid(model)  
    idsr <- sd(residuals)  
    data.frame(permno = unique(.$permno), idsr = idsr)  
  })

# Assign each security to its portfolio (based on earlier portfolio classification)
# Match each security with its portfolio
beta_results3 <- beta_results3 %>%
  inner_join(beta_results_only, by = "permno")  

# Calculate the average standard deviation of residuals (idsr) for each portfolio
# Average standard deviation of residuals across securities in the portfolio
avg_stddev_residuals_by_portfolio <- beta_results3 %>%
  group_by(portfolio) %>%
  summarise(avg_idsr = mean(idsr, na.rm = TRUE))  


# option 2: using beta_results2
beta_results3 <- beta_results2 %>%
  inner_join(beta_results_only, by = "permno")  # Match each security with its portfolio

# Calculate the average standard deviation of residuals (idsr) for each portfolio
avg_stddev_residuals_by_portfolio2 <- beta_results3 %>%
  group_by(portfolio) %>%
  summarise(avg_idsr = mean(idsr, na.rm = TRUE))  # Average standard deviation of residuals across securities in the portfolio


# Row 7 
# SD of the portfolio residuals 
# avg SD of individual security residuals 

#Merge data for SD residuals (row 5) and avg SD of reisduals (row 6)
residual_risk <- stddev_residuals_by_portfolio %>%
  inner_join(avg_stddev_residuals_by_portfolio, by = "portfolio")

# Calculate the ratio (s(ε_p) / s̅_p,t-1(ε_i))
residual_risk <- residual_risk_comparison %>%
  mutate(ratio = stddev_residuals / avg_idsr)
 


# Merge rows 5, 6, and 7 with existing table2
#table2 <- table2 %>%
#  inner_join(average_returns_by_portfolio, by = "portfolio") %>%
#  inner_join(standard_error_by_portfolio, by = "portfolio")

#View the updated table
#print(table2)
