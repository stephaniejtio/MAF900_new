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

#start by Stephanie
#portfolio formation 
#calculate BETA for period 1926-1929
capm_data1 <- capm_data %>%
  filter(month >= as.Date("1926-01-01") & month <= as.Date("1929-12-31"))

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
            mean_std_beta = mean(std_error, na.rm = TRUE),
            mean_idsr = mean(idsr, na.rm = TRUE))

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
run_fama_macbeth_analysis <- function(capm_data, portfolio_start, portfolio_end, estimation_start, estimation_end, estimation_end1, estimation_end2, estimation_end3, testing_start, testing_end) {
  
  #portfolio Formation Period: Calculate BETA for portfolio_start to portfolio_end
  portfolio_data <- capm_data %>%
    filter(month >= as.Date(portfolio_start) & month <= as.Date(portfolio_end))
  
  beta_results <- portfolio_data %>%
    group_by(permno) %>%
    do(tidy(lm(raw_ret ~ raw_mkt, data = .)))
  
  #filter beta results and assign portfolios
  beta_results_only <- beta_results %>%
    filter(term == "raw_mkt") %>%
    select(permno, beta = estimate, std_error = std.error, t_statistic = statistic, p_value = p.value) %>%
    filter(!is.na(beta))
  
  #calculate the number of securities and assign portfolios based on ranked betas
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
  
  #sort by portfolio in matched_results and extract unique permno
  permno_by_portfolio1 <- beta_results_only %>%
    arrange(portfolio) %>%            
    group_by(portfolio) %>%          
    distinct(permno) %>%      
    ungroup() 
  
  #testing Period: Calculate returns for each portfolio
  testing_period_data <- capm_data %>%
    filter(month >= as.Date(testing_start) & month <= as.Date(testing_end))
  
  testing_period_data <- testing_period_data %>%
    inner_join(permno_by_portfolio1, by = "permno")
  
  #sort by portfolio in matched_results and extract unique permno
  permno_by_portfolio3 <- testing_period_data %>%
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
              mean_std_beta = mean(std_error, na.rm = TRUE),
              mean_idsr = mean(idsr, na.rm = TRUE))
  
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
    mutate(matched_count = matched_count,
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
  testing_start = "1935-01-10",
  testing_end = "1938-12-31"
)

#add time periods to the periods list
combined_periods <- c(list(extra_period), periods)

#loop through each period and calculate results
results <- lapply(combined_periods, function(combined_periods) {
  run_fama_macbeth_analysis(capm_data, combined_periods$portfolio_start, combined_periods$portfolio_end, combined_periods$estimation_start, combined_periods$estimation_end, combined_periods$estimation_end1, combined_periods$estimation_end2, combined_periods$estimation_end3, combined_periods$testing_start, combined_periods$testing_end)
})

#combine results into a data frame
combined_results <- do.call(rbind, lapply(results, as.data.frame))

#install.packages("openxlsx")
library(openxlsx)
#create an Excel file
write.xlsx(combined_results, "TABLE 2.xlsx")

#finish by Kristina


#TABLE 3
# portfolio return 
# calculate gama0, as the intercept 
# gama1 betap, Beta Coefficient for Market Risk




# Filter data for the period
filtered_data <- average_by_portfolio_month3 %>%
  filter(month >= as.Date("1935-01-01") & month <= as.Date("1968-06-30"))


# average_by_portfolio_month3 to obtain the average returns and market data
# and add the residuals and variance

# regression for each portfolio to get the residuals
portfolio_residuals <- filtered_data %>%
  group_by(portfolio) %>%
  do({
    model <- lm(avg_ret ~ avg_mkt, data = .)
    residuals <- resid(model)
    data.frame(month = .$month, residuals = residuals)
  })


# calculate the variance of residuals for each portfolio
residuals_var_by_portfolio <- portfolio_residuals %>%
  group_by(portfolio) %>%
  summarise(residuals_var = var(residuals, na.rm = TRUE))


# calculate beta for each portfolio
# use the average returns (avg_ret) and market returns (avg_mkt) for each portfolio.

beta_by_portfolio <- filtered_data %>%
  group_by(portfolio) %>%
  do({
    model <- lm(avg_ret ~ avg_mkt, data = .)
    beta_p <- coef(model)[2]  # slope (beta)
    data.frame(portfolio = .$portfolio[1], beta_p = beta_p)
  })



# merge 
filtered_data <- filtered_data %>%
  inner_join(beta_by_portfolio, by = "portfolio")



# merge the residual variance into the main dataset and calculate beta_p_sq
prepared_data <- filtered_data %>%
  inner_join(residuals_var_by_portfolio, by = "portfolio") %>%
  mutate(beta_p_sq = beta_p^2)  # the square of the beta











# function to calculate Fama-MacBeth coefficients
calculate_fama_macbeth_coefficients <- function(data) {
  # time-series regression of portfolio returns (Rp) on factors (beta_p, beta_p^2, residual risk)
  regressions <- data %>%
    group_by(month) %>%
    do({
      model <- lm(avg_ret ~ beta_p + I(beta_p^2) + residuals_var, data = .)
      tidy(model)
    })
  
  # calculate the average of the coefficients 
  #(cross-sectional averages)
  gamma_coefficients <- regressions %>%
    group_by(term) %>%
    summarise(
      gamma_mean = mean(estimate, na.rm = TRUE),
      gamma_std_error = sd(estimate, na.rm = TRUE) / sqrt(n())
    )
  
  return(gamma_coefficients)
}

# use prepared_data for regressions
# prepared_data should have columns: avg_ret (portfolio return), beta_p (portfolio beta), beta_p_sq (beta squared), residuals_var (variance of residuals)

# Run Fama-MacBeth regression to compute gamma coefficients for the period 1935 to June 1968
gamma_results <- calculate_fama_macbeth_coefficients(prepared_data)

# Print the gamma coefficients (gamma0, gamma1, gamma2, gamma3)
#print(gamma_results)



