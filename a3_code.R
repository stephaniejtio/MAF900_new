#the following codes are for MAF900 assignment 3 in T3 2024. 
#start from kristina
#install.packages("broom")
library(broom)
library(RPostgres)
library(tidyverse)
library(RSQLite)
library(furrr)
library(lubridate)
library(dplyr)

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
    shrout, # Shares outstanding (in thousands)
    altprc, # Last traded price in a month
  ) |> collect()

# save collected data in local database
a3_data <- dbConnect(
  SQLite(),
  "data/a3_data.sqlite",
  extended_types = TRUE)

dbWriteTable(a3_data,
             "crsp_monthly",
             value = crsp_monthly,
             overwrite = TRUE
)

#collect FF 3 factor data
temp <- tempfile(fileext = ".zip")
download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip",temp)
temp1<- unzip(temp, exdir = ".")
ff_3factors_monthly <- read_csv(temp1, skip=5) 
names(ff_3factors_monthly) <- c('dt', 'rmrf', 'smb', 'hml', 'rf')
unlink(temp)
unlink(temp1)

#create date variable, filter the data for the sample period, convert chr data to numeric
ff_3factors_mon <- ff_3factors_monthly |> 
  filter(nchar(dt) == 6) |> 
  mutate(yr = str_sub(dt,1,4), mon= str_sub(dt,-2,-1),  
         date = make_date(year= yr, month = mon, day = 01), 
         mkt_excess = as.numeric(rmrf), smb = as.numeric(smb),
         hml = as.numeric(hml), rf = as.numeric(rf)) |> 
  filter(date >='1926-01-01' & date <= '2023-12-31') |> 
  select(c('date','mkt_excess','smb','hml','rf'))

#save FF 3 factor monthly data in local database
dbWriteTable(a3_data,
             "ff_3factors_monthly",
             value = ff_3factors_mon,
             overwrite = TRUE
)

#collect data
a3_data <- dbConnect(
  SQLite(),
  "data/a3_data.sqlite",
  extended_types = TRUE)

crsp_monthly <- tbl(a3_data,"crsp_monthly") |> collect()
ff_3factors_mon <- tbl(a3_data,"ff_3factors_monthly") |> collect()

#combining the data used in this study to capm_data (we use raw return to analyze)
capm_data <- ff_3factors_mon %>%
  mutate(month = floor_date(date, "month")) %>%
  inner_join(
    crsp_monthly %>%
      mutate(month = floor_date(date, "month")) %>%
      select(permno, month, ret),
    by = c('month')
  ) %>%
  mutate(
    raw_ret = ret * 100,          
    raw_mkt = mkt_excess - rf    
  ) %>%
  select(permno, month, raw_ret, raw_mkt) %>%
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

#creat a variable named portfolio
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

#use inner_join to find matching permno
matched_results <- beta_results_only2 %>%
  inner_join(beta_results_only, by = "permno")
#in this file, beta.x is from initial estimation period, beta.y is from portfolio building period. 

#see how many permno match
matched_count <- n_distinct(matched_results$permno)

#recalculate the porfolio beta and idiosyncratic risk during initial estimation period
beta_means_by_portfolio <- matched_results %>%
  group_by(portfolio) %>%              
  summarise(mean_beta = mean(beta.x, na.rm = TRUE),
            mean_idsr = mean(idsr, na.rm = TRUE))

#calculate the portfolio return
#find the same permno in capm_data2 and matched_results
matched_results <- capm_data2 %>%
  inner_join(matched_results, by = "permno")

average_by_portfolio_month <- matched_results %>%
  group_by(portfolio, month) %>%   #group by portfolio and month
  summarise(
    avg_ret = mean(raw_ret, na.rm = TRUE), 
    avg_mkt = mean(raw_mkt, na.rm = TRUE) 
  )

#linear regression calculates the R^2 between avg_ret and avg_mkt
r_squared_by_portfolio <- average_by_portfolio_month %>%
  group_by(portfolio) %>% 
  do({
    model <- lm(avg_ret ~ avg_mkt, data = .)
    r_squared <- summary(model)$r.squared  
    data.frame(r_squared = r_squared) 
  }) %>%
  ungroup() 

#calculate the standard deviation of avg_ret by portfolio classification
stddev_by_portfolio <- average_by_portfolio_month %>%
  group_by(portfolio) %>% 
  summarise(
    stddev_avg_ret = sd(avg_ret, na.rm = TRUE)
  ) %>%
  ungroup() 

#creat a new file named table2 to merge together
table2 <- stddev_by_portfolio %>%
  inner_join(r_squared_by_portfolio, by = "portfolio") %>%
  inner_join(beta_means_by_portfolio, by = "portfolio") 
#finished by Kristina 



