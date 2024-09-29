#the following codes are for MAF900 assignment 3 in T3 2024. 
#start from kristina
install.packages("broom")
library(broom)
library(RPostgres)
library(tidyverse)
library(RSQLite)

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

#combining the data used in this study to capm_data
capm_data <- ff_3factors_mon |> mutate(month = floor_date(date, "month")) |> 
  inner_join(
    crsp_monthly |> mutate(month = floor_date(date, "month")) |> 
      select (permno, month, ret),
    by = c('month')
  ) |> mutate (ret_excess = (ret*100 - rf)) |> 
  select (permno, month, ret_excess, mkt_excess) |> 
  arrange(permno, month) |> 
  drop_na(ret_excess, mkt_excess)
#finished by kristina 

#Stephanie Start
#Portfolio formation 
#Calculate BETA for period 1926-1929
capm_data1 <- capm_data %>%
  filter(month >= as.Date("1926-01-01") & month <= as.Date("1929-12-31"))

# Ensure that 'mkt_excess' is in percentage form
#capm_data1$mkt_excess <- as.numeric(capm_data1$mkt_excess)
#capm_data1$ret_excess <- as.numeric(capm_data1$ret_excess)

# Calculate beta for each company
beta_results <- capm_data1 %>%
  group_by(permno) %>%
  do(tidy(lm(ret_excess ~ mkt_excess, data = .)))

print(beta_results)


beta_results_only <- beta_results %>%
  filter(term == "mkt_excess") %>%
  select(permno, beta = estimate, std_error = std.error, t_statistic = statistic, p_value = p.value)

print(beta_results_only) # beta results for each of the company 

sum(is.na(beta_results_only$beta)) #check NA values 

#remove for NA beta
beta_results_only <- beta_results_only %>%
  filter(!is.na(beta))


# rank the betas to divide the companies into 20 portfolios 
# quantile approach
# use quantile-based breaks for 20 portfolios
breaks <- quantile(beta_results_only$beta, probs = seq(0, 1, length.out = 21))

# cut() with these breaks to assign portfolios
beta_results_ranked <- beta_results_only %>%
  mutate(portfolio = cut(beta, breaks = breaks, labels = FALSE, include.lowest = TRUE))

portfolio_distribution <- beta_results_ranked %>%
  group_by(portfolio) %>%
  summarise(count = n(), .groups = 'drop')

print(portfolio_distribution)
#from the result, each portfolio has 38 or 39 stocks 











#create the summary (data) for each of the portfolio 
portfolio_summary <- beta_results_ranked %>%
  group_by(portfolio) %>%
  summarise(
    avg_beta = mean(beta),
    std_dev_beta = sd(beta),
    count = n() # count number of stocks in each portfolio 
  )
print(portfolio_summary)

# visualise to help analysis 
# to see the distribution of betas across portfolios 
ggplot(beta_results_ranked, aes(x = factor(portfolio), y = beta, fill = factor(portfolio))) +
  geom_boxplot() +
  labs(title = "Distribution of Betas Across Portfolios",
       x = "Portfolio",
       y = "Beta") +
  theme_minimal()

#note
# 1st period 26-29 
# define the sample within the period 
# use the ret_excess





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

beta_results_ranked <- beta_results_only %>%
  mutate(portfolio = cut(beta, breaks = breaks, labels = FALSE, include.lowest = TRUE))

portfolio_distribution <- beta_results_ranked %>%
  group_by(portfolio) %>%
  summarise(count = n(), .groups = 'drop')

print(portfolio_distribution)

# results based on the paper 
cat("Total securities (N):", N, "\n")
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







