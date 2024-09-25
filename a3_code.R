#the following codes are for MAF900 assignment 3 in T3 2024. 
#start from kristina
library(RPostgres)
library(tidyverse)
library(RSQLite)

#connect to our wrds
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
capm_data <- ff_3fact_mon |> mutate(month = floor_date(date, "month")) |> 
  inner_join(
    crsp_monthly |> mutate(month = floor_date(date, "month")) |> 
      select (permno, month, ret),
    by = c('month')
  ) |> mutate (ret_excess = (ret*100 - rf)) |> 
  select (permno, month, ret_excess, mkt_excess) |> 
  arrange(permno, month) |> 
  drop_na(ret_excess, mkt_excess)
#finished by kristina 



#start by Stephanie 
# Calculate excess returns
capm_data2 <- capm_data %>%
mutate(stock_excess = ret_excess - mkt_excess)



# Assign stocks to deciles based on their excess returns
capm_data2 <- capm_data2 %>%
group_by(permno) %>%
mutate(decile = ntile(stock_excess, 10))
# Calculate average returns by decile
portfolio_returns <- capm_data2 %>%
group_by(month, decile) %>%
summarise(avg_return = mean(stock_excess, na.rm = TRUE),
.groups = 'drop')

if (nrow(data) < min_obs) {
beta <- as.numeric(NA)
} else {
fit <- lm(ret_excess ~ mkt_excess, data = capm_data2)
beta <- as.numeric(coefficients(fit)[2])
}

#Create a time series B
estimate_capm <- function(data, min_obs = 1) {
if (nrow(data) < min_obs) {
beta <- as.numeric(NA)
} else {
fit <- lm(ret_excess ~ mkt_excess, data = capm_data2)
beta <- as.numeric(coefficients(fit)[2])
}
return(beta)
}

   
capm_data3 <- capm_data2 |> filter(permno == '10000') 
#Creating Rolling Windows
slide_period(.x = capm_data3, #input data 
.f = ~.x, # function or formula
.i = capm_data3$month, # index for rolling window
.period = "month", # unit of period 
.before = 5 # use current and past 5 periods
) 
