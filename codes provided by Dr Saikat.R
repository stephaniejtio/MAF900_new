
########################################################################################
## Fama-Macbeth (1973) replication partial Codes ## 
########################################################################################
# Following is a partial code for replicating Fama-Macbeth (1973). 
# The code below collects data between 1926 to 1968 and runs portfolio formation, initial 
# estimation an testing for period 1 of Table 1.
# The codes below uses some of the coding tricks we have learned in the class, Students
# may get some ideas from these codes to do carryout their own replication project.
########################################################################################

library(RPostgres)
library(tidyverse)
library(RSQLite)
library(slider)
library(furrr)
library(purrr)
library(modelsummary)
library(tseries)


wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='saikat')



### Collect CRSP monthly stock return data ####

msf_db <- tbl(wrds, sql("select * from crsp.msf"))

start_date <- ymd("1926-01-01")
end_date <- ymd("1968-12-31")

FM_ret <- msf_db |>
  filter(date >= start_date & date <= end_date) |>
  select(
    permno, # Security identifier
    date, # Date of the observation
    ret # Return
  ) |> collect()


### Stock and Excchage Identifier ###
msenames_db <- tbl(wrds, sql("select * from crsp.msenames"))

fm_stockids <- msenames_db |>
  select (permno, primexch)|> collect()|>unique()|> filter(primexch == 'N')

fm_data <- FM_ret |> inner_join(
  fm_stockids |>
    select(permno, primexch), by = c("permno"))


#### Connect to local database ###

MAF900_data <- dbConnect(
  SQLite(),
  "data/MAF900_data.sqlite",
  extended_types = TRUE)


dbWriteTable(MAF900_data,
             "fm_ret",
             value = fm_data,
             overwrite = TRUE)

###  Create Fisher Index - for Rm ##
Fs_Idx <- fm_data|> group_by(date)|> summarise(fsi_rm = mean(ret, na.rm = TRUE))

## Combine Stock return and market return i.e. fisher index return ##

fm_data <- fm_data |>select(permno,date,ret)|>inner_join(Fs_Idx, by = c("date"))|> arrange(permno,date)

fm_data <- tbl(MAF900_data, "fm_ret")|> collect()


port_form_bdate <- ymd("1926-01-31")
port_form_edate <- ymd("1929-12-31")


estimate_beta <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    beta <- as.numeric(NA)
  } else {
    fit <- lm(ret ~ mkt, data = data)
    beta <- as.numeric(coefficients(fit)[2])
  }
  return(beta)
}

## Estimate portfolio formation beta and rank them ###
pform_beta <- fm_data|> mutate(mkt=fsi_rm)|>select(permno,date,ret,mkt)|>
  filter(between(date,port_form_bdate,port_form_edate))|> 
  nest(data = c(date, ret, mkt))|>
  mutate(beta = map(
    data,
    ~ estimate_beta(., min_obs = 47)
  )) |> unnest(c(beta)) |>
  select(permno,beta) |>
  drop_na()|> mutate(rank = ((dense_rank(beta))))|> arrange(rank)


### Allocate stocks to 20 portfolios based on beta ranks  
################################################################
 no_stocks <- nrow(pform_beta)
 nm <- as.integer(no_stocks%/%20)
 k <- as.integer(round(((20*(no_stocks/20-nm)))))
 
 
 if((no_stocks%%2) == 0)
   {
   pform_beta <- pform_beta|>
     mutate(port_no = case_when(
          rank <= (nm + k/2) ~ 1,
          (rank > (nm + k/2) & rank <= (no_stocks- nm - k/2)) ~  if_else((rank-k/2)%%nm == 0,as.integer((rank-k/2)/nm),(1+as.integer((rank-k/2)/nm))), #(1+as.integer((rank-k/2)/nm)),    
          rank > ( no_stocks- nm - k/2) ~ 20,
         .default = NA
       ))
   }
 
  if( no_stocks%%2!= 0)  { 
    print("Here")
    pform_beta <- pform_beta|>
      mutate(port_no = case_when(
      rank <= (nm + (k-1)/2) ~ 1,
      (rank > (nm + (k-1)/2) & rank <= (no_stocks- nm - (k-1)/2 - 1)) ~ if_else((rank-(k-1)/2)%%nm == 0,as.integer((rank-(k-1)/2)/nm),(1+as.integer((rank-(k-1)/2)/nm))),#(1+as.integer(rank/nm)),    
      rank > ( no_stocks- nm - (k-1)/2 - 1) ~ 20
    ))
  }
 ##################################################################################
 
 ## Check number of stocks in each portfolio 
# cnt <- pform_beta |> group_by(port_no)|> summarise(n())
 

    
 init_port_bdate <- ymd("1930-01-31")
 init_port_edate <- ymd("1934-12-31")
 
    
 port_bdate <- ymd("1930-01-31")
 port_edate <- ymd("1938-12-31")
 
 test_bdate <- ymd("1935-01-31")
 test_edate <- ymd("1938-12-31")
 
 
 ### functions for Beta & Idiosyncratic Risk - estimation ###
 
 estimate_bs <- function(data, min_obs = 1) {
   chk_data <- data|> drop_na()
   if (nrow(chk_data) < min_obs) {
     beta <- as.numeric(NA)
     s <- as.numeric(NA)
   } else {
     fit <- lm(ret ~ mkt, data = data)
     beta <- as.numeric(coefficients(fit)[2])
     s <- sd(resid(fit))
   }
   return(tibble(beta,s))
 }
 
 ### function for Beta & Idiosyncratic Risk - rolling estimation ###
 
 roll_bs_estimation <- function(data, yrs, min_obs) {
    data <- data |> 
      arrange(date)
   
   betas <- slide_period(
     .x = data,
     .i = data$date,
     .period = "year",
     .f = ~ estimate_bs(., min_obs),
     .before = yrs - 1,
     .complete = FALSE
   )
   
   dt1 <- data |> mutate(yr = year(date))|>select(yr)|> distinct()  
   return(tibble(
     date = dt1$yr,
     betas) |> unnest(betas) )
 }
 
    
 ## Estimate individual stock betas and s starting from 1934; then update yrly;
 ## for calculating portfolio betas and portfolio idiosyncratic risks(s).
 
 p_beta <- fm_data|> mutate(mkt=fsi_rm)|>select(permno,date,ret,mkt)|>
   filter(between(date,port_bdate,port_edate))|>
   nest(data = c(date, ret, mkt))|>
   mutate(beta = map(data,
     ~  roll_bs_estimation(., yrs = 5, min_obs = 60)
   ))|>select(permno,beta)|> unnest(beta)|>filter(date>="1934")|>drop_na()
 
 ## join "pform_beta" and "p_beta" to add portfolio no's("port_no") to "p_beta"
 ## and create port_risks
 
 ### Prepare data for monthly portfolio return and risk calculations
 
 for_port_risks <- p_beta |> left_join(pform_beta|>select(permno,port_no), by=c("permno"))|> 
   select(permno,date,beta,s,port_no)|>mutate(p_yr=date)
 
 for_port_ret <- fm_data|>select(permno,date,ret)|>
   filter(between(date,test_bdate,test_edate))|>
   mutate(yr = year(date), p_yr = yr-1)|>drop_na()
 
 ## Combine data for monthly portfolio return and risk calculations
 for_port_ret_risks <- for_port_ret|> left_join(for_port_risks|> 
                        select(-c(date)),by=c("permno","p_yr"))|> drop_na()|> 
                        mutate(b_sq = beta^2)
                                        

 ## Calculate monthly portfolio return and risks (i.e.portfolio beta, beta sq, and portfolio n)
 port_ret_risks <-  for_port_ret_risks |> group_by(port_no,date)|>
   summarise(port_ret = mean(ret, na.rm= TRUE), port_beta = mean(beta, na.rm = TRUE),
             port_beta2 = mean(b_sq, na.rm = TRUE),
             port_s = mean(s, na.rm=TRUE), no_stks = n())
 

  ### functions for monthly cross section regs ###
 ### g's are the gamma coefficients, g01, g02 refer to gamma 0 and gamma 1 for model 1(m1) i.e. 
 ## monthly cross section regression model with beta only, Table 3 reports coefficients of 3
 # other models (m2,m3,m4) - model 4 (m4) is the full model with beta, beta square and idiosyncratic risk.
 estimate_cs <- function(data, min_obs = 1) {
   chk_data <- data|> drop_na()
   if (nrow(chk_data) < min_obs) {
     g01 <- as.numeric(NA)
     g11 <- as.numeric(NA)
     g02 <- as.numeric(NA)
     g12 <- as.numeric(NA)
     g22 <- as.numeric(NA)
     g03 <- as.numeric(NA)
     g13 <- as.numeric(NA)
     g33 <- as.numeric(NA)
     g04 <- as.numeric(NA)
     g14 <- as.numeric(NA)
     g24 <- as.numeric(NA)
     g34 <- as.numeric(NA)
   } else {
     m1 <- lm(port_ret ~ port_beta, data = data)
     m2<-  lm(port_ret ~ port_beta + port_beta2, data = data)
     m3 <- lm(port_ret ~ port_beta + port_s, data = data)
     m4 <- lm(port_ret ~ port_beta + port_beta2 + port_s, data = data)
     
     g01 <- as.numeric(coefficients(m1)[1])
     g11 <- as.numeric(coefficients(m1)[2])
     g02 <- as.numeric(coefficients(m2)[1])
     g12 <- as.numeric(coefficients(m2)[2])
     g22 <- as.numeric(coefficients(m2)[3])
     g03 <- as.numeric(coefficients(m3)[1])
     g13 <- as.numeric(coefficients(m3)[2])
     g33 <- as.numeric(coefficients(m3)[3])
     g04 <- as.numeric(coefficients(m4)[1])
     g14 <- as.numeric(coefficients(m4)[2])
     g24 <- as.numeric(coefficients(m4)[3])
     g34 <- as.numeric(coefficients(m4)[4])
   }
   return(tibble(g01,g11,g02,g12,g22,g03,g13,g33,g04,g14,g24,g34))
 }
 
 cs_estimates <- port_ret_risks |> group_by(date)|>
   mutate(estimate_cs(pick(everything()))) |> ungroup() 
 
 ### Table 3 average gammas, t values and other stats ##### 
 cs_summary <-  cs_estimates |> summarise(Avg_g01=mean(g01, na.rm=TRUE),Avg_g11=mean(g11, na.rm=TRUE),
                                          Avg_g02=mean(g02, na.rm=TRUE),Avg_g12=mean(g12, na.rm=TRUE), 
                                          Avg_g22=mean(g22, na.rm=TRUE),Avg_g03=mean(g03, na.rm=TRUE),
                                          Avg_g13=mean(g13, na.rm=TRUE),Avg_g33=mean(g33, na.rm=TRUE),
                                          Avg_g04=mean(g04, na.rm=TRUE),Avg_g14=mean(g14, na.rm=TRUE),
                                          Avg_g24=mean(g24, na.rm=TRUE),Avg_g34=mean(g34, na.rm=TRUE),
                                          noob = n(), sdev_g01 = sd(g01,na.rm=TRUE), sdev_g11 = sd(g11,na.rm=TRUE),
                                          sdev_g02 = sd(g02,na.rm=TRUE), sdev_g12 = sd(g12,na.rm=TRUE),
                                          sdev_g22 = sd(g22,na.rm=TRUE), sdev_g03 = sd(g03,na.rm=TRUE),
                                          sdev_g13 = sd(g13,na.rm=TRUE), sdev_g33 = sd(g33,na.rm=TRUE),
                                          sdev_g04 = sd(g04,na.rm=TRUE), sdev_g14 = sd(g14,na.rm=TRUE),
                                          sdev_g24 = sd(g24,na.rm=TRUE), sdev_g34 = sd(g34,na.rm=TRUE)) |> 
               mutate(t_g01 = Avg_g01/(sdev_g01/sqrt(noob)),t_g11 = Avg_g11/(sdev_g11/sqrt(noob)),
                      t_g02 = Avg_g02/(sdev_g02/sqrt(noob)),t_g12 = Avg_g12/(sdev_g12/sqrt(noob)),
                      t_g22 = Avg_g22/(sdev_g22/sqrt(noob)),t_g03 = Avg_g03/(sdev_g03/sqrt(noob)),
                      t_g13 = Avg_g13/(sdev_g13/sqrt(noob)),t_g33 = Avg_g33/(sdev_g33/sqrt(noob)),
                      t_g04 = Avg_g04/(sdev_g04/sqrt(noob)),t_g14 = Avg_g14/(sdev_g14/sqrt(noob)),
                      t_g24 = Avg_g24/(sdev_g24/sqrt(noob)),t_g34 = Avg_g34/(sdev_g34/sqrt(noob)))
 

 ### Calculate Auto Correlations of the gammas(i.e.monthly regression coefficients) for Table 3 ###
  
 acf_1 <- data.frame(acf = double())
  g_acfs<- data.frame()
  cn <- names(cs_estimates)
  for(i in 8:ncol(cs_estimates)) {
   var <- paste("acf",cn[i],sep="_")
  acfs<- acf(cs_estimates[i], lag.max = 1,type = c("partial"),plot = FALSE)
  acf_1 <- data.frame(acfs$acf)
  names(acf_1) <- var
  if(i==8){
    g_acfs<- acf_1 
  }else{
    g_acfs<- cbind(g_acfs,acf_1)
    }
  }
  
  ##################################
  
  

  

 