##Read me before you check the repository. 
#The Aim of This Repository
This project replicates the empirical analysis from Fama and MacBeth's 1973 paper, Risk, Return, and Equilibrium: Empirical Tests. The replication focuses on US stock data from January 1926 to December 2023 and reproduces the results in Tables 1, 2, 3, and 4 of the original paper.
The project is completed as part of the MAF900 unit for T2 2024. The assignment includes data collection, empirical analysis, and report creation using R / RStudio.

#Background of Fama and MacBeth
The 1973 paper by Eugene F. Fama and James D. MacBeth, "Risk, Return, and Equilibrium: Empirical Tests," is pivotal in finance, introducing rigorous empirical methods to test the Capital Asset Pricing Model (CAPM). Their innovative approach, which combined time series and cross-sectional regression analyses to assess the relationship between asset returns and market risk, has significantly influenced asset pricing theory and led to the development of multi-factor models. 

#Members of The Group
The project is conducted by Kristina Li and Stephanie Tio. 

#Replication Logic
We follow the 1973 Paper: 1. Collecting data, filtering data. 2. Using Period 1(1926-1938) to mock the results of Table 2 first, and then create function to generate table2 by updating each year. 3. Integrating all table 2 betas and std_residuals results to one data frame to run the regression of table 3. 4. Generating table 4. 

#Members Responsibility 
In the file named a3_code.R, we specify each member's input, e.g., start by xxx, finish by xxx, improved by xxx. 

For details:
Both members are responsible README.md and Report.doc. 

Additionally, 

Kristina is responsible for the following parts: 
1. Data collection 
2. Data filtering based on data requirements
3. Statistics calculation of β_(p,t-1), s(β_{p,t-1}), r(Rp, Rm)^2, s(Rp) of period 1(1926-1938) mocking in table 2
4. Creating a function to do the rest of periods. 
5. Defining all periods from table 1, creating table 1 and table 2. 
6. Creating a function to collect all beta values got in table 2 until beta_0. 
7. Creating a function to collect all beta values got in table 2 from beta_1 to beta_3, average portfolio return, standard deviation of residuals and organize the betas into a exercisable table for the regression of table3, defining periods of the function. 
8. Creating a function to create panel a and panel d in table 3. 
9. Integrating all panels together to create table 3. 


Stephanie is responsible for the following parts: 
1. Portfolio formation of period 1(1926-1938) to mock
2. Statistics calculation of s(εp), sp,t-1(εi) of period 1 mocking in table 2.
3. Creating a function to do the rest of periods. 
4. Creating a function to collect all beta values got in table 2 until beta_0. 
5. Creating a function to create panel b and panel c in table 3. 


#Files in Github
1. README.md -> This file, introducing this project. 
2. table1.xlsx -> replication results. 
3. table2.xlsx -> replication results. 
4. table3.xlsx -> replication results. 
5. table4.xlsx -> replication results. 
6. Discussion between Kristina and Stephanie -> All discussion between two members during this project period. 
7. a3_code.R -> All codes. 
8. codes provided by Dr Saikat.R -> Dr Saikat's share. 

#Statement of Participation
We promise that each member contributes to the assignment and is satisfied with each other's contributions. Please reach out if you have any questions. 

Member 1
Kristina Li

Member 2
Stephanie Tio

Date: 20241017


#Reference
Fama, E. F., & MacBeth, J. D. (1973). Risk, return, and equilibrium: Empirical tests. Journal of political economy, 81(3), 607-636.
