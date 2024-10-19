##Read me before you check the repository. 
#The Aim of This Repository
This project replicates the empirical analysis from Fama and MacBeth's 1973 paper, Risk, Return, and Equilibrium: Empirical Tests. The replication focuses on US stock data from January 1926 to December 2023 and reproduces the results in Tables 1, 2, 3, and 4 of the original paper.
The project is completed as part of the MAF900 unit for T2 2024. The assignment includes data collection, empirical analysis, and report creation using R / RStudio.

#Background of Fama and Macbeth
The 1973 paper by Eugene F. Fama and James D. MacBeth, "Risk, Return, and Equilibrium: Empirical Tests," is pivotal in finance, introducing rigorous empirical methods to test the Capital Asset Pricing Model (CAPM). Their innovative approach, which combined time series and cross-sectional regression analyses to assess the relationship between asset returns and market risk, has significantly influenced asset pricing theory and led to the development of multi-factor models. 

#Members of The Group
The project is conducted by Kristina Li and Stephanie Tio. 

#Replication Logic
We follow the 1973 Paper: 1. Collecting data, filtering data. 2. Use Period 1(1926-1938) to mock the results of Table 2 first, and then create function to generate Table 2 by updating each year. 3. Integrating all table 2 betas and std_residuals results to one data frame to run the regression of table 3. 4. Generating table 4. 

#Members Responsibility 
In the file named a3_code.R, we specify each member's input, e.g., start by xxx, finish by xxx, improved by xxx. 

For details:
Both members are responsible README.md and Report.doc. 

Additionally, 

Kristina is responsible for the following parts: 
1. Data collection 
2. Data filtering based on data requirements
3. Statistics calculation of β_(p,t-1), s(β_{p,t-1}), r(Rp, Rm)^2, s(Rp) of period 1(1926-1938) mocking in table 2
4. Creating a function to do the rest of the periods. 
5. Defining all periods from Table 1, creating table 1 and table 2. 
6. Creating a function to collect all beta values got in Table 2 until beta_0. 
7. Creating a function to collect all beta values got in table 2 from beta_1 to beta_3, average portfolio return, standard deviation of residuals and organize the betas into an exercisable table for the regression of table 3, defining periods of the function. 
8. Create a function to create panel a and panel d in Table 3. 
9. Integrating all panels together to create table 3. 
10. Table 4. 


Stephanie is responsible for the following parts: 
1. Calculate Betas for each stock aligning with Fama's approach 
2. Ranking the Betas to 20 portfolios (Portfolio Formation) and calculate the size for each portfolio
3. Form regression calculating standard deviation of residuals for each portfolio
4. Statistics calculation of s(ε^p), sˉp,t−1(ε^i), s(ε^p)/sˉp,t−1(ε^i) for Table 2
5. Create a Function to automate and calculate for other and the rest of the periods based on Fama's paper
with each portfolio, estimation, and testing periods 
6. Create a function to collect all betas values in Table 2 until beta_0
7. Collect FF 3 factor data and Create Pre-requisites Function Gamma Table 3 
8. Creating a function to create Panel B and Panel C for Table 3


#Files in Github
1. README.md -> Introducing this project's aim, objectives, and details. 
2. table1.xlsx -> Replication results output. 
3. table2.xlsx -> Replication results output.
4. table3.xlsx -> Replication results output.
5. table4.xlsx -> Replication results output. 
6. Discussion between Kristina and Stephanie -> All discussions between two members during this project period. 
7. a3_code.R -> All codes to replicate Table 1, 2, 3, 4, aligning the same approach with the Fama and MacBeth (1973) paper based on the assignment's specifications and requirements. 
8. The codes provided by Dr Saikat.R -> Dr Saikat's share. 
9. Kristina's statement of responsibility -> Responsibility Statement
10. Stephanie's statement of responsibility -> Responsibility Statement

#Statement of Participation
We promise and ensure that each member has contributed to the project and is satisfied with each other's contributions. 
Please feel free to reach out if you have any questions or clarifications.

Member 1
Kristina Li

Member 2
Stephanie Tio

Date: 20241017


#Reference
Fama, E. F., & MacBeth, J. D. (1973). Risk, return, and equilibrium: Empirical tests. Journal of political economy, 81(3), 607-636.
