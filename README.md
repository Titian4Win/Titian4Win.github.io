# Titian4Win.github.io
# This paper finds that banning single-family zoning laws costs $33,272 smaller than for Minneapolis than Saint Paul. Zoning laws increase housing values. When zoning laws are taken away, there's a drop in housing prices because there's an increase in the supply of available units. The following paper examines the effect of banning single-family zoning laws within Minneapolis. In December 2018, Minneapolis approved a housing plan, Minneapolis 2040, where 70% of the city's land will be banned from single-family zoning laws. The town approved building non-single-family homes in January 2020. I compared Minneapolis to a neighboring city, Saint Paul, from 2016-2021.
setwd('/Users/andrewscpu/Desktop/here/Minneapolis Saint Paul')
#install packages
install.packages("languageserver")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("tidyverse")
#install.packages("modelsummary")
#install.packages("fixest")
#install.packages("foreign")
#install.packages("lubridate")
#install.packages("AER")
#install.packages("stargazer")
#install.packages("zoo")
#install.packages("ggplot2")
#install.packages("readr")
#install.packages("data.table")
#install.packages("skimr")
#install.packages("scales")
#install.packages("Ecdat")
#library(Ecdat)
library(skimr)
library(scales)
library(ggplot2)
library(modelsummary)
library(AER)
library(stargazer)
library(fixest)
library(dplyr)
library(readr)
library(tidyverse)
library(foreign)
library(lubridate)
library(zoo)
library(readr)
library(data.table)
Minneapolis_Saint_Paul <- fread("/Users/andrewscpu/Desktop/here/Minneapolis Saint Paul/Minneapolis_Saint_Paul.csv")
str(Minneapolis_Saint_Paul)
Saint_Paul = filter (Minneapolis_Saint_Paul, cty_twnshp_nme == "St. Paul")
Minneapolis = filter(Minneapolis_Saint_Paul,cty_twnshp_nme == "Minneapolis")
#Now sum data for each quarter
Minneapolis$dates <- as.yearqtr(Minneapolis$dates, format = "%Y-Q%") 
Saint_Paul$dates <- as.yearqtr(Saint_Paul$dates, format = "%Y-Q%")
Minneapolis$tot_purch_amt = pmin(Minneapolis$tot_purch_amt, 1550045)
Saint_Paul$tot_purch_amt = pmin(Saint_Paul$tot_purch_amt, 1550045)
aggregated_data_M=aggregate(Minneapolis$tot_purch_amt,by=list(Minneapolis$dates),FUN= mean)
aggregated_data_SP=aggregate(Saint_Paul$tot_purch_amt,by=list(Saint_Paul$dates), FUN= mean)
aggregated_data_SPM =aggregate(Minneapolis_Saint_Paul$tot_purch_amt,by=list(Minneapolis_Saint_Paul$dates), FUN= mean)
#output per quarter
head(aggregated_data_M)
head(aggregated_data_SP)
colnames(aggregated_data_M)[2] ="M"
colnames(aggregated_data_SP)[2] ="SP"
MSP <- merge(aggregated_data_M,aggregated_data_SP,by="Group.1") 
p = ggplot(aggregated_data_M, mapping = aes(x=Group.1, y= M), colour = genus) + 
  geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+geom_line(linetype = "dashed")+ geom_line(color="red")+ labs(title = "Minneapolis", x = "Year", y = "Mean Transactions Price")+scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))


p = p + geom_point()
plot(p)
p = ggplot(aggregated_data_SP, aes(x=Group.1, y= SP), colour = genus) + 
  geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+geom_line(linetype = "dashed")+ geom_line(color="blue")+ labs(title = "Saint Paul", x = "Year", y = "Mean Transactions Price")+scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))
p = p + geom_point()
plot(p)
#skim(aggregated_data_SPM)
skim(aggregated_data_M)
skim(aggregated_data_SP)
Book1 <- fread("/Users/andrewscpu/Desktop/here/Minneapolis Saint Paul/Book1.csv")
# Treatment variable
Book1 <- Book1 %>%
    mutate(Treated = cty_twnshp_name == 'M' & 
           quarter %in% c('2019 Q1', '2019 Q2', '2019 Q3', '2019 Q4','2020 Q1','2020 Q2','2020 Q2','2020 Q3', '2020 Q4', '2021 Q1', '2021 Q2', '2021 Q3', '2021 Q4'))

# estimate the regression with fixed effcets by state and quarter
res_1 = clfe <- feols(tot_purch_amt ~ Treated | cty_twnshp_name + quarter, Book1)
msummary(clfe, stars = c('*' = .1, '**' = .05, '***' = .01))
#Use only Pretreatment data
filter(Book1, quarter >= quarter %in% c("2015 Q2"), quarter %in% c("2017 Q2"))
Book1 <- Book1 %>% 
  mutate(FakeTreat1 = cty_twnshp_name == "M" &
           quarter %in% c ('2018 Q2', '2018 Q3'),
         FakeTreat2 = cty_twnshp_name == "M" &
           quarter == '2018 Q3')
# Run the same model we did before but with out fake treatment
clfel <- feols(tot_purch_amt ~ FakeTreat1,
               data = Book1)
clfe2 <- feols(tot_purch_amt ~ FakeTreat2,
               data = Book1)
msummary(list(clfel,clfe2), stars = c('*' = .1, '**' = .05, '***' =.01))
#treatment
Book1 <- Book1 %>% mutate(M = cty_twnshp_name == 'M')

#reference period 
clfe <- feols(tot_purch_amt ~ Treated,
           data = Book1)
# Interact quarter with being in the treated group using
# the fixest i() function, which also lets us specify
clfe <- feols(tot_purch_amt ~ i(M, quarter, ref = quarter %in% c ("2018 Q3")), data = Book1)
#clfe <- feols(tot_purch_amt ~ i(M, quarter, ref = quarter %in% c ("2018 Q3")) |cty_twnshp_name + quarter, data = Book1 )
coefplot(clfe)
```
