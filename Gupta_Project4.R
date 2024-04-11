#Name : Sanchi Gupta
#Date : 15 October 2023


#Clean Canvas----
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notation for entire R session

library(pacman)
library(lubridate)
library(janitor)
library(tidyverse)
library(dplyr)
library(ggplot2)



#Project 4----
job_applications<- read.csv("Job Applicants.csv")


##Cleaning & Exploring----
#Cleaning col names
job_applications <-clean_names(job_applications)

#extracting useful cols
applications<- subset(job_applications,select= 
                        -c(x,accessibility,employment, mental_health, have_worked_with))

#viewing data type
str(applications)

#descriptive statistics 
summary(applications)

#omitting N/A cols
applications<- na.omit(applications)



##Visualizations----
###Visualisation 1----
total_applicants<- data.frame(applications%>%
                                group_by(gender)%>%
                                summarise(total =n()))
total_applicants<- mutate(total_applicants, percentage=round(100*total/sum(total), 1))


ggplot(total_applicants, aes(x = "", y = percentage, fill = gender)) +
  geom_col()+ 
  ggtitle("Applicants by Gender")+
  geom_text(aes(label= percentage),   position = position_stack(vjust = 0.09)) +
  coord_polar(theta = "y")

###Q1----
#Which country has the highest job applicants?

#summarized the dataset for each country
Ques1<- data.frame(applications%>%
                     group_by(country)%>%
                     summarise(gender =n()))

#renamed col 'gender' to 'applicants'
Ques1 <-Ques1 %>% 
  rename(applicants= gender)

#arranged the applicants in desc order
Ques1<- arrange(Ques1,desc(applicants))

#sliced the top 10 countries
Ques1_f <- slice(Ques1,1:10)
 
#plotted horizontal barograph for the above question
ggplot(Ques1_f, aes(x = applicants, y= reorder(country,applicants), fill= country)) +
  geom_bar(stat="identity") +
  labs(title = "Top Job Applicants by Country", x = "Number of Applicants", y = "Country")+
  scale_x_continuous(
    breaks = seq(0,15000, by=2000),
    limits = c(0,15000))

###Q2----
#Is there a correlation between years of professional coding experience and salary?

#because the dataset was large therefore we made a sample for 500 observations
sample1<- sample_n(applications,500)

#finding correlation for professional coding and salary
correlation <- cor(sample1$years_code_pro, sample1$previous_salary)

#plotted a scatterplot for years of professional coding and salary with correlation
ggplot(sample1, aes(x = years_code_pro, y = previous_salary)) +
  geom_point() +
  labs(title = "Relationship between Years of Professional Coding Experience and Salary",
  x= "Years of Professional Coding Experience",
  y= "Salary") +
  scale_x_continuous(
    breaks = seq(0,50, by=5),
    limits = c(0,50))+
  theme_gray()+
  geom_text(x = 40, y = 190000, label = paste("CORRELATION: ", round(correlation, 3)))



###Q3----
#What is the average and median salary of individuals in the dataset?

salary_summary<- data.frame(
  applications%>%
    summarise(
      average_salary= mean(applications$previous_salary),
      median_salary= median(applications$previous_salary),
  sd_salary= sd(applications$previous_salary)))

###Q4----
#Is there a correlation between ‘YearsCode’ and ‘YearsCodePro’ for each country?

#summarized dataset by each counrty for ‘YearsCode’ and ‘YearsCodePro’ using group by
Ques4<- data.frame(applications%>%
                     group_by(country)%>%
                     summarise(avg_years_code_pro =round(mean(years_code_pro),3),
                               avg_years_code= round(mean(years_code),3)))
                               

#to create the linear graphs for each country
ggplot(Ques4, aes(x = avg_years_code, y = avg_years_code_pro, color="countries")) +
  geom_point() +
  labs(title = "Correlation between 'YearsCode' and 'YearsCodePro' by Country", 
       x = "Years of Coding Experience", 
       y = "Years of Professional Coding Experience") +
  geom_smooth(method = "lm", se = TRUE, color="black", size= 0.40)

#to answer follow-up question that 'Which countries have the most experienced coders?'

top_countries_procoders<- arrange(Ques4,desc(avg_years_code_pro))
top_countries_procoders<- slice (top_countries_procoders,1:10)
top_countries_procoders<- subset(top_countries_procoders,select = -c(avg_years_code))

