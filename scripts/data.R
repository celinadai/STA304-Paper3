#### Preamble ####
# Purpose: Prepare and clean the data from U.S. 2021 GSS
# Author: Shengyi Dai, Suofeiya Guo 
# Data: 15 March 2022
# Contact: celina.dai@mail.utoronto.ca,sofia.guo@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the 2021 U.S. GSS data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?


#### Workspace setup ####
# Use R Projects, not setwd().
library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
# Read in the raw data. 
raw_data <- haven::read_dta("inputs/data/2021_stata/gss2021.dta")

####Prepare Data####
# Just keep some variables that may be of interest
names(raw_data)

reduced_data <- 
  raw_data %>% 
  select(sex, 
         age,
         degree,
         income,
         finrela,
         satfin,
         happy,
  )


rm(raw_data)


#### Recode Data ####

#Questions for sex:
# What is your sex when you born?
#Options are from the codebook "GSS 2021 Codebook R1b.pdf"
reduced_data <- 
  reduced_data %>% 
  
  mutate(sex = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female",
  ))%>%
  #Questions for degree:
  # What is your degree?
  #Options are from the codebook "GSS 2021 Codebook R1b.pdf"
  mutate(degree = case_when(
    degree == 0 ~ "Less than high school",
    degree == 1 ~ "Hight school",
    degree == 2 ~ "associate/junior college",
    degree == 3 ~ "Bachelors",
    degree == 4 ~ "Graduate",
  ))%>%
  #Questions for happy:
  #Are you happy everyda?
  #Options are from the codebook "GSS 2021 Codebook R1b.pdf"
  mutate(happy = case_when(
    happy == 1 ~ "Very happy",
    happy == 2 ~ "Pretty happy",
    happy == 3 ~ "Not too happy",
  ))%>%
  
  #Questions for finrela:
  #Compare with American families in general, are you family' income above average, average, or below average?
  #Options are from the codebook "GSS 2021 Codebook R1b.pdf"
  mutate(finrela = case_when(
    finrela == 1 ~ "Far below average",
    finrela == 2 ~ "Below average",
    finrela == 3 ~ "Average",
    finrela == 4 ~ "Above average",
    finrela == 5 ~ "Far above average"
  ))%>%
  #Questions for satfin:
  #Are you satisfied with your income?
  #Options are from the codebook "GSS 2021 Codebook R1b.pdf"
  mutate(satfin = case_when(
    satfin == 1 ~ "Pretty satisfied",
    satfin == 2 ~ "More of less satisfied",
    satfin == 3 ~ "Not satisfied",
  ))%>%
  filter(!is.na(sex), !is.na(degree), !is.na(age), !is.na(income), !is.na(happy),
         !is.na(finrela), !is.na(satfin))%>%
  filter(age >= 18)%>%
  filter(income < 13)%>%
  rename(income_vs_average = finrela,
         satisfaction = satfin)
#### Some relationship between variables ####
#count of income
reduced_data%>%
  ggplot(aes(x=income))+
  geom_histogram(bins=12, color="black", fill="pink")+
  labs(x="Income")
#count of degree
reduced_data%>%
  ggplot(aes(x=degree))+
  geom_bar(color="black", fill="purple")+
  labs(x="degree")
#degree with income filled vs. happiness
reduced_data%>%
  ggplot(aes(x=degree,fill=as.factor(income_vs_average)))+
  geom_bar(binwidth = 0.15)+
  theme(axis.text.x = element_text(color = "grey20", size= 6))
#Income vs. average with satisfaction filled in
reduced_data%>%
  ggplot(aes(fill=satisfaction, x=income_vs_average)) + 
  geom_bar(position = "dodge")+theme_classic()+
  theme(axis.text.x = element_text(color = "grey20", size= 6))

#### Model ####
reduced_new <- reduced_data%>%
  mutate(happy = case_when(
    happy == "Not too happy" ~ "Not too happy",
    happy == "Pretty happy" ~ "Happy",
    happy == "Very happy" ~ "Happy"
  ))
set.seed(1000)
training.samples <- reduced_new$happy %>%
  createDataPartition(p=0.8, list=FALSE)
train.data <- reduced_new[training.samples,]
test.data <- reduced_new[-training.samples,]
model <- glm(as.factor(happy) ~ income, data = train.data, family = binomial)
summary(model)
prob <- model%>%
  predict(test.data, type="response")
predict.classes <- ifelse(prob > 0.5, "Happy", "Not too happy")
mean(predict.classes == test.data$happy)
train.data %>%
  mutate(prob = ifelse(happy == "Happy", 1, 0))%>%
  ggplot(aes(income, prob))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

#### Save ####
write.csv(reduced_data, "outputs/data/cleaned_gss.csv")
