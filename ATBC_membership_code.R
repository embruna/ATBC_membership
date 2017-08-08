# R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING THE DATASETS USED IN ANALYSIS OF ATBC MEMBERSHIP AND MEETING ATTENDANCE TRENDS
# Package and R versions:

# stringdist = 0.9.4.4
# RecordLinkage = 0.4.10
# vegan = 2.4.2
# tidyverse = 1.0
# MuMIn = 1.15.6
# nlme = 3.1.128
# WDI = 2.4
# R version = 3.3.1

library(tidyverse)
library(RecordLinkage)
library(stringdist)
library(vegan)
library(WDI)
library(nlme)
library(MuMIn)

# Clear out everything from the environment 
rm(list=ls())

##############################################################
##############################################################
#
# SET UP THE TEMPORAL FRAME OF YOUR STUDY
# This avoids mistakes, esnures consistent analyses and figures
#
FirstYear=2013
LastYear=2016
#
###############################################################
##############################################################





##############################################################
##############################################################
#
# DATA UPLOAD & ORGANIZATION
# load the individual CSV files and save them as dataframes
#
##############################################################
##############################################################

##############################################################
# DATA CLEANUP & ORGANIZATION: Membership
##############################################################

member16<-read.csv("./Data/BTP-ATBC_MemberReport_9-21-16.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE, stringsAsFactors = FALSE, strip.white=TRUE )
member15<-read.csv("./Data/BTP-ATBC_MemberReport_9-21-15.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE, stringsAsFactors = FALSE, strip.white=TRUE )
member14<-read.csv("./Data/BTP-ATBC_MemberReport_8-21-14.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE, stringsAsFactors = FALSE, strip.white=TRUE )
member13<-read.csv("./Data/BTP-ATBC_MemberReport_8-20-13.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE, stringsAsFactors = FALSE, strip.white=TRUE )
# Note use of strip.white=TRUE to remove leading and trailing white spaces 


colnames(member13)
colnames(member14)
colnames(member15)
colnames(member16)
# One of the column names is different
member15<-rename(member15, SubReference=SubsReference) #rename the columns

#Bind the membership data 
members<-rbind(member13,member14,member15,member16) 
write.csv(members, file="./Data/members.csv", row.names = T) #export it as a csv file

#Don't Need the original files or Messy ChoData cluttering up the Env't so lets delete
rm(member13,member14,member15,member16)


# There are many different categories, so tried to standardize them
members$MembershipCategory<-as.factor(members$MembershipCategory)
levels(members$MembershipCategory)

levels(members$MembershipCategory)[levels(members$MembershipCategory)=="2016 Award Winners"] <- "Award Winner"
levels(members$MembershipCategory)[levels(members$MembershipCategory)=="2015 Award Winners"] <- "Award Winner"
levels(members$MembershipCategory)[levels(members$MembershipCategory)=="Student Member 3 Years Online"] <- "Student Member"
levels(members$MembershipCategory)[levels(members$MembershipCategory)=="Student Member Online"] <- "Student Member"
levels(members$MembershipCategory)[levels(members$MembershipCategory)=="Student Member Print & Online"] <- "Student Member"
levels(members$MembershipCategory)[levels(members$MembershipCategory)=="Patron/Fellow Online Only"] <- "Patron-Fellow"
levels(members$MembershipCategory)[levels(members$MembershipCategory)=="Patron/Fellow Member Print&Online"] <- "Patron-Fellow"
levels(members$MembershipCategory)[levels(members$MembershipCategory)=="Editorial Member Online"] <- "Editor"
levels(members$MembershipCategory)[levels(members$MembershipCategory)=="Member 3 Years Online"] <- "Regular Member"
levels(members$MembershipCategory)[levels(members$MembershipCategory)=="Member Online"] <- "Regular Member"
levels(members$MembershipCategory)[levels(members$MembershipCategory)=="Member Print & Online"] <- "Regular Member"
levels(members$MembershipCategory)[levels(members$MembershipCategory)=="Free Student Online Only 1 Year"] <- "Free Student Member - 1yr"
levels(members$MembershipCategory)[levels(members$MembershipCategory)=="FREE Student 3yrs Online to Biotropica Only"] <- "Free Student Member - 3yr"
levels(members$MembershipCategory)[levels(members$MembershipCategory)=="FREE Student 3yrs Online Access to Biotropica only"] <- "Free Student Member - 3yr"

# NEED TO DO
# Correct names (join 1st, middle, last)
# How many unique ID numbers
# How many people in all 4 years, 3 years, 2 years, 1 year

# NUMBER OF UNIQUE MEMBERS DURING TIME FRAME UNDER CONSIDERATION
TOTALMEMBERS<-members %>% summarize(Total = n_distinct(SubReference))

# NUMBER OF MEMBERS PER YEAR
members$SubReference<-as.factor(members$SubReference)
Members_per_Year<-members %>% group_by(Year) %>% summarize(Total = n_distinct(SubReference))

# NUMBER OF MEMBERS PER YEAR, BY CATEGORY
Members_per_CatPerYr<-members %>% group_by(MembershipCategory, Year) %>% summarize(Total = n_distinct(SubReference))

# Number of years each person was a member
# there are 2 that are listed 2x in one year, so group by year as well
YrsOfMembership<-members %>% group_by(SubReference) %>% count(SubReference)
YrsOfMembership$n<-as.factor(YrsOfMembership$n)

str(YrsOfMembership)



YrsOfMembershipSummary<-YrsOfMembership %>%  group_by(n) %>% count(n) %>% mutate(freq = nn / sum(nn))

