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
LastYear=2017
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
member17<-read.csv("./Data/BTP-ATBC_MemberReport_6-21-17.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE, stringsAsFactors = FALSE, strip.white=TRUE )
member16<-read.csv("./Data/BTP-ATBC_MemberReport_9-21-16.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE, stringsAsFactors = FALSE, strip.white=TRUE )
member15<-read.csv("./Data/BTP-ATBC_MemberReport_9-21-15.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE, stringsAsFactors = FALSE, strip.white=TRUE )
member14<-read.csv("./Data/BTP-ATBC_MemberReport_8-21-14.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE, stringsAsFactors = FALSE, strip.white=TRUE )
member13<-read.csv("./Data/BTP-ATBC_MemberReport_8-20-13.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE, stringsAsFactors = FALSE, strip.white=TRUE )
# Note use of strip.white=TRUE to remove leading and trailing white spaces 


colnames(member13)
colnames(member14)
colnames(member15)
colnames(member16)
colnames(member17)
# One of the column names is different
member15<-rename(member15, SubReference=SubsReference) #rename the columns

#Bind the membership data 
members<-rbind(member13,member14,member15,member16,member17) 
write.csv(members, file="./Data/members.csv", row.names = T) #export it as a csv file

#Don't Need the original files or Messy ChoData cluttering up the Env't so lets delete
rm(member13,member14,member15,member16,member17)


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
TOTALMEMBERS

# NUMBER OF MEMBERS PER YEAR
members$SubReference<-as.factor(members$SubReference)
Members_per_Year<-members %>% group_by(Year) %>% summarize(Total = n_distinct(SubReference))
Members_per_Year

# NUMBER OF MEMBERS PER YEAR, BY CATEGORY
Members_per_CatPerYr<-members %>% group_by(MembershipCategory, Year) %>% summarize(Total = n_distinct(SubReference))
Members_per_CatPerYr

# Number of years each person was a member
# there are 2 that are listed 2x in one year, so group by year as well
YrsOfMembership<-members %>% group_by(SubReference) %>% count(SubReference)
YrsOfMembership$n<-as.factor(YrsOfMembership$n)

five_yr_members<-which(YrsOfMembership$n==5)
core_members<-semi_join(members,YrsOfMembership[five_yr_members,],by="SubReference")

str(YrsOfMembership)
YrsOfMembershipSummary<-YrsOfMembership %>%  group_by(n) %>% count(n) %>% mutate(freq = nn / sum(nn))

# YrsOfMembership<-members %>% group_by(SubReference) %>% arrange(SubReference)

# Fate of those that were members in 2013 
Fate2013<- members %>% select (SubReference, Year) %>% group_by(SubReference, Year) %>% 
  arrange (SubReference, Year) 
Fate2013$Member<-(as.factor("Y"))
Fate2013$Year<-as.factor(Fate2013$Year)
Fate2013<-as.data.frame(Fate2013)
Fate2013 <-Fate2013 %>% group_by(SubReference, Year) %>% filter(row_number(Year) == 1)

Fate2013<-Fate2013 %>% spread(Year,Member)

names(Fate2013)[2] <- "YR2013"
names(Fate2013)[3] <- "YR2014"
names(Fate2013)[4] <- "YR2015"
names(Fate2013)[5] <- "YR2016"
names(Fate2013)[6] <- "YR2017"
Fate2013<-as.data.frame(Fate2013)
Fate2013$SUM<-(ncol(Fate2013)-1)
Fate2013$SUM<-Fate2013$SUM-rowSums(is.na(Fate2013))

One_Year<-Fate2013 %>% filter(YR2013 == "Y" & SUM == 1) %>% summarize(n_distinct(SubReference))
Two_Year<-Fate2013 %>% filter(YR2013 == "Y" & YR2014 == "Y" & SUM == 2) %>% summarize(n_distinct(SubReference))
Three_Year<-Fate2013 %>% filter(YR2013 == "Y" & YR2014 == "Y"& YR2015 == "Y" & SUM == 3) %>% summarize(n_distinct(SubReference))
Four_Year<-Fate2013 %>% filter(YR2013 == "Y" & YR2014 == "Y"& YR2015 == "Y" & YR2016 == "Y" & SUM == 4) %>% summarize(n_distinct(SubReference))
Five_Year<-Fate2013 %>% filter(YR2013 == "Y" & SUM == 5) %>% summarize(n_distinct(SubReference))

Count<-c(One_Year,Two_Year,Three_Year,Four_Year,Five_Year)
YearsStaying<-c(1,2,3,4,5)
DropOff2013<-cbind(Count,YearsStaying)
DropOff2013<-as.data.frame(DropOff2013, row.names = FALSE)
DropOff2013$Count<-unlist(DropOff2013$Count)
DropOff2013$YearsStaying<-unlist(DropOff2013$YearsStaying)
DropOff2013$Percent<-DropOff2013$Count/sum(DropOff2013$Count)*100