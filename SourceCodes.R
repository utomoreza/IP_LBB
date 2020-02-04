library(readxl)
library(lubridate)
library(tidyverse)
library(ggthemes)
library(scales)
library(plotly)
library(DT)

year20182019 <- read_xlsx("year2018-2019.xlsx")

DF <- year20182019 %>% select(-c(length(year20182019))) %>% drop_na()

DF <- DF %>% filter(Dimension != "Labor Force Size" & 
                         Dimension != "Total Employment" &
                         Dimension != "Unemployment Rate")

DF$Metro <- as.factor(sapply(as.character(DF$Metro), switch, 
                             "National" = "National",
                             "Atlanta" = "Atlanta", 
                             "New York City" = "New York City", 
                             "Los Angeles" = "Los Angeles", 
                             "Philadelphia" = "Philadelphia",
                             "Houston" = "Houston", 
                             "Seattle" = "Seattle", 
                             "San Francisco" = "San Francisco", 
                             "Chicago" = "Chicago",
                             "Boston" = "Boston", 
                             "Washington DC" = "Washington DC",
                             "U.S." = "National", 
                             "New-York-City" = "New York City",
                             "Los-Angeles" = "Los Angeles", 
                             "San-Francisco" = "San Francisco",
                             "Washington-DC" = "Washington DC"))

DF$Dimension_Type <- as.factor(DF$Dimension_Type)

DF$Measure <- as.factor(DF$Measure)

isString7Chars <- function(input) {
     if (nchar(input) == 7) {
          output <- paste0(input, '-01')
     } else {
          output <- input
     }
}

DF$Date <- as.character(sapply(DF$Date, isString7Chars)) %>% 
     ymd()

DropDollar <- function(input){
     output <- gsub("\\$", "", input)
     output <- gsub(" ", "", output)
     output <- gsub(",", "", output)
     return(output)
}

DF$Value <- round(as.numeric(sapply(DF$Value, DropDollar)))

########################################################
######### below are codes for plot1c #########
########################################################

Salary_Jobs <- DF %>%
     filter(Dimension_Type == "Job Title") %>%
     select(Metro, Date, Dimension, Value) %>% 
     rename(Area = Metro, Job_Title = Dimension, Salary = Value)

Salary_Jobs$Job_Title <- as.factor(Salary_Jobs$Job_Title)

Q1_func <- function(year) {
     Q1 <- Salary_Jobs[year(Salary_Jobs$Date) == year,]
     
     maxPay_eachArea <- vector()
     maxTitle_eachArea <- vector()
     
     areas <- levels(Q1$Area)
     for (area in areas) {
          temp <- as.data.frame(Q1[Q1$Area == area,])
          
          avg <- vector()
          titles <- vector()
          
          temp$Job_Title <-  droplevels(temp$Job_Title)
          jobs <- levels(temp$Job_Title)
          
          for (job in jobs) {
               avg_buffer <- round(mean(temp[temp$Job_Title == job, "Salary"]))
               avg <- c(avg, avg_buffer)
               titles <- c(titles, job)
          }
          
          max_pay <- max(avg)
          max_title <- titles[which.max(avg)]
          
          maxPay_eachArea <- c(maxPay_eachArea, max_pay)
          maxTitle_eachArea <- c(maxTitle_eachArea, max_title)
          
     }
     Q1_ans <- as.data.frame(cbind(Area = areas,
                                   Job_Title = maxTitle_eachArea, 
                                   Salary = as.numeric(maxPay_eachArea)))
     
     return(Q1_ans)
}

Q1_ans2018 <- Q1_func(2018)
Q1_ans2018 <- Q1_ans2018 %>% arrange(desc(Salary))
Q1_ans2018_subset <- Q1_ans2018[c(1,2,3,4,5,7,8,9,10,11),]
Q1_ans2018_subset$Salary <- as.numeric(as.character(Q1_ans2018_subset$Salary))

Q1_ans2019 <- Q1_func(2019)
Q1_ans2019 <- Q1_ans2019 %>% arrange(desc(Salary))
Q1_ans2019_subset <- Q1_ans2019[c(1,2,3,4,5,7,8,9,10,11),]
Q1_ans2019_subset$Salary <- as.numeric(as.character(Q1_ans2019_subset$Salary))

Year <- c(rep(2018, 10), rep(2019, 10))
Q1_ans_subset <- rbind(Q1_ans2018_subset, Q1_ans2019_subset)
Q1_ans_subset <- cbind(Q1_ans_subset, Year)

########################################################
######### below are codes for plot2 #########
########################################################

Salary_Sectors <- DF %>%
     filter(Dimension_Type == "Industry" & Measure == "Median Base Pay") %>%
     select(Metro, Date, Dimension, Value) %>% 
     rename(Area = Metro, Sector = Dimension, Salary = Value)

Salary_Sectors$Sector <- as.factor(Salary_Sectors$Sector)

Q2_func <- function(year) {
     Q2 <- Salary_Sectors[year(Salary_Sectors$Date) == year & Salary_Sectors$Area == "Boston",]
     Q2 <- as.data.frame(Q2)
     
     pay <- vector()
     Q2$Sector <-  droplevels(Q2$Sector)
     sectors <- levels(Q2$Sector)
     for (sector in sectors) {
          pay_buffer <- round(mean(Q2[Q2$Sector == sector, "Salary"]))
          pay <- c(pay, pay_buffer)
     }
     year <- rep(year, length(sectors))
     return(data.frame(Year = year, Sector = sectors, Salary = pay))
}

Q2_ans2018 <- Q2_func(2018)
Q2_ans2018 <- Q2_ans2018 %>% arrange(desc(Salary))

Q2_ans2019 <- Q2_func(2019)
Q2_ans2019 <- Q2_ans2019 %>% arrange(desc(Salary))

Q2_ans <- rbind(head(Q2_ans2018, 5), head(Q2_ans2019, 5))

########################################################
######### below are codes for plot4a #########
########################################################

TimeSeries_Salary <- DF %>%
     filter(Dimension_Type == "Timeseries" & Dimension == "Metro Median Base Pay") %>%
     select(Metro, Date, Value) %>%
     group_by(Metro, Date) %>%
     summarise(Salary = mean(Value)) %>%
     ungroup()

SA_withoutNational <- TimeSeries_Salary %>% filter(Metro != "National")

########################################################
######### below are codes for plot4b #########
########################################################

TimeSeries_JobOpenings <- DF %>%
     filter(Dimension_Type == "Timeseries" & Dimension == "Job Openings") %>%
     select(Metro, Date, Value) %>%
     group_by(Metro, Date) %>%
     summarise(Job_Openings = mean(Value)) %>%
     ungroup()

JO_withoutNational <- TimeSeries_JobOpenings %>% filter(Metro != "National")

########################################################
######### below are codes for plot4c #########
########################################################

SA <- TimeSeries_Salary %>% 
     mutate(Month = month(Date, label = T), Year = year(Date)) %>% 
     group_by(Metro, Month, Year) %>% 
     summarise(Mean_Salary = mean(Salary)) %>% 
     ungroup() %>% 
     arrange(Year)

JO <- TimeSeries_JobOpenings %>% 
     mutate(Month = month(Date, label = T), Year = year(Date)) %>% 
     group_by(Metro, Month, Year) %>% 
     summarise(Mean_JobOpenings = mean(Job_Openings)) %>% 
     ungroup() %>%
     arrange(Year)

SA_JO <- cbind(SA, JO$Mean_JobOpenings) %>% 
     rename(Salary = Mean_Salary, Job_Openings = "JO$Mean_JobOpenings")