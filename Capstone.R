# head(ddata)
# str(ddata)
# dim(ddata)
# sum(is.na(ddata))
# # 1428 NULL Values Present
# colSums(is.na(ddata))
# # No.of dependents- 3
# # Performance.tag- 1425  
# names(ddata)

# # install.packages("stringr")
# # install.packages("tidyr")
# # install.packages("plyr")
# # install.packages("dplyr", INSTALL_opts = c('--no-lock'))
# # install.packages("ggplot2")
# # install.packages("lubridate")
# # install.packages("corrplot")
# # install.packages("VIM")
# # install.packages("treemap")
# # install.packages("cowplot")
# # install.packages("Information")
# # install.packages("DMwR")
# # install.packages("merTools")
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library(GGally)
library(Information)
library(plyr)
library(caTools)
library(MASS)
library(car)
library(caret)
library(rpart)
library(DMwR)
library(randomForest)
library(merTools)

ddata <- read.csv("Demographic data.csv", stringsAsFactors = F,na.strings = c("NA",'',"Na",' '))
cbdata <- read.csv("Credit Bureau data.csv", stringsAsFactors = F,na.strings = c("NA",'',"Na",' '))

# Check the data
dim(ddata)
# Rows: 71295 Cols:12
dim(cbdata)
# Rows: 71295 Cols:19

# Check for unique rows
length(unique(ddata$Application.ID))
length(unique(cbdata$Application.ID))
# 71292 Unique values in both datasets. 3 Duplicates in both.

# Remove the duplicate Rows
ddata <- distinct(ddata, Application.ID,.keep_all = TRUE)
cbdata <- distinct(cbdata, Application.ID,.keep_all = TRUE)

# Remove Performance Tag from demographic data before joinin with credit bureau data
ddata <- ddata[ , !(names(ddata) %in% 'Performance.Tag')]

# Merge the data on ApplicationID
data <- merge(x = ddata, y = cbdata, by = "Application.ID")

# Have a look at the data
head(data)

####################### Data Cleaning ###################################
# Age 
summary(data$Age)
# Age has negative values also. Ideally, age should be grater than 18 to get credit card.
# Remove records where age < 18
data <- data %>% filter(Age>=18)
#----------------------------------------------------------------------------------------
# Income
summary(data$Income)
# Income cannot be negative. Remove those rows
data <- data %>% filter(Income>=0)
#----------------------------------------------------------------------------------------
# Check for NA Values
colSums(is.na(data))
# Gender- 1
# Marital Status- 5
# No of Dependents- 2
# Education- 119
# Profession- 13
# Type of Residence- 8
# Avgas CC Utilization in last 12 months- 1053
# No.of.trades.opened.in.last.6.months -1
# Presence.of.open.home.loan- 272
# Outstanding.Balance- 272
# Performance.Tag- 1425

# Separate the dataset with NA values in Performance tag for Testing the model
data_with_perf_tag_na <- data[which(is.na(data$Performance.Tag)),]

# Remove rows with NA for Performance Tag
data <- data[!(is.na(data$Performance.Tag)),]
summary(data)

# Remove other rows with NA values
data <- na.omit(data)
summary(data)


######### Find Outliers for numeric variables

#### Income 
plot_grid(ggplot(data, aes(Income))+ geom_histogram(binwidth = 12.5) + ggtitle("Income"),
          ggplot(data, aes(x="",y=Income))+ geom_boxplot(width=0.1)+ ggtitle("Income"), 
          align = "v",ncol = 2)
# No visible Outliers
#----------------------------------------------------------------------------------------
#### No.of.months.in.current.residence
plot_grid(ggplot(data, aes(No.of.months.in.current.residence))+ geom_histogram(binwidth = 15) + ggtitle("No.of.months.in.current.residence"),
          ggplot(data, aes(x="",y=No.of.months.in.current.residence))+ geom_boxplot(width=0.1)+ ggtitle("No.of.months.in.current.residence"), 
          align = "v",ncol = 2)
# No visible Outliers
#----------------------------------------------------------------------------------------
#### No.of.months.in.current.company
plot_grid(ggplot(data, aes(No.of.months.in.current.company))+ geom_histogram(binwidth = 15) + ggtitle("No.of.months.in.current.company"),
          ggplot(data, aes(x="",y=No.of.months.in.current.company))+ geom_boxplot(width=0.1)+ ggtitle("No.of.months.in.current.company"), 
          align = "v",ncol = 2)
# Data has few Outliers
quantile(data$No.of.months.in.current.company,seq(0,1,.01))
# Sudden jump from 99% to 100%. Cap values to 74
data$No.of.months.in.current.company[which(data$No.of.months.in.current.company>74)] <- 74
#----------------------------------------------------------------------------------------
#### Avgas.CC.Utilization.in.last.12.months
plot_grid(ggplot(data, aes(Avgas.CC.Utilization.in.last.12.months))+ geom_histogram(binwidth = 15) + ggtitle("Avgas.CC.Utilization.in.last.12.months"),
          ggplot(data, aes(x="",y=Avgas.CC.Utilization.in.last.12.months))+ geom_boxplot(width=0.1)+ ggtitle("Avgas.CC.Utilization.in.last.12.months"), 
          align = "v",ncol = 2)
# Data has few Outliers
quantile(data$Avgas.CC.Utilization.in.last.12.months,seq(0,1,.01))
# Sudden jump from 94% to 95%. Cap values to 91
data$Avgas.CC.Utilization.in.last.12.months[which(data$Avgas.CC.Utilization.in.last.12.months>91)] <- 91
#----------------------------------------------------------------------------------------
#### Outstanding.Balance
plot_grid(ggplot(data, aes(Outstanding.Balance))+ geom_histogram(binwidth = 60000) + ggtitle("Outstanding.Balance"),
          ggplot(data, aes(x="",y=Outstanding.Balance))+ geom_boxplot(width=0.1)+ ggtitle("Outstanding.Balance"), 
          align = "v",ncol = 2)
# No visible Outliers
#----------------------------------------------------------------------------------------
#### Total.No.of.Trades
plot_grid(ggplot(data, aes(Total.No.of.Trades))+ geom_histogram(binwidth = 10) + ggtitle("Total.No.of.Trades"),
          ggplot(data, aes(x="",y=Total.No.of.Trades))+ geom_boxplot(width=0.1)+ ggtitle("Total.No.of.Trades"), 
          align = "v",ncol = 2)
# Data has Outliers
quantile(data$Total.No.of.Trades,seq(0,1,.01))
# Sudden jump from 99% to 100%. Cap values to 31
data$Total.No.of.Trades[which(data$Total.No.of.Trades>31)] <- 31


######### Derived Variables
data <- mutate(data, age_group = ifelse(Age <=30, 'Young',
	if_else(Age > 30 & Age <= 60, 'MiddleAge','SeniorCitizen')))

data <- mutate(data, salary_group = ifelse(Income <=10, 'Low Income',
	if_else(Income > 10 & Income <= 30, 'Middle Income','High Income')))



####################### Plotting Data ###################################
# Correlation between numeric variables
ggpairs(data[, c('Age', 'No.of.dependents', 'Income', 'No.of.months.in.current.residence', 'No.of.months.in.current.company',
				'No.of.times.90.DPD.or.worse.in.last.6.months', 'No.of.times.60.DPD.or.worse.in.last.6.months',
				'No.of.times.30.DPD.or.worse.in.last.6.months', 'No.of.times.90.DPD.or.worse.in.last.12.months',
				'No.of.times.60.DPD.or.worse.in.last.12.months', 'No.of.times.30.DPD.or.worse.in.last.12.months',
				'Avgas.CC.Utilization.in.last.12.months','No.of.trades.opened.in.last.6.months',
				'No.of.trades.opened.in.last.12.months', 'No.of.PL.trades.opened.in.last.6.months',
				'No.of.PL.trades.opened.in.last.12.months', 'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.',
				'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.', 'Outstanding.Balance',
				'Total.No.of.Trades')])

# Gender vs Performance Tag
plot_1<- ggplot(data, aes(factor(Performance.Tag), group = Gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  facet_grid(~Gender) +
  labs(title='Gender vs Performance Tag', x='Performance Tag',y='Percentage') + 
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_discrete(name = "Performance Tag")
plot_1

# Age Group vs Performance Tag
plot_2<- ggplot(data, aes(factor(Performance.Tag), group = age_group)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  labs(title='Age Group vs Performance Tag', x='Performance Tag',y='Percentage') + 
  facet_grid(~age_group) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_discrete(name = "Performance Tag")
plot_2

# Salary Group vs Performance Tag
plot_3<- ggplot(data, aes(factor(Performance.Tag), group = salary_group)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  labs(title='Salary Group vs Performance Tag', x='Performance Tag',y='Percentage') + 
  facet_grid(~salary_group) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_discrete(name = "Performance Tag")
plot_3

# Education vs Performance Tag
plot_4<- ggplot(data, aes(factor(Performance.Tag), group = Education)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  labs(title='Education vs Performance Tag', x='Performance Tag',y='Percentage') + 
  facet_grid(~Education) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_discrete(name = "Performance Tag")
plot_4

# Profession vs Performance Tag
plot_5<- ggplot(data, aes(factor(Performance.Tag), group = Profession)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  labs(title='Profession vs Performance Tag', x='Performance Tag',y='Percentage') + 
  facet_grid(~Profession) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_discrete(name = "Performance Tag")
plot_5

####################### WOE and IV ###################################
# Data for WOE after removing the derived variables
data_WOE <- data[ , -which(names(data) %in% c("age_group","salary_group"))]
summary(data_WOE)

# Binning
data_WOE$Age <- 
  as.factor(cut(data_WOE$Age, breaks = 15))

data_WOE$No.of.dependents <- 
  as.factor(cut(data_WOE$No.of.dependents, breaks = 6))

data_WOE$Income <- 
  as.factor(cut(data_WOE$Income, breaks = 15))

data_WOE$No.of.months.in.current.residence <- 
  as.factor(cut(data_WOE$No.of.months.in.current.residence, breaks = 15))

data_WOE$No.of.months.in.current.company <- 
  as.factor(cut(data_WOE$No.of.months.in.current.company, breaks = 15))

data_WOE$No.of.times.90.DPD.or.worse.in.last.6.months <- 
  as.factor(cut(data_WOE$No.of.times.90.DPD.or.worse.in.last.6.months, breaks = 8))

data_WOE$No.of.times.60.DPD.or.worse.in.last.6.months <- 
  as.factor(cut(data_WOE$No.of.times.60.DPD.or.worse.in.last.6.months, breaks = 8))

data_WOE$No.of.times.30.DPD.or.worse.in.last.6.months <- 
  as.factor(cut(data_WOE$No.of.times.30.DPD.or.worse.in.last.6.months, breaks = 8))

data_WOE$No.of.times.90.DPD.or.worse.in.last.12.months <- 
  as.factor(cut(data_WOE$No.of.times.90.DPD.or.worse.in.last.12.months, breaks = 10))

data_WOE$No.of.times.60.DPD.or.worse.in.last.12.months <- 
  as.factor(cut(data_WOE$No.of.times.60.DPD.or.worse.in.last.12.months, breaks = 10))

data_WOE$No.of.times.30.DPD.or.worse.in.last.12.months <- 
  as.factor(cut(data_WOE$No.of.times.30.DPD.or.worse.in.last.12.months, breaks = 10))

data_WOE$Avgas.CC.Utilization.in.last.12.months <- 
  as.factor(cut(data_WOE$Avgas.CC.Utilization.in.last.12.months, breaks = 10))

data_WOE$No.of.trades.opened.in.last.6.months <- 
  as.factor(cut(data_WOE$No.of.trades.opened.in.last.6.months, breaks = 10))

data_WOE$No.of.trades.opened.in.last.12.months <- 
  as.factor(cut(data_WOE$No.of.trades.opened.in.last.12.months, breaks = 10))

data_WOE$No.of.PL.trades.opened.in.last.6.months <- 
  as.factor(cut(data_WOE$No.of.PL.trades.opened.in.last.6.months, breaks = 10))

data_WOE$No.of.PL.trades.opened.in.last.12.months <- 
  as.factor(cut(data_WOE$No.of.PL.trades.opened.in.last.12.months, breaks = 10))

data_WOE$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- 
  as.factor(cut(data_WOE$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., breaks = 15))

data_WOE$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 
  as.factor(cut(data_WOE$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., breaks = 18))

data_WOE$Presence.of.open.home.loan <- 
  as.factor(cut(data_WOE$Presence.of.open.home.loan, breaks = 3))

data_WOE$Outstanding.Balance <- 
  as.factor(cut(data_WOE$Outstanding.Balance, breaks = 15))

data_WOE$Total.No.of.Trades <- 
  as.factor(cut(data_WOE$Total.No.of.Trades, breaks = 15))

data_WOE$Presence.of.open.auto.loan <- 
  as.factor(cut(data_WOE$Presence.of.open.auto.loan, breaks = 3))

IV <- create_infotables(data = data_WOE, y='Performance.Tag',bins = 10, parallel = FALSE)
IV_value <- data.frame(IV$Summary)

# Avgas.CC.Utilization.in.last.12.months	                        0.3196123
# No.of.trades.opened.in.last.12.months	                            0.3075795
# No.of.PL.trades.opened.in.last.12.months	                        0.2686921
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.	0.2663039
# Outstanding.Balance												0.2550174
# Total.No.of.Trades												0.2533396
# No.of.times.30.DPD.or.worse.in.last.6.months						0.2499031
# No.of.PL.trades.opened.in.last.6.months							0.2334098
# No.of.times.30.DPD.or.worse.in.last.12.months						0.2228656
# No.of.times.90.DPD.or.worse.in.last.12.months						0.2204230
# No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.	0.2188786
# No.of.times.60.DPD.or.worse.in.last.6.months						0.2156838

IV$Tables

#### Plotting Transformed Variables
plot_infotables(IV,"Age")
# Person in age 18-25 are more likely to default
plot_infotables(IV,"Gender")
# Male Customers have high chance of defaulting
plot_infotables(IV,"Marital.Status..at.the.time.of.application.")
# Single People have high WOE value
plot_infotables(IV,"No.of.dependents")
# People with around 2 dependents are more likely to default
plot_infotables(IV,"Income")
# Ironically people with Higher Incomes have low WOE value
plot_infotables(IV,"Education")
# "Others" has high WOE
plot_infotables(IV,"Profession")
# SE has high WOE
plot_infotables(IV,"Type.of.residence")
# People whose type of residence is not disclosed have low WOE
plot_infotables(IV,"No.of.months.in.current.residence")
plot_infotables(IV,"No.of.months.in.current.company")
# People with 50-60 months with current company have low WOE
plot_infotables(IV,"No.of.times.90.DPD.or.worse.in.last.6.months")
plot_infotables(IV,"No.of.times.60.DPD.or.worse.in.last.6.months")
plot_infotables(IV,"No.of.times.30.DPD.or.worse.in.last.6.months")

plot_infotables(IV,"No.of.times.90.DPD.or.worse.in.last.12.months")
plot_infotables(IV,"No.of.times.60.DPD.or.worse.in.last.12.months")
plot_infotables(IV,"No.of.times.30.DPD.or.worse.in.last.12.months")

plot_infotables(IV,"Avgas.CC.Utilization.in.last.12.months")

plot_infotables(IV,"No.of.trades.opened.in.last.6.months")

#Replacing the actual values of the variables with corresponding WOE values
data_WOE$Age <- mapvalues(data_WOE$Age, from = IV$Tables$Age$Age, to = IV$Tables$Age$WOE)
data_WOE$Gender <- mapvalues(data_WOE$Gender, from = IV$Tables$Gender$Gender, to = IV$Tables$Gender$WOE)
data_WOE$Marital.Status..at.the.time.of.application. <- mapvalues(data_WOE$Marital.Status..at.the.time.of.application., from = IV$Tables$Marital.Status..at.the.time.of.application.$Marital.Status..at.the.time.of.application., to = IV$Tables$Marital.Status..at.the.time.of.application.$WOE)
data_WOE$No.of.dependents <- mapvalues(data_WOE$No.of.dependents, from = IV$Tables$No.of.dependents$No.of.dependents, to = IV$Tables$No.of.dependents$WOE)
data_WOE$Income <- mapvalues(data_WOE$Income, from = IV$Tables$Income$Income, to = IV$Tables$Income$WOE)
data_WOE$Education <- mapvalues(data_WOE$Education, from = IV$Tables$Education$Education, to = IV$Tables$Education$WOE)
data_WOE$Profession <- mapvalues(data_WOE$Profession, from = IV$Tables$Profession$Profession, to = IV$Tables$Profession$WOE)
data_WOE$Type.of.residence <- mapvalues(data_WOE$Type.of.residence, from = IV$Tables$Type.of.residence$Type.of.residence, to = IV$Tables$Type.of.residence$WOE)
data_WOE$No.of.months.in.current.residence <- mapvalues(data_WOE$No.of.months.in.current.residence, from = IV$Tables$No.of.months.in.current.residence$No.of.months.in.current.residence, to = IV$Tables$No.of.months.in.current.residence$WOE)
data_WOE$No.of.months.in.current.company <- mapvalues(data_WOE$No.of.months.in.current.company, from = IV$Tables$No.of.months.in.current.company$No.of.months.in.current.company, to = IV$Tables$No.of.months.in.current.company$WOE)
data_WOE$No.of.times.90.DPD.or.worse.in.last.6.months <- mapvalues(data_WOE$No.of.times.90.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$No.of.times.90.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$WOE)
data_WOE$No.of.times.60.DPD.or.worse.in.last.6.months <- mapvalues(data_WOE$No.of.times.60.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$No.of.times.60.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$WOE)
data_WOE$No.of.times.30.DPD.or.worse.in.last.6.months <- mapvalues(data_WOE$No.of.times.30.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$No.of.times.30.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$WOE)
data_WOE$No.of.times.90.DPD.or.worse.in.last.12.months <- mapvalues(data_WOE$No.of.times.90.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$No.of.times.90.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$WOE)
data_WOE$No.of.times.60.DPD.or.worse.in.last.12.months <- mapvalues(data_WOE$No.of.times.60.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$No.of.times.60.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$WOE)
data_WOE$No.of.times.30.DPD.or.worse.in.last.12.months <- mapvalues(data_WOE$No.of.times.30.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$No.of.times.30.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$WOE)
data_WOE$Avgas.CC.Utilization.in.last.12.months <- mapvalues(data_WOE$Avgas.CC.Utilization.in.last.12.months, from = IV$Tables$Avgas.CC.Utilization.in.last.12.months$Avgas.CC.Utilization.in.last.12.months, to = IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE)
data_WOE$No.of.trades.opened.in.last.6.months <- mapvalues(data_WOE$No.of.trades.opened.in.last.6.months, from = IV$Tables$No.of.trades.opened.in.last.6.months$No.of.trades.opened.in.last.6.months, to = IV$Tables$No.of.trades.opened.in.last.6.months$WOE)
data_WOE$No.of.trades.opened.in.last.12.months <- mapvalues(data_WOE$No.of.trades.opened.in.last.12.months, from = IV$Tables$No.of.trades.opened.in.last.12.months$No.of.trades.opened.in.last.12.months, to = IV$Tables$No.of.trades.opened.in.last.12.months$WOE)
data_WOE$No.of.PL.trades.opened.in.last.6.months <- mapvalues(data_WOE$No.of.PL.trades.opened.in.last.6.months, from = IV$Tables$No.of.PL.trades.opened.in.last.6.months$No.of.PL.trades.opened.in.last.6.months, to = IV$Tables$No.of.PL.trades.opened.in.last.6.months$WOE)
data_WOE$No.of.PL.trades.opened.in.last.12.months <- mapvalues(data_WOE$No.of.PL.trades.opened.in.last.12.months, from = IV$Tables$No.of.PL.trades.opened.in.last.12.months$No.of.PL.trades.opened.in.last.12.months, to = IV$Tables$No.of.PL.trades.opened.in.last.12.months$WOE)
data_WOE$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- mapvalues(data_WOE$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., from = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., to = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$WOE)
data_WOE$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- mapvalues(data_WOE$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., from = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., to = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$WOE)
data_WOE$Presence.of.open.home.loan <- mapvalues(data_WOE$Presence.of.open.home.loan, from = IV$Tables$Presence.of.open.home.loan$Presence.of.open.home.loan, to = IV$Tables$Presence.of.open.home.loan$WOE)
data_WOE$Outstanding.Balance <- mapvalues(data_WOE$Outstanding.Balance, from = IV$Tables$Outstanding.Balance$Outstanding.Balance, to = IV$Tables$Outstanding.Balance$WOE)
data_WOE$Total.No.of.Trades <- mapvalues(data_WOE$Total.No.of.Trades, from = IV$Tables$Total.No.of.Trades$Total.No.of.Trades, to = IV$Tables$Total.No.of.Trades$WOE)
data_WOE$Presence.of.open.auto.loan <- mapvalues(data_WOE$Presence.of.open.auto.loan, from = IV$Tables$Presence.of.open.auto.loan$Presence.of.open.auto.loan, to = IV$Tables$Presence.of.open.auto.loan$WOE)


####################################################################################################
###################################### LOGISTIC REGRESSION #########################################
####################################################################################################

####################################### PREPARING DATA #############################################

########################################################################
############################ COMBINED DATA #############################
########################################################################
logistic_data <- data

# Remove Age Group, Salary Group and ApplicationID
logistic_data <- logistic_data[ , -which(names(logistic_data) %in% c("age_group","salary_group","Application.ID"))]

# Convert Categorical Variables to factors
categorical_variables <- c('Gender', 'Marital.Status..at.the.time.of.application.', 'Education', 'Profession',
                            'Type.of.residence','Presence.of.open.auto.loan','Presence.of.open.home.loan','Performance.Tag')
logistic_data[, categorical_variables] <- lapply(categorical_variables, function(x) as.factor(as.character(logistic_data[, x])))

# Remove Performance.Tag from list of categorical variables for dummy variable creation
categorical_variables <- categorical_variables[categorical_variables != 'Performance.Tag']
# Convert Factor variables to dummy variables
factor_variables <- logistic_data[ , which(names(logistic_data) %in% categorical_variables)]
factor_variables_dummy<- data.frame(sapply(factor_variables,function(x) data.frame(model.matrix(~x-1,data =factor_variables))[,-1]))
logistic_data <- cbind(factor_variables_dummy,logistic_data[ , -which(names(logistic_data) %in% categorical_variables)])


# Scale the Numeric Variables
numeric_variables <- c('Age', 'No.of.dependents', 'Income', 'No.of.months.in.current.residence', 'No.of.months.in.current.company',
                        'No.of.times.90.DPD.or.worse.in.last.6.months', 'No.of.times.60.DPD.or.worse.in.last.6.months',
                        'No.of.times.30.DPD.or.worse.in.last.6.months', 'No.of.times.90.DPD.or.worse.in.last.12.months',
                        'No.of.times.60.DPD.or.worse.in.last.12.months', 'No.of.times.30.DPD.or.worse.in.last.12.months',
                        'Avgas.CC.Utilization.in.last.12.months','No.of.trades.opened.in.last.6.months',
                        'No.of.trades.opened.in.last.12.months', 'No.of.PL.trades.opened.in.last.6.months',
                        'No.of.PL.trades.opened.in.last.12.months', 'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.',
                        'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.', 'Outstanding.Balance',
                        'Total.No.of.Trades')
logistic_data[, numeric_variables] <- lapply(numeric_variables, function(x) scale(logistic_data[, x]))

# Split into Train and Test
set.seed(1)
ntrain <- sample.split(logistic_data$Performance.Tag, SplitRatio = 0.70)

logistic_data_train <- logistic_data[ntrain, ]
logistic_data_test <- logistic_data[!ntrain, ]


########################################################################
########################## DEMOGRAPHIC DATA ############################
########################################################################
logistic_demographic_data <- data %>%
  dplyr::select(names(ddata[,-12]),Performance.Tag)

# Remove ApplicationID
logistic_demographic_data <- logistic_demographic_data[ , -which(names(logistic_demographic_data) %in% c("Application.ID"))]

# Convert Categorical Variables to factors
categorical_variables <- c('Gender', 'Marital.Status..at.the.time.of.application.', 'Education', 'Profession',
                            'Type.of.residence','Performance.Tag')
logistic_demographic_data[, categorical_variables] <- lapply(categorical_variables, function(x) as.factor(as.character(logistic_demographic_data[, x])))


# Scale the Numeric Variables
numeric_variables <- c('Age', 'No.of.dependents', 'Income', 'No.of.months.in.current.residence', 'No.of.months.in.current.company')
logistic_demographic_data[, numeric_variables] <- lapply(numeric_variables, function(x) scale(logistic_demographic_data[, x]))

# Remove Performance.Tag from list of categorical variables for dummy variable creation
categorical_variables <- categorical_variables[categorical_variables != 'Performance.Tag']

# Convert Factor variables to dummy variables
factor_variables <- logistic_demographic_data[ , which(names(logistic_demographic_data) %in% categorical_variables)]
factor_variables_dummy<- data.frame(sapply(factor_variables,function(x) data.frame(model.matrix(~x-1,data =factor_variables))[,-1]))
logistic_demographic_data <- cbind(factor_variables_dummy,logistic_demographic_data[ , -which(names(logistic_demographic_data) %in% categorical_variables)])

# Split into Train and Test
set.seed(1)
ntrain <- sample.split(logistic_demographic_data$Performance.Tag, SplitRatio = 0.70)

logistic_demographic_data_train <- logistic_demographic_data[ntrain, ]
logistic_demographic_data_test <- logistic_demographic_data[!ntrain, ]

####################################### BUILDING MODELS ############################################
##################### MODEL 1 #######################
################# Demographic Data ##################
################## Without SMOTE ####################
# Build Model
logistic_demographic_model_1 <- glm(Performance.Tag ~ ., data = logistic_demographic_data_train, family = "binomial")
summary(logistic_demographic_model_1) 
# AIC: 16708

# Step AIC
logistic_demographic_model_2 <- stepAIC(logistic_demographic_model_1, direction = "both")
summary(logistic_demographic_model_2)
vif(logistic_demographic_model_2)
# AIC: 16687

# Remove Type.of.residence.xOthers due to low p-value
logistic_demographic_model_3 <- glm(Performance.Tag ~ Profession.xSE + 
                                    Income + No.of.months.in.current.residence + No.of.months.in.current.company,
                                data = logistic_demographic_data_train, family = "binomial")
summary(logistic_demographic_model_3)
vif(logistic_demographic_model_3)
# AIC: 16688

# Remove Profession.xSE due to low p-value
logistic_demographic_model_4 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                                No.of.months.in.current.company,
                                data = logistic_demographic_data_train, family = "binomial")
summary(logistic_demographic_model_4)
vif(logistic_demographic_model_4)
# AIC: 16690

# Final Model
logistic_demographic_model <- logistic_demographic_model_4

##### Model Evaluation #####
logistic_demographic_model_predicted = predict(logistic_demographic_model, type = "response", newdata = logistic_demographic_data_test)  
summary(logistic_demographic_model_predicted)

# Add the prediction probability with the test data set for further model evaluation steps.
logistic_demographic_data_test$prob <- logistic_demographic_model_predicted

# View the test dataset including prediction probabity.
View(logistic_demographic_data_test)

# Let's use the probability cutoff of 50%.
actual <- factor(ifelse(logistic_demographic_data_test$Performance.Tag==1,"Yes","No"))
predicted <- factor(ifelse(logistic_demographic_model_predicted >= 0.50, "Yes", "No"))

confusionMatrix(predicted, actual, positive = "Yes")
# Accuracy : 95.78%
# Sensitivity : 0%       
# Specificity : 100%

# Find optimal cutoff
confusion_matrix_iteration <- function(cutoff,data_predicted) 
{
  predicted <- factor(ifelse(data_predicted >= cutoff, "Yes", "No"))
  conf_matrix <- confusionMatrix(predicted, actual, positive = "Yes")
  accuracy <- conf_matrix$overall[1]
  sensitivity <- conf_matrix$byClass[1]
  specificity <- conf_matrix$byClass[2]
  matrix_row <- t(as.matrix(c(sensitivity, specificity, accuracy))) 
  colnames(matrix_row) <- c("Sensitivity", "Specificity", "Accuracy")
  return(matrix_row)
}

calculate_cutoff <- function(data_predicted)
{
  s = seq(.01,.80,length=100)
  # Initialize a 100x3 matrix with 0 as default value.
  cutoff_matrix = matrix(0,100,3)
  # Call the perform_fn in a loop and assign the output to OUT matrix for "sensitivity", "specificity", "accuracy".
  for(i in 1:100)
  {
    cutoff_matrix[i,] = confusion_matrix_iteration(s[i],data_predicted)
  } 
  plot(s, cutoff_matrix[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2) +
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  lines(s,cutoff_matrix[,2],col="darkgreen",lwd=2) +
  lines(s,cutoff_matrix[,3],col=4,lwd=2) 

  # Add legends to the plot created earlier.
  legend(0.60,0.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy")) 

  # Add a box over the plot.
  box()

  # Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
  cutoff <- s[which(abs(cutoff_matrix[,1]-cutoff_matrix[,2])<0.12)]
  return(cutoff)
}

cutoff <- calculate_cutoff(logistic_demographic_model_predicted)


predicted <- factor(ifelse(logistic_demographic_model_predicted >=cutoff, "Yes", "No"))
confusionMatrix(predicted, actual, positive = "Yes")
# Accuracy : 52.59%
# Sensitivity : 59.17%       
# Specificity : 52.29%

##################### MODEL 2 #######################
################# Demographic Data ##################
#################### With SMOTE #####################
# Check if data is balanced
table(logistic_demographic_data_train$Performance.Tag)
#     0     1 
# 45969  2024 
# The data is unbalanced

logistic_demographic_data_train_SMOTE <- SMOTE(Performance.Tag ~ ., logistic_demographic_data_train, perc.over = 100, perc.under=200)
table(logistic_demographic_data_train_SMOTE$Performance.Tag)
#    0    1 
# 4048 4048 

# Build Model
logistic_demographic_model_1_SMOTE <- glm(Performance.Tag ~ ., data = logistic_demographic_data_train_SMOTE, family = "binomial")
summary(logistic_demographic_model_1_SMOTE) 
# AIC: 11132

# Step AIC
logistic_demographic_model_2_SMOTE <- stepAIC(logistic_demographic_model_1_SMOTE, direction = "both")
summary(logistic_demographic_model_2_SMOTE)
vif(logistic_demographic_model_2_SMOTE)
# AIC: 11113

# Remove Profession.xSE_PROF due to low p-value
logistic_demographic_model_3_SMOTE <- glm(formula = Performance.Tag ~ Marital.Status..at.the.time.of.application. + 
                                      Education.xMasters + Age + Income + 
                                      No.of.months.in.current.residence + No.of.months.in.current.company, 
                                    family = "binomial", data = logistic_demographic_data_train_SMOTE)
summary(logistic_demographic_model_3_SMOTE)
vif(logistic_demographic_model_3_SMOTE)
# AIC: 11113

# Remove Marital.Status..at.the.time.of.application. due to low p-value
logistic_demographic_model_4_SMOTE <- glm(formula = Performance.Tag ~ Education.xMasters + Age + Income + 
                                      No.of.months.in.current.residence + No.of.months.in.current.company, 
                                    family = "binomial", data = logistic_demographic_data_train_SMOTE)
summary(logistic_demographic_model_4_SMOTE)
vif(logistic_demographic_model_4_SMOTE)
# AIC: 11114

# Remove No.of.months.in.current.residence due to low p-value
logistic_demographic_model_5_SMOTE <- glm(formula = Performance.Tag ~ Education.xMasters + Age + Income + 
                                      No.of.months.in.current.company, 
                                    family = "binomial", data = logistic_demographic_data_train_SMOTE)
summary(logistic_demographic_model_5_SMOTE)
vif(logistic_demographic_model_5_SMOTE)
# AIC: 11114

# Remove Education.xMasters due to low p-value
logistic_demographic_model_6_SMOTE <- glm(formula = Performance.Tag ~ Age + Income + 
                                      No.of.months.in.current.company, 
                                    family = "binomial", data = logistic_demographic_data_train_SMOTE)
summary(logistic_demographic_model_6_SMOTE)
vif(logistic_demographic_model_6_SMOTE)
# AIC: 11116

# Remove Age due to low p-value
logistic_demographic_model_7_SMOTE <- glm(formula = Performance.Tag ~ Income + 
                                      No.of.months.in.current.company, 
                                    family = "binomial", data = logistic_demographic_data_train_SMOTE)
summary(logistic_demographic_model_7_SMOTE)
vif(logistic_demographic_model_7_SMOTE)
# AIC: 11117

# Final Model
logistic_demographic_model <- logistic_demographic_model_7_SMOTE

##### Model Evaluation #####
logistic_demographic_model_predicted = predict(logistic_demographic_model, type = "response", newdata = logistic_demographic_data_test)  
summary(logistic_demographic_model_predicted)

# Add the prediction probability with the test data set for further model evaluation steps.
logistic_demographic_data_test$prob <- logistic_demographic_model_predicted

# View the test dataset including prediction probabity.
View(logistic_demographic_data_test)

# Let's use the probability cutoff of 50%.
actual <- factor(ifelse(logistic_demographic_data_test$Performance.Tag==1,"Yes","No"))
predicted <- factor(ifelse(logistic_demographic_model_predicted >= 0.50, "Yes", "No"))

confusionMatrix(predicted, actual, positive = "Yes")
# Accuracy : 53.46%
# Sensitivity : 56.74%       
# Specificity : 53.31%

# Find optimal cutoff
cutoff <- calculate_cutoff(logistic_demographic_model_predicted)


predicted <- factor(ifelse(logistic_demographic_model_predicted >=cutoff, "Yes", "No"))
confusionMatrix(predicted, actual, positive = "Yes")
# Accuracy : 56.05%
# Sensitivity : 54.09%       
# Specificity : 56.13%
##################### MODEL 3 #######################
################## Combined Data ####################
################## Without SMOTE ####################
# Build Model
logistic_model_1 <- glm(Performance.Tag ~ ., data = logistic_data_train, family = "binomial")
summary(logistic_model_1) 
# AIC: 16135

# Step AIC
logistic_model_2 <- stepAIC(logistic_model_1, direction = "both")
summary(logistic_model_2)
sort(vif(logistic_model_2),decreasing = TRUE)
# AIC: 16105

# Remove No.of.Inquiries.in.last.6.months..excluding.home...auto.loans due to low p-value
logistic_model_3 <- glm(formula = Performance.Tag ~ Profession.xSE + Type.of.residence.xOthers + 
                          No.of.months.in.current.residence + No.of.months.in.current.company + 
                          No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.30.DPD.or.worse.in.last.6.months + 
                          No.of.times.90.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                          No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades, family = "binomial", data = logistic_data_train)
summary(logistic_model_3)
sort(vif(logistic_model_3),decreasing = TRUE)
# AIC: 16106

# Remove No.of.PL.trades.opened.in.last.6.months due to low p-value
logistic_model_4 <- glm(formula = Performance.Tag ~ Profession.xSE + Type.of.residence.xOthers + 
                          No.of.months.in.current.residence + No.of.months.in.current.company + 
                          No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.30.DPD.or.worse.in.last.6.months + 
                          No.of.times.90.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                          No.of.PL.trades.opened.in.last.12.months + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades, family = "binomial", data = logistic_data_train)
summary(logistic_model_4)
sort(vif(logistic_model_4),decreasing = TRUE)
# AIC: 16106

# Remove Type.of.residence.xOthers due to low p-value
logistic_model_5 <- glm(formula = Performance.Tag ~ Profession.xSE + 
                          No.of.months.in.current.residence + No.of.months.in.current.company + 
                          No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.30.DPD.or.worse.in.last.6.months + 
                          No.of.times.90.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                          No.of.PL.trades.opened.in.last.12.months + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades, family = "binomial", data = logistic_data_train)
summary(logistic_model_5)
sort(vif(logistic_model_5),decreasing = TRUE)
# AIC: 16107

# Remove No.of.months.in.current.residence due to low p-value
logistic_model_6 <- glm(formula = Performance.Tag ~ Profession.xSE + 
                          No.of.months.in.current.company + 
                          No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.30.DPD.or.worse.in.last.6.months + 
                          No.of.times.90.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                          No.of.PL.trades.opened.in.last.12.months + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades, family = "binomial", data = logistic_data_train)
summary(logistic_model_6)
sort(vif(logistic_model_6),decreasing = TRUE)
# AIC: 16107

# Remove Profession.xSE due to low p-value
logistic_model_7 <- glm(formula = Performance.Tag ~  
                          No.of.months.in.current.company + 
                          No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.30.DPD.or.worse.in.last.6.months + 
                          No.of.times.90.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                          No.of.PL.trades.opened.in.last.12.months + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades, family = "binomial", data = logistic_data_train)
summary(logistic_model_7)
sort(vif(logistic_model_7),decreasing = TRUE)
# AIC: 16108

# Remove No.of.times.90.DPD.or.worse.in.last.6.months due to low p-value
logistic_model_8 <- glm(formula = Performance.Tag ~  
                          No.of.months.in.current.company + 
                          No.of.times.30.DPD.or.worse.in.last.6.months + 
                          No.of.times.90.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                          No.of.PL.trades.opened.in.last.12.months + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades, family = "binomial", data = logistic_data_train)
summary(logistic_model_8)
sort(vif(logistic_model_8),decreasing = TRUE)
# AIC: 16111

# Remove No.of.times.90.DPD.or.worse.in.last.12.months due to low p-value
logistic_model_9 <- glm(formula = Performance.Tag ~  
                          No.of.months.in.current.company + 
                          No.of.times.30.DPD.or.worse.in.last.6.months + 
                          Avgas.CC.Utilization.in.last.12.months + 
                          No.of.PL.trades.opened.in.last.12.months + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades, family = "binomial", data = logistic_data_train)
summary(logistic_model_9)
sort(vif(logistic_model_9),decreasing = TRUE)
# AIC: 16110

# Remove No.of.times.90.DPD.or.worse.in.last.12.months due to low p-value
logistic_model_10 <- glm(formula = Performance.Tag ~  
                          No.of.times.30.DPD.or.worse.in.last.6.months + 
                          Avgas.CC.Utilization.in.last.12.months + 
                          No.of.PL.trades.opened.in.last.12.months + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades, family = "binomial", data = logistic_data_train)
summary(logistic_model_10)
sort(vif(logistic_model_10),decreasing = TRUE)
# AIC: 16110

# Final Model
logistic_model <- logistic_model_10

##### Model Evaluation #####
logistic_model_predicted = predict(logistic_model, type = "response", newdata = logistic_data_test)  
summary(logistic_model_predicted)

# Add the prediction probability with the test data set for further model evaluation steps.
logistic_data_test$prob <- logistic_model_predicted

# View the test dataset including prediction probabity.
View(logistic_data_test)

# Let's use the probability cutoff of 50%.
actual <- factor(ifelse(logistic_data_test$Performance.Tag==1,"Yes","No"))
predicted <- factor(ifelse(logistic_model_predicted >= 0.50, "Yes", "No"))

confusionMatrix(predicted, actual, positive = "Yes")
# Accuracy : 95.78%
# Sensitivity : 0%       
# Specificity : 100%

# Find optimal cutoff
cutoff <- calculate_cutoff(logistic_model_predicted)


predicted <- factor(ifelse(logistic_model_predicted >=cutoff, "Yes", "No"))
confusionMatrix(predicted, actual, positive = "Yes")
# Accuracy : 58.94%
# Sensitivity : 66.78%       
# Specificity : 58.59%
##################### MODEL 4 #######################
################## Combined Data ####################
#################### With SMOTE #####################
# Check if data is balanced
table(logistic_data_train$Performance.Tag)
#     0     1 
# 45969  2024 
# The data is unbalanced

logistic_data_train_SMOTE <- SMOTE(Performance.Tag ~ ., logistic_data_train, perc.over = 100, perc.under=200)
table(logistic_data_train_SMOTE$Performance.Tag)
#    0    1 
# 4048 4048 

# Build Model
logistic_model_1_SMOTE <- glm(Performance.Tag ~ ., data = logistic_data_train_SMOTE, family = "binomial")
summary(logistic_model_1_SMOTE) 
# AIC: 10534

# Step AIC
logistic_model_2_SMOTE <- stepAIC(logistic_model_1_SMOTE, direction = "both")
summary(logistic_model_2_SMOTE)
sort(vif(logistic_model_2_SMOTE),decreasing = TRUE)
# AIC: 10510

# Remove No.of.times.30.DPD.or.worse.in.last.6.months due to low p-value
logistic_model_3_SMOTE <- glm(formula = Performance.Tag ~ Marital.Status..at.the.time.of.application. + 
                        Profession.xSE_PROF + Type.of.residence.xOthers + Presence.of.open.auto.loan + 
                        No.of.dependents + Income + No.of.months.in.current.residence + 
                        No.of.months.in.current.company + No.of.times.60.DPD.or.worse.in.last.6.months + 
                        No.of.times.90.DPD.or.worse.in.last.12.months + 
                        No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                        No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                        No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                        Total.No.of.Trades, family = "binomial", data = logistic_data_train_SMOTE)
summary(logistic_model_3_SMOTE)
sort(vif(logistic_model_3_SMOTE),decreasing = TRUE)
# AIC: 10512

# Remove Profession.xSE_PROF due to low p-value
logistic_model_4_SMOTE <- glm(formula = Performance.Tag ~ Marital.Status..at.the.time.of.application. + 
                        Type.of.residence.xOthers + Presence.of.open.auto.loan + 
                        No.of.dependents + Income + No.of.months.in.current.residence + 
                        No.of.months.in.current.company + No.of.times.60.DPD.or.worse.in.last.6.months + 
                        No.of.times.90.DPD.or.worse.in.last.12.months + 
                        No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                        No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                        No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                        Total.No.of.Trades, family = "binomial", data = logistic_data_train_SMOTE)
summary(logistic_model_4_SMOTE)
sort(vif(logistic_model_4_SMOTE),decreasing = TRUE)
# AIC: 10512

# Remove Type.of.residence.xOthers due to low p-value
logistic_model_5_SMOTE <- glm(formula = Performance.Tag ~ Marital.Status..at.the.time.of.application. + 
                        Presence.of.open.auto.loan + 
                        No.of.dependents + Income + No.of.months.in.current.residence + 
                        No.of.months.in.current.company + No.of.times.60.DPD.or.worse.in.last.6.months + 
                        No.of.times.90.DPD.or.worse.in.last.12.months + 
                        No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                        No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                        No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                        Total.No.of.Trades, family = "binomial", data = logistic_data_train_SMOTE)
summary(logistic_model_5_SMOTE)
sort(vif(logistic_model_5_SMOTE),decreasing = TRUE)
# AIC: 10513

# Final Model
logistic_model <- logistic_model_5_SMOTE

##### Model Evaluation #####
logistic_model_predicted = predict(logistic_model, type = "response", newdata = logistic_data_test)  
summary(logistic_model_predicted)

# Add the prediction probability with the test data set for further model evaluation steps.
logistic_data_test$prob <- logistic_model_predicted

# View the test dataset including prediction probabity.
View(logistic_data_test)

# Let's use the probability cutoff of 50%.
actual <- factor(ifelse(logistic_data_test$Performance.Tag==1,"Yes","No"))
predicted <- factor(ifelse(logistic_model_predicted >= 0.50, "Yes", "No"))

confusionMatrix(predicted, actual, positive = "Yes")
# Accuracy : 60.78%
# Sensitivity : 65.62%       
# Specificity : 60.57%

# Find optimal cutoff
cutoff <- calculate_cutoff(logistic_model_predicted)


predicted <- factor(ifelse(logistic_model_predicted >=cutoff, "Yes", "No"))
confusionMatrix(predicted, actual, positive = "Yes")
# Accuracy : 62.29%
# Sensitivity : 63.32%       
# Specificity : 62.24%



####################################################################################################
######################################### DECISION TREE ############################################
####################################################################################################

####################################### PREPARING DATA #############################################
decisiontree_data <- data

# Convert Categorical Variables to factors
categorical_variables <- c('Gender', 'Marital.Status..at.the.time.of.application.', 'Education', 'Profession',
                            'Type.of.residence','Presence.of.open.auto.loan','Presence.of.open.home.loan','Performance.Tag')
decisiontree_data[, categorical_variables] <- lapply(categorical_variables, function(x) as.factor(as.character(decisiontree_data[, x])))

# Remove Age Group, Salary Group and ApplicationID
decisiontree_data <- decisiontree_data[ , -which(names(decisiontree_data) %in% c("age_group","salary_group","Application.ID"))]

# Split into Train and Test
set.seed(1)
ntrain <- sample.split(decisiontree_data$Performance.Tag, SplitRatio = 0.70)
decisiontree_data_train <- decisiontree_data[ntrain, ]
decisiontree_data_test <- decisiontree_data[!ntrain, ]

####################################### BUILDING MODELS ############################################
##################### MODEL 5 #######################
################## Combined Data ####################
################## Without SMOTE ####################
######## Build standard Decision Tree Model
decisiontree_model_1 <- rpart(Performance.Tag ~ .,
                              data = decisiontree_data_train,
                              method = "class")                   
prp(decisiontree_model_1)

# Predict on Test Data
decisiontree_model_1_predicted <- predict(decisiontree_model_1, decisiontree_data_test, type = "class")

# Confusion Matrix
confusionMatrix(decisiontree_model_1_predicted, decisiontree_data_test$Performance.Tag)
# Accuracy : 95.78%
# Sensitivity: 100%
# Specificity: 0%

######## Use Information Gain
decisiontree_model_2 <- rpart(Performance.Tag ~ .,         
                              data = decisiontree_data_train,     
                              method = "class",            
                              parms = list(split = "information"))
prp(decisiontree_model_2)

# Predict on Test Data
decisiontree_model_2_predicted <- predict(decisiontree_model_2, decisiontree_data_test, type = "class")

# Confusion Matrix
confusionMatrix(decisiontree_model_2_predicted, decisiontree_data_test$Performance.Tag)
# Accuracy : 95.78%
# Sensitivity: 100%
# Specificity: 0%

######## Tune Hyperparameters
decisiontree_model_3 <- rpart(Performance.Tag ~ .,                          # formula
                                  data = decisiontree_data_train,           # training data
                                  method = "class",                         # classification or regression
                                  control = rpart.control(minsplit = 1000,  # min observations for node
                                                          minbucket = 1000, # min observations for leaf node
                                                          cp = 0.05))       # complexity parameter
prp(decisiontree_model_3)

# Predict on Test Data
decisiontree_model_3_predicted <- predict(decisiontree_model_3, decisiontree_data_test, type = "class")

# Confusion Matrix
confusionMatrix(decisiontree_model_3_predicted, decisiontree_data_test$Performance.Tag)
# Accuracy : 95.78%
# Sensitivity: 100%
# Specificity: 0%

######## Increase Complexity
decisiontree_model_4 <- rpart(Performance.Tag ~ .,                          # formula
                              data = decisiontree_data_train,               # training data
                              method = "class",                             # classification or regression
                              control = rpart.control(minsplit = 1,         # min observations for node
                                                      minbucket = 1,        # min observations for leaf node
                                                      cp = 0.001))          # complexity parameter
prp(decisiontree_model_4)

# Predict on Test Data
decisiontree_model_4_predicted <- predict(decisiontree_model_4, decisiontree_data_test, type = "class")

# Confusion Matrix
confusionMatrix(decisiontree_model_4_predicted, decisiontree_data_test$Performance.Tag)
# Accuracy : 95.78%
# Sensitivity: 100%
# Specificity: 0%

######## Cross Test 
decisiontree_model_5 <- train(Performance.Tag ~ .,
                              data = decisiontree_data_train,
                              method = "rpart",
                              metric = "Accuracy",
                              trControl = trainControl(method = "cv", number = 5),
                              tuneGrid = expand.grid(cp = seq(0, 0.02, 0.0025)),
                              control = rpart.control(minsplit = 50,
                                                      minbucket = 20))
# Predict on Test Data
decisiontree_model_5_predicted <- predict(decisiontree_model_5, decisiontree_data_test)

# Confusion Matrix
confusionMatrix(decisiontree_model_5_predicted, decisiontree_data_test$Performance.Tag)
# Accuracy : 95.78%
# Sensitivity: 100%
# Specificity: 0%

ggplot(data = data.frame(decisiontree_model_5$results), aes(x = cp, y = Accuracy*100)) +
  geom_line() +
  geom_point() +
  labs(x = "Complexity Parameter (CP)", y = "Accuracy", title = "CP vs Accuracy")

# The model seems to just memorize the values as there is no proper distribution of data and is highly unbalanced.
# So even though accuracy is high, sensitivity and specificity values are not acceptable. Hence model is rejected.

##################### MODEL 6 #######################
################## Combined Data ####################
#################### With SMOTE #####################
# Check if data is balanced
table(decisiontree_data_train$Performance.Tag)
#     0     1 
# 45969  2024 
# The data is unbalanced

decisiontree_data_train_SMOTE <- SMOTE(Performance.Tag ~ ., decisiontree_data_train, perc.over = 100, perc.under=200)
table(decisiontree_data_train_SMOTE$Performance.Tag)
#    0    1 
# 4048 4048 

######## Build standard Decision Tree Model
decisiontree_model_1_SMOTE <- rpart(Performance.Tag ~ .,
                              data = decisiontree_data_train_SMOTE,
                              method = "class")                   
prp(decisiontree_model_1_SMOTE)

# Predict on Test Data
decisiontree_model_1_SMOTE_predicted <- predict(decisiontree_model_1_SMOTE, decisiontree_data_test, type = "class")

# Confusion Matrix
confusionMatrix(decisiontree_model_1_SMOTE_predicted, decisiontree_data_test$Performance.Tag)
# Accuracy : 65.52%
# Sensitivity: 65.82%
# Specificity: 58.47%

######## Use Information Gain
decisiontree_model_2_SMOTE <- rpart(Performance.Tag ~ .,         
                              data = decisiontree_data_train_SMOTE,     
                              method = "class",            
                              parms = list(split = "information"))
prp(decisiontree_model_2_SMOTE)

# Predict on Test Data
decisiontree_model_2_SMOTE_predicted <- predict(decisiontree_model_2_SMOTE, decisiontree_data_test, type = "class")

# Confusion Matrix
confusionMatrix(decisiontree_model_2_SMOTE_predicted, decisiontree_data_test$Performance.Tag)
# Accuracy : 65.52%
# Sensitivity: 65.82%
# Specificity: 58.47%

######## Tune Hyperparameters
decisiontree_model_3_SMOTE <- rpart(Performance.Tag ~ .,                          # formula
                                  data = decisiontree_data_train_SMOTE,           # training data
                                  method = "class",                         # classification or regression
                                  control = rpart.control(minsplit = 1000,  # min observations for node
                                                          minbucket = 1000, # min observations for leaf node
                                                          cp = 0.05))       # complexity parameter
prp(decisiontree_model_3_SMOTE)

# Predict on Test Data
decisiontree_model_3_SMOTE_predicted <- predict(decisiontree_model_3_SMOTE, decisiontree_data_test, type = "class")

# Confusion Matrix
confusionMatrix(decisiontree_model_3_SMOTE_predicted, decisiontree_data_test$Performance.Tag)
# Accuracy : 70.26%
# Sensitivity: 71.08%
# Specificity: 51.55%

######## Increase Complexity
decisiontree_model_4_SMOTE <- rpart(Performance.Tag ~ .,                          # formula
                              data = decisiontree_data_train_SMOTE,               # training data
                              method = "class",                             # classification or regression
                              control = rpart.control(minsplit = 1,         # min observations for node
                                                      minbucket = 1,        # min observations for leaf node
                                                      cp = 0.001))          # complexity parameter
prp(decisiontree_model_4_SMOTE)

# Predict on Test Data
decisiontree_model_4_SMOTE_predicted <- predict(decisiontree_model_4_SMOTE, decisiontree_data_test, type = "class")

# Confusion Matrix
confusionMatrix(decisiontree_model_4_SMOTE_predicted, decisiontree_data_test$Performance.Tag)
# Accuracy : 73.77%
# Sensitivity: 75.23%
# Specificity: 40.71%

######## Cross Test 
decisiontree_model_5_SMOTE <- train(Performance.Tag ~ .,
                              data = decisiontree_data_train_SMOTE,
                              method = "rpart",
                              metric = "Accuracy",
                              trControl = trainControl(method = "cv", number = 5),
                              tuneGrid = expand.grid(cp = seq(0, 0.02, 0.0025)),
                              control = rpart.control(minsplit = 50,
                                                      minbucket = 20))
# Predict on Test Data
decisiontree_model_5_SMOTE_predicted <- predict(decisiontree_model_5_SMOTE, decisiontree_data_test)

# Confusion Matrix
confusionMatrix(decisiontree_model_5_SMOTE_predicted, decisiontree_data_test$Performance.Tag)
# Accuracy : 74.36%
# Sensitivity: 75.72%
# Specificity: 43.46%

ggplot(data = data.frame(decisiontree_model_5_SMOTE$results), aes(x = cp, y = Accuracy*100)) +
  geom_line() +
  geom_point() +
  labs(x = "Complexity Parameter (CP)", y = "Accuracy", title = "CP vs Accuracy")



####################################################################################################
######################################### RANDOM FOREST ############################################
####################################################################################################

####################################### PREPARING DATA #############################################
########################################################################
########################## DEMOGRAPHIC DATA ############################
########################################################################
str(data)

# Duplicate data
randomforest_demographic_data <- data %>%
  dplyr::select(names(ddata[,-12]),Performance.Tag)

str(randomforest_demographic_data)

# Remove Application ID 
randomforest_demographic_data <- randomforest_demographic_data[ , -which(names(randomforest_demographic_data) %in% c("Application.ID"))]

# Separate Numeric and Categorical Variables and convert to numeric and factor respectively
numeric_variables <- c('Age', 'No.of.dependents', 'Income', 'No.of.months.in.current.residence', 'No.of.months.in.current.company')
categorical_variables <- c('Gender', 'Marital.Status..at.the.time.of.application.', 'Education', 'Profession',
                 'Type.of.residence', 'Performance.Tag')

randomforest_demographic_data[, numeric_variables] <- lapply(numeric_variables, function(x) as.numeric(as.character(randomforest_demographic_data[, x])))
randomforest_demographic_data[, categorical_variables] <- lapply(categorical_variables, function(x) as.factor(as.character(randomforest_demographic_data[, x])))

# Mix up the data to remove bias
set.seed(100)
randomforest_demographic_data <- randomforest_demographic_data[sample(nrow(randomforest_demographic_data)), ]

# Split into Train and Test
ntrain <- as.integer(nrow(randomforest_demographic_data)*0.7)
randomforest_demographic_data_train <- randomforest_demographic_data[1:ntrain, ]
randomforest_demographic_data_test <- randomforest_demographic_data[(ntrain+1):nrow(randomforest_demographic_data), ]

########################################################################
############################ COMBINED DATA #############################
########################################################################
str(data)

# Duplicate data
randomforest_data <- data
str(randomforest_data)

# Remove Age Group, Salary Group and ApplicationID
randomforest_data <- randomforest_data[ , -which(names(randomforest_data) %in% c("age_group","salary_group","Application.ID"))]

# Separate Numeric and Categorical Variables and convert to numeric and factor respectively
numeric_variables <- c('Age', 'No.of.dependents', 'Income', 'No.of.months.in.current.residence', 'No.of.months.in.current.company',
                        'No.of.times.90.DPD.or.worse.in.last.6.months', 'No.of.times.60.DPD.or.worse.in.last.6.months',
                        'No.of.times.30.DPD.or.worse.in.last.6.months', 'No.of.times.90.DPD.or.worse.in.last.12.months',
                        'No.of.times.60.DPD.or.worse.in.last.12.months', 'No.of.times.30.DPD.or.worse.in.last.12.months',
                        'Avgas.CC.Utilization.in.last.12.months','No.of.trades.opened.in.last.6.months',
                        'No.of.trades.opened.in.last.12.months', 'No.of.PL.trades.opened.in.last.6.months',
                        'No.of.PL.trades.opened.in.last.12.months', 'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.',
                        'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.', 'Outstanding.Balance',
                        'Total.No.of.Trades')

categorical_variables <- c('Gender', 'Marital.Status..at.the.time.of.application.', 'Education', 'Profession',
                            'Type.of.residence','Presence.of.open.auto.loan','Presence.of.open.home.loan','Performance.Tag')

randomforest_data[, numeric_variables] <- lapply(numeric_variables, function(x) as.numeric(as.character(randomforest_data[, x])))
randomforest_data[, categorical_variables] <- lapply(categorical_variables, function(x) as.factor(as.character(randomforest_data[, x])))

# Mix up the data to remove bias
set.seed(1)
randomforest_data <- randomforest_data[sample(nrow(randomforest_data)), ]

# Split into Train and Test
ntrain <- as.integer(nrow(randomforest_data)*0.7)
randomforest_data_train <- randomforest_data[1:ntrain, ]
randomforest_data_test <- randomforest_data[(ntrain+1):nrow(randomforest_data), ]

####################################### BUILDING MODELS ############################################
##################### MODEL 7 #######################
################# Demographic Data ##################
################## Without SMOTE ####################
# Build Random Forest Model
randomforest_demographic_model <- randomForest(Performance.Tag ~ ., data=randomforest_demographic_data_train,
                                                   proximity=FALSE, ntree=500, mtry=5, do.trace=TRUE, na.action=na.omit)
randomforest_demographic_model

# Predict on Test Data
randomforest_demographic_predicted <- predict(randomforest_demographic_model, newdata=randomforest_demographic_data_test)
table(randomforest_demographic_predicted, randomforest_demographic_data_test$Performance.Tag)

# randomforest_demographic_predicted     0     1
#                                  0 19704   865
#                                  1     0     0

# Confusion Matrix
confmatrix_demographic_randomforest <- confusionMatrix(randomforest_demographic_predicted, randomforest_demographic_data_test$Performance.Tag)
confmatrix_demographic_randomforest
# Accuracy : 95.79%
# Sensitivity: 100%
# Specificity: 0%

##################### MODEL 8 #######################
################# Demographic Data ##################
#################### With SMOTE #####################
# Check if data is balanced
table(randomforest_demographic_data_train$Performance.Tag)
#     0     1 
# 45966  2026
# The data is unbalanced

randomforest_demographic_data_train_SMOTE <- SMOTE(Performance.Tag ~ ., randomforest_demographic_data_train, perc.over = 100,k=5,perc.under=200)
table(randomforest_demographic_data_train_SMOTE$Performance.Tag)
#    0    1 
# 4052 4052 

# Build Random Forest Model
set.seed(100)
randomforest_demographic_model_SMOTE <- randomForest(Performance.Tag ~ ., data=randomforest_demographic_data_train_SMOTE, proximity=FALSE,
                                                ntree=1000, mtry=5, do.trace=TRUE, na.action=na.omit)
randomforest_demographic_model_SMOTE

# Find optimal value of mtry
optimal_mtry_demographic_SMOTE <- tuneRF(randomforest_demographic_data_train_SMOTE[,-11],randomforest_demographic_data_train_SMOTE$Performance.Tag,
                                         ntreeTry=500, stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)
best_mtry_demographic_SMOTE <- optimal_mtry_demographic_SMOTE[optimal_mtry_demographic_SMOTE[, 2] == min(optimal_mtry_demographic_SMOTE[, 2]), 1]
print(optimal_mtry_demographic_SMOTE)
print(best_mtry_demographic_SMOTE)

randomforest_demographic_model_SMOTE <- randomForest(Performance.Tag ~ ., data=randomforest_demographic_data_train_SMOTE, proximity=FALSE,
                                                ntree=200, mtry=best_mtry_demographic_SMOTE, do.trace=TRUE, na.action=na.omit)

# Predict on Test Data
randomforest_demographic_predicted_SMOTE <- predict(randomforest_demographic_model_SMOTE, newdata=randomforest_demographic_data_test)
table(randomforest_demographic_predicted_SMOTE, randomforest_demographic_data_test$Performance.Tag)

# randomforest_demographic_predicted_SMOTE     0     1
#                                        0 14762   551
#                                        1  4942   314

#Confusion Matrix
confmatrix_demographic_randomforest_SMOTE <- confusionMatrix(randomforest_demographic_predicted_SMOTE, randomforest_demographic_data_test$Performance.Tag)
confmatrix_demographic_randomforest_SMOTE
# Accuracy : 73.29%
# Sensitivity: 74.91%
# Specificity: 36.30%

##################### MODEL 9 #######################
################## Combined Data ####################
################## Without SMOTE ####################
# Build Random Forest Model
randomforest_model <- randomForest(Performance.Tag ~ ., data=randomforest_data_train, proximity=FALSE,
                                       ntree=500, mtry=5, do.trace=TRUE, na.action=na.omit)
randomforest_model

# Find optimal value of mtry
optimal_mtry <- tuneRF(randomforest_data_train[,-28],randomforest_data_train$Performance.Tag,
                       ntreeTry=200, stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)
best_mtry <- optimal_mtry[optimal_mtry[, 2] == min(optimal_mtry[, 2]), 1]
print(optimal_mtry)
print(best_mtry)

randomforest_model <- randomForest(Performance.Tag ~ ., data=randomforest_data_train, proximity=FALSE,
                                       ntree=200, mtry=best_mtry, do.trace=TRUE, na.action=na.omit)

# Predict on Test Data
randomforest_predicted <- predict(randomforest_model, newdata=randomforest_data_test)
table(randomforest_predicted, randomforest_data_test$Performance.Tag)

# randomforest_predicted     0     1
#                      0 19652   917
#                      1     0     0

# Confusion Matrix
confmatrix_randomforest <- confusionMatrix(randomforest_predicted, randomforest_data_test$Performance.Tag)
confmatrix_randomforest
# Accuracy : 95.54%
# Sensitivity: 100%
# Specificity: 0%

##################### MODEL 10 ######################
################## Combined Data ####################
#################### With SMOTE #####################
# Check if data is balanced
table(randomforest_data_train$Performance.Tag)
#     0     1 
# 46018  1974 
# The data is unbalanced

randomforest_data_train_SMOTE <- SMOTE(Performance.Tag ~ ., randomforest_data_train, perc.over = 100,k=5,perc.under=200)
table(randomforest_data_train_SMOTE$Performance.Tag)
#    0    1 
# 3948 3948 

# Build Random Forest Model
set.seed(1)
randomforest_model_SMOTE <- randomForest(Performance.Tag ~ ., data=randomforest_data_train_SMOTE, 
                                   proximity=FALSE, ntree=500, mtry=5, do.trace=TRUE, na.action=na.omit)
randomforest_model_SMOTE

# Find optimal value of mtry
optimal_mtry_SMOTE <- tuneRF(randomforest_data_train_SMOTE[,-28],randomforest_data_train_SMOTE$Performance.Tag, ntreeTry=500,
                     stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best_mtry_SMOTE <- optimal_mtry_SMOTE[optimal_mtry_SMOTE[, 2] == min(optimal_mtry_SMOTE[, 2]), 1]
print(optimal_mtry_SMOTE)
print(best_mtry_SMOTE)

randomforest_model_SMOTE <- randomForest(Performance.Tag ~ ., data=randomforest_data_train_SMOTE, proximity=FALSE,
                                    ntree=200, mtry=best_mtry_SMOTE, do.trace=TRUE, na.action=na.omit)

# Predict on Test Data
randomforest_predicted_SMOTE <- predict(randomforest_model_SMOTE, newdata=randomforest_data_test)
table(randomforest_predicted_SMOTE, randomforest_data_test$Performance.Tag)

# randomforest_predicted_SMOTE     0     1
#                            0 15471   576
#                            1  4181   341

# Confusion Matrix
confmatrix_randomforest_SMOTE <- confusionMatrix(randomforest_predicted_SMOTE, randomforest_data_test$Performance.Tag)
confmatrix_randomforest_SMOTE
# Accuracy : 76.87%
# Sensitivity: 78.72%
# Specificity: 37.18%



####################################################################################################
#################################### APPLICATION SCORECARD #########################################
####################################################################################################
# Predict default and non-default probabilities
randomforest_data_test$predict_default <- predict(randomforest_model_SMOTE, newdata = randomforest_data_test,type='prob')
randomforest_data_test$predict_non_default <- (1-(randomforest_data_test$predict_default))

randomforest_data_test$odds <-  log(randomforest_data_test$predict_non_default/randomforest_data_test$predict_default)

PDO <- 20
BaseScore <- 400
Odds <- 10

#Calculating Factor & Offset
factor_value=PDO/log(2)

offset_value=BaseScore-(Factor*log(Odds))

print("equation is : score = 333.5614 + (28.8539 * log(odds))")

randomforest_data_test$score <- offset_value + (factor_value * randomforest_data_test$odds)

summary(randomforest_data_test$score)

quantile(randomforest_data_test$score,seq(0,1,0.2))

# Set Cutoff at 358 based on quantile values
scorecard_cutoff_value = 358

# Find out Percentage of people identified if cutoff is set at 358
people_default_below_cutoff <- length(which(randomforest_data_test$Performance.Tag==1 & randomforest_data_test$score[,2]<scorecard_cutoff_value))
people_default_total<-length(which(randomforest_data_test$Performance.Tag==1))

percentage_people_default_under_cutoff <- ceiling((people_default_below_cutoff/people_default_total)*100)

percentage_people_default_under_cutoff

#Plotting the score distribution for all the applicants of the test data
ggplot(randomforest_data_test, aes(x = score[,2],color=Performance.Tag))+geom_bar()+geom_vline(aes(xintercept = scorecard_cutoff_value))+
  labs(x="Score",y="Count",title="Score Distribution for all applicants")+
  annotate("text", x=350,y=4000, colour = "black",hjust=0, vjust=0, size=4,label=paste("Defaults covered by cut off of 358: " ,percentage_people_default_under_cutoff,"%"))


############################# PREDICT ON OUTRIGHT REJECTED CANDIDATES ##############################

pref_tag_NA_candidates <- data_with_perf_tag_na
# Remove ApplicationID
pref_tag_NA_candidates <- pref_tag_NA_candidates[ , -which(names(pref_tag_NA_candidates) %in% c("Application.ID"))]

# Separate Numeric and Categorical Variables and convert to numeric and factor respectively
numeric_variables <- c('Age', 'No.of.dependents', 'Income', 'No.of.months.in.current.residence', 'No.of.months.in.current.company',
                        'No.of.times.90.DPD.or.worse.in.last.6.months', 'No.of.times.60.DPD.or.worse.in.last.6.months',
                        'No.of.times.30.DPD.or.worse.in.last.6.months', 'No.of.times.90.DPD.or.worse.in.last.12.months',
                        'No.of.times.60.DPD.or.worse.in.last.12.months', 'No.of.times.30.DPD.or.worse.in.last.12.months',
                        'Avgas.CC.Utilization.in.last.12.months','No.of.trades.opened.in.last.6.months',
                        'No.of.trades.opened.in.last.12.months', 'No.of.PL.trades.opened.in.last.6.months',
                        'No.of.PL.trades.opened.in.last.12.months', 'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.',
                        'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.', 'Outstanding.Balance',
                        'Total.No.of.Trades')

categorical_variables <- c('Gender', 'Marital.Status..at.the.time.of.application.', 'Education', 'Profession',
                            'Type.of.residence','Presence.of.open.auto.loan','Presence.of.open.home.loan')

pref_tag_NA_candidates[, numeric_variables] <- lapply(numeric_variables, function(x) as.numeric(as.character(pref_tag_NA_candidates[, x])))
pref_tag_NA_candidates[, categorical_variables] <- lapply(categorical_variables, function(x) as.factor(as.character(pref_tag_NA_candidates[, x])))





pref_tag_NA_candidates$predict_default <- predict(randomforest_model_SMOTE, newdata = pref_tag_NA_candidates,type='prob')
pref_tag_NA_candidates$predict_non_default <- (1-(pref_tag_NA_candidates$predict_default))

pref_tag_NA_candidates$odds <-  log(pref_tag_NA_candidates$predict_non_default/pref_tag_NA_candidates$predict_default)
pref_tag_NA_candidates$score <- offset_value + (factor_value * pref_tag_NA_candidates$odds)

people_default_below_cutoff <- length(which(pref_tag_NA_candidates$score[,2]<scorecard_cutoff_value))
people_default_total<-nrow(pref_tag_NA_candidates)

percentage_people_default_under_cutoff <- (people_default_below_cutoff/people_default_total)*100

percentage_people_default_under_cutoff

#Plotting the score distribution for all the applicants of the test data
ggplot(randomforest_data_test, aes(x = score[,2],color=Performance.Tag))+geom_bar()+geom_vline(aes(xintercept = scorecard_cutoff_value))+
  labs(x="Score",y="Count",title="Score Distribution for all applicants")+
  annotate("text", x=350,y=4000, colour = "black",hjust=0, vjust=0, size=4,label=paste("Defaults covered by cut off of 358: " ,percentage_people_default_under_cutoff,"%"))
