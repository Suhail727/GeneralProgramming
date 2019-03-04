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


model_data <- data_WOE
#Removing ID column
model_data <- model_data[,-which(names(model_data) %in% c("Application.ID"))]

#Splitting into train and test data
set.seed(1)
split_indices <- sample.split(model_data$Performance.Tag, SplitRatio = 0.70)

train_WOE <- model_data[split_indices, ]
test_WOE <- model_data[!split_indices, ]

#Initial Model 
logistic_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train_WOE) 
summary(logistic_1) #AIC: 16180

#StepAIC run
logistic_2 <- stepAIC(logistic_1, direction = "both")
summary(logistic_2) #AIC: 16040

logistic_3 <- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                    Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,family = "binomial", data= train_WOE)
summary(logistic_3)
vif(logistic_3)


### Model Evaluation

### Test Data ####

temp_model_1<- logistic_3
#predicted probabilities of Performance for test data

test_pred = predict(temp_model_1, type = "response", 
                    newdata = test_WOE[,-35])


# Let's see the summary 

summary(test_pred)

# Add the prediction probability with the test data set for further model evaluation steps.
test_WOE$prob <- test_pred

# View the test dataset including prediction probabity.
View(test_WOE)

# Let's use the probability cutoff of 50%.

test_pred_performance <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_performance <- factor(ifelse(test_WOE$Performance.Tag==1,"Yes","No"))


table(test_actual_performance,test_pred_performance)
#test_pred_performance
#test_actual_performance    No
#No  19693
#Yes   867

test_conf <- confusionMatrix(test_pred_performance, test_actual_performance, positive = "Yes")
test_conf

perform_fn <- function(cutoff) 
{
  predicted_performance <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_performance, test_actual_performance, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability
summary(test_pred)

# create 100s of sequence number between 0.01 and 0.80, which will be used as cut-off value
# in each iteration to find out optimal value.
s = seq(.01,.80,length=100)

# Initialize a 100x3 matrix with 0 as default value.
OUT = matrix(0,100,3)

# Call the perform_fn in a loop and assign the output to OUT matrix for "sensitivity", "specificity", "accuracy".
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2) +
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  lines(s,OUT[,2],col="darkgreen",lwd=2) +
  lines(s,OUT[,3],col=4,lwd=2) 


# Add legends to the plot created earlier.

legend(0.60,0.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy")) 

# Add a box over the plot.
box()

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.12)]


# Let's choose a cutoff value of 0.0498 for final model

test_cutoff_performance <- factor(ifelse(test_pred >=0.0498, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_performance, test_actual_performance, positive = "Yes")

# Get Accuracy, Sensitivity and Specificity from the confusion matrix using 
# cut-off value - 0.1776.
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec


####################### MODEL 2 ###################################
############ LOGISTIC REGRESSION ON DEMOGRAPHIC DATASET ###########
ddata_clean <- data %>%
  dplyr::select(names(ddata[,-12]),Performance.Tag)

# # Bringing the variables in the correct format
# #Gender
# ddata_clean$Gender <- as.factor(ddata_clean$Gender)

# #Marital.Status..at.the.time.of.application.
# ddata_clean$Marital.Status..at.the.time.of.application. <- as.factor(ddata_clean$Marital.Status..at.the.time.of.application.)

# #Education
# ddata_clean$Education <- as.factor(ddata_clean$Education)

# #Profession
# ddata_clean$Profession <- as.factor(ddata_clean$Profession)

# #Type.of.residence
# ddata_clean$Type.of.residence <- as.factor(ddata_clean$Type.of.residence)

# #Performance.Tag
# ddata_clean$Performance.Tag <- as.factor(ddata_clean$Performance.Tag)

# # Normalising continuous variables 
# ddata_clean$Age<- scale(ddata_clean$Age)
# ddata_clean$No.of.dependents<- scale(ddata_clean$No.of.dependents)
# ddata_clean$Income<- scale(ddata_clean$Income)
# ddata_clean$No.of.months.in.current.residence<- scale(ddata_clean$No.of.months.in.current.residence)
# ddata_clean$No.of.months.in.current.company<- scale(ddata_clean$No.of.months.in.current.company)

# # Subsetting the categorical variables to a dataframe
# ddata_clean_categorical<- 
#   ddata_clean[,c('Gender','Marital.Status..at.the.time.of.application.','Education'
#                          ,'Profession', 'Type.of.residence')]

# # Converting the categorical variables to factor
# ddata_clean_fact<- data.frame(sapply(ddata_clean_categorical, function(x) factor(x)))

# # Creating dummy variables for categorical columns
# dummies<- data.frame(sapply(ddata_clean_fact, 
#                             function(x) data.frame(model.matrix(~x-1,data =ddata_clean_fact))[,-1]))

# # The Final dataset
# ddata_clean_final<- cbind(ddata_clean[,-c(1,3,4,7,8,9)],dummies) 
# View(ddata_clean_final) #4300 variables of 52 variables

#Splitting the data to create Train and Test data
set.seed(100)

indices <- sample.split(ddata_clean_final$'Performance.Tag', SplitRatio = 0.7)

train <- ddata_clean_final[indices,]
test <- ddata_clean_final[!(indices), ]

#Model Creation

#Initial model
model_1 <- glm(Performance.Tag ~ ., data = train, family = "binomial")
summary(model_1) #AIC: 16704

#Step-wise selection
model_2 <- stepAIC(model_1, direction = "both")
summary(model_2) #AIC: 16682

#Removing Education.xOthers as this variable is less significant
model_3 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                 No.of.months.in.current.company + Profession.xSE, data = train, family = 'binomial')
summary(model_3) #AIC : 16683
vif(model_3)

#Removing Profession.xSE as this variable is having comparatively high p value
model_4 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + No.of.months.in.current.company,
               data = train, family = 'binomial')
summary(model_4) #AIC : 16684
#With 3 significant variables in the model
temp_demogrpahic_model1 <- model_4


### Model Evaluation

#predicted probabilities of Performance for test data
test_pred = predict(temp_demogrpahic_model1, type = "response", 
                    newdata = test[,-6])  

# Let's see the summary 
summary(test_pred)

# Add the prediction probability with the test data set for further model evaluation steps.
test$prob <- test_pred

# View the test dataset including prediction probabity.
View(test)

# Let's use the probability cutoff of 50%.
test_pred_performance <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_performance <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))

table(test_actual_performance,test_pred_performance)
#                       test_pred_performance
#test_actual_performance    No
#No  19693
#Yes   867

test_conf <- confusionMatrix(test_pred_performance, test_actual_performance, positive = "Yes")
test_conf

#Accuracy : 95%
#Sensitivity : 0%       
#Specificity : 100%

# Here we can see accuaracy of the model is on higher side (95%) but sensitivity is very less.
# As per business scenario we have to predict Performance rate hence Sensitivity need to be on
# on the higher side.  

# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability
summary(test_pred)

# create 100s of sequence number between 0.01 and 0.80, which will be used as cut-off value
# in each iteration to find out optimal value.
s = seq(.01,.80,length=100)

# Initialize a 100x3 matrix with 0 as default value.
OUT = matrix(0,100,3)

# Call the perform_fn in a loop and assign the output toOUT matrix for "sensitivity", "specificity", "accuracy".
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2) +
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  lines(s,OUT[,2],col="darkgreen",lwd=2) +
  lines(s,OUT[,3],col=4,lwd=2) 

# Add legends to the plot created earlier.

legend(0.60,0.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy")) 

# Add a box over the plot.
box()

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.12)]


# Let's choose a cutoff value of 0.0419 for final model

test_cutoff_performance <- factor(ifelse(test_pred >=0.0419, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_performance, test_actual_performance, positive = "Yes")

# Get Accuracy, Sensitivity and Specificity from the confusion matrix using 

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

# Show Accuracy
acc
# Accuracy : 0.522
# Show Sensitivity
sens
# Sensitivity : 0.597
# Show Specificity
spec
# Specificity : 0.5196


#################################### MODEL 3 ###################################
############ LOGISTIC REGRESSION ON DEMOGRAPHIC DATASET (WITH SMOTE) ###########
demo_smote_train <- train
table(demo_smote_train$Performance.Tag)
#Clearly the dataset is unbalanced with number of 0's being much more than number of 1's

train_demo_SMOTE <- SMOTE(Performance.Tag ~ ., demo_smote_train, perc.over = 100,perc.under=200)
table(train_demo_SMOTE$Performance.Tag)
#Now the dataset is evenly balanced with equal number of 0 and 1

#Model Building
logistic_1_Demo_SMOTE <- glm(Performance.Tag ~ ., family = "binomial", data = train_demo_SMOTE) 
summary(logistic_1_Demo_SMOTE)   #AIC: 11139
vif(logistic_1_Demo_SMOTE)

# Using stepwise algorithm for removing insignificant variables   
logistic_2_Demo_SMOTE <- stepAIC(logistic_1_Demo_SMOTE, direction = "both")
summary(logistic_2_Demo_SMOTE) #AIC: 11123
vif(logistic_2_Demo_SMOTE)
# Removing Age as this variable is not significant
logistic_3_Demo_SMOTE <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.company + 
                               Education.xMasters + Education.xPhd + Profession.xSE, family = "binomial", 
                             data = train_demo_SMOTE)
summary(logistic_3_Demo_SMOTE) #AIC: 11123

#With 5 significant variables in the model
temp_demogrpahic_SMOTE_log_model1 <- logistic_3_Demo_SMOTE

### Model Evaluation

test_demo_SMOTE <- test
#predicted probabilities of Performance for test data
test_pred = predict(temp_demogrpahic_SMOTE_log_model1, type = "response", 
                    newdata = test_demo_SMOTE[,-6])  

# Let's see the summary 
summary(test_pred)

#Removing prob column from test_demo_SMOTE. This column refers to the probabilty of logistic model without SMOTE
test_demo_SMOTE <- test_demo_SMOTE[ , -which(names(test_demo_SMOTE) %in% c("prob"))]

# Add the prediction probability with the test data set for further model evaluation steps.
test_demo_SMOTE$prob <- test_pred

# View the test dataset including prediction probabity.
View(test_demo_SMOTE)

# Let's use the probability cutoff of 50%.
test_pred_performance <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_performance <- factor(ifelse(test_demo_SMOTE$Performance.Tag==1,"Yes","No"))

table(test_actual_performance,test_pred_performance)
#test_pred_performance
#test_actual_performance    No   Yes
#No  10366  9327
#Yes   350   517

test_conf <- confusionMatrix(test_pred_performance, test_actual_performance, positive = "Yes")
test_conf

#Accuracy : 53%
#Sensitivity : 59%       
#Specificity : 52%

# Here we can see accuaracy, sensitivity and specificity of the model is too low.

# Creating cutoff values from 0.33 to 0.62 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability
summary(test_pred)

# create 100s of sequence number between 0.33 and 0.62, which will be used as cut-off value
# in each iteration to find out optimal value.
s = seq(.33,.62,length=100)

# Initialize a 100x3 matrix with 0 as default value.
OUT = matrix(0,100,3)

# Call the perform_fn in a loop and assign the output toOUT matrix for "sensitivity", "specificity", "accuracy".
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2) +
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  lines(s,OUT[,2],col="darkgreen",lwd=2) +
  lines(s,OUT[,3],col=4,lwd=2) 

# Add legends to the plot created earlier.
legend(0.60,0.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy")) 

# Add a box over the plot.
box()

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.1)]

# Let's choose a cutoff value of 0.51 for final model

test_cutoff_performance <- factor(ifelse(test_pred >=0.51, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_performance, test_actual_performance, positive = "Yes")

# Get Accuracy, Sensitivity and Specificity from the confusion matrix using 

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

# Show Accuracy
acc
# Accuracy : 0.58
# Show Sensitivity
sens
# Sensitivity : 0.52
# Show Specificity
spec
# Specificity : 0.58


#################################### MODEL 4 ###################################
############ LOGISTIC REGRESSION ON MERGED DATASET (WITH SMOTE) ################
combined_SMOTE_log_train <- train_WOE #Taking a backup
str(combined_SMOTE_log_train)
table(combined_SMOTE_log_train$Performance.Tag)
#0     1 
#45969  2024 
#Clearly the dataset is not balanced because number of 0 is more than number of 1.
combined_SMOTE_log_train$Gender = as.factor(combined_SMOTE_log_train$Gender)
combined_SMOTE_log_train$Marital.Status..at.the.time.of.application. = as.factor(combined_SMOTE_log_train$Marital.Status..at.the.time.of.application.)
combined_SMOTE_log_train$Education = as.factor(combined_SMOTE_log_train$Education)
combined_SMOTE_log_train$Profession = as.factor(combined_SMOTE_log_train$Profession)
combined_SMOTE_log_train$Type.of.residence = as.factor(combined_SMOTE_log_train$Type.of.residence)
combined_SMOTE_log_train$Performance.Tag = as.factor(combined_SMOTE_log_train$Performance.Tag)
train_merged_SMOTE <- SMOTE(Performance.Tag ~ ., combined_SMOTE_log_train, perc.over = 100,k=5,perc.under=200)
table(train_merged_SMOTE$Performance.Tag)

#0    1 
#4048 4048 
# The dataset is now balanced with equal number of 0 and 1.
# Variable data are getting distorted. Need to confirm with mentor

#Model Building
logistic_1_Merged_SMOTE <- glm(Performance.Tag ~ ., family = "binomial", data = train_merged_SMOTE) 
summary(logistic_1_Merged_SMOTE)   #AIC: 9929
vif(logistic_1_Merged_SMOTE)

# Using stepwise algorithm for removing insignificant variables   
logistic_2_Merged_SMOTE <- stepAIC(logistic_1_Merged_SMOTE, direction = "both")
summary(logistic_2_Merged_SMOTE) #AIC: 9877
vif(logistic_2_Merged_SMOTE)


#Removing Outstanding.Balance due to very less significance value and high vif value
logistic_3 <- glm(formula = Performance.Tag ~ Gender + Marital.Status..at.the.time.of.application. + 
      Income + Education + Profession + Type.of.residence + No.of.months.in.current.company + 
      No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.30.DPD.or.worse.in.last.6.months + 
      No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
      Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
      No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
      Presence.of.open.home.loan + Presence.of.open.auto.loan, 
    family = "binomial", data = train_merged_SMOTE)
summary(logistic_3)
vif(logistic_3)

#Removing No.of.times.30.DPD.or.worse.in.last.6.months due to very less significance value and high vif value
logistic_4 <- glm(formula = Performance.Tag ~ Gender + Marital.Status..at.the.time.of.application. + 
                    Income + Education + Profession + Type.of.residence + No.of.months.in.current.company + 
                    No.of.times.90.DPD.or.worse.in.last.6.months+
                    No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                    Presence.of.open.home.loan + Presence.of.open.auto.loan, 
                  family = "binomial", data = train_merged_SMOTE)
summary(logistic_4)
vif(logistic_4)

#Removing No.of.Inquiries.in.last.12.months..excluding.home...auto.loans due to very less significance value and high vif value
logistic_5 <- glm(formula = Performance.Tag ~ Gender + Marital.Status..at.the.time.of.application. + 
                    Income + Education + Profession + Type.of.residence + No.of.months.in.current.company + 
                    No.of.times.90.DPD.or.worse.in.last.6.months+
                    No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                    Presence.of.open.home.loan + Presence.of.open.auto.loan, 
                  family = "binomial", data = train_merged_SMOTE)
summary(logistic_5)
vif(logistic_5)

#Removing No.of.trades.opened.in.last.12.months due to very less significance value and high vif value
logistic_6 <- glm(formula = Performance.Tag ~ Gender + Marital.Status..at.the.time.of.application. + 
                    Income + Education + Profession + Type.of.residence + No.of.months.in.current.company + 
                    No.of.times.90.DPD.or.worse.in.last.6.months+
                    No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    Avgas.CC.Utilization.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                    Presence.of.open.home.loan + Presence.of.open.auto.loan, 
                  family = "binomial", data = train_merged_SMOTE)
summary(logistic_6)
vif(logistic_6)

#Removing No.of.times.90.DPD.or.worse.in.last.6.months due to very less significance value and high vif value
logistic_7 <- glm(formula = Performance.Tag ~ Gender + Marital.Status..at.the.time.of.application. + 
                    Income + Education + Profession + Type.of.residence + No.of.months.in.current.company + 
                    No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    Avgas.CC.Utilization.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                    Presence.of.open.home.loan + Presence.of.open.auto.loan, 
                  family = "binomial", data = train_merged_SMOTE)
summary(logistic_7)
vif(logistic_7)

#Removing Income due to very less significance value and high vif value
logistic_8 <- glm(formula = Performance.Tag ~ Gender + Marital.Status..at.the.time.of.application. + 
                    Education + Profession + Type.of.residence + No.of.months.in.current.company + 
                    No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    Avgas.CC.Utilization.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                    Presence.of.open.home.loan + Presence.of.open.auto.loan, 
                  family = "binomial", data = train_merged_SMOTE)
summary(logistic_8)
vif(logistic_8)

#Removing Income due to very less significance value and high vif value
logistic_9 <- glm(formula = Performance.Tag ~ Gender + Marital.Status..at.the.time.of.application. + 
                    Profession + Type.of.residence + No.of.months.in.current.company + 
                    No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    Avgas.CC.Utilization.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                    Presence.of.open.home.loan + Presence.of.open.auto.loan, 
                  family = "binomial", data = train_merged_SMOTE)
summary(logistic_9)
vif(logistic_9)

#Removing Profession due to very less significance value and high vif value
logistic_10 <- glm(formula = Performance.Tag ~ Gender + Marital.Status..at.the.time.of.application. + 
                    Type.of.residence + No.of.months.in.current.company + 
                    No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    Avgas.CC.Utilization.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                    Presence.of.open.home.loan + Presence.of.open.auto.loan, 
                  family = "binomial", data = train_merged_SMOTE)
summary(logistic_10)
vif(logistic_10)

#Removing No.of.months.in.current.company due to very less significance value and high vif value
logistic_11 <- glm(formula = Performance.Tag ~ Gender + Marital.Status..at.the.time.of.application. + 
                     Type.of.residence +
                     No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                     Avgas.CC.Utilization.in.last.12.months + 
                     No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                     Presence.of.open.home.loan + Presence.of.open.auto.loan, 
                   family = "binomial", data = train_merged_SMOTE)
summary(logistic_11)
vif(logistic_11)
#With 6 significant variables in the model
temp_merged_SMOTE_log_model1 <- logistic_11


### Model Evaluation

test_merged_SMOTE <- test_No_WOE
test_merged_SMOTE <- test_merged_SMOTE[ , -which(names(test_merged_SMOTE) %in% c("prob"))]

#predicted probabilities of Performance for test data
test_pred = predict(temp_merged_SMOTE_log_model1, type = "response", 
                    newdata = test_merged_SMOTE[,-35])  

# Let's see the summary 
summary(test_pred)


# Add the prediction probability with the test data set for further model evaluation steps.
test_merged_SMOTE$prob <- test_pred

# View the test dataset including prediction probabity.
View(test_merged_SMOTE)

# Let's use the probability cutoff of 50%.
test_pred_performance <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_performance <- factor(ifelse(test_merged_SMOTE$Performance.Tag==1,"Yes","No"))

table(test_actual_performance,test_pred_performance)
#test_pred_performance
#test_actual_performance    No   Yes
#No  11706  7987
#Yes   270   597

test_conf <- confusionMatrix(test_pred_performance, test_actual_performance, positive = "Yes")
test_conf

#Accuracy : 60%
#Sensitivity : 68%       
#Specificity : 60%
# Here we can see accuaracy, sensitivity and specificity of the model is too low.

# Creating cutoff values from 0.27 to 0.91 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability
summary(test_pred)

# create 100s of sequence number between 0.27 and 0.91, which will be used as cut-off value
# in each iteration to find out optimal value.
s = seq(.27,.91,length=100)

# Initialize a 100x3 matrix with 0 as default value.
OUT = matrix(0,100,3)

# Call the perform_fn in a loop and assign the output toOUT matrix for "sensitivity", "specificity", "accuracy".
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2) +
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  lines(s,OUT[,2],col="darkgreen",lwd=2) +
  lines(s,OUT[,3],col=4,lwd=2) 

# Add legends to the plot created earlier.
legend(0.60,0.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy")) 

# Add a box over the plot.
box()

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.015)]

# Let's choose a cutoff value of 0.528 for final model

test_cutoff_performance <- factor(ifelse(test_pred >=0.522, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_performance, test_actual_performance, positive = "Yes")

# Get Accuracy, Sensitivity and Specificity from the confusion matrix using 

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

# Show Accuracy
acc
# Accuracy : 0.64
# Show Sensitivity
sens
# Sensitivity : 0.63
# Show Specificity
spec
# Specificity : 0.64










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
##################### MODEL x #######################
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

##################### MODEL x #######################
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

##################### MODEL x #######################
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

##################### MODEL x #######################
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
##################### MODEL x #######################
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

##################### MODEL x #######################
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
logistic_demographic_model_1 <- glm(Performance.Tag ~ ., data = train, family = "binomial")
summary(logistic_demographic_model_1) 
# AIC: 16705

# Step AIC
logistic_demographic_model_2 <- stepAIC(logistic_demographic_model_1, direction = "both")
summary(logistic_demographic_model_2)
vif(logistic_demographic_model_2)
# AIC: 16683

# Remove Education.xOthers due to low p-value
logistic_demographic_model_3 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                                No.of.months.in.current.company + Profession.xSE,
                                data = train, family = "binomial")
summary(logistic_demographic_model_3)
vif(logistic_demographic_model_3)
# AIC: 16683

# Remove Profession.xSE due to low p-value
logistic_demographic_model_4 <- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                                No.of.months.in.current.company,
                                data = train, family = "binomial")
summary(logistic_demographic_model_4)
vif(logistic_demographic_model_4)
# AIC: 16684

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
confusion_matrix_iteration <- function(cutoff) 
{
  predicted <- factor(ifelse(logistic_demographic_model_predicted >= cutoff, "Yes", "No"))
  conf_matrix <- confusionMatrix(predicted, actual, positive = "Yes")
  accuracy <- conf_matrix$overall[1]
  sensitivity <- conf_matrix$byClass[1]
  specificity <- conf_matrix$byClass[2]
  matrix_row <- t(as.matrix(c(sensitivity, specificity, accuracy))) 
  colnames(matrix_row) <- c("Sensitivity", "Specificity", "Accuracy")
  return(matrix_row)
}

calculate_cutoff <- function()
{
  s = seq(.01,.80,length=100)
  # Initialize a 100x3 matrix with 0 as default value.
  cutoff_matrix = matrix(0,100,3)
  # Call the perform_fn in a loop and assign the output to OUT matrix for "sensitivity", "specificity", "accuracy".
  for(i in 1:100)
  {
    cutoff_matrix[i,] = confusion_matrix_iteration(s[i])
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
  cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.12)]
  return(cutoff)
}

cutoff <- calculate_cutoff()


predicted <- factor(ifelse(logistic_demographic_model_predicted >=cutoff, "Yes", "No"))
confusionMatrix(predicted, actual, positive = "Yes")
# Accuracy : 52.45%
# Sensitivity : 59.74%       
# Specificity : 52.12%