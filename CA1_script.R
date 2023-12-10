#PREPARATION
setwd("~/College Classes/CCT/Data Exploration & Preparation/CA1")
#Loading the data set
data <- read.csv("./STATS.Terrorism/globalterrorismdb_0522dist.csv")
View(data)
variables <- colnames(data)
summary(data)
variables

#install.packages("dplyr")
library(dplyr)

#Cleaning of explanatory text variables
data <- select(data, -location, -summary,-motive, -weapdetail, -propcomment, -addnotes, -scite1, -scite2, -scite3)


#Creating a single column for date yyyy/mm/dd
date <- as.Date(paste(data$iyear, data$imonth, data$iday, sep = "-"))
data <- data %>%
  mutate(date = date) %>%
  select(eventid, date, everything())
data <- select(data, -iyear, -imonth, -iday)
data <- select(data, -approxdate)

#Cleaning variables that are not part of our analysis
data <- select(data, -latitude, -longitude, -specificity, -nkillus, -nwoundus, -nhostkidus, -ransomamtus, -ransomamt, -ransompaidus, -dbsource)

#Replace empty spaces with NA
data <- data %>% mutate_at(-c(1, 2), ~ ifelse(. == "", NA, .))

#Remove columns with more than 80% missing values excluding some columns we want to keep
columns_to_exclude <- c("eventid", "propextent_txt", "propvalue", "nkillter", "nwoundte")
data_subframe <- select(data, columns_to_exclude)
data_cleaned <- data %>%
  select_if(~ sum(!is.na(.)) / length(.) > 0.8)
View(data_subframe)
View(data_cleaned)
View(data)
data_merge <- merge(data_cleaned, data_subframe, by = "eventid", all = TRUE)
View(data_merge)
data <- data_merge

#Remove columns codes
data_cleaned <- select(data, -country, -region,-attacktype1, -targtype1, -targsubtype1, -natlty1, -weaptype1, -weapsubtype1)
data <- data_cleaned

#Remove columns that are not part of our analysis
data_cleaned <- select(data, -extended, -vicinity, -doubtterr, -ishostkid, -multiple, -guncertain1, -individual, -INT_LOG, -INT_IDEO, -INT_MISC, -INT_ANY)
data <- data_cleaned

#Decode variables
data_cleaned$crit1 <- as.logical(data$crit1)
data_cleaned$crit2 <- as.logical(data$crit2)
data_cleaned$crit3 <- as.logical(data$crit3)
data_cleaned$success <- as.logical(data$success)
data_cleaned$suicide <- as.logical(data$suicide)
data_cleaned$property <- ifelse(data$property == 1, TRUE, ifelse(data$property == 0, FALSE, "Unknown"))
data <- data_cleaned
summary(data)

# nwound & nkill NA to 0
data$nkill[is.na(data$nkill)] <- 0
data$nkillter[is.na(data$nkillter)] <- 0
data$nwound[is.na(data$nwound)] <- 0
data$nwoundte[is.na(data$nwoundte)] <- 0
data$propvalue[is.na(data$propvalue)] <- 0

#Merge of wound and killed columns
data$total_wound <- data$nwound + data$nwoundte
data$total_kill <- data$nkill + data$nkillter
data <- select(data, -nwound, -nwoundte, -nkillter, -nkill)
count_0_kill <- table(data$total_kill)["0"]
count_0_wound <- table(data$total_wound)["0"]

#Data propvalue -99 to 0 and counting them
#http://127.0.0.1:41253/graphics/plot_zoom_png?width=1920&height=1009
count_prop_unknown <- table(data$propvalue)["-99"]
data$propvalue <- ifelse(data$propvalue == -99, 0, data$propvalue)
count_0_prop <- table(data$propvalue)["0"]

#Checking for outliers
library(ggplot2)
ggplot(data = data, mapping = aes(y = total_wound)) + geom_boxplot()
ggplot(data = data, mapping = aes(y = total_kill)) + geom_boxplot()
ggplot(data = data, mapping = aes(y = propvalue)) + geom_boxplot()
summary(data)
propValue_max <- data[which.max(data$propvalue), ]
total_kill_max <- data[which.max(data$total_kill), ]
total_wound_max <- data[which.max(data$total_wound), ]

#Export the Data set
write.csv(data, "./STATS.Terrorism/globalterrorism_prepared.csv", row.names=FALSE)


#EDA
#Reload the Data set
data <- read.csv("./STATS.Terrorism/globalterrorism_prepared.csv")


year <- as.POSIXct(data$date, format = "%Y-%m-%d")
year <- format(year, format="%Y")
decade <- floor(as.numeric(year) / 10) * 10
data$decade <- decade

data$casualties <- data$total_kill + data$total_wound

library(ggplot2)
library(dplyr)
library(reshape2)

#Min-Max normalization
#library(caret)
#library(lattice)

#process <- preProcess(as.data.frame(data$propvalue), method=c("range"))
#norm_scale <- predict(process, as.data.frame(data$propvalue))
#data$propvalue2 <- norm_scale

#process <- preProcess(as.data.frame(data$total_kill), method=c("range"))
#norm_scale <- predict(process, as.data.frame(data$total_kill))
#data$total_kill2 <- norm_scale

#process <- preProcess(as.data.frame(data$total_wound), method=c("range"))
#norm_scale <- predict(process, as.data.frame(data$total_wound))
#data$total_wound2 <- norm_scale

#Plots 
ggplot(data = data, mapping = aes(x = region_txt, y = casualties, fill = decade)) + 
  labs(title = "Terrorist attacks by region overtime", x = "Region") +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot(data = data, mapping = aes( x = attacktype1_txt, y = casualties, fill = decade)) + 
  labs(title = "Type of attack vs casualties overtime",) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot(data = data, mapping = aes( x = targtype1_txt, y = casualties, fill = decade)) + 
  labs(title = "Target type casualties overtime",) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

summary(data)

#Create mode function https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getSecondMode <- function(v) {
  uniqv <- unique(v)
  freq <- tabulate(match(v, uniqv))
  second_max_index <- which(freq == sort(freq, decreasing = TRUE)[2])
  uniqv[second_max_index]
}

#Get measures of center
getMode(data$country_txt)
getMode(data$region_txt)
getMode(data$provstate)
getSecondMode(data$city)
getMode(data$attacktype1_txt)
getMode(data$targtype1_txt)
getMode(data$target1)
getMode(data$natlty1_txt)
getSecondMode(data$gname)
getMode(data$weaptype1_txt)
getMode(data$weapsubtype1_txt)
data$property <- as.logical(data$property)
getSecondMode(data$propextent_txt)
getMode(data$total_kill)
getMode(data$total_wound)


summary(data$property)
summary(data$crit1)
summary(data$crit2)
summary(data$crit3)
summary(data$success)
summary(data$suicide)
summary(data$total_wound)
summary(data$propvalue)
summary(data$total_kill)

colnames(data)

#Correlation between numeric variables 
#https://www.geeksforgeeks.org/how-to-calculate-correlation-between-multiple-variables-in-r/
cormat <- cor(data[, c("total_kill", "total_wound", "propvalue")])

melted_cormat <- melt(cormat)

# Heatmap http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization 
# https://stackoverflow.com/questions/14290364/create-heatmap-with-values-from-matrix-in-ggplot2

ggplot(melted_cormat, aes(Var1, Var2)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = "deepskyblue", high = "dodgerblue4")

#sort by casualties, show the most affected countries


Casualties_by_country <- data %>% 
  group_by(country_txt) %>% 
  summarise(casualties=sum(casualties))



Casualties_by_country_decade <- data %>%
  mutate(decade = as.character(decade)) %>%
  group_by(country_txt, decade) %>%
  summarise(casualties = sum(casualties))


sorted_casualties_by_country <- Casualties_by_country[order(Casualties_by_country$casualties, decreasing=TRUE),]
slice <- slice(sorted_casualties_by_country, 1:10)

sorted_casualties_by_country_decade <- Casualties_by_country_decade[order(Casualties_by_country_decade$casualties, decreasing=TRUE),]
slice2 <- sorted_casualties_by_country_decade[sorted_casualties_by_country_decade$country_txt %in% slice$country_txt, ]


ggplot(data = slice2, aes(fill=as.character(decade), y=casualties, x=country_txt)) + 
  labs(title = "The 10 most affected countries by casualties overtime", x = "Country", fill = "Decade") +
  geom_bar(position="stack", stat="identity") + scale_fill_brewer(palette = "set2")

library(psych)	
description <- describe(data)

#coding region
region <- unique(data$region_txt)
code_region <- c(1:12)
names(code_region) = region
data$region <- code_region[data$region_txt]

#coding country
country <- unique(data$country_txt)
code_country <- c(1:204)
names(code_country) = country
data$country <- code_country[data$country_txt]

#coding attacktype
attacktype <- unique(data$attacktype1_txt)
code_attacktype <- c(1:9)
names(code_attacktype) = attacktype
data$attacktype <- code_attacktype[data$attacktype1_txt]

#chisq test to evaluate the relationship between two categorical variables
#Create a contingency table 
chi_country_attacktype<- table(data$country_txt, data$attacktype1_txt)
#mytable2 <- table(data$country, data$attacktype)
chi_country_attacktype<- table(data$country_txt, data$attacktype1_txt)
#Perform chi-squared test
chisq.test(chi_country_attacktype)
#chisq.test(mytable2)

chi_country_txt_gname<- table(data$gname, data$country_txt)
chisq.test(chi_country_txt_gname)

chi_attacktype_target1<- table(data$attacktype1_txt, data$target1)
chisq.test(chi_attacktype_target1)

chi_attacktype_target1<- table(data$attacktype1_txt, data$target1)
chisq.test(chi_attacktype_target1)


#PCA https://www.datacamp.com/tutorial/pca-analysis-r

str(data)

#coding natlty1_txt 
natlty1_txt  <- unique(data$natlty1_txt)
code_natlty1_txt <- c(1:length(natlty1_txt))
names(code_natlty1_txt) = natlty1_txt
data$natlty1 <- code_natlty1_txt[data$natlty1_txt]

#coding propextent_txt
propextent_txt  <- unique(data$propextent_txt)
code_propextent_txt <- c(1:length(propextent_txt))
names(code_propextent_txt) = propextent_txt
data$propextent <- code_propextent_txt[data$propextent_txt]

#coding weapsubtype1_txt
weapsubtype1_txt  <- unique(data$weapsubtype1_txt)
code_weapsubtype1_txt <- c(1:length(weapsubtype1_txt))
names(code_weapsubtype1_txt) = weapsubtype1_txt
data$weapsubtype1_txt <- code_weapsubtype1_txt[data$weapsubtype1_txt]

#coding weaptype1_txt
weaptype1_txt  <- unique(data$weaptype1_txt)
code_weaptype1_txt <- c(1:length(weaptype1_txt))
names(code_weaptype1_txt) = weaptype1_txt
data$weaptype1 <- code_weaptype1_txt[data$weaptype1_txt]

#coding gname
gname  <- unique(data$gname)
code_gname <- c(1:length(gname))
names(code_gname) = gname
data$code_gname <- code_gname[data$gname]

#coding target1 
target1   <- unique(data$target1)
code_target1 <- c(1:length(target1))
names(code_target1) = target1
data$code_target1 <- code_target1[data$target1]

#coding targsubtype1_txt
targsubtype1_txt   <- unique(data$targsubtype1_txt)
code_targsubtype1_txt <- c(1:length(targsubtype1_txt))
names(code_targsubtype1_txt) = targsubtype1_txt
data$targsubtype1 <- code_targsubtype1_txt[data$targsubtype1_txt]

#coding targtype1_txt 
targtype1_txt  <- unique(data$targtype1_txt)
code_targtype1_txt <- c(1:length(targtype1_txt))
names(code_targtype1_txt) = targtype1_txt
data$targtype1 <- code_targtype1_txt[data$targtype1_txt]


data_cleaned <- select(data, -natlty1_txt, -region_txt, -date, -country_txt, -provstate, -attacktype1_txt, -propextent_txt, -weaptype1_txt, -gname, -target1, -targsubtype1_txt, -targtype1_txt, -city)

str(data_cleaned)

#take out binary variables
data_cleaned <- select(data, -crit1, -crit2, -crit3, -success, -suicide, -property)

PCA2 <- prcomp(na.omit(data_cleaned), scale = TRUE, center = TRUE, tol = 0)
PCA2
summary(PCA2)

plot(PCA2, main="",col="dodgerblue3")
title(main="Principal Components Importance for Students")


install.packages("corrr")
library(corrr)
install.packages("ggcorrplot")
library(ggcorrplot)
numerical_data <- na.omit(data_cleaned)
data_normalized <- scale(numerical_data)
corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)
data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1:7]

library(factoextra)
fviz_eig(data.pca, addlabels = TRUE)



