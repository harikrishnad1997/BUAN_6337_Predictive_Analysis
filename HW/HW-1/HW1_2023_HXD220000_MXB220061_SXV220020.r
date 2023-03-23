
## HW_1

rm(list = ls())
demo = F
require(psych)
require(data.table)

# Section - 1

if(demo) {setwd("~/Library/Mobile Documents/com~apple~CloudDocs/School Work/Sem 2/BUAN 6337/HW/HW-1/pset1")}

SwineFlu <- fread("SwinFlu2009.csv",
                  na.strings = c("NA", ""), 
                  sep = "auto",
                  stringsAsFactors = FALSE,
                  data.table = TRUE)
str(SwineFlu)
colClasses = c("observation_id","firstcase_date_id",
               "firstcase_continent_id","country",
               "firstcasereport_date","cum_case_April",
               "cum_case_May","cum_case_June","cum_case_July","cum_case_August",
               "cum_case_Aug09","firstdeath_date_id","firstdeath_continent_id",
               "firstdeath_date","cum_death_May","cum_death_June","cum_death_July",
               "cum_death_August","cum_death_September","cum_death_October","cum_death_November","cum_death_December")

colnames(SwineFlu) <- colClasses

if(demo) {str(SwineFlu)
  View(SwineFlu)}

SwineFlu$firstcasereport_date <- as.Date(SwineFlu$firstcasereport_date,format = "%m/%d/%Y")
SwineFlu$firstdeath_date <- as.Date(SwineFlu$firstdeath_date,format = "%m/%d/%Y")

SwineFlu$Datediff_calc <- difftime(SwineFlu$firstcasereport_date,as.Date("2009-04-29"),units = "days")

SwineFlu2009_days_from_first_incidence <- subset(SwineFlu,select = c("firstcase_date_id","country","Datediff_calc"))

fwrite(SwineFlu2009_days_from_first_incidence,"SwineFlu2009_days_from_first_incidence.csv")
str(SwineFlu2009_days_from_first_incidence)
## Section 2

pizza <- fread("Pizza.csv")

str(pizza)

print(pizza)

lapply(pizza,class)

describe(pizza)

if(demo) {View(pizza)}

## Survey Num column in the data frame should be a factor variable as it is unique and is not ordinal in nature.

## fread identifies the column as an integer and assume its an continuous 
pizza <- fread("Pizza.csv",header = T,colClasses = c("factor","integer","integer","integer","integer","integer"))
if(demo) {str(pizza)}

pizza$avg_rating <- rowMeans(pizza[,2:6],na.rm = T)
if(demo) {str(pizza)
  describe(pizza)
  }

## Section 3

hotel <- fread("Hotel.csv")

## The problem is first row has 11 fields but the dataset has 12.

hotel <- fread("Hotel.csv", fill=TRUE, na.strings = c("NA", ""),
               sep = "auto", data.table = TRUE, stringsAsFactors = FALSE)
if(demo) {str(hotel)}
colnames(hotel)[1] <- "room_no"
colnames(hotel)[2] <- "no_of_guests"

hotel$check_in_date <- as.Date(with(hotel,paste(V3,V4,V5,sep="-")),"%m-%d-%Y")
hotel$check_out_date <- as.Date(with(hotel,paste(V6,V7,V8,sep="-")),"%m-%d-%Y")
if(demo) {str(hotel)}
hotel[,':='(internet_usage=as.numeric(ifelse(V9 == "YES",V10,0)))]
hotel[,':='(room_type=ifelse(V9 == "YES",as.character(V11),as.character(V10)))]
hotel[,':='(room_rate=ifelse(V9 == "YES",V12,strtoi(V11)))]
hotelColNames <- c("room_no","no_of_guests","check_in_date","check_out_date","internet_usage","room_type","room_rate")
hotel_cleaned <- subset(hotel,select = hotelColNames)
str(hotel_cleaned)
hotel_cleaned$subtotal <- hotel_cleaned$room_rate * as.numeric(difftime(hotel_cleaned$check_out_date,hotel_cleaned$check_in_date,units = "days")) + 10 * (hotel_cleaned$no_of_guests-1) * as.numeric(difftime(hotel_cleaned$check_out_date, hotel_cleaned$check_in_date, units="days")) + ifelse(hotel_cleaned$internet_usage > 0 , 9.95 + 5.95 * (hotel_cleaned$internet_usage),0)

hotel_cleaned$total <- round(hotel_cleaned$subtotal * 1.0875,2)
if(demo) {summary(hotel)}
(results <- hotel_cleaned[which(hotel_cleaned$room_no == 247 & hotel_cleaned$check_in_date == "2014-02-07"),])
results$total
