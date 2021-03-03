#Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(tm)
library(sentimentr)
library(bsts)
library(data.table)

#Read data
a_data = read.csv("15_06_2016-15_06_2020__Genius.csv", stringsAsFactors = FALSE)
b_data = read.csv("15_06_2016-15_06_2020__TomTom.csv", stringsAsFactors = FALSE)
c_data = read.csv("15_06_2016-15_06_2020__CoPilot.csv", stringsAsFactors = FALSE)
d_data = read.csv("15_06_2016-15_06_2020__Navmii.csv", stringsAsFactors = FALSE)
e_data = read.csv("15_06_2016-15_06_2020__Sygic.csv", stringsAsFactors = FALSE)
f_data = read.csv("15_06_2016-15_06_2020__Mapsme.csv", stringsAsFactors = FALSE)
g_data = read.csv("12_09_2018-15_06_2020__Waze.csv", stringsAsFactors = FALSE)
h_data = read.csv("15_06_2016-15_06_2020__Here.csv", stringsAsFactors = FALSE)

##########################Descriptive statistics
#Genius maps
#Create review frequency per date table
a_date_table = setNames(data.frame(table(a_data$date)),c("Date","Count"))
a_date_table$Date = as.Date(a_date_table$Date)
a_date_table$Count = as.integer((a_date_table$Count))
#Fill in any dates with 0 reviews
a_date_table_full = a_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  tidyr::complete(Date = seq.Date(min(Date), max(Date), by="day"))
#Populate empty dates with 0
a_date_table_full[is.na(a_date_table_full)] <- 0
a_data_cut = a_date_table_full
rm(a_date_table)
rm(a_date_table_full)

#Break review frequency per days, weeks, months and years
a_data_cut$day_cut = cut(a_data_cut$Date, breaks = "day")
a_data_cut$week_cut = cut(a_data_cut$Date, breaks = "week")
a_data_cut$month_cut = cut(a_data_cut$Date, breaks = "month")
a_data_cut$year_cut = cut(a_data_cut$Date, breaks = "year")

#Create daily review frequency table
a_daily = aggregate(Count ~ day_cut, a_data_cut, sum)
a_daily$day_cut = as.Date(a_daily$day_cut)
a_daily$year = lubridate::year(a_daily$day_cut)
a_daily$day = lubridate::day(a_daily$day_cut)

#Create weekly review frequency table
a_weekly = aggregate(Count ~ week_cut, a_data_cut, sum)
a_weekly$week_cut = as.Date(a_weekly$week_cut)
a_weekly$year = lubridate::year(a_weekly$week_cut)
a_weekly$week = lubridate::week(a_weekly$week_cut)

#Create monthly review frequency table
a_monthly = aggregate(Count ~ month_cut, a_data_cut, sum)
a_monthly$month_cut = as.Date(a_monthly$month_cut)
a_monthly$year = lubridate::year(a_monthly$month_cut)
a_monthly$month = lubridate::month(a_monthly$month_cut)

#Create yearly review frequency table
a_yearly = aggregate(Count ~ year_cut, a_data_cut, sum)
a_yearly$year_cut = as.Date(a_yearly$year_cut)
a_yearly$year = lubridate::year(a_yearly$year_cut)
a_yearly = a_yearly[2:4, ]

rm(a_data_cut)

####TomTom
b_date_table = setNames(data.frame(table(b_data$date)),c("Date","Count"))
b_date_table$Date = as.Date(b_date_table$Date)
b_date_table$Count = as.integer((b_date_table$Count))
b_date_table_full = b_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
b_date_table_full[is.na(b_date_table_full)] <- 0
b_data_cut = b_date_table_full
rm(b_date_table)
rm(b_date_table_full)

b_data_cut$day_cut = cut(b_data_cut$Date, breaks = "day")
b_data_cut$week_cut = cut(b_data_cut$Date, breaks = "week")
b_data_cut$month_cut = cut(b_data_cut$Date, breaks = "month")
b_data_cut$year_cut = cut(b_data_cut$Date, breaks = "year")

b_daily = aggregate(Count ~ day_cut, b_data_cut, sum)
b_daily$day_cut = as.Date(b_daily$day_cut)
b_daily$year = lubridate::year(b_daily$day_cut)
b_daily$day = lubridate::day(b_daily$day_cut)


b_weekly = aggregate(Count ~ week_cut, b_data_cut, sum)
b_weekly$week_cut = as.Date(b_weekly$week_cut)
b_weekly$year = lubridate::year(b_weekly$week_cut)
b_weekly$week = lubridate::week(b_weekly$week_cut)


b_monthly = aggregate(Count ~ month_cut, b_data_cut, sum)
b_monthly$month_cut = as.Date(b_monthly$month_cut)
b_monthly$year = lubridate::year(b_monthly$month_cut)
b_monthly$month = lubridate::month(b_monthly$month_cut)


b_yearly = aggregate(Count ~ year_cut, b_data_cut, sum)
b_yearly$year_cut = as.Date(b_yearly$year_cut)
b_yearly$year = lubridate::year(b_yearly$year_cut)
b_yearly = b_yearly[2:4, ]
rm(b_data_cut)

##CoPilot
c_date_table = setNames(data.frame(table(c_data$date)),c("Date","Count"))
c_date_table$Date = as.Date(c_date_table$Date)
c_date_table$Count = as.integer((c_date_table$Count))
c_date_table_full = c_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
c_date_table_full[is.na(c_date_table_full)] <- 0
c_data_cut = c_date_table_full
rm(c_date_table)
rm(c_date_table_full)

c_data_cut$day_cut = cut(c_data_cut$Date, breaks = "day")
c_data_cut$week_cut = cut(c_data_cut$Date, breaks = "week")
c_data_cut$month_cut = cut(c_data_cut$Date, breaks = "month")
c_data_cut$year_cut = cut(c_data_cut$Date, breaks = "year")

c_daily = aggregate(Count ~ day_cut, c_data_cut, sum)
c_daily$day_cut = as.Date(c_daily$day_cut)
c_daily$year = lubridate::year(c_daily$day_cut)
c_daily$day = lubridate::day(c_daily$day_cut)


c_weekly = aggregate(Count ~ week_cut, c_data_cut, sum)
c_weekly$week_cut = as.Date(c_weekly$week_cut)
c_weekly$year = lubridate::year(c_weekly$week_cut)
c_weekly$week = lubridate::week(c_weekly$week_cut)


c_monthly = aggregate(Count ~ month_cut, c_data_cut, sum)
c_monthly$month_cut = as.Date(c_monthly$month_cut)
c_monthly$year = lubridate::year(c_monthly$month_cut)
c_monthly$month = lubridate::month(c_monthly$month_cut)


c_yearly = aggregate(Count ~ year_cut, c_data_cut, sum)
c_yearly$year_cut = as.Date(c_yearly$year_cut)
c_yearly$year = lubridate::year(c_yearly$year_cut)
c_yearly = c_yearly[2:4, ]
rm(c_data_cut)

#Navmii
d_date_table = setNames(data.frame(table(d_data$date)),c("Date","Count"))
d_date_table$Date = as.Date(d_date_table$Date)
d_date_table$Count = as.integer((d_date_table$Count))
d_date_table_full = d_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
d_date_table_full[is.na(d_date_table_full)] <- 0
d_data_cut = d_date_table_full
rm(d_date_table)
rm(d_date_table_full)

d_data_cut$day_cut = cut(d_data_cut$Date, breaks = "day")
d_data_cut$week_cut = cut(d_data_cut$Date, breaks = "week")
d_data_cut$month_cut = cut(d_data_cut$Date, breaks = "month")
d_data_cut$year_cut = cut(d_data_cut$Date, breaks = "year")

d_daily = aggregate(Count ~ day_cut, d_data_cut, sum)
d_daily$day_cut = as.Date(d_daily$day_cut)
d_daily$year = lubridate::year(d_daily$day_cut)
d_daily$day = lubridate::day(d_daily$day_cut)


d_weekly = aggregate(Count ~ week_cut, d_data_cut, sum)
d_weekly$week_cut = as.Date(d_weekly$week_cut)
d_weekly$year = lubridate::year(d_weekly$week_cut)
d_weekly$week = lubridate::week(d_weekly$week_cut)


d_monthly = aggregate(Count ~ month_cut, d_data_cut, sum)
d_monthly$month_cut = as.Date(d_monthly$month_cut)
d_monthly$year = lubridate::year(d_monthly$month_cut)
d_monthly$month = lubridate::month(d_monthly$month_cut)


d_yearly = aggregate(Count ~ year_cut, d_data_cut, sum)
d_yearly$year_cut = as.Date(d_yearly$year_cut)
d_yearly$year = lubridate::year(d_yearly$year_cut)
d_yearly = d_yearly[2:4, ]
rm(d_data_cut)

#Sygic
e_date_table = setNames(data.frame(table(e_data$date)),c("Date","Count"))
e_date_table$Date = as.Date(e_date_table$Date)
e_date_table$Count = as.integer((e_date_table$Count))
e_date_table_full = e_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
e_date_table_full[is.na(e_date_table_full)] <- 0
e_data_cut = e_date_table_full
rm(e_date_table)
rm(e_date_table_full)

e_data_cut$day_cut = cut(e_data_cut$Date, breaks = "day")
e_data_cut$week_cut = cut(e_data_cut$Date, breaks = "week")
e_data_cut$month_cut = cut(e_data_cut$Date, breaks = "month")
e_data_cut$year_cut = cut(e_data_cut$Date, breaks = "year")

e_daily = aggregate(Count ~ day_cut, e_data_cut, sum)
e_daily$day_cut = as.Date(e_daily$day_cut)
e_daily$year = lubridate::year(e_daily$day_cut)
e_daily$day = lubridate::day(e_daily$day_cut)


e_weekly = aggregate(Count ~ week_cut, e_data_cut, sum)
e_weekly$week_cut = as.Date(e_weekly$week_cut)
e_weekly$year = lubridate::year(e_weekly$week_cut)
e_weekly$week = lubridate::week(e_weekly$week_cut)


e_monthly = aggregate(Count ~ month_cut, e_data_cut, sum)
e_monthly$month_cut = as.Date(e_monthly$month_cut)
e_monthly$year = lubridate::year(e_monthly$month_cut)
e_monthly$month = lubridate::month(e_monthly$month_cut)


e_yearly = aggregate(Count ~ year_cut, e_data_cut, sum)
e_yearly$year_cut = as.Date(e_yearly$year_cut)
e_yearly$year = lubridate::year(e_yearly$year_cut)
e_yearly = e_yearly[2:4, ]
rm(e_data_cut)

#####Mapsme
f_date_table = setNames(data.frame(table(f_data$date)),c("Date","Count"))
f_date_table$Date = as.Date(f_date_table$Date)
f_date_table$Count = as.integer((f_date_table$Count))
f_date_table_full = f_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
f_date_table_full[is.na(f_date_table_full)] <- 0
f_data_cut = f_date_table_full
rm(f_date_table)
rm(f_date_table_full)

f_data_cut$day_cut = cut(f_data_cut$Date, breaks = "day")
f_data_cut$week_cut = cut(f_data_cut$Date, breaks = "week")
f_data_cut$month_cut = cut(f_data_cut$Date, breaks = "month")
f_data_cut$year_cut = cut(f_data_cut$Date, breaks = "year")

f_daily = aggregate(Count ~ day_cut, f_data_cut, sum)
f_daily$day_cut = as.Date(f_daily$day_cut)
f_daily$year = lubridate::year(f_daily$day_cut)
f_daily$day = lubridate::day(f_daily$day_cut)


f_weekly = aggregate(Count ~ week_cut, f_data_cut, sum)
f_weekly$week_cut = as.Date(f_weekly$week_cut)
f_weekly$year = lubridate::year(f_weekly$week_cut)
f_weekly$week = lubridate::week(f_weekly$week_cut)


f_monthly = aggregate(Count ~ month_cut, f_data_cut, sum)
f_monthly$month_cut = as.Date(f_monthly$month_cut)
f_monthly$year = lubridate::year(f_monthly$month_cut)
f_monthly$month = lubridate::month(f_monthly$month_cut)


f_yearly = aggregate(Count ~ year_cut, f_data_cut, sum)
f_yearly$year_cut = as.Date(f_yearly$year_cut)
f_yearly$year = lubridate::year(f_yearly$year_cut)
f_yearly = f_yearly[2:4, ]
rm(f_data_cut)

##Waze
g_date_table = setNames(data.frame(table(g_data$date)),c("Date","Count"))
g_date_table$Date = as.Date(g_date_table$Date)
g_date_table$Count = as.integer((g_date_table$Count))
g_date_table_full = g_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
g_date_table_full[is.na(g_date_table_full)] <- 0
g_data_cut = g_date_table_full
rm(g_date_table)
rm(g_date_table_full)

g_data_cut$day_cut = cut(g_data_cut$Date, breaks = "day")
g_data_cut$week_cut = cut(g_data_cut$Date, breaks = "week")
g_data_cut$month_cut = cut(g_data_cut$Date, breaks = "month")
g_data_cut$year_cut = cut(g_data_cut$Date, breaks = "year")

g_daily = aggregate(Count ~ day_cut, g_data_cut, sum)
g_daily$day_cut = as.Date(g_daily$day_cut)
g_daily$year = lubridate::year(g_daily$day_cut)
g_daily$day = lubridate::day(g_daily$day_cut)


g_weekly = aggregate(Count ~ week_cut, g_data_cut, sum)
g_weekly$week_cut = as.Date(g_weekly$week_cut)
g_weekly$year = lubridate::year(g_weekly$week_cut)
g_weekly$week = lubridate::week(g_weekly$week_cut)


g_monthly = aggregate(Count ~ month_cut, g_data_cut, sum)
g_monthly$month_cut = as.Date(g_monthly$month_cut)
g_monthly$year = lubridate::year(g_monthly$month_cut)
g_monthly$month = lubridate::month(g_monthly$month_cut)


g_yearly = aggregate(Count ~ year_cut, g_data_cut, sum)
g_yearly$year_cut = as.Date(g_yearly$year_cut)
g_yearly$year = lubridate::year(g_yearly$year_cut)
rm(g_data_cut)

###Here
h_date_table = setNames(data.frame(table(h_data$date)),c("Date","Count"))
h_date_table$Date = as.Date(h_date_table$Date)
h_date_table$Count = as.integer((h_date_table$Count))
h_date_table_full = h_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
h_date_table_full[is.na(h_date_table_full)] <- 0
h_data_cut = h_date_table_full
rm(h_date_table)
rm(h_date_table_full)

h_data_cut$day_cut = cut(h_data_cut$Date, breaks = "day")
h_data_cut$week_cut = cut(h_data_cut$Date, breaks = "week")
h_data_cut$month_cut = cut(h_data_cut$Date, breaks = "month")
h_data_cut$year_cut = cut(h_data_cut$Date, breaks = "year")

h_daily = aggregate(Count ~ day_cut, h_data_cut, sum)
h_daily$day_cut = as.Date(h_daily$day_cut)
h_daily$year = lubridate::year(h_daily$day_cut)
h_daily$day = lubridate::day(h_daily$day_cut)


h_weekly = aggregate(Count ~ week_cut, h_data_cut, sum)
h_weekly$week_cut = as.Date(h_weekly$week_cut)
h_weekly$year = lubridate::year(h_weekly$week_cut)
h_weekly$week = lubridate::week(h_weekly$week_cut)


h_monthly = aggregate(Count ~ month_cut, h_data_cut, sum)
h_monthly$month_cut = as.Date(h_monthly$month_cut)
h_monthly$year = lubridate::year(h_monthly$month_cut)
h_monthly$month = lubridate::month(h_monthly$month_cut)


h_yearly = aggregate(Count ~ year_cut, h_data_cut, sum)
h_yearly$year_cut = as.Date(h_yearly$year_cut)
h_yearly$year = lubridate::year(h_yearly$year_cut)
h_yearly = h_yearly[2:4, ]
rm(h_data_cut)

#Add app names to each table
a_daily$app = "1"
a_weekly$app = "1"
a_monthly$app = "1"
a_yearly$app = "1"
b_daily$app = "2"
b_weekly$app = "2"
b_monthly$app = "2"
b_yearly$app = "2"
c_daily$app = "3"
c_weekly$app = "3"
c_monthly$app = "3"
c_yearly$app = "3"
d_daily$app = "4"
d_weekly$app = "4"
d_monthly$app = "4"
d_yearly$app = "4"
e_daily$app = "5"
e_weekly$app = "5"
e_monthly$app = "5"
e_yearly$app = "5"
f_daily$app = "6"
f_weekly$app = "6"
f_monthly$app = "6"
f_yearly$app = "6"
g_daily$app = "7"
g_weekly$app = "7"
g_monthly$app = "7"
g_yearly$app = "7"
g_daily$app = "8"
h_weekly$app = "8"
h_monthly$app = "8"
h_yearly$app = "8"

#Join all daily tables in to one table
drf1 = full_join(a_daily, b_daily)
drf2 = full_join(drf1, c_daily)
drf3 = full_join(drf2, d_daily)
drf4 = full_join(drf3, e_daily)
drf5 = full_join(drf4, f_daily)
drf6 = full_join(drf5, g_daily)
daily_review_freq = full_join(drf6, h_daily)
rm(drf1)
rm(drf2)
rm(drf3)
rm(drf4)
rm(drf5)
rm(drf6)
#Join all weekly tables in to one table
wrf1 = full_join(a_weekly, b_weekly)
wrf2 = full_join(wrf1, c_weekly)
wrf3 = full_join(wrf2, d_weekly)
wrf4 = full_join(wrf3, e_weekly)
wrf5 = full_join(wrf4, f_weekly)
wrf6 = full_join(wrf5, g_weekly)
weekly_review_freq = full_join(wrf6, h_weekly)
rm(wrf1)
rm(wrf2)
rm(wrf3)
rm(wrf4)
rm(wrf5)
rm(wrf6)
#Join all monthly tables in to one table
mrf1 = full_join(a_monthly, b_monthly)
mrf2 = full_join(mrf1, c_monthly)
mrf3 = full_join(mrf2, d_monthly)
mrf4 = full_join(mrf3, e_monthly)
mrf5 = full_join(mrf4, f_monthly)
mrf6 = full_join(mrf5, g_monthly)
monthly_review_freq = full_join(mrf6, h_monthly)
rm(mrf1)
rm(mrf2)
rm(mrf3)
rm(mrf4)
rm(mrf5)
rm(mrf6)
#Join all yearly tables in to one table
yrf1 = full_join(a_yearly, b_yearly)
yrf2 = full_join(yrf1, c_yearly)
yrf3 = full_join(yrf2, d_yearly)
yrf4 = full_join(yrf3, e_yearly)
yrf5 = full_join(yrf4, f_yearly)
yrf6 = full_join(yrf5, g_yearly)
yearly_review_freq = full_join(yrf6, h_yearly)
rm(yrf1)
rm(yrf2)
rm(yrf3)
rm(yrf4)
rm(yrf5)
rm(yrf6)

#Remove individual data
rm(a_weekly)
rm(b_weekly)
rm(c_weekly)
rm(d_weekly)
rm(e_weekly)
rm(f_weekly)
rm(g_weekly)
rm(h_weekly)
rm(a_monthly)
rm(b_monthly)
rm(c_monthly)
rm(d_monthly)
rm(e_monthly)
rm(f_monthly)
rm(g_monthly)
rm(h_monthly)
rm(a_yearly)
rm(b_yearly)
rm(c_yearly)
rm(d_yearly)
rm(e_yearly)
rm(f_yearly)
rm(g_yearly)
rm(h_yearly)

#Check data
#Genius Maps
mean(a_daily$Count)
mean(weekly_review_freq[weekly_review_freq$app==1, 2])
mean(monthly_review_freq[monthly_review_freq$app==1, 2])
mean(yearly_review_freq[yearly_review_freq$app==1, 2])
#TomTom
mean(b_daily$Count)
mean(weekly_review_freq[weekly_review_freq$app==2, 2])
mean(monthly_review_freq[monthly_review_freq$app==2, 2])
mean(yearly_review_freq[yearly_review_freq$app==2, 2])
#CoPilot
mean(c_daily$Count)
mean(weekly_review_freq[weekly_review_freq$app==3, 2])
mean(monthly_review_freq[monthly_review_freq$app==3, 2])
mean(yearly_review_freq[yearly_review_freq$app==3, 2])
#Navmii
mean(d_daily$Count)
mean(weekly_review_freq[weekly_review_freq$app==4, 2])
mean(monthly_review_freq[monthly_review_freq$app==4, 2])
mean(yearly_review_freq[yearly_review_freq$app==4, 2])
#Sygic
mean(e_daily$Count)
mean(weekly_review_freq[weekly_review_freq$app==5, 2])
mean(monthly_review_freq[monthly_review_freq$app==5, 2])
mean(yearly_review_freq[yearly_review_freq$app==5, 2])
#Maps.me
mean(f_daily$Count)
mean(weekly_review_freq[weekly_review_freq$app==6, 2])
mean(monthly_review_freq[monthly_review_freq$app==6, 2])
mean(yearly_review_freq[yearly_review_freq$app==6, 2])
#Waze
mean(g_daily$Count)
mean(weekly_review_freq[weekly_review_freq$app==7, 2])
mean(monthly_review_freq[monthly_review_freq$app==7, 2])
mean(yearly_review_freq[yearly_review_freq$app==7, 2])
#Here
mean(h_daily$Count)
mean(weekly_review_freq[weekly_review_freq$app==8, 2])
mean(monthly_review_freq[monthly_review_freq$app==8, 2])
mean(yearly_review_freq[yearly_review_freq$app==8, 2])

#Create app names
app = c("Genius Maps", "TomTom", "CoPilot", "Navmii",
        "Sygic", "Maps.me", "Waze", "Here")
#Create datamap structure
base = c(nrow(a_data), nrow(b_data), nrow(c_data), nrow(d_data),
         nrow(e_data), nrow(f_data), nrow(g_data), nrow(h_data))
#Create daily table
day = c(mean(a_daily$Count),
        mean(b_daily$Count),
        mean(c_daily$Count),
        mean(d_daily$Count),
        mean(e_daily$Count),
        mean(f_daily$Count),
        mean(g_daily$Count),
        mean(h_daily$Count))
#Create weekly table
week = c(mean(weekly_review_freq[weekly_review_freq$app==1, 2]),
         mean(weekly_review_freq[weekly_review_freq$app==2, 2]),
         mean(weekly_review_freq[weekly_review_freq$app==3, 2]),
         mean(weekly_review_freq[weekly_review_freq$app==4, 2]),
         mean(weekly_review_freq[weekly_review_freq$app==5, 2]),
         mean(weekly_review_freq[weekly_review_freq$app==6, 2]),
         mean(weekly_review_freq[weekly_review_freq$app==7, 2]),
         mean(weekly_review_freq[weekly_review_freq$app==8, 2]))
#Create monthly table
month = c(mean(monthly_review_freq[monthly_review_freq$app==1, 2]),
          mean(monthly_review_freq[monthly_review_freq$app==2, 2]),
          mean(monthly_review_freq[monthly_review_freq$app==3, 2]),
          mean(monthly_review_freq[monthly_review_freq$app==4, 2]),
          mean(monthly_review_freq[monthly_review_freq$app==5, 2]),
          mean(monthly_review_freq[monthly_review_freq$app==6, 2]),
          mean(monthly_review_freq[monthly_review_freq$app==7, 2]),
          mean(monthly_review_freq[monthly_review_freq$app==8, 2]))
#Create yearly table
year = c(mean(yearly_review_freq[yearly_review_freq$app==1, 2]),
         mean(yearly_review_freq[yearly_review_freq$app==2, 2]),
         mean(yearly_review_freq[yearly_review_freq$app==3, 2]),
         mean(yearly_review_freq[yearly_review_freq$app==4, 2]),
         mean(yearly_review_freq[yearly_review_freq$app==5, 2]),
         mean(yearly_review_freq[yearly_review_freq$app==6, 2]),
         mean(yearly_review_freq[yearly_review_freq$app==7, 2]),
         mean(yearly_review_freq[yearly_review_freq$app==8, 2]))
#Create data frame
descriptives = data.frame(app, base, day, week, month, year)
#Round decimals
descriptives$day = round(descriptives$day, 1)
descriptives$week = round(descriptives$week, 1)
descriptives$month = round(descriptives$month, 1)
descriptives$year = round(descriptives$year, 1)

#Write data
write.csv(descriptives, file = 'descriptives.csv', row.names = FALSE)

#Remove
rm(a_daily)
rm(b_daily)
rm(c_daily)
rm(d_daily)
rm(e_daily)
rm(f_daily)
rm(g_daily)
rm(h_daily)
rm(app)
rm(base)
rm(day)
rm(month)
rm(year)
rm(week)
rm(descriptives)
rm(daily_review_freq)
rm(weekly_review_freq)
rm(monthly_review_freq)
rm(yearly_review_freq)



##########################Reviews through time
#####Genius
#Create review frequency per date table
a_date_table = setNames(data.frame(table(a_data$date)),c("Date","Count"))
a_date_table$Date = as.Date(a_date_table$Date)
a_date_table$Count = as.integer((a_date_table$Count))
#Fill in any dates with 0 reviews
a_date_table_full = a_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  tidyr::complete(Date = seq.Date(min(Date), max(Date), by="day"))
#Populate empty dates with 0
a_date_table_full[is.na(a_date_table_full)] <- 0
a_data_cut = a_date_table_full
rm(a_date_table)
rm(a_date_table_full)

#Break review frequency per days, weeks and months
a_data_cut$day_cut = cut(a_data_cut$Date, breaks = "day")
a_data_cut$week_cut = cut(a_data_cut$Date, breaks = "week")
a_data_cut$month_cut = cut(a_data_cut$Date, breaks = "month")
#Create daily review frequency table
a_daily = aggregate(Count ~ day_cut, a_data_cut, sum)
a_daily$day_cut = as.Date(a_daily$day_cut)
a_daily$year = lubridate::year(a_daily$day_cut)
a_daily$day = lubridate::day(a_daily$day_cut)
a_daily$day_cut = as.Date(a_daily$day_cut)
#Create weekly review frequency table
a_weekly = aggregate(Count ~ week_cut, a_data_cut, sum)
a_weekly$week_cut = as.Date(a_weekly$week_cut)
a_weekly$year = lubridate::year(a_weekly$week_cut)
a_weekly$week = lubridate::week(a_weekly$week_cut)
a_weekly$week_cut = as.Date(a_weekly$week_cut)
#Create monthly review frequency table
a_monthly = aggregate(Count ~ month_cut, a_data_cut, sum)
a_monthly$month_cut = as.Date(a_monthly$month_cut)
a_monthly$year = lubridate::year(a_monthly$month_cut)
a_monthly$month = lubridate::month(a_monthly$month_cut)
a_monthly$month_cut = as.Date(a_monthly$month_cut)
rm(a_data_cut)

####TomTom
b_date_table = setNames(data.frame(table(b_data$date)),c("Date","Count"))
b_date_table$Date = as.Date(b_date_table$Date)
b_date_table$Count = as.integer((b_date_table$Count))
b_date_table_full = b_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
b_date_table_full[is.na(b_date_table_full)] <- 0
b_data_cut = b_date_table_full
rm(b_date_table)
rm(b_date_table_full)

b_data_cut$day_cut = cut(b_data_cut$Date, breaks = "day")
b_data_cut$week_cut = cut(b_data_cut$Date, breaks = "week")
b_data_cut$month_cut = cut(b_data_cut$Date, breaks = "month")

b_daily = aggregate(Count ~ day_cut, b_data_cut, sum)
b_daily$day_cut = as.Date(b_daily$day_cut)
b_daily$year = lubridate::year(b_daily$day_cut)
b_daily$day = lubridate::day(b_daily$day_cut)


b_weekly = aggregate(Count ~ week_cut, b_data_cut, sum)
b_weekly$week_cut = as.Date(b_weekly$week_cut)
b_weekly$year = lubridate::year(b_weekly$week_cut)
b_weekly$week = lubridate::week(b_weekly$week_cut)


b_monthly = aggregate(Count ~ month_cut, b_data_cut, sum)
b_monthly$month_cut = as.Date(b_monthly$month_cut)
b_monthly$year = lubridate::year(b_monthly$month_cut)
b_monthly$month = lubridate::month(b_monthly$month_cut)

rm(b_data_cut)

##CoPilot
c_date_table = setNames(data.frame(table(c_data$date)),c("Date","Count"))
c_date_table$Date = as.Date(c_date_table$Date)
c_date_table$Count = as.integer((c_date_table$Count))
c_date_table_full = c_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
c_date_table_full[is.na(c_date_table_full)] <- 0
c_data_cut = c_date_table_full
rm(c_date_table)
rm(c_date_table_full)

c_data_cut$day_cut = cut(c_data_cut$Date, breaks = "day")
c_data_cut$week_cut = cut(c_data_cut$Date, breaks = "week")
c_data_cut$month_cut = cut(c_data_cut$Date, breaks = "month")

c_daily = aggregate(Count ~ day_cut, c_data_cut, sum)
c_daily$day_cut = as.Date(c_daily$day_cut)
c_daily$year = lubridate::year(c_daily$day_cut)
c_daily$day = lubridate::day(c_daily$day_cut)


c_weekly = aggregate(Count ~ week_cut, c_data_cut, sum)
c_weekly$week_cut = as.Date(c_weekly$week_cut)
c_weekly$year = lubridate::year(c_weekly$week_cut)
c_weekly$week = lubridate::week(c_weekly$week_cut)


c_monthly = aggregate(Count ~ month_cut, c_data_cut, sum)
c_monthly$month_cut = as.Date(c_monthly$month_cut)
c_monthly$year = lubridate::year(c_monthly$month_cut)
c_monthly$month = lubridate::month(c_monthly$month_cut)

rm(c_data_cut)

####Navmii
d_date_table = setNames(data.frame(table(d_data$date)),c("Date","Count"))
d_date_table$Date = as.Date(d_date_table$Date)
d_date_table$Count = as.integer((d_date_table$Count))
d_date_table_full = d_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
d_date_table_full[is.na(d_date_table_full)] <- 0
d_data_cut = d_date_table_full
rm(d_date_table)
rm(d_date_table_full)

d_data_cut$day_cut = cut(d_data_cut$Date, breaks = "day")
d_data_cut$week_cut = cut(d_data_cut$Date, breaks = "week")
d_data_cut$month_cut = cut(d_data_cut$Date, breaks = "month")

d_daily = aggregate(Count ~ day_cut, d_data_cut, sum)
d_daily$day_cut = as.Date(d_daily$day_cut)
d_daily$year = lubridate::year(d_daily$day_cut)
d_daily$day = lubridate::day(d_daily$day_cut)


d_weekly = aggregate(Count ~ week_cut, d_data_cut, sum)
d_weekly$week_cut = as.Date(d_weekly$week_cut)
d_weekly$year = lubridate::year(d_weekly$week_cut)
d_weekly$week = lubridate::week(d_weekly$week_cut)


d_monthly = aggregate(Count ~ month_cut, d_data_cut, sum)
d_monthly$month_cut = as.Date(d_monthly$month_cut)
d_monthly$year = lubridate::year(d_monthly$month_cut)
d_monthly$month = lubridate::month(d_monthly$month_cut)

rm(d_data_cut)

####Sygic
e_date_table = setNames(data.frame(table(e_data$date)),c("Date","Count"))
e_date_table$Date = as.Date(e_date_table$Date)
e_date_table$Count = as.integer((e_date_table$Count))
e_date_table_full = e_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
e_date_table_full[is.na(e_date_table_full)] <- 0
e_data_cut = e_date_table_full
rm(e_date_table)
rm(e_date_table_full)

e_data_cut$day_cut = cut(e_data_cut$Date, breaks = "day")
e_data_cut$week_cut = cut(e_data_cut$Date, breaks = "week")
e_data_cut$month_cut = cut(e_data_cut$Date, breaks = "month")

e_daily = aggregate(Count ~ day_cut, e_data_cut, sum)
e_daily$day_cut = as.Date(e_daily$day_cut)
e_daily$year = lubridate::year(e_daily$day_cut)
e_daily$day = lubridate::day(e_daily$day_cut)


e_weekly = aggregate(Count ~ week_cut, e_data_cut, sum)
e_weekly$week_cut = as.Date(e_weekly$week_cut)
e_weekly$year = lubridate::year(e_weekly$week_cut)
e_weekly$week = lubridate::week(e_weekly$week_cut)


e_monthly = aggregate(Count ~ month_cut, e_data_cut, sum)
e_monthly$month_cut = as.Date(e_monthly$month_cut)
e_monthly$year = lubridate::year(e_monthly$month_cut)
e_monthly$month = lubridate::month(e_monthly$month_cut)

rm(e_data_cut)

#####Mapsme
f_date_table = setNames(data.frame(table(f_data$date)),c("Date","Count"))
f_date_table$Date = as.Date(f_date_table$Date)
f_date_table$Count = as.integer((f_date_table$Count))
f_date_table_full = f_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
f_date_table_full[is.na(f_date_table_full)] <- 0
f_data_cut = f_date_table_full
rm(f_date_table)
rm(f_date_table_full)

f_data_cut$day_cut = cut(f_data_cut$Date, breaks = "day")
f_data_cut$week_cut = cut(f_data_cut$Date, breaks = "week")
f_data_cut$month_cut = cut(f_data_cut$Date, breaks = "month")

f_daily = aggregate(Count ~ day_cut, f_data_cut, sum)
f_daily$day_cut = as.Date(f_daily$day_cut)
f_daily$year = lubridate::year(f_daily$day_cut)
f_daily$day = lubridate::day(f_daily$day_cut)


f_weekly = aggregate(Count ~ week_cut, f_data_cut, sum)
f_weekly$week_cut = as.Date(f_weekly$week_cut)
f_weekly$year = lubridate::year(f_weekly$week_cut)
f_weekly$week = lubridate::week(f_weekly$week_cut)


f_monthly = aggregate(Count ~ month_cut, f_data_cut, sum)
f_monthly$month_cut = as.Date(f_monthly$month_cut)
f_monthly$year = lubridate::year(f_monthly$month_cut)
f_monthly$month = lubridate::month(f_monthly$month_cut)

rm(f_data_cut)

##Waze
g_date_table = setNames(data.frame(table(g_data$date)),c("Date","Count"))
g_date_table$Date = as.Date(g_date_table$Date)
g_date_table$Count = as.integer((g_date_table$Count))
g_date_table_full = g_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
g_date_table_full[is.na(g_date_table_full)] <- 0
g_data_cut = g_date_table_full
rm(g_date_table)
rm(g_date_table_full)

g_data_cut$day_cut = cut(g_data_cut$Date, breaks = "day")
g_data_cut$week_cut = cut(g_data_cut$Date, breaks = "week")
g_data_cut$month_cut = cut(g_data_cut$Date, breaks = "month")

g_daily = aggregate(Count ~ day_cut, g_data_cut, sum)
g_daily$day_cut = as.Date(g_daily$day_cut)
g_daily$year = lubridate::year(g_daily$day_cut)
g_daily$day = lubridate::day(g_daily$day_cut)


g_weekly = aggregate(Count ~ week_cut, g_data_cut, sum)
g_weekly$week_cut = as.Date(g_weekly$week_cut)
g_weekly$year = lubridate::year(g_weekly$week_cut)
g_weekly$week = lubridate::week(g_weekly$week_cut)


g_monthly = aggregate(Count ~ month_cut, g_data_cut, sum)
g_monthly$month_cut = as.Date(g_monthly$month_cut)
g_monthly$year = lubridate::year(g_monthly$month_cut)
g_monthly$month = lubridate::month(g_monthly$month_cut)
rm(g_data_cut)

###Here
h_date_table = setNames(data.frame(table(h_data$date)),c("Date","Count"))#Count number of rows for each date
h_date_table$Date = as.Date(h_date_table$Date)
h_date_table$Count = as.integer((h_date_table$Count))
h_date_table_full = h_date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day")) #Add any days with 0 reviews
h_date_table_full[is.na(h_date_table_full)] <- 0 #Populate those days with 0
h_data_cut = h_date_table_full
rm(h_date_table)
rm(h_date_table_full)
#Cut dates to days, weeks and months
h_data_cut$day_cut = cut(h_data_cut$Date, breaks = "day")
h_data_cut$week_cut = cut(h_data_cut$Date, breaks = "week")
h_data_cut$month_cut = cut(h_data_cut$Date, breaks = "month")

#Create daily tables
h_daily = aggregate(Count ~ day_cut, h_data_cut, sum)
h_daily$day_cut = as.Date(h_daily$day_cut)
h_daily$year = lubridate::year(h_daily$day_cut)
h_daily$day = lubridate::day(h_daily$day_cut)

#Create weekly tables
h_weekly = aggregate(Count ~ week_cut, h_data_cut, sum)
h_weekly$week_cut = as.Date(h_weekly$week_cut)
h_weekly$year = lubridate::year(h_weekly$week_cut)
h_weekly$week = lubridate::week(h_weekly$week_cut)

#Create monthly tables
h_monthly = aggregate(Count ~ month_cut, h_data_cut, sum)
h_monthly$month_cut = as.Date(h_monthly$month_cut)
h_monthly$year = lubridate::year(h_monthly$month_cut)
h_monthly$month = lubridate::month(h_monthly$month_cut)

rm(h_data_cut)

#Add app names to each table
a_daily$app = "1"
a_weekly$app = "1"
a_monthly$app = "1"
b_daily$app = "2"
b_weekly$app = "2"
b_monthly$app = "2"
c_daily$app = "3"
c_weekly$app = "3"
c_monthly$app = "3"
d_daily$app = "4"
d_weekly$app = "4"
d_monthly$app = "4"
e_daily$app = "5"
e_weekly$app = "5"
e_monthly$app = "5"
f_daily$app = "6"
f_weekly$app = "6"
f_monthly$app = "6"
g_daily$app = "7"
g_weekly$app = "7"
g_monthly$app = "7"
h_daily$app = "8"
h_weekly$app = "8"
h_monthly$app = "8"

#Join all daily tables in to one table
drf1 = full_join(a_daily, b_daily)
drf2 = full_join(drf1, c_daily)
drf3 = full_join(drf2, d_daily)
drf4 = full_join(drf3, e_daily)
drf5 = full_join(drf4, f_daily)
drf6 = full_join(drf5, g_daily)
daily_review_freq = full_join(drf6, h_daily)
rm(drf1)
rm(drf2)
rm(drf3)
rm(drf4)
rm(drf5)
rm(drf6)
#Join all weekly tables in to one table
wrf1 = full_join(a_weekly, b_weekly)
wrf2 = full_join(wrf1, c_weekly)
wrf3 = full_join(wrf2, d_weekly)
wrf4 = full_join(wrf3, e_weekly)
wrf5 = full_join(wrf4, f_weekly)
wrf6 = full_join(wrf5, g_weekly)
weekly_review_freq = full_join(wrf6, h_weekly)
rm(wrf1)
rm(wrf2)
rm(wrf3)
rm(wrf4)
rm(wrf5)
rm(wrf6)
#Join all monthly tables in to one table
mrf1 = full_join(a_monthly, b_monthly)
mrf2 = full_join(mrf1, c_monthly)
mrf3 = full_join(mrf2, d_monthly)
mrf4 = full_join(mrf3, e_monthly)
mrf5 = full_join(mrf4, f_monthly)
mrf6 = full_join(mrf5, g_monthly)
monthly_review_freq = full_join(mrf6, h_monthly)
rm(mrf1)
rm(mrf2)
rm(mrf3)
rm(mrf4)
rm(mrf5)
rm(mrf6)

#Remove individual data
rm(a_daily)
rm(b_daily)
rm(c_daily)
rm(d_daily)
rm(e_daily)
rm(f_daily)
rm(g_daily)
rm(h_daily)
rm(a_weekly)
rm(b_weekly)
rm(c_weekly)
rm(d_weekly)
rm(e_weekly)
rm(f_weekly)
rm(g_weekly)
rm(h_weekly)
rm(a_monthly)
rm(b_monthly)
rm(c_monthly)
rm(d_monthly)
rm(e_monthly)
rm(f_monthly)
rm(g_monthly)
rm(h_monthly)

#Write data
write.csv(daily_review_freq, file = 'daily_review_freq.csv', row.names = FALSE)
write.csv(weekly_review_freq, file = 'weekly_review_freq.csv', row.names = FALSE)
write.csv(monthly_review_freq, file = 'monthly_review_freq.csv', row.names = FALSE)


###Create charts
#Add colors
my_colors2 <- c("#62A8D8", "#BEBA06", "#3DBD9F", "#FAA316",
                "#FF4C43", "#8BC858", "#8962A6", "#ED6D92")

#Define app factors
daily_review_freq$app = factor(daily_review_freq$app, 
                               levels = c(1,2,3,4,5,6,7,8),
                               labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                          "Sygic", "Mapsme", "Waze", "Here"))
weekly_review_freq$app = factor(weekly_review_freq$app, 
                                levels = c(1,2,3,4,5,6,7,8),
                                labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                           "Sygic", "Mapsme", "Waze", "Here"))
monthly_review_freq$app = factor(monthly_review_freq$app, 
                                 levels = c(1,2,3,4,5,6,7,8),
                                 labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                            "Sygic", "Mapsme", "Waze", "Here"))
#Charts
daily_review_freq %>%
  ggplot(aes(day_cut, Count, group=app, colour = app)) +
  geom_line() +
  scale_x_date(name = "Time (daily)", breaks = as.Date(c("2017-01-01",'2018-01-01',
                                                          "2019-01-01", "2020-01-01")),
               labels = c("2017","2018","2019","2020")) +
  scale_color_manual(values=my_colors2) +
  ylab("Number of reviews") +
  xlab("Time (daily)") +
  ylim(0,max(daily_review_freq$Count)+0.1*max(daily_review_freq$Count)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Reviews through time - daily")

weekly_review_freq %>%
  ggplot(aes(week_cut, Count, group=app, color=app)) +
  geom_line() +
  scale_x_date(name = "Time (weekly)", breaks = as.Date(c("2017-01-01",'2018-01-01',
                                                         "2019-01-01", "2020-01-01")),
               labels = c("2017","2018","2019","2020")) +
  scale_color_manual(values=my_colors2) +
  ylab("Number of reviews") +
  xlab("Time (weekly)") +
  ylim(0,max(weekly_review_freq$Count)+0.1*max(weekly_review_freq$Count)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Reviews through time - weekly")

monthly_review_freq %>%
  ggplot(aes(month_cut, Count, group=app, color=app)) +
  geom_line() +
  scale_x_date(name = "Time (monthly)", breaks = as.Date(c("2017-01-01",'2018-01-01',
                                                          "2019-01-01", "2020-01-01")),
               labels = c("2017","2018","2019","2020")) +
  scale_color_manual(values=my_colors2) +
  ylab("Number of reviews") +
  xlab("Time (monthly)") +
  ylim(0,max(monthly_review_freq$Count)+0.1*max(monthly_review_freq$Count)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Reviews through time - monthly")

#Remove working data
rm(daily_review_freq)
rm(weekly_review_freq)
rm(monthly_review_freq)
rm(my_colors2)

############Seasonalities in review frequency
#Genius maps
#Calculate cumulative review frequency for each day of the week
a_daily = a_data %>% 
  dplyr::group_by("Day" = lubridate::wday(date)) %>% 
  tally()
a_daily$pct = (a_daily$n/sum(a_daily$n)*100)
#Calculate cumulative review frequency for each week of the year
a_weekly = a_data %>% 
  dplyr::group_by("Week" = lubridate::week(date)) %>% 
  tally()
a_weekly$pct = (a_weekly$n/sum(a_weekly$n)*100)
#Calculate cumulative review frequency for each month
a_monthly = a_data %>% 
  dplyr::group_by("Month" = lubridate::month(date)) %>% 
  tally()
a_monthly$pct = (a_monthly$n/sum(a_monthly$n)*100)

#TomTom
b_daily = b_data %>% 
  dplyr::group_by("Day" = lubridate::wday(date)) %>% 
  tally()
b_daily$pct = (b_daily$n/sum(b_daily$n)*100)

b_weekly = b_data %>% 
  dplyr::group_by("Week" = lubridate::week(date)) %>% 
  tally()
b_weekly$pct = (b_weekly$n/sum(b_weekly$n)*100)

b_monthly = b_data %>% 
  dplyr::group_by("Month" = lubridate::month(date)) %>% 
  tally()
b_monthly$pct = (b_monthly$n/sum(b_monthly$n)*100)

#CoPilot
c_daily = c_data %>% 
  dplyr::group_by("Day" = lubridate::wday(date)) %>% 
  tally()
c_daily$pct = (c_daily$n/sum(c_daily$n)*100)

c_weekly = c_data %>% 
  dplyr::group_by("Week" = lubridate::week(date)) %>% 
  tally()
c_weekly$pct = (c_weekly$n/sum(c_weekly$n)*100)

c_monthly = c_data %>% 
  dplyr::group_by("Month" = lubridate::month(date)) %>% 
  tally()
c_monthly$pct = (c_monthly$n/sum(c_monthly$n)*100)

#Navmii
d_daily = d_data %>% 
  dplyr::group_by("Day" = lubridate::wday(date)) %>% 
  tally()
d_daily$pct = (d_daily$n/sum(d_daily$n)*100)

d_weekly = d_data %>% 
  dplyr::group_by("Week" = lubridate::week(date)) %>% 
  tally()
d_weekly$pct = (d_weekly$n/sum(d_weekly$n)*100)

d_monthly = d_data %>% 
  dplyr::group_by("Month" = lubridate::month(date)) %>% 
  tally()
d_monthly$pct = (d_monthly$n/sum(d_monthly$n)*100)

#Sygic
e_daily = e_data %>% 
  dplyr::group_by("Day" = lubridate::wday(date)) %>% 
  tally()
e_daily$pct = (e_daily$n/sum(e_daily$n)*100)

e_weekly = e_data %>% 
  dplyr::group_by("Week" = lubridate::week(date)) %>% 
  tally()
e_weekly$pct = (e_weekly$n/sum(e_weekly$n)*100)

e_monthly = e_data %>% 
  dplyr::group_by("Month" = lubridate::month(date)) %>% 
  tally()
e_monthly$pct = (e_monthly$n/sum(e_monthly$n)*100)

#Mapsme
f_daily = f_data %>% 
  dplyr::group_by("Day" = lubridate::wday(date)) %>% 
  tally()
f_daily$pct = (f_daily$n/sum(f_daily$n)*100)

f_weekly = f_data %>% 
  dplyr::group_by("Week" = lubridate::week(date)) %>% 
  tally()
f_weekly$pct = (f_weekly$n/sum(f_weekly$n)*100)

f_monthly = f_data %>% 
  dplyr::group_by("Month" = lubridate::month(date)) %>% 
  tally()
f_monthly$pct = (f_monthly$n/sum(f_monthly$n)*100)

#Waze
g_daily = g_data %>% 
  dplyr::group_by("Day" = lubridate::wday(date)) %>% 
  tally()
g_daily$pct = (g_daily$n/sum(g_daily$n)*100)

g_weekly = g_data %>% 
  dplyr::group_by("Week" = lubridate::week(date)) %>% 
  tally()
g_weekly$pct = (g_weekly$n/sum(g_weekly$n)*100)

g_monthly = g_data %>% 
  dplyr::group_by("Month" = lubridate::month(date)) %>% 
  tally()
g_monthly$pct = (g_monthly$n/sum(g_monthly$n)*100)

#Here
h_daily = h_data %>% 
  dplyr::group_by("Day" = lubridate::wday(date)) %>% 
  tally()
h_daily$pct = (h_daily$n/sum(h_daily$n)*100)

h_weekly = h_data %>% 
  dplyr::group_by("Week" = lubridate::week(date)) %>% 
  tally()
h_weekly$pct = (h_weekly$n/sum(h_weekly$n)*100)

h_monthly = h_data %>% 
  dplyr::group_by("Month" = lubridate::month(date)) %>% 
  tally()
h_monthly$pct = (h_monthly$n/sum(h_monthly$n)*100)

#Add app names to each table
a_daily$app = "1"
a_weekly$app = "1"
a_monthly$app = "1"
b_daily$app = "2"
b_weekly$app = "2"
b_monthly$app = "2"
c_daily$app = "3"
c_weekly$app = "3"
c_monthly$app = "3"
d_daily$app = "4"
d_weekly$app = "4"
d_monthly$app = "4"
e_daily$app = "5"
e_weekly$app = "5"
e_monthly$app = "5"
f_daily$app = "6"
f_weekly$app = "6"
f_monthly$app = "6"
g_daily$app = "7"
g_weekly$app = "7"
g_monthly$app = "7"
h_daily$app = "8"
h_weekly$app = "8"
h_monthly$app = "8"

#Join all daily tables in to one table
drf1 = full_join(a_daily, b_daily)
drf2 = full_join(drf1, c_daily)
drf3 = full_join(drf2, d_daily)
drf4 = full_join(drf3, e_daily)
drf5 = full_join(drf4, f_daily)
drf6 = full_join(drf5, g_daily)
daily_seasonal_review_freq = full_join(drf6, h_daily)
rm(drf1)
rm(drf2)
rm(drf3)
rm(drf4)
rm(drf5)
rm(drf6)
#Join all weekly tables in to one table
wrf1 = full_join(a_weekly, b_weekly)
wrf2 = full_join(wrf1, c_weekly)
wrf3 = full_join(wrf2, d_weekly)
wrf4 = full_join(wrf3, e_weekly)
wrf5 = full_join(wrf4, f_weekly)
wrf6 = full_join(wrf5, g_weekly)
weekly_seasonal_review_freq = full_join(wrf6, h_weekly)
rm(wrf1)
rm(wrf2)
rm(wrf3)
rm(wrf4)
rm(wrf5)
rm(wrf6)
#Join all monthly tables in to one table
mrf1 = full_join(a_monthly, b_monthly)
mrf2 = full_join(mrf1, c_monthly)
mrf3 = full_join(mrf2, d_monthly)
mrf4 = full_join(mrf3, e_monthly)
mrf5 = full_join(mrf4, f_monthly)
mrf6 = full_join(mrf5, g_monthly)
monthly_seasonal_review_freq = full_join(mrf6, h_monthly)
rm(mrf1)
rm(mrf2)
rm(mrf3)
rm(mrf4)
rm(mrf5)
rm(mrf6)

#Remove individual data
rm(a_daily)
rm(b_daily)
rm(c_daily)
rm(d_daily)
rm(e_daily)
rm(f_daily)
rm(g_daily)
rm(h_daily)
rm(a_weekly)
rm(b_weekly)
rm(c_weekly)
rm(d_weekly)
rm(e_weekly)
rm(f_weekly)
rm(g_weekly)
rm(h_weekly)
rm(a_monthly)
rm(b_monthly)
rm(c_monthly)
rm(d_monthly)
rm(e_monthly)
rm(f_monthly)
rm(g_monthly)
rm(h_monthly)

#Write data
write.csv(daily_seasonal_review_freq, file = 'daily_seasonal_review_freq.csv', row.names = FALSE)
write.csv(weekly_seasonal_review_freq, file = 'weekly_seasonal_review_freq.csv', row.names = FALSE)
write.csv(monthly_seasonal_review_freq, file = 'monthly_seasonal_review_freq.csv', row.names = FALSE)

###Create charts
#Add colors
my_colors2 <- c("#62A8D8", "#BEBA06", "#3DBD9F", "#FAA316",
                "#FF4C43", "#8BC858", "#8962A6", "#ED6D92")

#Define app factors
daily_seasonal_review_freq$app = factor(daily_seasonal_review_freq$app, 
                               levels = c(1,2,3,4,5,6,7,8),
                               labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                          "Sygic", "Mapsme", "Waze", "Here"))
weekly_seasonal_review_freq$app = factor(weekly_seasonal_review_freq$app, 
                                levels = c(1,2,3,4,5,6,7,8),
                                labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                           "Sygic", "Mapsme", "Waze", "Here"))
monthly_seasonal_review_freq$app = factor(monthly_seasonal_review_freq$app, 
                                 levels = c(1,2,3,4,5,6,7,8),
                                 labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                            "Sygic", "Mapsme", "Waze", "Here"))
#Charts
daily_seasonal_review_freq %>%
  ggplot(aes(Day, pct, group=app, colour = app)) +
  geom_line() +
  scale_x_continuous(name = "Days", breaks = 1:7,
                     labels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                "Friday", "Saturday", "Sunday")) +
  ylab("Share of reviews (in %)") +
  scale_color_manual(values=my_colors2) +
  geom_text(label = round(daily_seasonal_review_freq$pct, digits = 0),
            nudge_x = 0.0, nudge_y = 0.5,
            check_overlap = F, size = 3, fontface = "bold") +
  ylim(0,max(daily_seasonal_review_freq$pct)+0.1*max(daily_seasonal_review_freq$pct)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Share of reviews through days of the week")

weekly_seasonal_review_freq %>%
  ggplot(aes(Week, pct, group=app, colour = app)) +
  geom_line() +
  scale_x_continuous(name = "Week number", n.breaks = 9) +
  ylab("Share of reviews (in %)") +
  scale_color_manual(values=my_colors2) +
  geom_text(label = round(weekly_seasonal_review_freq$pct, digits = 0),
            nudge_x = 0.0, nudge_y = 0.5,
            check_overlap = F, size = 3, fontface = "bold") +
  ylim(0,max(weekly_seasonal_review_freq$pct)+0.1*max(weekly_seasonal_review_freq$pct)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Share of reviews through weeks of the year")

monthly_seasonal_review_freq %>%
  ggplot(aes(Month, pct, group=app, colour = app)) +
  geom_line() +
  scale_x_continuous(name = "Months", breaks = 1:12,
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  ylab("Share of reviews (in %)") +
  scale_color_manual(values=my_colors2) +
  geom_text(label = round(monthly_seasonal_review_freq$pct, digits = 0),
            nudge_x = 0.0, nudge_y = 0.5,
            check_overlap = F, size = 3, fontface = "bold") +
  ylim(0,max(monthly_seasonal_review_freq$pct)+0.1*max(monthly_seasonal_review_freq$pct)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Share of reviews through months")

#Remove working data
rm(daily_seasonal_review_freq)
rm(weekly_seasonal_review_freq)
rm(monthly_seasonal_review_freq)
rm(my_colors2)

##########################Average rating through time
#Genius
#Calculate average star rating per day
a_date_table = aggregate(stars ~ date, a_data, mean)
a_date_table$date = as.Date(a_date_table$date)
a_data_cut = a_date_table
rm(a_date_table)

#Break star rating per days, weeks and months
a_data_cut$day_cut = cut(a_data_cut$date, breaks = "day")
a_data_cut$week_cut = cut(a_data_cut$date, breaks = "week")
a_data_cut$month_cut = cut(a_data_cut$date, breaks = "month")
#Create daily table
a_daily = aggregate(stars ~ day_cut, a_data_cut, mean)
a_daily$day_cut = as.Date(a_daily$day_cut)
a_daily$year = lubridate::year(a_daily$day_cut)
a_daily$day = lubridate::day(a_daily$day_cut)
#Create weekly table
a_weekly = aggregate(stars ~ week_cut, a_data_cut, mean)
a_weekly$week_cut = as.Date(a_weekly$week_cut)
a_weekly$year = lubridate::year(a_weekly$week_cut)
a_weekly$week = lubridate::week(a_weekly$week_cut)
#Create monthly table
a_monthly = aggregate(stars ~ month_cut, a_data_cut, mean)
a_monthly$month_cut = as.Date(a_monthly$month_cut)
a_monthly$year = lubridate::year(a_monthly$month_cut)
a_monthly$month = lubridate::month(a_monthly$month_cut)

rm(a_data_cut)

#TomTom
b_date_table = aggregate(stars ~ date, b_data, mean)
b_date_table$date = as.Date(b_date_table$date)
b_datb_cut = b_date_table
rm(b_date_table)

b_datb_cut$day_cut = cut(b_datb_cut$date, breaks = "day")
b_datb_cut$week_cut = cut(b_datb_cut$date, breaks = "week")
b_datb_cut$month_cut = cut(b_datb_cut$date, breaks = "month")

b_daily = aggregate(stars ~ day_cut, b_datb_cut, mean)
b_daily$day_cut = as.Date(b_daily$day_cut)
b_daily$year = lubridate::year(b_daily$day_cut)
b_daily$day = lubridate::day(b_daily$day_cut)

b_weekly = aggregate(stars ~ week_cut, b_datb_cut, mean)
b_weekly$week_cut = as.Date(b_weekly$week_cut)
b_weekly$year = lubridate::year(b_weekly$week_cut)
b_weekly$week = lubridate::week(b_weekly$week_cut)

b_monthly = aggregate(stars ~ month_cut, b_datb_cut, mean)
b_monthly$month_cut = as.Date(b_monthly$month_cut)
b_monthly$year = lubridate::year(b_monthly$month_cut)
b_monthly$month = lubridate::month(b_monthly$month_cut)

rm(b_datb_cut)

#CoPilot
c_date_table = aggregate(stars ~ date, c_data, mean)
c_date_table$date = as.Date(c_date_table$date)
c_datc_cut = c_date_table
rm(c_date_table)

c_datc_cut$day_cut = cut(c_datc_cut$date, breaks = "day")
c_datc_cut$week_cut = cut(c_datc_cut$date, breaks = "week")
c_datc_cut$month_cut = cut(c_datc_cut$date, breaks = "month")

c_daily = aggregate(stars ~ day_cut, c_datc_cut, mean)
c_daily$day_cut = as.Date(c_daily$day_cut)
c_daily$year = lubridate::year(c_daily$day_cut)
c_daily$day = lubridate::day(c_daily$day_cut)

c_weekly = aggregate(stars ~ week_cut, c_datc_cut, mean)
c_weekly$week_cut = as.Date(c_weekly$week_cut)
c_weekly$year = lubridate::year(c_weekly$week_cut)
c_weekly$week = lubridate::week(c_weekly$week_cut)

c_monthly = aggregate(stars ~ month_cut, c_datc_cut, mean)
c_monthly$month_cut = as.Date(c_monthly$month_cut)
c_monthly$year = lubridate::year(c_monthly$month_cut)
c_monthly$month = lubridate::month(c_monthly$month_cut)

rm(c_datc_cut)

#Navmii
d_date_table = aggregate(stars ~ date, d_data, mean)
d_date_table$date = as.Date(d_date_table$date)
d_datd_cut = d_date_table
rm(d_date_table)

d_datd_cut$day_cut = cut(d_datd_cut$date, breaks = "day")
d_datd_cut$week_cut = cut(d_datd_cut$date, breaks = "week")
d_datd_cut$month_cut = cut(d_datd_cut$date, breaks = "month")

d_daily = aggregate(stars ~ day_cut, d_datd_cut, mean)
d_daily$day_cut = as.Date(d_daily$day_cut)
d_daily$year = lubridate::year(d_daily$day_cut)
d_daily$day = lubridate::day(d_daily$day_cut)

d_weekly = aggregate(stars ~ week_cut, d_datd_cut, mean)
d_weekly$week_cut = as.Date(d_weekly$week_cut)
d_weekly$year = lubridate::year(d_weekly$week_cut)
d_weekly$week = lubridate::week(d_weekly$week_cut)

d_monthly = aggregate(stars ~ month_cut, d_datd_cut, mean)
d_monthly$month_cut = as.Date(d_monthly$month_cut)
d_monthly$year = lubridate::year(d_monthly$month_cut)
d_monthly$month = lubridate::month(d_monthly$month_cut)

rm(d_datd_cut)

#Sygic
e_date_table = aggregate(stars ~ date, e_data, mean)
e_date_table$date = as.Date(e_date_table$date)
e_date_cut = e_date_table
rm(e_date_table)

e_date_cut$day_cut = cut(e_date_cut$date, breaks = "day")
e_date_cut$week_cut = cut(e_date_cut$date, breaks = "week")
e_date_cut$month_cut = cut(e_date_cut$date, breaks = "month")

e_daily = aggregate(stars ~ day_cut, e_date_cut, mean)
e_daily$day_cut = as.Date(e_daily$day_cut)
e_daily$year = lubridate::year(e_daily$day_cut)
e_daily$day = lubridate::day(e_daily$day_cut)

e_weekly = aggregate(stars ~ week_cut, e_date_cut, mean)
e_weekly$week_cut = as.Date(e_weekly$week_cut)
e_weekly$year = lubridate::year(e_weekly$week_cut)
e_weekly$week = lubridate::week(e_weekly$week_cut)

e_monthly = aggregate(stars ~ month_cut, e_date_cut, mean)
e_monthly$month_cut = as.Date(e_monthly$month_cut)
e_monthly$year = lubridate::year(e_monthly$month_cut)
e_monthly$month = lubridate::month(e_monthly$month_cut)

rm(e_date_cut)

#Maps.me
f_date_table = aggregate(stars ~ date, f_data, mean)
f_date_table$date = as.Date(f_date_table$date)
f_date_cut = f_date_table
rm(f_date_table)

f_date_cut$day_cut = cut(f_date_cut$date, breaks = "day")
f_date_cut$week_cut = cut(f_date_cut$date, breaks = "week")
f_date_cut$month_cut = cut(f_date_cut$date, breaks = "month")

f_daily = aggregate(stars ~ day_cut, f_date_cut, mean)
f_daily$day_cut = as.Date(f_daily$day_cut)
f_daily$year = lubridate::year(f_daily$day_cut)
f_daily$day = lubridate::day(f_daily$day_cut)

f_weekly = aggregate(stars ~ week_cut, f_date_cut, mean)
f_weekly$week_cut = as.Date(f_weekly$week_cut)
f_weekly$year = lubridate::year(f_weekly$week_cut)
f_weekly$week = lubridate::week(f_weekly$week_cut)

f_monthly = aggregate(stars ~ month_cut, f_date_cut, mean)
f_monthly$month_cut = as.Date(f_monthly$month_cut)
f_monthly$year = lubridate::year(f_monthly$month_cut)
f_monthly$month = lubridate::month(f_monthly$month_cut)

rm(f_date_cut)

#Waze
g_date_table = aggregate(stars ~ date, g_data, mean)
g_date_table$date = as.Date(g_date_table$date)
g_date_cut = g_date_table
rm(g_date_table)

g_date_cut$day_cut = cut(g_date_cut$date, breaks = "day")
g_date_cut$week_cut = cut(g_date_cut$date, breaks = "week")
g_date_cut$month_cut = cut(g_date_cut$date, breaks = "month")

g_daily = aggregate(stars ~ day_cut, g_date_cut, mean)
g_daily$day_cut = as.Date(g_daily$day_cut)
g_daily$year = lubridate::year(g_daily$day_cut)
g_daily$day = lubridate::day(g_daily$day_cut)

g_weekly = aggregate(stars ~ week_cut, g_date_cut, mean)
g_weekly$week_cut = as.Date(g_weekly$week_cut)
g_weekly$year = lubridate::year(g_weekly$week_cut)
g_weekly$week = lubridate::week(g_weekly$week_cut)

g_monthly = aggregate(stars ~ month_cut, g_date_cut, mean)
g_monthly$month_cut = as.Date(g_monthly$month_cut)
g_monthly$year = lubridate::year(g_monthly$month_cut)
g_monthly$month = lubridate::month(g_monthly$month_cut)

rm(g_date_cut)

#Here
h_date_table = aggregate(stars ~ date, h_data, mean)
h_date_table$date = as.Date(h_date_table$date)
h_date_cut = h_date_table
rm(h_date_table)

h_date_cut$day_cut = cut(h_date_cut$date, breaks = "day")
h_date_cut$week_cut = cut(h_date_cut$date, breaks = "week")
h_date_cut$month_cut = cut(h_date_cut$date, breaks = "month")

h_daily = aggregate(stars ~ day_cut, h_date_cut, mean)
h_daily$day_cut = as.Date(h_daily$day_cut)
h_daily$year = lubridate::year(h_daily$day_cut)
h_daily$day = lubridate::day(h_daily$day_cut)

h_weekly = aggregate(stars ~ week_cut, h_date_cut, mean)
h_weekly$week_cut = as.Date(h_weekly$week_cut)
h_weekly$year = lubridate::year(h_weekly$week_cut)
h_weekly$week = lubridate::week(h_weekly$week_cut)

h_monthly = aggregate(stars ~ month_cut, h_date_cut, mean)
h_monthly$month_cut = as.Date(h_monthly$month_cut)
h_monthly$year = lubridate::year(h_monthly$month_cut)
h_monthly$month = lubridate::month(h_monthly$month_cut)

rm(h_date_cut)

#Add app names to each table
a_daily$app = "1"
a_weekly$app = "1"
a_monthly$app = "1"
b_daily$app = "2"
b_weekly$app = "2"
b_monthly$app = "2"
c_daily$app = "3"
c_weekly$app = "3"
c_monthly$app = "3"
d_daily$app = "4"
d_weekly$app = "4"
d_monthly$app = "4"
e_daily$app = "5"
e_weekly$app = "5"
e_monthly$app = "5"
f_daily$app = "6"
f_weekly$app = "6"
f_monthly$app = "6"
g_daily$app = "7"
g_weekly$app = "7"
g_monthly$app = "7"
h_daily$app = "8"
h_weekly$app = "8"
h_monthly$app = "8"


#Join all daily tables in to one table
drf1 = full_join(a_daily, b_daily)
drf2 = full_join(drf1, c_daily)
drf3 = full_join(drf2, d_daily)
drf4 = full_join(drf3, e_daily)
drf5 = full_join(drf4, f_daily)
drf6 = full_join(drf5, g_daily)
daily_avg_rtg = full_join(drf6, h_daily)
rm(drf1)
rm(drf2)
rm(drf3)
rm(drf4)
rm(drf5)
rm(drf6)
#Join all weekly tables in to one table
wrf1 = full_join(a_weekly, b_weekly)
wrf2 = full_join(wrf1, c_weekly)
wrf3 = full_join(wrf2, d_weekly)
wrf4 = full_join(wrf3, e_weekly)
wrf5 = full_join(wrf4, f_weekly)
wrf6 = full_join(wrf5, g_weekly)
weekly_avg_rtg = full_join(wrf6, h_weekly)
rm(wrf1)
rm(wrf2)
rm(wrf3)
rm(wrf4)
rm(wrf5)
rm(wrf6)
#Join all monthly tables in to one table
mrf1 = full_join(a_monthly, b_monthly)
mrf2 = full_join(mrf1, c_monthly)
mrf3 = full_join(mrf2, d_monthly)
mrf4 = full_join(mrf3, e_monthly)
mrf5 = full_join(mrf4, f_monthly)
mrf6 = full_join(mrf5, g_monthly)
monthly_avg_rtg = full_join(mrf6, h_monthly)
rm(mrf1)
rm(mrf2)
rm(mrf3)
rm(mrf4)
rm(mrf5)
rm(mrf6)

#Remove individual data
rm(a_daily)
rm(b_daily)
rm(c_daily)
rm(d_daily)
rm(e_daily)
rm(f_daily)
rm(g_daily)
rm(h_daily)
rm(a_weekly)
rm(b_weekly)
rm(c_weekly)
rm(d_weekly)
rm(e_weekly)
rm(f_weekly)
rm(g_weekly)
rm(h_weekly)
rm(a_monthly)
rm(b_monthly)
rm(c_monthly)
rm(d_monthly)
rm(e_monthly)
rm(f_monthly)
rm(g_monthly)
rm(h_monthly)

#Write data
write.csv(daily_avg_rtg, file = 'daily_avg_rtg.csv', row.names = FALSE)
write.csv(weekly_avg_rtg, file = 'weekly_avg_rtg.csv', row.names = FALSE)
write.csv(monthly_avg_rtg, file = 'monthly_avg_rtg.csv', row.names = FALSE)


###Create charts
#Add colors
my_colors2 <- c("#62A8D8", "#BEBA06", "#3DBD9F", "#FAA316",
                "#FF4C43", "#8BC858", "#8962A6", "#ED6D92")

#Define app factors
daily_avg_rtg$app = factor(daily_avg_rtg$app, 
                               levels = c(1,2,3,4,5,6,7,8),
                               labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                          "Sygic", "Mapsme", "Waze", "Here"))
weekly_avg_rtg$app = factor(weekly_avg_rtg$app, 
                                levels = c(1,2,3,4,5,6,7,8),
                                labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                           "Sygic", "Mapsme", "Waze", "Here"))
monthly_avg_rtg$app = factor(monthly_avg_rtg$app, 
                                 levels = c(1,2,3,4,5,6,7,8),
                                 labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                            "Sygic", "Mapsme", "Waze", "Here"))
#Charts
daily_avg_rtg %>%
  ggplot(aes(day_cut, stars, group=app, colour = app)) +
  stat_smooth(method = 'gam', formula = NULL)  +
  scale_x_date(name = "Time (daily)", breaks = as.Date(c("2017-01-01",'2018-01-01',
                                                         "2019-01-01", "2020-01-01")),
               labels = c("2017","2018","2019","2020")) +
  scale_color_manual(values=my_colors2) +
  ylab("Average rating") +
  xlab("Time (daily)") +
  ylim(1,5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Average rating through time - daily smoothed")

weekly_avg_rtg %>%
  ggplot(aes(week_cut, stars, group=app, color=app)) +
  geom_line() +
  scale_x_date(name = "Time (weekly)", breaks = as.Date(c("2017-01-01",'2018-01-01',
                                                          "2019-01-01", "2020-01-01")),
               labels = c("2017","2018","2019","2020")) +
  scale_color_manual(values=my_colors2) +
  ylab("Average rating") +
  xlab("Time (weekly)") +
  ylim(1,5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Average rating through time - weekly")

monthly_avg_rtg %>%
  ggplot(aes(month_cut, stars, group=app, color=app)) +
  geom_line() +
  scale_x_date(name = "Time (monthly)", breaks = as.Date(c("2017-01-01",'2018-01-01',
                                                           "2019-01-01", "2020-01-01")),
               labels = c("2017","2018","2019","2020")) +
  scale_color_manual(values=my_colors2) +
  ylab("Average rating") +
  xlab("Time (monthly)") +
  ylim(1,5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Average rating through time - monthly")

#Remove working data
rm(daily_avg_rtg)
rm(weekly_avg_rtg)
rm(monthly_avg_rtg)
rm(my_colors2)

#########Seasonalities in average rating
#Genious
#Calculate average star rating per day
a_stars = aggregate(stars ~ date, a_data, mean)
a_stars$date = as.Date(a_stars$date)
a_stars$stars = as.numeric(a_stars$stars)
#Break star rating per day of the week, week of the year and month
a_stars$day = lubridate::wday(a_stars$date)
a_stars$week = lubridate::week(a_stars$date)
a_stars$month = lubridate::month(a_stars$date)
#Create daily, weekly and monthly tables
a_daily = aggregate(stars ~ day, a_stars, mean)
a_weekly = aggregate(stars ~ week, a_stars, mean)
a_monthly = aggregate(stars ~ month, a_stars, mean)
rm(a_stars)

#TomTom
b_stars = aggregate(stars ~ date, b_data, mean)
b_stars$date = as.Date(b_stars$date)
b_stars$stars = as.numeric(b_stars$stars)

b_stars$day = lubridate::wday(b_stars$date)
b_stars$week = lubridate::week(b_stars$date)
b_stars$month = lubridate::month(b_stars$date)
b_daily = aggregate(stars ~ day, b_stars, mean)
b_weekly = aggregate(stars ~ week, b_stars, mean)
b_monthly = aggregate(stars ~ month, b_stars, mean)
rm(b_stars)

#CoPilot
c_stars = aggregate(stars ~ date, c_data, mean)
c_stars$date = as.Date(c_stars$date)
c_stars$stars = as.numeric(c_stars$stars)

c_stars$day = lubridate::wday(c_stars$date)
c_stars$week = lubridate::week(c_stars$date)
c_stars$month = lubridate::month(c_stars$date)
c_daily = aggregate(stars ~ day, c_stars, mean)
c_weekly = aggregate(stars ~ week, c_stars, mean)
c_monthly = aggregate(stars ~ month, c_stars, mean)
rm(c_stars)

#Navmii
d_stars = aggregate(stars ~ date, d_data, mean)
d_stars$date = as.Date(d_stars$date)
d_stars$stars = as.numeric(d_stars$stars)

d_stars$day = lubridate::wday(d_stars$date)
d_stars$week = lubridate::week(d_stars$date)
d_stars$month = lubridate::month(d_stars$date)
d_daily = aggregate(stars ~ day, d_stars, mean)
d_weekly = aggregate(stars ~ week, d_stars, mean)
d_monthly = aggregate(stars ~ month, d_stars, mean)
rm(d_stars)

#Sygic
e_stars = aggregate(stars ~ date, e_data, mean)
e_stars$date = as.Date(e_stars$date)
e_stars$stars = as.numeric(e_stars$stars)

e_stars$day = lubridate::wday(e_stars$date)
e_stars$week = lubridate::week(e_stars$date)
e_stars$month = lubridate::month(e_stars$date)
e_daily = aggregate(stars ~ day, e_stars, mean)
e_weekly = aggregate(stars ~ week, e_stars, mean)
e_monthly = aggregate(stars ~ month, e_stars, mean)
rm(e_stars)

#Maps.me
f_stars = aggregate(stars ~ date, f_data, mean)
f_stars$date = as.Date(f_stars$date)
f_stars$stars = as.numeric(f_stars$stars)

f_stars$day = lubridate::wday(f_stars$date)
f_stars$week = lubridate::week(f_stars$date)
f_stars$month = lubridate::month(f_stars$date)
f_daily = aggregate(stars ~ day, f_stars, mean)
f_weekly = aggregate(stars ~ week, f_stars, mean)
f_monthly = aggregate(stars ~ month, f_stars, mean)
rm(f_stars)

#Waze
g_stars = aggregate(stars ~ date, g_data, mean)
g_stars$date = as.Date(g_stars$date)
g_stars$stars = as.numeric(g_stars$stars)

g_stars$day = lubridate::wday(g_stars$date)
g_stars$week = lubridate::week(g_stars$date)
g_stars$month = lubridate::month(g_stars$date)
g_daily = aggregate(stars ~ day, g_stars, mean)
g_weekly = aggregate(stars ~ week, g_stars, mean)
g_monthly = aggregate(stars ~ month, g_stars, mean)
rm(g_stars)

#Here
h_stars = aggregate(stars ~ date, h_data, mean)
h_stars$date = as.Date(h_stars$date)
h_stars$stars = as.numeric(h_stars$stars)

h_stars$day = lubridate::wday(h_stars$date)
h_stars$week = lubridate::week(h_stars$date)
h_stars$month = lubridate::month(h_stars$date)
h_daily = aggregate(stars ~ day, h_stars, mean)
h_weekly = aggregate(stars ~ week, h_stars, mean)
h_monthly = aggregate(stars ~ month, h_stars, mean)
rm(h_stars)

#Add app names to each table
a_daily$app = "1"
a_weekly$app = "1"
a_monthly$app = "1"
b_daily$app = "2"
b_weekly$app = "2"
b_monthly$app = "2"
c_daily$app = "3"
c_weekly$app = "3"
c_monthly$app = "3"
d_daily$app = "4"
d_weekly$app = "4"
d_monthly$app = "4"
e_daily$app = "5"
e_weekly$app = "5"
e_monthly$app = "5"
f_daily$app = "6"
f_weekly$app = "6"
f_monthly$app = "6"
g_daily$app = "7"
g_weekly$app = "7"
g_monthly$app = "7"
h_daily$app = "8"
h_weekly$app = "8"
h_monthly$app = "8"


#Join all daily tables in to one table
drf1 = full_join(a_daily, b_daily)
drf2 = full_join(drf1, c_daily)
drf3 = full_join(drf2, d_daily)
drf4 = full_join(drf3, e_daily)
drf5 = full_join(drf4, f_daily)
drf6 = full_join(drf5, g_daily)
daily_seasonal_avg_rtg = full_join(drf6, h_daily)
rm(drf1)
rm(drf2)
rm(drf3)
rm(drf4)
rm(drf5)
rm(drf6)
#Join all weekly tables in to one table
wrf1 = full_join(a_weekly, b_weekly)
wrf2 = full_join(wrf1, c_weekly)
wrf3 = full_join(wrf2, d_weekly)
wrf4 = full_join(wrf3, e_weekly)
wrf5 = full_join(wrf4, f_weekly)
wrf6 = full_join(wrf5, g_weekly)
weekly_seasonal_avg_rtg = full_join(wrf6, h_weekly)
rm(wrf1)
rm(wrf2)
rm(wrf3)
rm(wrf4)
rm(wrf5)
rm(wrf6)
#Join all monthly tables in to one table
mrf1 = full_join(a_monthly, b_monthly)
mrf2 = full_join(mrf1, c_monthly)
mrf3 = full_join(mrf2, d_monthly)
mrf4 = full_join(mrf3, e_monthly)
mrf5 = full_join(mrf4, f_monthly)
mrf6 = full_join(mrf5, g_monthly)
monthly_seasonal_avg_rtg = full_join(mrf6, h_monthly)
rm(mrf1)
rm(mrf2)
rm(mrf3)
rm(mrf4)
rm(mrf5)
rm(mrf6)

#Remove individual data
rm(a_daily)
rm(b_daily)
rm(c_daily)
rm(d_daily)
rm(e_daily)
rm(f_daily)
rm(g_daily)
rm(h_daily)
rm(a_weekly)
rm(b_weekly)
rm(c_weekly)
rm(d_weekly)
rm(e_weekly)
rm(f_weekly)
rm(g_weekly)
rm(h_weekly)
rm(a_monthly)
rm(b_monthly)
rm(c_monthly)
rm(d_monthly)
rm(e_monthly)
rm(f_monthly)
rm(g_monthly)
rm(h_monthly)

#Write data
write.csv(daily_seasonal_avg_rtg, file = 'daily_seasonal_avg_rtg.csv', row.names = FALSE)
write.csv(weekly_seasonal_avg_rtg, file = 'weekly_seasonal_avg_rtg.csv', row.names = FALSE)
write.csv(monthly_seasonal_avg_rtg, file = 'monthly_seasonal_avg_rtg.csv', row.names = FALSE)

###Create charts
#Add colors
my_colors2 <- c("#62A8D8", "#BEBA06", "#3DBD9F", "#FAA316",
                "#FF4C43", "#8BC858", "#8962A6", "#ED6D92")

#Define app factors
daily_seasonal_avg_rtg$app = factor(daily_seasonal_avg_rtg$app, 
                                        levels = c(1,2,3,4,5,6,7,8),
                                        labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                                   "Sygic", "Mapsme", "Waze", "Here"))
weekly_seasonal_avg_rtg$app = factor(weekly_seasonal_avg_rtg$app, 
                                         levels = c(1,2,3,4,5,6,7,8),
                                         labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                                    "Sygic", "Mapsme", "Waze", "Here"))
monthly_seasonal_avg_rtg$app = factor(monthly_seasonal_avg_rtg$app, 
                                          levels = c(1,2,3,4,5,6,7,8),
                                          labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                                     "Sygic", "Mapsme", "Waze", "Here"))
#Charts
daily_seasonal_avg_rtg %>%
  ggplot(aes(day, stars, group=app, colour = app)) +
  geom_line() +
  scale_x_continuous(name = "Days of the week", breaks = 1:7,
                     labels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                "Friday", "Saturday", "Sunday")) +
  ylab("Average rating") +
  scale_color_manual(values=my_colors2) +
  ylim(1,5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Average rating through days of the week")

weekly_seasonal_avg_rtg %>%
  ggplot(aes(week, stars, group=app, colour = app)) +
  geom_line() +
  scale_x_continuous(name = "Week number", n.breaks = 9) +
  ylab("Average rating") +
  scale_color_manual(values=my_colors2) +
  ylim(1,5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Average rating through weeks of the year")

monthly_seasonal_avg_rtg %>%
  ggplot(aes(month, stars, group=app, colour = app)) +
  geom_line() +
  scale_x_continuous(name = "Months", breaks = 1:12,
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  ylab("Average rating") +
  scale_color_manual(values=my_colors2) +
  ylim(1,5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Average rating through months")

#Remove working data
rm(daily_seasonal_avg_rtg)
rm(weekly_seasonal_avg_rtg)
rm(monthly_seasonal_avg_rtg)
rm(my_colors2)

###################################################Sentiment through time
#Batching data function for sentiment_by(), it makes sentiment calculation quicker for bigger dataframes
batch_sentiment_by <- function(reviews, batch_size = 200, ...) {
  review_batches <- split(reviews, ceiling(seq_along(reviews)/batch_size))
  x <- rbindlist(lapply(review_batches, sentiment_by, ...))
  x[, element_id := .I]
  x[]
}
#Genius maps
#Calculate sentiment of each review
sentiment=batch_sentiment_by(a_data$review)
a_sentiment = data.frame(date = a_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)
#Calculate average sentiment for each day
a_date_table = aggregate(sentiment ~ date, a_sentiment, mean)
a_date_table$date = as.Date(a_date_table$date)
a_data_cut = a_date_table
rm(a_sentiment)
rm(a_date_table)

#Break sentiment per days, wekks and months
a_data_cut$day_cut = cut(a_data_cut$date, breaks = "day")
a_data_cut$week_cut = cut(a_data_cut$date, breaks = "week")
a_data_cut$month_cut = cut(a_data_cut$date, breaks = "month")
#Create daily sentiment table
a_daily = aggregate(sentiment ~ day_cut, a_data_cut, mean)
a_daily$day_cut = as.Date(a_daily$day_cut)
a_daily$year = lubridate::year(a_daily$day_cut)
a_daily$day = lubridate::day(a_daily$day_cut)
a_daily$day_cut = as.Date(a_daily$day_cut)
#Create weekly sentiment table
a_weekly = aggregate(sentiment ~ week_cut, a_data_cut, mean)
a_weekly$week_cut = as.Date(a_weekly$week_cut)
a_weekly$year = lubridate::year(a_weekly$week_cut)
a_weekly$week = lubridate::week(a_weekly$week_cut)
a_weekly$week_cut = as.Date(a_weekly$week_cut)
#Create monthly sentiment table
a_monthly = aggregate(sentiment ~ month_cut, a_data_cut, mean)
a_monthly$month_cut = as.Date(a_monthly$month_cut)
a_monthly$year = lubridate::year(a_monthly$month_cut)
a_monthly$month = lubridate::month(a_monthly$month_cut)
a_monthly$month_cut = as.Date(a_monthly$month_cut)
rm(a_data_cut)

#TomTom
sentiment=batch_sentiment_by(b_data$review)
b_sentiment = data.frame(date = b_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)

b_date_table = aggregate(sentiment ~ date, b_sentiment, mean)
b_date_table$date = as.Date(b_date_table$date)
b_data_cut = b_date_table
rm(b_sentiment)
rm(b_date_table)

b_data_cut$day_cut = cut(b_data_cut$date, breaks = "day")
b_data_cut$week_cut = cut(b_data_cut$date, breaks = "week")
b_data_cut$month_cut = cut(b_data_cut$date, breaks = "month")

b_daily = aggregate(sentiment ~ day_cut, b_data_cut, mean)
b_daily$day_cut = as.Date(b_daily$day_cut)
b_daily$year = lubridate::year(b_daily$day_cut)
b_daily$day = lubridate::day(b_daily$day_cut)
b_daily$day_cut = as.Date(b_daily$day_cut)

b_weekly = aggregate(sentiment ~ week_cut, b_data_cut, mean)
b_weekly$week_cut = as.Date(b_weekly$week_cut)
b_weekly$year = lubridate::year(b_weekly$week_cut)
b_weekly$week = lubridate::week(b_weekly$week_cut)
b_weekly$week_cut = as.Date(b_weekly$week_cut)

b_monthly = aggregate(sentiment ~ month_cut, b_data_cut, mean)
b_monthly$month_cut = as.Date(b_monthly$month_cut)
b_monthly$year = lubridate::year(b_monthly$month_cut)
b_monthly$month = lubridate::month(b_monthly$month_cut)
b_monthly$month_cut = as.Date(b_monthly$month_cut)
rm(b_data_cut)

#CoPilot
sentiment=batch_sentiment_by(c_data$review)
c_sentiment = data.frame(date = c_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)

c_date_table = aggregate(sentiment ~ date, c_sentiment, mean)
c_date_table$date = as.Date(c_date_table$date)
c_data_cut = c_date_table
rm(c_sentiment)
rm(c_date_table)

c_data_cut$day_cut = cut(c_data_cut$date, breaks = "day")
c_data_cut$week_cut = cut(c_data_cut$date, breaks = "week")
c_data_cut$month_cut = cut(c_data_cut$date, breaks = "month")

c_daily = aggregate(sentiment ~ day_cut, c_data_cut, mean)
c_daily$day_cut = as.Date(c_daily$day_cut)
c_daily$year = lubridate::year(c_daily$day_cut)
c_daily$day = lubridate::day(c_daily$day_cut)
c_daily$day_cut = as.Date(c_daily$day_cut)

c_weekly = aggregate(sentiment ~ week_cut, c_data_cut, mean)
c_weekly$week_cut = as.Date(c_weekly$week_cut)
c_weekly$year = lubridate::year(c_weekly$week_cut)
c_weekly$week = lubridate::week(c_weekly$week_cut)
c_weekly$week_cut = as.Date(c_weekly$week_cut)

c_monthly = aggregate(sentiment ~ month_cut, c_data_cut, mean)
c_monthly$month_cut = as.Date(c_monthly$month_cut)
c_monthly$year = lubridate::year(c_monthly$month_cut)
c_monthly$month = lubridate::month(c_monthly$month_cut)
c_monthly$month_cut = as.Date(c_monthly$month_cut)
rm(c_data_cut)

#Navmii
sentiment=batch_sentiment_by(d_data$review)
d_sentiment = data.frame(date = d_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)

d_date_table = aggregate(sentiment ~ date, d_sentiment, mean)
d_date_table$date = as.Date(d_date_table$date)
d_data_cut = d_date_table
rm(d_sentiment)
rm(d_date_table)

d_data_cut$day_cut = cut(d_data_cut$date, breaks = "day")
d_data_cut$week_cut = cut(d_data_cut$date, breaks = "week")
d_data_cut$month_cut = cut(d_data_cut$date, breaks = "month")

d_daily = aggregate(sentiment ~ day_cut, d_data_cut, mean)
d_daily$day_cut = as.Date(d_daily$day_cut)
d_daily$year = lubridate::year(d_daily$day_cut)
d_daily$day = lubridate::day(d_daily$day_cut)
d_daily$day_cut = as.Date(d_daily$day_cut)

d_weekly = aggregate(sentiment ~ week_cut, d_data_cut, mean)
d_weekly$week_cut = as.Date(d_weekly$week_cut)
d_weekly$year = lubridate::year(d_weekly$week_cut)
d_weekly$week = lubridate::week(d_weekly$week_cut)
d_weekly$week_cut = as.Date(d_weekly$week_cut)

d_monthly = aggregate(sentiment ~ month_cut, d_data_cut, mean)
d_monthly$month_cut = as.Date(d_monthly$month_cut)
d_monthly$year = lubridate::year(d_monthly$month_cut)
d_monthly$month = lubridate::month(d_monthly$month_cut)
d_monthly$month_cut = as.Date(d_monthly$month_cut)
rm(d_data_cut)

#Sygic
sentiment=batch_sentiment_by(e_data$review)
e_sentiment = data.frame(date = e_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)

e_date_table = aggregate(sentiment ~ date, e_sentiment, mean)
e_date_table$date = as.Date(e_date_table$date)
e_data_cut = e_date_table
rm(e_sentiment)
rm(e_date_table)

e_data_cut$day_cut = cut(e_data_cut$date, breaks = "day")
e_data_cut$week_cut = cut(e_data_cut$date, breaks = "week")
e_data_cut$month_cut = cut(e_data_cut$date, breaks = "month")

e_daily = aggregate(sentiment ~ day_cut, e_data_cut, mean)
e_daily$day_cut = as.Date(e_daily$day_cut)
e_daily$year = lubridate::year(e_daily$day_cut)
e_daily$day = lubridate::day(e_daily$day_cut)
e_daily$day_cut = as.Date(e_daily$day_cut)

e_weekly = aggregate(sentiment ~ week_cut, e_data_cut, mean)
e_weekly$week_cut = as.Date(e_weekly$week_cut)
e_weekly$year = lubridate::year(e_weekly$week_cut)
e_weekly$week = lubridate::week(e_weekly$week_cut)
e_weekly$week_cut = as.Date(e_weekly$week_cut)

e_monthly = aggregate(sentiment ~ month_cut, e_data_cut, mean)
e_monthly$month_cut = as.Date(e_monthly$month_cut)
e_monthly$year = lubridate::year(e_monthly$month_cut)
e_monthly$month = lubridate::month(e_monthly$month_cut)
e_monthly$month_cut = as.Date(e_monthly$month_cut)
rm(e_data_cut)

#Maps.me
sentiment=batch_sentiment_by(f_data$review)
f_sentiment = data.frame(date = f_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)

f_date_table = aggregate(sentiment ~ date, f_sentiment, mean)
f_date_table$date = as.Date(f_date_table$date)
f_data_cut = f_date_table
rm(f_sentiment)
rm(f_date_table)

f_data_cut$day_cut = cut(f_data_cut$date, breaks = "day")
f_data_cut$week_cut = cut(f_data_cut$date, breaks = "week")
f_data_cut$month_cut = cut(f_data_cut$date, breaks = "month")

f_daily = aggregate(sentiment ~ day_cut, f_data_cut, mean)
f_daily$day_cut = as.Date(f_daily$day_cut)
f_daily$year = lubridate::year(f_daily$day_cut)
f_daily$day = lubridate::day(f_daily$day_cut)
f_daily$day_cut = as.Date(f_daily$day_cut)

f_weekly = aggregate(sentiment ~ week_cut, f_data_cut, mean)
f_weekly$week_cut = as.Date(f_weekly$week_cut)
f_weekly$year = lubridate::year(f_weekly$week_cut)
f_weekly$week = lubridate::week(f_weekly$week_cut)
f_weekly$week_cut = as.Date(f_weekly$week_cut)

f_monthly = aggregate(sentiment ~ month_cut, f_data_cut, mean)
f_monthly$month_cut = as.Date(f_monthly$month_cut)
f_monthly$year = lubridate::year(f_monthly$month_cut)
f_monthly$month = lubridate::month(f_monthly$month_cut)
f_monthly$month_cut = as.Date(f_monthly$month_cut)
rm(f_data_cut)

#Waze
sentiment=batch_sentiment_by(g_data$review)
g_sentiment = data.frame(date = g_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)

g_date_table = aggregate(sentiment ~ date, g_sentiment, mean)
g_date_table$date = as.Date(g_date_table$date)
g_data_cut = g_date_table
rm(g_sentiment)
rm(g_date_table)

g_data_cut$day_cut = cut(g_data_cut$date, breaks = "day")
g_data_cut$week_cut = cut(g_data_cut$date, breaks = "week")
g_data_cut$month_cut = cut(g_data_cut$date, breaks = "month")

g_daily = aggregate(sentiment ~ day_cut, g_data_cut, mean)
g_daily$day_cut = as.Date(g_daily$day_cut)
g_daily$year = lubridate::year(g_daily$day_cut)
g_daily$day = lubridate::day(g_daily$day_cut)
g_daily$day_cut = as.Date(g_daily$day_cut)

g_weekly = aggregate(sentiment ~ week_cut, g_data_cut, mean)
g_weekly$week_cut = as.Date(g_weekly$week_cut)
g_weekly$year = lubridate::year(g_weekly$week_cut)
g_weekly$week = lubridate::week(g_weekly$week_cut)
g_weekly$week_cut = as.Date(g_weekly$week_cut)

g_monthly = aggregate(sentiment ~ month_cut, g_data_cut, mean)
g_monthly$month_cut = as.Date(g_monthly$month_cut)
g_monthly$year = lubridate::year(g_monthly$month_cut)
g_monthly$month = lubridate::month(g_monthly$month_cut)
g_monthly$month_cut = as.Date(g_monthly$month_cut)
rm(g_data_cut)

#Here
sentiment=batch_sentiment_by(h_data$review)
h_sentiment = data.frame(date = h_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)

h_date_table = aggregate(sentiment ~ date, h_sentiment, mean)
h_date_table$date = as.Date(h_date_table$date)
h_data_cut = h_date_table
rm(h_sentiment)
rm(h_date_table)

h_data_cut$day_cut = cut(h_data_cut$date, breaks = "day")
h_data_cut$week_cut = cut(h_data_cut$date, breaks = "week")
h_data_cut$month_cut = cut(h_data_cut$date, breaks = "month")

h_daily = aggregate(sentiment ~ day_cut, h_data_cut, mean)
h_daily$day_cut = as.Date(h_daily$day_cut)
h_daily$year = lubridate::year(h_daily$day_cut)
h_daily$day = lubridate::day(h_daily$day_cut)
h_daily$day_cut = as.Date(h_daily$day_cut)

h_weekly = aggregate(sentiment ~ week_cut, h_data_cut, mean)
h_weekly$week_cut = as.Date(h_weekly$week_cut)
h_weekly$year = lubridate::year(h_weekly$week_cut)
h_weekly$week = lubridate::week(h_weekly$week_cut)
h_weekly$week_cut = as.Date(h_weekly$week_cut)

h_monthly = aggregate(sentiment ~ month_cut, h_data_cut, mean)
h_monthly$month_cut = as.Date(h_monthly$month_cut)
h_monthly$year = lubridate::year(h_monthly$month_cut)
h_monthly$month = lubridate::month(h_monthly$month_cut)
h_monthly$month_cut = as.Date(h_monthly$month_cut)
rm(h_data_cut)

rm(batch_sentiment_by)
#Add app names to each table
a_daily$app = "1"
a_weekly$app = "1"
a_monthly$app = "1"
b_daily$app = "2"
b_weekly$app = "2"
b_monthly$app = "2"
c_daily$app = "3"
c_weekly$app = "3"
c_monthly$app = "3"
d_daily$app = "4"
d_weekly$app = "4"
d_monthly$app = "4"
e_daily$app = "5"
e_weekly$app = "5"
e_monthly$app = "5"
f_daily$app = "6"
f_weekly$app = "6"
f_monthly$app = "6"
g_daily$app = "7"
g_weekly$app = "7"
g_monthly$app = "7"
h_daily$app = "8"
h_weekly$app = "8"
h_monthly$app = "8"


#Join all daily tables in to one table
drf1 = full_join(a_daily, b_daily)
drf2 = full_join(drf1, c_daily)
drf3 = full_join(drf2, d_daily)
drf4 = full_join(drf3, e_daily)
drf5 = full_join(drf4, f_daily)
drf6 = full_join(drf5, g_daily)
daily_sent = full_join(drf6, h_daily)
rm(drf1)
rm(drf2)
rm(drf3)
rm(drf4)
rm(drf5)
rm(drf6)
#Join all weekly tables in to one table
wrf1 = full_join(a_weekly, b_weekly)
wrf2 = full_join(wrf1, c_weekly)
wrf3 = full_join(wrf2, d_weekly)
wrf4 = full_join(wrf3, e_weekly)
wrf5 = full_join(wrf4, f_weekly)
wrf6 = full_join(wrf5, g_weekly)
weekly_sent = full_join(wrf6, h_weekly)
rm(wrf1)
rm(wrf2)
rm(wrf3)
rm(wrf4)
rm(wrf5)
rm(wrf6)
#Join all monthly tables in to one table
mrf1 = full_join(a_monthly, b_monthly)
mrf2 = full_join(mrf1, c_monthly)
mrf3 = full_join(mrf2, d_monthly)
mrf4 = full_join(mrf3, e_monthly)
mrf5 = full_join(mrf4, f_monthly)
mrf6 = full_join(mrf5, g_monthly)
monthly_sent = full_join(mrf6, h_monthly)
rm(mrf1)
rm(mrf2)
rm(mrf3)
rm(mrf4)
rm(mrf5)
rm(mrf6)

#Remove individual data
rm(a_daily)
rm(b_daily)
rm(c_daily)
rm(d_daily)
rm(e_daily)
rm(f_daily)
rm(g_daily)
rm(h_daily)
rm(a_weekly)
rm(b_weekly)
rm(c_weekly)
rm(d_weekly)
rm(e_weekly)
rm(f_weekly)
rm(g_weekly)
rm(h_weekly)
rm(a_monthly)
rm(b_monthly)
rm(c_monthly)
rm(d_monthly)
rm(e_monthly)
rm(f_monthly)
rm(g_monthly)
rm(h_monthly)

#Reevaluate high sentimetn
eval_daily_sentiment = daily_sent %>% mutate(sentiment = if_else(sentiment > 1, 1, sentiment))
eval_weekly_sentiment = weekly_sent %>% mutate(sentiment = if_else(sentiment > 1, 1, sentiment))
eval_monthly_sentiment = monthly_sent %>% mutate(sentiment = if_else(sentiment > 1, 1, sentiment))
daily_sent$sentiment = eval_daily_sentiment$sentiment
weekly_sent$sentiment = eval_weekly_sentiment$sentiment
monthly_sent$sentiment = eval_monthly_sentiment$sentiment
rm(eval_daily_sentiment)
rm(eval_weekly_sentiment)
rm(eval_monthly_sentiment)
#Round sentiment values
daily_sent$sentiment = round(daily_sent$sentiment, digits = 2)
weekly_sent$sentiment = round(weekly_sent$sentiment, digits = 2)
monthly_sent$sentiment = round(monthly_sent$sentiment, digits = 2)

#Write data
write.csv(daily_sent, file = 'daily_sent.csv', row.names = FALSE)
write.csv(weekly_sent, file = 'weekly_sent.csv', row.names = FALSE)
write.csv(monthly_sent, file = 'monthly_sent.csv', row.names = FALSE)


###Create charts
#Add colors
my_colors2 <- c("#62A8D8", "#BEBA06", "#3DBD9F", "#FAA316",
                "#FF4C43", "#8BC858", "#8962A6", "#ED6D92")

#Define app factors
daily_sent$app = factor(daily_sent$app, 
                        levels = c(1,2,3,4,5,6,7,8),
                        labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                   "Sygic", "Mapsme", "Waze", "Here"))
weekly_sent$app = factor(weekly_sent$app, 
                         levels = c(1,2,3,4,5,6,7,8),
                         labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                    "Sygic", "Mapsme", "Waze", "Here"))
monthly_sent$app = factor(monthly_sent$app, 
                          levels = c(1,2,3,4,5,6,7,8),
                          labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                     "Sygic", "Mapsme", "Waze", "Here"))

#Charts
daily_sent %>%
  ggplot(aes(day_cut, sentiment, group=app, colour = app)) +
  geom_line() +
  scale_x_date(name = "Time (daily)", breaks = as.Date(c("2017-01-01",'2018-01-01',
                                                         "2019-01-01", "2020-01-01")),
               labels = c("2017","2018","2019","2020")) +
  scale_color_manual(values=my_colors2) +
  ylab("Sentiment") +
  xlab("Time (daily)") +
  ylim(-1,1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Sentiment through time - daily")

weekly_sent %>%
  ggplot(aes(week_cut, sentiment, group=app, color=app)) +
  geom_line() +
  scale_x_date(name = "Time (weekly)", breaks = as.Date(c("2017-01-01",'2018-01-01',
                                                          "2019-01-01", "2020-01-01")),
               labels = c("2017","2018","2019","2020")) +
  scale_color_manual(values=my_colors2) +
  ylab("Sentiment") +
  xlab("Time (weekly)") +
  ylim(-1,1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Sentiment through time - weekly")

monthly_sent %>%
  ggplot(aes(month_cut, sentiment, group=app, color=app)) +
  geom_line() +
  scale_x_date(name = "Time (monthly)", breaks = as.Date(c("2017-01-01",'2018-01-01',
                                                           "2019-01-01", "2020-01-01")),
               labels = c("2017","2018","2019","2020")) +
  scale_color_manual(values=my_colors2) +
  ylab("Sentiment") +
  xlab("Time (monthly)") +
  ylim(-1,1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Sentiment through time - monthly")

#Remove working data
rm(daily_sent)
rm(weekly_sent)
rm(monthly_sent)

rm(my_colors2)
#########Seasonalities in sentiment
#Batching data function for sentiment_by(), it makes sentiment calculation quicker for bigger dataframes
batch_sentiment_by <- function(reviews, batch_size = 200, ...) {
  review_batches <- split(reviews, ceiling(seq_along(reviews)/batch_size))
  x <- rbindlist(lapply(review_batches, sentiment_by, ...))
  x[, element_id := .I]
  x[]
}
#Genius maps
#Calculate sentiment of each review
sentiment=batch_sentiment_by(a_data$review)
a_sentiment = data.frame(date = a_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)
#Calculate average sentiment for each day
a_sent = aggregate(sentiment ~ date, a_sentiment, mean)
a_sent$date = as.Date(a_sent$date)
a_sent$sentiment = as.numeric(a_sent$sentiment)
#Break sentiment per days of the week, weeks in the year and months
a_sent$day = lubridate::wday(a_sent$date)
a_sent$week = lubridate::week(a_sent$date)
a_sent$month = lubridate::month(a_sent$date)
#Create daily, weekly and monthly tables
a_daily = aggregate(sentiment ~ day, a_sent, mean)
a_weekly = aggregate(sentiment ~ week, a_sent, mean)
a_monthly = aggregate(sentiment ~ month, a_sent, mean)
rm(a_sentiment)
rm(a_sent)

#TomTom
sentiment=batch_sentiment_by(b_data$review)
b_sentiment = data.frame(date = b_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)
b_sent = aggregate(sentiment ~ date, b_sentiment, mean)
b_sent$date = as.Date(b_sent$date)
b_sent$sentiment = as.numeric(b_sent$sentiment)
b_sent$day = lubridate::wday(b_sent$date)
b_sent$week = lubridate::week(b_sent$date)
b_sent$month = lubridate::month(b_sent$date)
b_daily = aggregate(sentiment ~ day, b_sent, mean)
b_weekly = aggregate(sentiment ~ week, b_sent, mean)
b_monthly = aggregate(sentiment ~ month, b_sent, mean)
rm(b_sentiment)
rm(b_sent)

#CoPilot
sentiment=batch_sentiment_by(c_data$review)
c_sentiment = data.frame(date = c_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)
c_sent = aggregate(sentiment ~ date, c_sentiment, mean)
c_sent$date = as.Date(c_sent$date)
c_sent$sentiment = as.numeric(c_sent$sentiment)
c_sent$day = lubridate::wday(c_sent$date)
c_sent$week = lubridate::week(c_sent$date)
c_sent$month = lubridate::month(c_sent$date)
c_daily = aggregate(sentiment ~ day, c_sent, mean)
c_weekly = aggregate(sentiment ~ week, c_sent, mean)
c_monthly = aggregate(sentiment ~ month, c_sent, mean)
rm(c_sentiment)
rm(c_sent)

#Navmii
sentiment=batch_sentiment_by(d_data$review)
d_sentiment = data.frame(date = d_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)
d_sent = aggregate(sentiment ~ date, d_sentiment, mean)
d_sent$date = as.Date(d_sent$date)
d_sent$sentiment = as.numeric(d_sent$sentiment)
d_sent$day = lubridate::wday(d_sent$date)
d_sent$week = lubridate::week(d_sent$date)
d_sent$month = lubridate::month(d_sent$date)
d_daily = aggregate(sentiment ~ day, d_sent, mean)
d_weekly = aggregate(sentiment ~ week, d_sent, mean)
d_monthly = aggregate(sentiment ~ month, d_sent, mean)
rm(d_sentiment)
rm(d_sent)

#Sygic
sentiment=batch_sentiment_by(e_data$review)
e_sentiment = data.frame(date = e_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)
e_sent = aggregate(sentiment ~ date, e_sentiment, mean)
e_sent$date = as.Date(e_sent$date)
e_sent$sentiment = as.numeric(e_sent$sentiment)
e_sent$day = lubridate::wday(e_sent$date)
e_sent$week = lubridate::week(e_sent$date)
e_sent$month = lubridate::month(e_sent$date)
e_daily = aggregate(sentiment ~ day, e_sent, mean)
e_weekly = aggregate(sentiment ~ week, e_sent, mean)
e_monthly = aggregate(sentiment ~ month, e_sent, mean)
rm(e_sentiment)
rm(e_sent)

#Mapsm.me
sentiment=batch_sentiment_by(f_data$review)
f_sentiment = data.frame(date = f_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)
f_sent = aggregate(sentiment ~ date, f_sentiment, mean)
f_sent$date = as.Date(f_sent$date)
f_sent$sentiment = as.numeric(f_sent$sentiment)
f_sent$day = lubridate::wday(f_sent$date)
f_sent$week = lubridate::week(f_sent$date)
f_sent$month = lubridate::month(f_sent$date)
f_daily = aggregate(sentiment ~ day, f_sent, mean)
f_weekly = aggregate(sentiment ~ week, f_sent, mean)
f_monthly = aggregate(sentiment ~ month, f_sent, mean)
rm(f_sentiment)
rm(f_sent)

#Waze
sentiment=batch_sentiment_by(g_data$review)
g_sentiment = data.frame(date = g_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)
g_sent = aggregate(sentiment ~ date, g_sentiment, mean)
g_sent$date = as.Date(g_sent$date)
g_sent$sentiment = as.numeric(g_sent$sentiment)
g_sent$day = lubridate::wday(g_sent$date)
g_sent$week = lubridate::week(g_sent$date)
g_sent$month = lubridate::month(g_sent$date)
g_daily = aggregate(sentiment ~ day, g_sent, mean)
g_weekly = aggregate(sentiment ~ week, g_sent, mean)
g_monthly = aggregate(sentiment ~ month, g_sent, mean)
rm(g_sentiment)
rm(g_sent)

#Here
sentiment=batch_sentiment_by(h_data$review)
h_sentiment = data.frame(date = h_data$date,
                         sentiment = sentiment$ave_sentiment)
rm(sentiment)
h_sent = aggregate(sentiment ~ date, h_sentiment, mean)
h_sent$date = as.Date(h_sent$date)
h_sent$sentiment = as.numeric(h_sent$sentiment)
h_sent$day = lubridate::wday(h_sent$date)
h_sent$week = lubridate::week(h_sent$date)
h_sent$month = lubridate::month(h_sent$date)
h_daily = aggregate(sentiment ~ day, h_sent, mean)
h_weekly = aggregate(sentiment ~ week, h_sent, mean)
h_monthly = aggregate(sentiment ~ month, h_sent, mean)
rm(h_sentiment)
rm(h_sent)

#Add app names to each table
a_daily$app = "1"
a_weekly$app = "1"
a_monthly$app = "1"
b_daily$app = "2"
b_weekly$app = "2"
b_monthly$app = "2"
c_daily$app = "3"
c_weekly$app = "3"
c_monthly$app = "3"
d_daily$app = "4"
d_weekly$app = "4"
d_monthly$app = "4"
e_daily$app = "5"
e_weekly$app = "5"
e_monthly$app = "5"
f_daily$app = "6"
f_weekly$app = "6"
f_monthly$app = "6"
g_daily$app = "7"
g_weekly$app = "7"
g_monthly$app = "7"
h_daily$app = "8"
h_weekly$app = "8"
h_monthly$app = "8"


#Join all daily tables in to one table
drf1 = full_join(a_daily, b_daily)
drf2 = full_join(drf1, c_daily)
drf3 = full_join(drf2, d_daily)
drf4 = full_join(drf3, e_daily)
drf5 = full_join(drf4, f_daily)
drf6 = full_join(drf5, g_daily)
daily_seasonal_sentiment = full_join(drf6, h_daily)
rm(drf1)
rm(drf2)
rm(drf3)
rm(drf4)
rm(drf5)
rm(drf6)
#Join all weekly tables in to one table
wrf1 = full_join(a_weekly, b_weekly)
wrf2 = full_join(wrf1, c_weekly)
wrf3 = full_join(wrf2, d_weekly)
wrf4 = full_join(wrf3, e_weekly)
wrf5 = full_join(wrf4, f_weekly)
wrf6 = full_join(wrf5, g_weekly)
weekly_seasonal_sentiment = full_join(wrf6, h_weekly)
rm(wrf1)
rm(wrf2)
rm(wrf3)
rm(wrf4)
rm(wrf5)
rm(wrf6)
#Join all monthly tables in to one table
mrf1 = full_join(a_monthly, b_monthly)
mrf2 = full_join(mrf1, c_monthly)
mrf3 = full_join(mrf2, d_monthly)
mrf4 = full_join(mrf3, e_monthly)
mrf5 = full_join(mrf4, f_monthly)
mrf6 = full_join(mrf5, g_monthly)
monthly_seasonal_sentiment = full_join(mrf6, h_monthly)
rm(mrf1)
rm(mrf2)
rm(mrf3)
rm(mrf4)
rm(mrf5)
rm(mrf6)

#Remove individual data
rm(a_daily)
rm(b_daily)
rm(c_daily)
rm(d_daily)
rm(e_daily)
rm(f_daily)
rm(g_daily)
rm(h_daily)
rm(a_weekly)
rm(b_weekly)
rm(c_weekly)
rm(d_weekly)
rm(e_weekly)
rm(f_weekly)
rm(g_weekly)
rm(h_weekly)
rm(a_monthly)
rm(b_monthly)
rm(c_monthly)
rm(d_monthly)
rm(e_monthly)
rm(f_monthly)
rm(g_monthly)
rm(h_monthly)
rm(batch_sentiment_by)

#Reevaluate high sentimetn
eval_daily_sentiment = daily_seasonal_sentiment %>% mutate(sentiment = if_else(sentiment > 1, 1, sentiment))
eval_weekly_sentiment = weekly_seasonal_sentiment %>% mutate(sentiment = if_else(sentiment > 1, 1, sentiment))
eval_monthly_sentiment = monthly_seasonal_sentiment %>% mutate(sentiment = if_else(sentiment > 1, 1, sentiment))
daily_seasonal_sentiment$sentiment = eval_daily_sentiment$sentiment
weekly_seasonal_sentiment$sentiment = eval_weekly_sentiment$sentiment
monthly_seasonal_sentiment$sentiment = eval_monthly_sentiment$sentiment
rm(eval_daily_sentiment)
rm(eval_weekly_sentiment)
rm(eval_monthly_sentiment)
#Round sentiment values
daily_seasonal_sentiment$sentiment = round(daily_seasonal_sentiment$sentiment, digits = 2)
weekly_seasonal_sentiment$sentiment = round(weekly_seasonal_sentiment$sentiment, digits = 2)
monthly_seasonal_sentiment$sentiment = round(monthly_seasonal_sentiment$sentiment, digits = 2)

#Write data
write.csv(daily_seasonal_sentiment, file = 'daily_seasonal_sentiment.csv', row.names = FALSE)
write.csv(weekly_seasonal_sentiment, file = 'weekly_seasonal_sentiment.csv', row.names = FALSE)
write.csv(monthly_seasonal_sentiment, file = 'monthly_seasonal_sentiment.csv', row.names = FALSE)

###Create charts
#Add colors
my_colors2 <- c("#62A8D8", "#BEBA06", "#3DBD9F", "#FAA316",
                "#FF4C43", "#8BC858", "#8962A6", "#ED6D92")

#Define app factors
daily_seasonal_sentiment$app = factor(daily_seasonal_sentiment$app, 
                                      levels = c(1,2,3,4,5,6,7,8),
                                      labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                                 "Sygic", "Mapsme", "Waze", "Here"))
weekly_seasonal_sentiment$app = factor(weekly_seasonal_sentiment$app, 
                                       levels = c(1,2,3,4,5,6,7,8),
                                       labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                                  "Sygic", "Mapsme", "Waze", "Here"))
monthly_seasonal_sentiment$app = factor(monthly_seasonal_sentiment$app, 
                                        levels = c(1,2,3,4,5,6,7,8),
                                        labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                                   "Sygic", "Mapsme", "Waze", "Here"))

#Charts
daily_seasonal_sentiment %>%
  ggplot(aes(day, sentiment, group=app, colour = app)) +
  geom_line() +
  scale_x_continuous(name = "Days of the week", breaks = 1:7,
                     labels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                "Friday", "Saturday", "Sunday")) +
  ylab("Average sentiment") +
  scale_color_manual(values=my_colors2) +
  ylim(-1,1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Average sentiment through days of the week")

weekly_seasonal_sentiment %>%
  ggplot(aes(week, sentiment, group=app, colour = app)) +
  geom_line() +
  scale_x_continuous(name = "Week number", n.breaks = 9) +
  ylab("Average sentiment") +
  scale_color_manual(values=my_colors2) +
  ylim(-1,1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Average sentiment through weeks of the year")

monthly_seasonal_sentiment %>%
  ggplot(aes(month, sentiment, group=app, colour = app)) +
  geom_line() +
  scale_x_continuous(name = "Months", breaks = 1:12,
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  ylab("Average sentiment") +
  scale_color_manual(values=my_colors2) +
  ylim(-1,1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Average sentiment through months")

#Remove working data
rm(daily_seasonal_sentiment)
rm(weekly_seasonal_sentiment)
rm(monthly_seasonal_sentiment)
rm(my_colors2)

####Frequency of words
#Genius Maps
#Create corpus for text analysis
corpus_review=Corpus(VectorSource(a_data$review))
corpus_review=tm_map(corpus_review, tolower) #all words to lower cases
corpus_review=tm_map(corpus_review, removePunctuation) #remove punctuations
corpus_review=tm_map(corpus_review, removeWords, stopwords("en")) #remove stopwords
corpus_review=tm_map(corpus_review, stemDocument) #stem all words
#Create tdm to further work on data
review_tdm <- TermDocumentMatrix(corpus_review) 
review_m <- as.matrix(review_tdm)
#Calculate word frequency and sort it
review_term_freq <- rowSums(review_m)
review_term_freq <- sort(review_term_freq, decreasing = T)
rm(review_m)
#Select only the top 20 most frequent words and add them to a neat table
tf0 = as.data.frame(review_term_freq[1:20])
a_wf = as.data.frame(rownames(tf0))
colnames(a_wf) = "words"
freq = as.data.frame(tf0[1])
colnames(freq) = "n"
a_wf$n = freq$n
a_wf$pct = (a_wf$n/nrow(a_data)*100)
a_wf$pct = round(a_wf$pct, digits = 1)
rm(freq)
rm(tf0)
rm(review_term_freq)
rm(review_tdm)
rm(corpus_review)

#TomTom
corpus_review=Corpus(VectorSource(b_data$review))
corpus_review=tm_map(corpus_review, tolower)
corpus_review=tm_map(corpus_review, removePunctuation)
corpus_review=tm_map(corpus_review, removeWords, stopwords("en"))
corpus_review=tm_map(corpus_review, stemDocument)
review_tdm <- TermDocumentMatrix(corpus_review)
review_m <- as.matrix(review_tdm)
review_term_freq <- rowSums(review_m)
review_term_freq <- sort(review_term_freq, decreasing = T)
rm(review_m)
tf0 = as.data.frame(review_term_freq[1:20])
b_wf = as.data.frame(rownames(tf0))
colnames(b_wf) = "words"
freq = as.data.frame(tf0[1])
colnames(freq) = "n"
b_wf$n = freq$n
b_wf$pct = (b_wf$n/nrow(b_data)*100)
b_wf$pct = round(b_wf$pct, digits = 1)
rm(freq)
rm(tf0)
rm(review_term_freq)
rm(review_tdm)
rm(corpus_review)

#CoPilot
corpus_review=Corpus(VectorSource(c_data$review))
corpus_review=tm_map(corpus_review, tolower)
corpus_review=tm_map(corpus_review, removePunctuation)
corpus_review=tm_map(corpus_review, removeWords, stopwords("en"))
corpus_review=tm_map(corpus_review, stemDocument)
review_tdm <- TermDocumentMatrix(corpus_review)
review_m <- as.matrix(review_tdm)
review_term_freq <- rowSums(review_m)
review_term_freq <- sort(review_term_freq, decreasing = T)
rm(review_m)
tf0 = as.data.frame(review_term_freq[1:20])
c_wf = as.data.frame(rownames(tf0))
colnames(c_wf) = "words"
freq = as.data.frame(tf0[1])
colnames(freq) = "n"
c_wf$n = freq$n
c_wf$pct = (c_wf$n/nrow(c_data)*100)
c_wf$pct = round(c_wf$pct, digits = 1)
rm(freq)
rm(tf0)
rm(review_term_freq)
rm(review_tdm)
rm(corpus_review)

#Navmii
corpus_review=Corpus(VectorSource(d_data$review))
corpus_review=tm_map(corpus_review, tolower)
corpus_review=tm_map(corpus_review, removePunctuation)
corpus_review=tm_map(corpus_review, removeWords, stopwords("en"))
corpus_review=tm_map(corpus_review, stemDocument)
review_tdm <- TermDocumentMatrix(corpus_review)
review_m <- as.matrix(review_tdm)
review_term_freq <- rowSums(review_m)
review_term_freq <- sort(review_term_freq, decreasing = T)
rm(review_m)
tf0 = as.data.frame(review_term_freq[1:20])
d_wf = as.data.frame(rownames(tf0))
colnames(d_wf) = "words"
freq = as.data.frame(tf0[1])
colnames(freq) = "n"
d_wf$n = freq$n
d_wf$pct = (d_wf$n/nrow(d_data)*100)
d_wf$pct = round(d_wf$pct, digits = 1)
rm(freq)
rm(tf0)
rm(review_term_freq)
rm(review_tdm)
rm(corpus_review)

#Sygic
corpus_review=Corpus(VectorSource(e_data$review))
corpus_review=tm_map(corpus_review, tolower)
corpus_review=tm_map(corpus_review, removePunctuation)
corpus_review=tm_map(corpus_review, removeWords, stopwords("en"))
corpus_review=tm_map(corpus_review, stemDocument)
review_tdm <- TermDocumentMatrix(corpus_review)
review_m <- as.matrix(review_tdm)
review_term_freq <- rowSums(review_m)
review_term_freq <- sort(review_term_freq, decreasing = T)
rm(review_m)
tf0 = as.data.frame(review_term_freq[1:20])
e_wf = as.data.frame(rownames(tf0))
colnames(e_wf) = "words"
freq = as.data.frame(tf0[1])
colnames(freq) = "n"
e_wf$n = freq$n
e_wf$pct = (e_wf$n/nrow(e_data)*100)
e_wf$pct = round(e_wf$pct, digits = 1)
rm(freq)
rm(tf0)
rm(review_term_freq)
rm(review_tdm)
rm(corpus_review)

#Maps.me
corpus_review=Corpus(VectorSource(f_data$review))
corpus_review=tm_map(corpus_review, tolower)
corpus_review=tm_map(corpus_review, removePunctuation)
corpus_review=tm_map(corpus_review, removeWords, stopwords("en"))
corpus_review=tm_map(corpus_review, stemDocument)
review_tdm <- TermDocumentMatrix(corpus_review)
review_m <- as.matrix(review_tdm)
review_term_freq <- rowSums(review_m)
review_term_freq <- sort(review_term_freq, decreasing = T)
rm(review_m)
tf0 = as.data.frame(review_term_freq[1:20])
f_wf = as.data.frame(rownames(tf0))
colnames(f_wf) = "words"
freq = as.data.frame(tf0[1])
colnames(freq) = "n"
f_wf$n = freq$n
f_wf$pct = (f_wf$n/nrow(f_data)*100)
f_wf$pct = round(f_wf$pct, digits = 1)
rm(freq)
rm(tf0)
rm(review_term_freq)
rm(review_tdm)
rm(corpus_review)

#Waze
corpus_review=Corpus(VectorSource(g_data$review))
corpus_review=tm_map(corpus_review, tolower)
corpus_review=tm_map(corpus_review, removePunctuation)
corpus_review=tm_map(corpus_review, removeWords, stopwords("en"))
corpus_review=tm_map(corpus_review, stemDocument)
review_tdm <- TermDocumentMatrix(corpus_review)
review_m <- as.matrix(review_tdm)
review_term_freq <- rowSums(review_m)
review_term_freq <- sort(review_term_freq, decreasing = T)
rm(review_m)
tf0 = as.data.frame(review_term_freq[1:20])
g_wf = as.data.frame(rownames(tf0))
colnames(g_wf) = "words"
freq = as.data.frame(tf0[1])
colnames(freq) = "n"
g_wf$n = freq$n
g_wf$pct = (g_wf$n/nrow(g_data)*100)
g_wf$pct = round(g_wf$pct, digits = 1)
rm(freq)
rm(tf0)
rm(review_term_freq)
rm(review_tdm)
rm(corpus_review)

#Here
corpus_review=Corpus(VectorSource(h_data$review))
corpus_review=tm_map(corpus_review, tolower)
corpus_review=tm_map(corpus_review, removePunctuation)
corpus_review=tm_map(corpus_review, removeWords, stopwords("en"))
corpus_review=tm_map(corpus_review, stemDocument)
review_tdm <- TermDocumentMatrix(corpus_review)
review_m <- as.matrix(review_tdm)
review_term_freq <- rowSums(review_m)
review_term_freq <- sort(review_term_freq, decreasing = T)
rm(review_m)
tf0 = as.data.frame(review_term_freq[1:20])
h_wf = as.data.frame(rownames(tf0))
colnames(h_wf) = "words"
freq = as.data.frame(tf0[1])
colnames(freq) = "n"
h_wf$n = freq$n
h_wf$pct = (h_wf$n/nrow(h_data)*100)
h_wf$pct = round(h_wf$pct, digits = 1)
rm(freq)
rm(tf0)
rm(review_term_freq)
rm(review_tdm)
rm(corpus_review)

#Add app names to each table
a_wf$app = "1"
b_wf$app = "2"
c_wf$app = "3"
d_wf$app = "4"
e_wf$app = "5"
f_wf$app = "6"
g_wf$app = "7"
h_wf$app = "8"

#Join all daily tables in to one table
drf1 = full_join(a_wf, b_wf)
drf2 = full_join(drf1, c_wf)
drf3 = full_join(drf2, d_wf)
drf4 = full_join(drf3, e_wf)
drf5 = full_join(drf4, f_wf)
drf6 = full_join(drf5, g_wf)
word_frequency = full_join(drf6, h_wf)
rm(drf1)
rm(drf2)
rm(drf3)
rm(drf4)
rm(drf5)
rm(drf6)

rm(a_wf)
rm(b_wf)
rm(c_wf)
rm(d_wf)
rm(e_wf)
rm(f_wf)
rm(g_wf)
rm(h_wf)

#Write data to be presented in a table inside the dashboard
write.csv(word_frequency, file = 'word_frequency.csv', row.names = FALSE)

#Create data to be presented on a chart inside the dasahboard
data = word_frequency

wf_data = data.frame(
  words1 = data[data$app == "1", "words"],
  pct1 = data[data$app == "1", "pct"],
  words2 = data[data$app == "2", "words"],
  pct2 = data[data$app == "2", "pct"],
  words3 = data[data$app == "3", "words"],
  pct3 = data[data$app == "3", "pct"],
  words4 = data[data$app == "4", "words"],
  pct4 = data[data$app == "4", "pct"],
  words5 = data[data$app == "5", "words"],
  pct5 = data[data$app == "5", "pct"],
  words6 = data[data$app == "6", "words"],
  pct6 = data[data$app == "6", "pct"],
  words7 = data[data$app == "7", "words"],
  pct7 = data[data$app == "7", "pct"],
  words8 = data[data$app == "8", "words"],
  pct8 = data[data$app == "8", "pct"]
)

write.csv(wf_data, file = 'word_frequency_v2.csv', row.names = FALSE)

#Create chart
my_colors2 <- c("#62A8D8", "#BEBA06", "#3DBD9F", "#FAA316",
                "#FF4C43", "#8BC858", "#8962A6", "#ED6D92")
#Change fill = my_colors2[x] to show different apps
wf_data %>% ggplot() +
  geom_col(aes(x=reorder(words1, -pct1), y=pct1), fill = my_colors2[1]) +
  geom_text(aes(x=reorder(words1, -pct1), y=pct1, label =pct1),
            position = position_dodge(0.9), size = 3.5, fontface = "bold", vjust = -1) +
  theme_bw()+
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold")) + 
  labs(x="Words",y="Share of words in reviews (%)",title=paste("Frequency of words - app_name"))

rm(data)
rm(wf_data)
rm(word_frequency)
rm(my_colors2)

##### Review frequency prediction using BSTS machine learning model
#Genius
#Create review frequency per date table
date_table = setNames(data.frame(table(a_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
#Fill in any dates with 0 reviews
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
#Populate empty dates with 0
date_table_full[is.na(date_table_full)] <- 0
train_data = date_table_full
rm(date_table)
#Create POSIXct data to work on TS model
ts_train = xts(train_data[, -1], order.by = as.POSIXct(train_data$Date))
#Model (I have worked with different models; BSTS, ARIMA and Exponential smoothing, and BSTS was the most accurate one)
#Feel free to work on the model and create more accurate one that suits your data
sdy <- sd(train_data$Count)
#Define model parameters for Semilocal Linear Trend BSTS
level.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
slope.ar1.prior <- Ar1CoefficientPrior(mu=0,
                                       sigma=0.001)
slope.mean.prior <- NormalPrior(mu=0,
                                sigma=0.1*sdy)
slope.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
state.specification <- AddSemilocalLinearTrend(list(),
                                               ts_train,
                                               level.sigma.prior=level.sigma.prior,
                                               slope.ar1.prior=slope.ar1.prior,
                                               slope.mean.prior=slope.mean.prior,
                                               slope.sigma.prior=slope.sigma.prior)
bsts_model <- bsts(ts_train,
                   state.specification=state.specification,
                   niter=10)
#Predict data for the next 365 days
pred_ts <- predict(bsts_model, horizon = 365)

#Remove
rm(ts_train)
rm(date_table_full)
rm(train_data)
rm(sdy)
rm(level.sigma.prior)
rm(slope.ar1.prior)
rm(slope.mean.prior)
rm(slope.sigma.prior)
rm(state.specification)
rm(bsts_model)

#Predicted data
#Take predicted data
mean = as.data.frame(pred_ts$mean)
colnames(mean) = "pred"
#Take 95% confidence interval of predicted data
interval1 = as.data.frame(pred_ts$interval)
interval = t(interval1)
interval = as.data.frame(interval)
low_interval = as.data.frame(interval$`2.5%`)
high_interval = as.data.frame(interval$`97.5%`)
colnames(low_interval) = "low"
colnames(high_interval) = "high"
low_interval = low_interval %>% mutate(low = if_else(low < 0, 0, low)) #Substitute negative predictions with 0
#Create dates
date = seq(as.Date("2020-06-16"), as.Date("2021-06-15"), by="days")
date = as.data.frame(date)
#Create predicted data table
data_new = data.frame(date = date, 
                      mean = mean, 
                      low_interval = low_interval, 
                      high_interval = high_interval)
#Remove
rm(date)
rm(high_interval)
rm(low_interval)
rm(interval)
rm(interval1)
rm(mean)
rm(pred_ts)

#Old data
#Create review frequency per date table
date_table = setNames(data.frame(table(a_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
#Fill in any dates with 0 reviews
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
#Populate empty dates with 0
date_table_full[is.na(date_table_full)] <- 0
data_old = date_table_full
rm(date_table)
rm(date_table_full)

#Create weekly cuts
data_old$week_cut = cut(data_old$Date, breaks = "week")

weekly_old_a = aggregate(Count ~ week_cut, data_old, sum)
weekly_old_a$week_cut = as.Date(weekly_old_a$week_cut)
weekly_old_a$year = lubridate::year(weekly_old_a$week_cut)
weekly_old_a$week = lubridate::week(weekly_old_a$week_cut)

#Predicted data - create weekly cuts
data_new$week_cut = cut(data_new$date, breaks = "week")

weekly_new_a = aggregate(pred ~ week_cut, data_new, sum)
weekly_new_a$week_cut = as.Date(weekly_new_a$week_cut)
weekly_new_a$year = lubridate::year(weekly_new_a$week_cut)
weekly_new_a$week = lubridate::week(weekly_new_a$week_cut)
#Predicted-low interval
weekly_low_a = aggregate(low ~ week_cut, data_new, sum)
weekly_low_a$week_cut = as.Date(weekly_low_a$week_cut)
weekly_low_a$year = lubridate::year(weekly_low_a$week_cut)
weekly_low_a$week = lubridate::week(weekly_low_a$week_cut)
weekly_new_a$low = weekly_low_a$low
#Predicted-high interval
weekly_high_a = aggregate(high ~ week_cut, data_new, sum)
weekly_high_a$week_cut = as.Date(weekly_high_a$week_cut)
weekly_high_a$year = lubridate::year(weekly_high_a$week_cut)
weekly_high_a$week = lubridate::week(weekly_high_a$week_cut)
weekly_new_a$high = weekly_high_a$high


rm(weekly_low_a)
rm(weekly_high_a)
rm(data_new)

#TomTom
date_table = setNames(data.frame(table(b_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
date_table_full[is.na(date_table_full)] <- 0
train_data = date_table_full
rm(date_table)

ts_train = xts(train_data[, -1], order.by = as.POSIXct(train_data$Date))
#Model
sdy <- sd(train_data$Count)
level.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
slope.ar1.prior <- Ar1CoefficientPrior(mu=0,
                                       sigma=0.001)
slope.mean.prior <- NormalPrior(mu=0,
                                sigma=0.1*sdy)
slope.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
state.specification <- AddSemilocalLinearTrend(list(),
                                               ts_train,
                                               level.sigma.prior=level.sigma.prior,
                                               slope.ar1.prior=slope.ar1.prior,
                                               slope.mean.prior=slope.mean.prior,
                                               slope.sigma.prior=slope.sigma.prior)
bsts_model <- bsts(ts_train,
                   state.specification=state.specification,
                   niter=10)
pred_ts <- predict(bsts_model, horizon = 365)

#Remove
rm(ts_train)
rm(date_table_full)
rm(train_data)
rm(sdy)
rm(level.sigma.prior)
rm(slope.ar1.prior)
rm(slope.mean.prior)
rm(slope.sigma.prior)
rm(state.specification)
rm(bsts_model)

#Predicted data
mean = as.data.frame(pred_ts$mean)
colnames(mean) = "pred"
interval1 = as.data.frame(pred_ts$interval)
interval = t(interval1)
interval = as.data.frame(interval)
low_interval = as.data.frame(interval$`2.5%`)
high_interval = as.data.frame(interval$`97.5%`)
colnames(low_interval) = "low"
colnames(high_interval) = "high"
low_interval = low_interval %>% mutate(low = if_else(low < 0, 0, low))
date = seq(as.Date("2020-06-16"), as.Date("2021-06-15"), by="days")
date = as.data.frame(date)
data_new = data.frame(date = date, 
                      mean = mean, 
                      low_interval = low_interval, 
                      high_interval = high_interval)
#Remove
rm(date)
rm(high_interval)
rm(low_interval)
rm(interval)
rm(interval1)
rm(mean)
rm(pred_ts)

#Old data
date_table = setNames(data.frame(table(b_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
date_table_full[is.na(date_table_full)] <- 0
data_old = date_table_full
rm(date_table)
rm(date_table_full)

#Create cuts
data_old$week_cut = cut(data_old$Date, breaks = "week")

weekly_old_b = aggregate(Count ~ week_cut, data_old, sum)
weekly_old_b$week_cut = as.Date(weekly_old_b$week_cut)
weekly_old_b$year = lubridate::year(weekly_old_b$week_cut)
weekly_old_b$week = lubridate::week(weekly_old_b$week_cut)

rm(data_old)
#Predicted data - create cuts
data_new$week_cut = cut(data_new$date, breaks = "week")

weekly_new_b = aggregate(pred ~ week_cut, data_new, sum)
weekly_new_b$week_cut = as.Date(weekly_new_b$week_cut)
weekly_new_b$year = lubridate::year(weekly_new_b$week_cut)
weekly_new_b$week = lubridate::week(weekly_new_b$week_cut)
#Predicted-low
weekly_low_b = aggregate(low ~ week_cut, data_new, sum)
weekly_low_b$week_cut = as.Date(weekly_low_b$week_cut)
weekly_low_b$year = lubridate::year(weekly_low_b$week_cut)
weekly_low_b$week = lubridate::week(weekly_low_b$week_cut)
weekly_new_b$low = weekly_low_b$low
#Predicted-high
weekly_high_b = aggregate(high ~ week_cut, data_new, sum)
weekly_high_b$week_cut = as.Date(weekly_high_b$week_cut)
weekly_high_b$year = lubridate::year(weekly_high_b$week_cut)
weekly_high_b$week = lubridate::week(weekly_high_b$week_cut)
weekly_new_b$high = weekly_high_b$high


rm(weekly_low_b)
rm(weekly_high_b)
rm(data_new)

#CoPilot
date_table = setNames(data.frame(table(c_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
date_table_full[is.na(date_table_full)] <- 0
train_data = date_table_full
rm(date_table)

ts_train = xts(train_data[, -1], order.by = as.POSIXct(train_data$Date))
#Model
sdy <- sd(train_data$Count)
level.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
slope.ar1.prior <- Ar1CoefficientPrior(mu=0,
                                       sigma=0.001)
slope.mean.prior <- NormalPrior(mu=0,
                                sigma=0.1*sdy)
slope.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
state.specification <- AddSemilocalLinearTrend(list(),
                                               ts_train,
                                               level.sigma.prior=level.sigma.prior,
                                               slope.ar1.prior=slope.ar1.prior,
                                               slope.mean.prior=slope.mean.prior,
                                               slope.sigma.prior=slope.sigma.prior)
bsts_model <- bsts(ts_train,
                   state.specification=state.specification,
                   niter=10)
pred_ts <- predict(bsts_model, horizon = 365)

#Remove
rm(ts_train)
rm(date_table_full)
rm(train_data)
rm(sdy)
rm(level.sigma.prior)
rm(slope.ar1.prior)
rm(slope.mean.prior)
rm(slope.sigma.prior)
rm(state.specification)
rm(bsts_model)

#Predicted data
mean = as.data.frame(pred_ts$mean)
colnames(mean) = "pred"
interval1 = as.data.frame(pred_ts$interval)
interval = t(interval1)
interval = as.data.frame(interval)
low_interval = as.data.frame(interval$`2.5%`)
high_interval = as.data.frame(interval$`97.5%`)
colnames(low_interval) = "low"
colnames(high_interval) = "high"
low_interval = low_interval %>% mutate(low = if_else(low < 0, 0, low))
date = seq(as.Date("2020-06-16"), as.Date("2021-06-15"), by="days")
date = as.data.frame(date)
data_new = data.frame(date = date, 
                      mean = mean, 
                      low_interval = low_interval, 
                      high_interval = high_interval)
#Remove
rm(date)
rm(high_interval)
rm(low_interval)
rm(interval)
rm(interval1)
rm(mean)
rm(pred_ts)

#Old data
date_table = setNames(data.frame(table(c_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
date_table_full[is.na(date_table_full)] <- 0
data_old = date_table_full
rm(date_table)
rm(date_table_full)

#Create cuts
data_old$week_cut = cut(data_old$Date, breaks = "week")

weekly_old_c = aggregate(Count ~ week_cut, data_old, sum)
weekly_old_c$week_cut = as.Date(weekly_old_c$week_cut)
weekly_old_c$year = lubridate::year(weekly_old_c$week_cut)
weekly_old_c$week = lubridate::week(weekly_old_c$week_cut)

rm(data_old)
#Predicted data - create cuts
data_new$week_cut = cut(data_new$date, breaks = "week")

weekly_new_c = aggregate(pred ~ week_cut, data_new, sum)
weekly_new_c$week_cut = as.Date(weekly_new_c$week_cut)
weekly_new_c$year = lubridate::year(weekly_new_c$week_cut)
weekly_new_c$week = lubridate::week(weekly_new_c$week_cut)
#Predicted-low
weekly_low_c = aggregate(low ~ week_cut, data_new, sum)
weekly_low_c$week_cut = as.Date(weekly_low_c$week_cut)
weekly_low_c$year = lubridate::year(weekly_low_c$week_cut)
weekly_low_c$week = lubridate::week(weekly_low_c$week_cut)
weekly_new_c$low = weekly_low_c$low
#Predicted-high
weekly_high_c = aggregate(high ~ week_cut, data_new, sum)
weekly_high_c$week_cut = as.Date(weekly_high_c$week_cut)
weekly_high_c$year = lubridate::year(weekly_high_c$week_cut)
weekly_high_c$week = lubridate::week(weekly_high_c$week_cut)
weekly_new_c$high = weekly_high_c$high

rm(weekly_low_c)
rm(weekly_high_c)
rm(data_new)

#Navmii
date_table = setNames(data.frame(table(d_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
date_table_full[is.na(date_table_full)] <- 0
train_data = date_table_full
rm(date_table)

ts_train = xts(train_data[, -1], order.by = as.POSIXct(train_data$Date))
#Model
sdy <- sd(train_data$Count)
level.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
slope.ar1.prior <- Ar1CoefficientPrior(mu=0,
                                       sigma=0.001)
slope.mean.prior <- NormalPrior(mu=0,
                                sigma=0.1*sdy)
slope.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
state.specification <- AddSemilocalLinearTrend(list(),
                                               ts_train,
                                               level.sigma.prior=level.sigma.prior,
                                               slope.ar1.prior=slope.ar1.prior,
                                               slope.mean.prior=slope.mean.prior,
                                               slope.sigma.prior=slope.sigma.prior)
bsts_model <- bsts(ts_train,
                   state.specification=state.specification,
                   niter=10)
pred_ts <- predict(bsts_model, horizon = 365)

#Remove
rm(ts_train)
rm(date_table_full)
rm(train_data)
rm(sdy)
rm(level.sigma.prior)
rm(slope.ar1.prior)
rm(slope.mean.prior)
rm(slope.sigma.prior)
rm(state.specification)
rm(bsts_model)

#Predicted data
mean = as.data.frame(pred_ts$mean)
colnames(mean) = "pred"
interval1 = as.data.frame(pred_ts$interval)
interval = t(interval1)
interval = as.data.frame(interval)
low_interval = as.data.frame(interval$`2.5%`)
high_interval = as.data.frame(interval$`97.5%`)
colnames(low_interval) = "low"
colnames(high_interval) = "high"
low_interval = low_interval %>% mutate(low = if_else(low < 0, 0, low))
date = seq(as.Date("2020-06-16"), as.Date("2021-06-15"), by="days")
date = as.data.frame(date)
data_new = data.frame(date = date, 
                      mean = mean, 
                      low_interval = low_interval, 
                      high_interval = high_interval)
#Remove
rm(date)
rm(high_interval)
rm(low_interval)
rm(interval)
rm(interval1)
rm(mean)
rm(pred_ts)

#Old data
date_table = setNames(data.frame(table(d_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
date_table_full[is.na(date_table_full)] <- 0
data_old = date_table_full
rm(date_table)
rm(date_table_full)

#Create cuts
data_old$week_cut = cut(data_old$Date, breaks = "week")

weekly_old_d = aggregate(Count ~ week_cut, data_old, sum)
weekly_old_d$week_cut = as.Date(weekly_old_d$week_cut)
weekly_old_d$year = lubridate::year(weekly_old_d$week_cut)
weekly_old_d$week = lubridate::week(weekly_old_d$week_cut)

#Predicted data - create cuts
data_new$week_cut = cut(data_new$date, breaks = "week")

weekly_new_d = aggregate(pred ~ week_cut, data_new, sum)
weekly_new_d$week_cut = as.Date(weekly_new_d$week_cut)
weekly_new_d$year = lubridate::year(weekly_new_d$week_cut)
weekly_new_d$week = lubridate::week(weekly_new_d$week_cut)
#Predicted-low
weekly_low_d = aggregate(low ~ week_cut, data_new, sum)
weekly_low_d$week_cut = as.Date(weekly_low_d$week_cut)
weekly_low_d$year = lubridate::year(weekly_low_d$week_cut)
weekly_low_d$week = lubridate::week(weekly_low_d$week_cut)
weekly_new_d$low = weekly_low_d$low
#Predicted-high
weekly_high_d = aggregate(high ~ week_cut, data_new, sum)
weekly_high_d$week_cut = as.Date(weekly_high_d$week_cut)
weekly_high_d$year = lubridate::year(weekly_high_d$week_cut)
weekly_high_d$week = lubridate::week(weekly_high_d$week_cut)
weekly_new_d$high = weekly_high_d$high


rm(weekly_low_d)
rm(weekly_high_d)
rm(data_new)

#Sygic
date_table = setNames(data.frame(table(e_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
date_table_full[is.na(date_table_full)] <- 0
train_data = date_table_full
rm(date_table)

ts_train = xts(train_data[, -1], order.by = as.POSIXct(train_data$Date))
#Model
sdy <- sd(train_data$Count)
level.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
slope.ar1.prior <- Ar1CoefficientPrior(mu=0,
                                       sigma=0.001)
slope.mean.prior <- NormalPrior(mu=0,
                                sigma=0.1*sdy)
slope.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
state.specification <- AddSemilocalLinearTrend(list(),
                                               ts_train,
                                               level.sigma.prior=level.sigma.prior,
                                               slope.ar1.prior=slope.ar1.prior,
                                               slope.mean.prior=slope.mean.prior,
                                               slope.sigma.prior=slope.sigma.prior)
bsts_model <- bsts(ts_train,
                   state.specification=state.specification,
                   niter=10)
pred_ts <- predict(bsts_model, horizon = 365)

#Remove
rm(ts_train)
rm(date_table_full)
rm(train_data)
rm(sdy)
rm(level.sigma.prior)
rm(slope.ar1.prior)
rm(slope.mean.prior)
rm(slope.sigma.prior)
rm(state.specification)
rm(bsts_model)

#Predicted data
mean = as.data.frame(pred_ts$mean)
colnames(mean) = "pred"
interval1 = as.data.frame(pred_ts$interval)
interval = t(interval1)
interval = as.data.frame(interval)
low_interval = as.data.frame(interval$`2.5%`)
high_interval = as.data.frame(interval$`97.5%`)
colnames(low_interval) = "low"
colnames(high_interval) = "high"
low_interval = low_interval %>% mutate(low = if_else(low < 0, 0, low))
date = seq(as.Date("2020-06-16"), as.Date("2021-06-15"), by="days")
date = as.data.frame(date)
data_new = data.frame(date = date, 
                      mean = mean, 
                      low_interval = low_interval, 
                      high_interval = high_interval)
#Remove
rm(date)
rm(high_interval)
rm(low_interval)
rm(interval)
rm(interval1)
rm(mean)
rm(pred_ts)

#Old data
date_table = setNames(data.frame(table(e_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
date_table_full[is.na(date_table_full)] <- 0
data_old = date_table_full
rm(date_table)
rm(date_table_full)

#Create cuts
data_old$week_cut = cut(data_old$Date, breaks = "week")

weekly_old_e = aggregate(Count ~ week_cut, data_old, sum)
weekly_old_e$week_cut = as.Date(weekly_old_e$week_cut)
weekly_old_e$year = lubridate::year(weekly_old_e$week_cut)
weekly_old_e$week = lubridate::week(weekly_old_e$week_cut)

#Predicted data - create cuts
data_new$week_cut = cut(data_new$date, breaks = "week")

weekly_new_e = aggregate(pred ~ week_cut, data_new, sum)
weekly_new_e$week_cut = as.Date(weekly_new_e$week_cut)
weekly_new_e$year = lubridate::year(weekly_new_e$week_cut)
weekly_new_e$week = lubridate::week(weekly_new_e$week_cut)
#Predicted-low
weekly_low_e = aggregate(low ~ week_cut, data_new, sum)
weekly_low_e$week_cut = as.Date(weekly_low_e$week_cut)
weekly_low_e$year = lubridate::year(weekly_low_e$week_cut)
weekly_low_e$week = lubridate::week(weekly_low_e$week_cut)
weekly_new_e$low = weekly_low_e$low
#Predicted-high
weekly_high_e = aggregate(high ~ week_cut, data_new, sum)
weekly_high_e$week_cut = as.Date(weekly_high_e$week_cut)
weekly_high_e$year = lubridate::year(weekly_high_e$week_cut)
weekly_high_e$week = lubridate::week(weekly_high_e$week_cut)
weekly_new_e$high = weekly_high_e$high


rm(weekly_low_e)
rm(weekly_high_e)
rm(data_new)


#Maps.me
date_table = setNames(data.frame(table(f_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
date_table_full[is.na(date_table_full)] <- 0
train_data = date_table_full
rm(date_table)

ts_train = xts(train_data[, -1], order.by = as.POSIXct(train_data$Date))
#Model
sdy <- sd(train_data$Count)
level.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
slope.ar1.prior <- Ar1CoefficientPrior(mu=0,
                                       sigma=0.001)
slope.mean.prior <- NormalPrior(mu=0,
                                sigma=0.1*sdy)
slope.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
state.specification <- AddSemilocalLinearTrend(list(),
                                               ts_train,
                                               level.sigma.prior=level.sigma.prior,
                                               slope.ar1.prior=slope.ar1.prior,
                                               slope.mean.prior=slope.mean.prior,
                                               slope.sigma.prior=slope.sigma.prior)
bsts_model <- bsts(ts_train,
                   state.specification=state.specification,
                   niter=10)
pred_ts <- predict(bsts_model, horizon = 365)

#Remove
rm(ts_train)
rm(date_table_full)
rm(train_data)
rm(sdy)
rm(level.sigma.prior)
rm(slope.ar1.prior)
rm(slope.mean.prior)
rm(slope.sigma.prior)
rm(state.specification)
rm(bsts_model)

#Predicted data
mean = as.data.frame(pred_ts$mean)
colnames(mean) = "pred"
interval1 = as.data.frame(pred_ts$interval)
interval = t(interval1)
interval = as.data.frame(interval)
low_interval = as.data.frame(interval$`2.5%`)
high_interval = as.data.frame(interval$`97.5%`)
colnames(low_interval) = "low"
colnames(high_interval) = "high"
low_interval = low_interval %>% mutate(low = if_else(low < 0, 0, low))
date = seq(as.Date("2020-06-16"), as.Date("2021-06-15"), by="days")
date = as.data.frame(date)
data_new = data.frame(date = date, 
                      mean = mean, 
                      low_interval = low_interval, 
                      high_interval = high_interval)
#Remove
rm(date)
rm(high_interval)
rm(low_interval)
rm(interval)
rm(interval1)
rm(mean)
rm(pred_ts)

#Old data
date_table = setNames(data.frame(table(f_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
date_table_full[is.na(date_table_full)] <- 0
data_old = date_table_full
rm(date_table)
rm(date_table_full)

#Create cuts
data_old$week_cut = cut(data_old$Date, breaks = "week")

weekly_old_f = aggregate(Count ~ week_cut, data_old, sum)
weekly_old_f$week_cut = as.Date(weekly_old_f$week_cut)
weekly_old_f$year = lubridate::year(weekly_old_f$week_cut)
weekly_old_f$week = lubridate::week(weekly_old_f$week_cut)

rm(data_old)
#Predicted data - create cuts
data_new$week_cut = cut(data_new$date, breaks = "week")

weekly_new_f = aggregate(pred ~ week_cut, data_new, sum)
weekly_new_f$week_cut = as.Date(weekly_new_f$week_cut)
weekly_new_f$year = lubridate::year(weekly_new_f$week_cut)
weekly_new_f$week = lubridate::week(weekly_new_f$week_cut)
#Predicted-low
weekly_low_f = aggregate(low ~ week_cut, data_new, sum)
weekly_low_f$week_cut = as.Date(weekly_low_f$week_cut)
weekly_low_f$year = lubridate::year(weekly_low_f$week_cut)
weekly_low_f$week = lubridate::week(weekly_low_f$week_cut)
weekly_new_f$low = weekly_low_f$low
#Predicted-high
weekly_high_f = aggregate(high ~ week_cut, data_new, sum)
weekly_high_f$week_cut = as.Date(weekly_high_f$week_cut)
weekly_high_f$year = lubridate::year(weekly_high_f$week_cut)
weekly_high_f$week = lubridate::week(weekly_high_f$week_cut)
weekly_new_f$high = weekly_high_f$high

rm(weekly_low_f)
rm(weekly_high_f)
rm(data_new)

#Waze
date_table = setNames(data.frame(table(g_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
date_table_full[is.na(date_table_full)] <- 0
train_data = date_table_full
rm(date_table)

ts_train = xts(train_data[, -1], order.by = as.POSIXct(train_data$Date))
#Model
sdy <- sd(train_data$Count)
level.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
slope.ar1.prior <- Ar1CoefficientPrior(mu=0,
                                       sigma=0.001)
slope.mean.prior <- NormalPrior(mu=0,
                                sigma=0.1*sdy)
slope.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
state.specification <- AddSemilocalLinearTrend(list(),
                                               ts_train,
                                               level.sigma.prior=level.sigma.prior,
                                               slope.ar1.prior=slope.ar1.prior,
                                               slope.mean.prior=slope.mean.prior,
                                               slope.sigma.prior=slope.sigma.prior)
bsts_model <- bsts(ts_train,
                   state.specification=state.specification,
                   niter=10)
pred_ts <- predict(bsts_model, horizon = 365)

#Remove
rm(ts_train)
rm(date_table_full)
rm(train_data)
rm(sdy)
rm(level.sigma.prior)
rm(slope.ar1.prior)
rm(slope.mean.prior)
rm(slope.sigma.prior)
rm(state.specification)
rm(bsts_model)

#Predicted data
mean = as.data.frame(pred_ts$mean)
colnames(mean) = "pred"
interval1 = as.data.frame(pred_ts$interval)
interval = t(interval1)
interval = as.data.frame(interval)
low_interval = as.data.frame(interval$`2.5%`)
high_interval = as.data.frame(interval$`97.5%`)
colnames(low_interval) = "low"
colnames(high_interval) = "high"
low_interval = low_interval %>% mutate(low = if_else(low < 0, 0, low))
date = seq(as.Date("2020-06-16"), as.Date("2021-06-15"), by="days")
date = as.data.frame(date)
data_new = data.frame(date = date, 
                      mean = mean, 
                      low_interval = low_interval, 
                      high_interval = high_interval)
#Remove
rm(date)
rm(high_interval)
rm(low_interval)
rm(interval)
rm(interval1)
rm(mean)
rm(pred_ts)

#Old data
date_table = setNames(data.frame(table(g_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
date_table_full[is.na(date_table_full)] <- 0
data_old = date_table_full
rm(date_table)
rm(date_table_full)

#Create cuts
data_old$week_cut = cut(data_old$Date, breaks = "week")

weekly_old_g = aggregate(Count ~ week_cut, data_old, sum)
weekly_old_g$week_cut = as.Date(weekly_old_g$week_cut)
weekly_old_g$year = lubridate::year(weekly_old_g$week_cut)
weekly_old_g$week = lubridate::week(weekly_old_g$week_cut)

rm(data_old)
#Predicted data - create cuts
data_new$week_cut = cut(data_new$date, breaks = "week")

weekly_new_g = aggregate(pred ~ week_cut, data_new, sum)
weekly_new_g$week_cut = as.Date(weekly_new_g$week_cut)
weekly_new_g$year = lubridate::year(weekly_new_g$week_cut)
weekly_new_g$week = lubridate::week(weekly_new_g$week_cut)
#Predicted-low
weekly_low_g = aggregate(low ~ week_cut, data_new, sum)
weekly_low_g$week_cut = as.Date(weekly_low_g$week_cut)
weekly_low_g$year = lubridate::year(weekly_low_g$week_cut)
weekly_low_g$week = lubridate::week(weekly_low_g$week_cut)
weekly_new_g$low = weekly_low_g$low
#Predicted-high
weekly_high_g = aggregate(high ~ week_cut, data_new, sum)
weekly_high_g$week_cut = as.Date(weekly_high_g$week_cut)
weekly_high_g$year = lubridate::year(weekly_high_g$week_cut)
weekly_high_g$week = lubridate::week(weekly_high_g$week_cut)
weekly_new_g$high = weekly_high_g$high

rm(weekly_low_g)
rm(weekly_high_g)
rm(data_new)

#Here
date_table = setNames(data.frame(table(h_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
date_table_full[is.na(date_table_full)] <- 0
train_data = date_table_full
rm(date_table)

ts_train = xts(train_data[, -1], order.by = as.POSIXct(train_data$Date))
#Model
sdy <- sd(train_data$Count)
level.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
slope.ar1.prior <- Ar1CoefficientPrior(mu=0,
                                       sigma=0.001)
slope.mean.prior <- NormalPrior(mu=0,
                                sigma=0.1*sdy)
slope.sigma.prior <- SdPrior(sigma.guess=0.01*sdy,
                             upper.limit=sdy,
                             sample.size=300)
state.specification <- AddSemilocalLinearTrend(list(),
                                               ts_train,
                                               level.sigma.prior=level.sigma.prior,
                                               slope.ar1.prior=slope.ar1.prior,
                                               slope.mean.prior=slope.mean.prior,
                                               slope.sigma.prior=slope.sigma.prior)
bsts_model <- bsts(ts_train,
                   state.specification=state.specification,
                   niter=10)
pred_ts <- predict(bsts_model, horizon = 365)

#Remove
rm(ts_train)
rm(date_table_full)
rm(train_data)
rm(sdy)
rm(level.sigma.prior)
rm(slope.ar1.prior)
rm(slope.mean.prior)
rm(slope.sigma.prior)
rm(state.specification)
rm(bsts_model)

#Predicted data
mean = as.data.frame(pred_ts$mean)
colnames(mean) = "pred"
interval1 = as.data.frame(pred_ts$interval)
interval = t(interval1)
interval = as.data.frame(interval)
low_interval = as.data.frame(interval$`2.5%`)
high_interval = as.data.frame(interval$`97.5%`)
colnames(low_interval) = "low"
colnames(high_interval) = "high"
low_interval = low_interval %>% mutate(low = if_else(low < 0, 0, low))
high_interval = high_interval %>% mutate(high = if_else(high < 0, 1, high))
mean = mean %>% mutate(pred = if_else(pred < 0, 0, pred))
date = seq(as.Date("2020-06-16"), as.Date("2021-06-15"), by="days")
date = as.data.frame(date)
data_new = data.frame(date = date, 
                      mean = mean, 
                      low_interval = low_interval, 
                      high_interval = high_interval)
#Remove
rm(date)
rm(high_interval)
rm(low_interval)
rm(interval)
rm(interval1)
rm(mean)
rm(pred_ts)

#Old data
date_table = setNames(data.frame(table(h_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
date_table_full = date_table %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
date_table_full[is.na(date_table_full)] <- 0
data_old = date_table_full
rm(date_table)
rm(date_table_full)

#Create cuts
data_old$week_cut = cut(data_old$Date, breaks = "week")

weekly_old_h = aggregate(Count ~ week_cut, data_old, sum)
weekly_old_h$week_cut = as.Date(weekly_old_h$week_cut)
weekly_old_h$year = lubridate::year(weekly_old_h$week_cut)
weekly_old_h$week = lubridate::week(weekly_old_h$week_cut)

rm(data_old)
#Predicted data - create cuts
data_new$week_cut = cut(data_new$date, breaks = "week")

weekly_new_h = aggregate(pred ~ week_cut, data_new, sum)
weekly_new_h$week_cut = as.Date(weekly_new_h$week_cut)
weekly_new_h$year = lubridate::year(weekly_new_h$week_cut)
weekly_new_h$week = lubridate::week(weekly_new_h$week_cut)
#Predicted-low
weekly_low_h = aggregate(low ~ week_cut, data_new, sum)
weekly_low_h$week_cut = as.Date(weekly_low_h$week_cut)
weekly_low_h$year = lubridate::year(weekly_low_h$week_cut)
weekly_low_h$week = lubridate::week(weekly_low_h$week_cut)
weekly_new_h$low = weekly_low_h$low
#Predicted-high
weekly_high_h = aggregate(high ~ week_cut, data_new, sum)
weekly_high_h$week_cut = as.Date(weekly_high_h$week_cut)
weekly_high_h$year = lubridate::year(weekly_high_h$week_cut)
weekly_high_h$week = lubridate::week(weekly_high_h$week_cut)
weekly_new_h$high = weekly_high_h$high

rm(weekly_low_h)
rm(weekly_high_h)
rm(data_new)


#Add app names to each table
weekly_new_a$app = "1"
weekly_new_b$app = "2"
weekly_new_c$app = "3"
weekly_new_d$app = "4"
weekly_new_e$app = "5"
weekly_new_f$app = "6"
weekly_new_g$app = "7"
weekly_new_h$app = "8"
weekly_old_a$app = "1"
weekly_old_b$app = "2"
weekly_old_c$app = "3"
weekly_old_d$app = "4"
weekly_old_e$app = "5"
weekly_old_f$app = "6"
weekly_old_g$app = "7"
weekly_old_h$app = "8"


#Join all daily tables in to one table
drf1 = full_join(weekly_new_a, weekly_new_b)
drf2 = full_join(drf1, weekly_new_c)
drf3 = full_join(drf2, weekly_new_d)
drf4 = full_join(drf3, weekly_new_e)
drf5 = full_join(drf4, weekly_new_f)
drf6 = full_join(drf5, weekly_new_g)
weekly_new_data = full_join(drf6, weekly_new_h)
rm(drf1)
rm(drf2)
rm(drf3)
rm(drf4)
rm(drf5)
rm(drf6)
#Join all weekly tables in to one table
wrf1 = full_join(weekly_old_a, weekly_old_b)
wrf2 = full_join(wrf1, weekly_old_c)
wrf3 = full_join(wrf2, weekly_old_d)
wrf4 = full_join(wrf3, weekly_old_e)
wrf5 = full_join(wrf4, weekly_old_f)
wrf6 = full_join(wrf5, weekly_old_g)
weekly_old_data = full_join(wrf6, weekly_old_h)
rm(wrf1)
rm(wrf2)
rm(wrf3)
rm(wrf4)
rm(wrf5)
rm(wrf6)

#Remove individual data
rm(weekly_new_a)
rm(weekly_new_b)
rm(weekly_new_c)
rm(weekly_new_d)
rm(weekly_new_e)
rm(weekly_new_f)
rm(weekly_new_g)
rm(weekly_new_h)
rm(weekly_old_a)
rm(weekly_old_b)
rm(weekly_old_c)
rm(weekly_old_d)
rm(weekly_old_e)
rm(weekly_old_f)
rm(weekly_old_g)
rm(weekly_old_h)


#Write data
write.csv(weekly_new_data, file = 'weekly_new_data.csv', row.names = FALSE)
write.csv(weekly_old_data, file = 'weekly_old_data.csv', row.names = FALSE)

#Chart
#Add colors
my_colors2 <- c("#62A8D8", "#BEBA06", "#3DBD9F", "#FAA316",
                "#FF4C43", "#8BC858", "#8962A6", "#ED6D92")

#Define app factors
weekly_new_data$app = factor(weekly_new_data$app, 
                               levels = c(1,2,3,4,5,6,7,8),
                               labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                          "Sygic", "Mapsme", "Waze", "Here"))
weekly_old_data$app = factor(weekly_old_data$app, 
                                levels = c(1,2,3,4,5,6,7,8),
                                labels = c("Genious", "TomTom", "CoPilot", "Navmii",
                                           "Sygic", "Mapsme", "Waze", "Here"))


weekly_old_data %>%
  ggplot(aes(week_cut, Count, group=app, color=app)) +
  geom_line() +
  stat_smooth(aes(x=week_cut, y=pred, group=app, ymax=high, ymin=low), 
              data=weekly_new_data) +
  scale_x_date(breaks = as.Date(c("2017-01-01",'2018-01-01',
                                  "2019-01-01", "2020-01-01")),
               labels = c("2017","2018","2019","2020")) +
  scale_color_manual(name="Legend", values=my_colors2) +
  ylab("Number of reviews") +
  xlab("Time (weekly)") +
  scale_y_continuous(breaks = seq(0, 600, by = 50)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.direction = "vertical",
        legend.position = c(0.9,0.75))+
  ggtitle("Reviews through time - weekly + Predicted number of reviews")

rm(weekly_new_data)
rm(weekly_old_data)
rm(my_colors2)

#####################Days with lowest ratings
#Genius
#Create review frequency per date table
date_table = setNames(data.frame(table(a_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
#Calculate average starrating for each day
star_table = aggregate(stars ~ date, a_data, mean)
star_table$date = as.Date(star_table$date)
star_table$stars = as.numeric(star_table$stars)
#Create stars and reviews per day table
a_date_stars = data.frame(date = date_table$Date, 
                          count = date_table$Count, 
                          star = star_table$stars)
rm(date_table)
rm(star_table)
#TomTom
date_table = setNames(data.frame(table(b_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
star_table = aggregate(stars ~ date, b_data, mean)
star_table$date = as.Date(star_table$date)
star_table$stars = as.numeric(star_table$stars)
b_date_stars = data.frame(date = date_table$Date, 
                          count = date_table$Count, 
                          star = star_table$stars)
rm(date_table)
rm(star_table)
#CoPilot
date_table = setNames(data.frame(table(c_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
star_table = aggregate(stars ~ date, c_data, mean)
star_table$date = as.Date(star_table$date)
star_table$stars = as.numeric(star_table$stars)
c_date_stars = data.frame(date = date_table$Date, 
                          count = date_table$Count, 
                          star = star_table$stars)
rm(date_table)
rm(star_table)
#Navmii
date_table = setNames(data.frame(table(d_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
star_table = aggregate(stars ~ date, d_data, mean)
star_table$date = as.Date(star_table$date)
star_table$stars = as.numeric(star_table$stars)
d_date_stars = data.frame(date = date_table$Date, 
                          count = date_table$Count, 
                          star = star_table$stars)
rm(date_table)
rm(star_table)
#Sygic
date_table = setNames(data.frame(table(e_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
star_table = aggregate(stars ~ date, e_data, mean)
star_table$date = as.Date(star_table$date)
star_table$stars = as.numeric(star_table$stars)
e_date_stars = data.frame(date = date_table$Date, 
                          count = date_table$Count, 
                          star = star_table$stars)
rm(date_table)
rm(star_table)
#Maps.me
date_table = setNames(data.frame(table(f_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
star_table = aggregate(stars ~ date, f_data, mean)
star_table$date = as.Date(star_table$date)
star_table$stars = as.numeric(star_table$stars)
f_date_stars = data.frame(date = date_table$Date, 
                          count = date_table$Count, 
                          star = star_table$stars)
rm(date_table)
rm(star_table)
#Waze
date_table = setNames(data.frame(table(g_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
star_table = aggregate(stars ~ date, g_data, mean)
star_table$date = as.Date(star_table$date)
star_table$stars = as.numeric(star_table$stars)
g_date_stars = data.frame(date = date_table$Date, 
                          count = date_table$Count, 
                          star = star_table$stars)
rm(date_table)
rm(star_table)
#Here
date_table = setNames(data.frame(table(h_data$date)),c("Date","Count"))
date_table$Date = as.Date(date_table$Date)
date_table$Count = as.integer((date_table$Count))
star_table = aggregate(stars ~ date, h_data, mean)
star_table$date = as.Date(star_table$date)
star_table$stars = as.numeric(star_table$stars)
h_date_stars = data.frame(date = date_table$Date, 
                          count = date_table$Count, 
                          star = star_table$stars)
rm(date_table)
rm(star_table)

#Add app names to each table
a_date_stars$app = "Genius Maps"
b_date_stars$app = "TomTom"
c_date_stars$app = "CoPilot"
d_date_stars$app = "Navmii"
e_date_stars$app = "Sygic"
f_date_stars$app = "Maps.me"
g_date_stars$app = "Waze"
h_date_stars$app = "Here"

#Join data in one table
drf1 = full_join(a_date_stars, b_date_stars)
drf2 = full_join(drf1, c_date_stars)
drf3 = full_join(drf2, d_date_stars)
drf4 = full_join(drf3, e_date_stars)
drf5 = full_join(drf4, f_date_stars)
drf6 = full_join(drf5, g_date_stars)
date_stars = full_join(drf6, h_date_stars)
rm(drf1)
rm(drf2)
rm(drf3)
rm(drf4)
rm(drf5)
rm(drf6)
#Remove individual data
rm(a_date_stars)
rm(b_date_stars)
rm(c_date_stars)
rm(d_date_stars)
rm(e_date_stars)
rm(f_date_stars)
rm(g_date_stars)
rm(h_date_stars)

#Round star rating values
date_stars$star = round(date_stars$star, 1)

write.csv(date_stars, file = 'date_stars.csv', row.names = FALSE)
#Remove table
rm(date_stars)

#END