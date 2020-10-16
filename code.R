library(readxl) #needed to load in the .xls and .xlsx (that I can find)
library(tidyverse) #no need to load this in, but it will make pretty graphs if we want
library(lubridate)

hourly_gate = read_xls('C:/Users/Librom/Downloads/science_hourly_gate_count.xls', skip = 4, col_names = TRUE)
View(hourly_gate)

class(hourly_gate[, 3])
HE8_performance = read.csv('C:/Users/Librom/Downloads/HE8_performance_daily_update_1.csv')
View(HE8_performance)

PCs_monthly = read_xlsx('C:/Users/Librom/Downloads/all_science_PCs_monthly.xlsx', range = "D2:P7", na = 'NA')
View(PCs_monthly)

#get counts of the amount of entries per hour
summat = apply(hourly_gate[, 3:9], 2, sum)
barplot(summat)
each_hour = summat / 24
each_hour
hist(hourly_gate$Total)
barplot(hourly_gate$Total, names.arg = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 0), cex.names = .8)

#extracting the time people were logged in + related data
HE8_performance$loginstamp[1]
login_time = strptime(HE8_performance$loginstamp,"%m/%d/%Y %H:%M")
logout_time = strptime(HE8_performance$logoutstamp,"%m/%d/%Y %H:%M")
time_logged_in = difftime(logout_time, login_time, unit = "mins")   #time difference in minutes
time_logged_in_hours = difftime(logout_time, login_time, unit = "hours")  #time difference in hours
?strptime
time = as.numeric(time_logged_in) #making a numeric vector of time in minutes
time_in_hours = as.numeric(time_logged_in_hours)  #making a numeric vector of time in hours

HE8_with_time = cbind(HE8_performance, time)  #making a dataframe with the time (in mins)
HE8_with_time_2 = cbind(HE8_with_time, time_in_hours) #making a dataframe with time (in hours)

View(HE8_with_time)
library(lubridate)
hour_ten = HE8_with_time[hour(HE8_with_time$loginstamp) > 10 & hour(HE8_with_time$logoutstamp) <= 11, ]

hours_in = format(as.POSIXct(strptime(HE8_with_time$loginstamp,"%m/%d/%Y %H:%M",tz="")), format = "%H:%M")
hours_out = format(as.POSIXct(strptime(HE8_with_time$logoutstamp,"%m/%d/%Y %H:%M",tz="")), format = "%H:%M")
class(hours_out)
HE8_with_time$Time_In = hours_in
HE8_with_time$Time_Out = hours_out

d = HE8_with_time[HE8_with_time$Time_In >= '10:00' & HE8_with_time$Time_Out < '11:00', ]
View(d)
#getting how often each machine was used
PWS190_count = sum(HE8_with_time$machine == 'PWS190')
PWS191_count = sum(HE8_with_time$machine == 'PWS191')
PWS192_count = sum(HE8_with_time$machine == 'PWS192')
PWS193_count = sum(HE8_with_time$machine == 'PWS193')
PWS194_count = sum(HE8_with_time$machine == 'PWS194')
PWS195_count = sum(HE8_with_time$machine == 'PWS195')
PWS196_count = sum(HE8_with_time$machine == 'PWS196')
PWS197_count = sum(HE8_with_time$machine == 'PWS197')
counts = cbind(PWS190_count, PWS191_count, PWS192_count, PWS193_count, PWS194_count, PWS195_count, PWS196_count, PWS197_count)

sum(counts)
nrow(na.omit(HE8_with_time))

#getting rid of the desktops in the data
x = HE8_with_time$machine == 'PWS190' | HE8_with_time$machine == 'PWS191' | HE8_with_time$machine == 'PWS192' | HE8_with_time$machine == 'PWS193' | HE8_with_time$machine == 'PWS194' | HE8_with_time$machine == 'PWS195' | HE8_with_time$machine == 'PWS196' | HE8_with_time$machine == 'PWS197'
levels(HE8_with_time$machine)
PWS_comps_names = c('PWS190', 'PWS191', 'PWS192', 'PWS193', 'PWS194', 'PWS195', 'PWS196', 'PWS197')
PWS_comps = HE8_with_time[x, ]
desktops = HE8_with_time[!x, ]

plot(PWS_comps$machine, PWS_comps$time)
levels(PWS_comps$machine)
z = PWS_comps
z = droplevels(z, exclude = x)

#early EDA/plots
mean(time, na.rm = TRUE)
quantile(time, na.rm = TRUE)

a = c('April 14 thru 20, 2019', 'April 21 thru 27, 2019', 'April 28 thru May 4, 2019',
      'August 04 thru 10, 2019', 'August 5 thru August 11, 2018', 'August 11 thru 17, 2019', 'August 12 thru August 18, 2018', 'August 18 thru 24, 2019', 'August 19 thru August 25, 2018', 'August 25 thru 31, 2019', 'August 26 thru September 1, 2018',
      'December 2 thru 8, 2018', 'December 9 thru 15, 2018', 'December 16 thru 22, 2018', 'December 30, 2018 thru January 5, 2019',
      'February 3 thru 9, 2019', 'February 10 thru 16, 2019', 'February 17 thru 23, 2019', 'February 24 thru March 2, 2019',
      'January 6 thru 12, 2019', 'January 13 thru 19, 2019', 'January 20 thru 26, 2019', 'January 27 thru February 2, 2019',
      'July 7 thru 13, 2019', 'July 13 thru 20, 2019', 'July 21 thru 27, 2019', 'July 28 thru August 03, 2019',
      'June 02 thru 08, 2019', 'June 09 thru 15, 2019', 'June 16 thru 22, 2019', 'June 23 thru 29, 2019', 'June 30-July 06, 2019',
      'March 3 thru 9, 2019', 'March 10 thru 16, 2019', 'March 17 thru 23, 2019', 'March 24 thru 30, 2019', 'March 31 thru April 6, 2019',
      'May 5 thru11, 2019', 'May 12 thru18, 2019', 'May 19 thru 25, 2019', 'May 25 thru June 01, 2019', 'November 4 thru 10, 2018', 'November 11 thru 17, 2018', 'November 18 thru 24, 2018', 'November 25 thru December 01, 2018',
      'October 06 thru 12, 2019', 'October 7 thru October 13, 2018', 'October 13 thru 19, 2019', 'October 14 thru October 20, 2018', 'October 20 thru October 26, 2019', 'October 21 thru October 27, 2018', 'October 27 thru November 02, 2019', 'October 28 thru November 3, 2018',
      'September 01 thru 07, 2019', 'September 2 thru 8, 2018', 'September 08 thru 14, 2019', 'September 9 thru 15, 2018', 'September 15 thru 21, 2019', 'September 16 thru 22, 2018', 'September 22 thru 28, 2019', 'September 23 thru 29, 2018', 'September 29 thru October 05, 2019', 'September 30 thru October 06, 2018')

full_gate_data = read_xls('C:/Users/Librom/Downloads/Capstone_Project/Gate_Traffic_Files/Weekly Gate Counts - April 7 thru 13, 2019.xls', skip = 4, col_names = FALSE, range = 'C4:I29', na = '.')

for (i in 1:length(a)) {
  
  x = paste('C:/Users/Librom/Downloads/Capstone_Project/Gate_Traffic_Files/Weekly Gate Counts - ', a[i], '.xls', sep = '')
  gate = read_xls(x, skip = 4, col_names = FALSE, range = 'C4:I29', na = '.')
  full_gate_data = cbind(full_gate_data, gate)
  
}

gate_traffic_col_names = c('Day', 'Date', '1', '2', '3', '4', '5' ,'6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '0')

copy = full_gate_data

gate_traffic = as.data.frame(t(full_gate_data))
colnames(gate_traffic) = gate_traffic_col_names
gate_traffic$Date = as.Date(as.numeric(as.character(gate_traffic$Date)), origin = "1899-12-30")

copy = gate_traffic

copy = apply(copy, 2, as.character)
copy = apply(copy[, 3:26], 2, as.numeric)
Total_by_Hour = apply(copy, 2, sum)
Total_by_Day = apply(copy, 1, sum)
gate_traffic = cbind(gate_traffic, Total_by_Day)
View(gate_traffic)
plot(gate_traffic$Date, gate_traffic$Total_by_Day)

write.csv(gate_traffic,"C:/Users/Librom/Downloads/Capstone_Project/gate_traffic_full.csv", row.names = FALSE)

test = gate_traffic[gate_traffic$Date > '2018-12-31', ]
View(year_2019)
par(mar = rep(2, 4))
plot(year_2019$Date, year_2019$total2, pch = 16, cex = .7, col = 'darkturquoise', main = 'Total Entries per Day in 2019', xlab = 'Date', ylab = 'Total Number of People')
plot(gate_traffic$Date, gate_traffic$Total_by_Day, pch = 16, cex = .7, col = 'darkturquoise', main = 'Total Entries per Day', xlab = 'Date', ylab = 'Total Number of People')

View(HE8[HE8$Min_Logged_in >= 1440, ])
over_24_hrs = HE8[HE8$Min_Logged_in >= 1440, ]
nrow(over_24_hrs)

Sunday = gate_traffic[gate_traffic$Day == 'Sunday', ]
Monday = gate_traffic[gate_traffic$Day == 'Monday', ]
Tuesday = gate_traffic[gate_traffic$Day == 'Tuesday', ]
Wednesday = gate_traffic[gate_traffic$Day == 'Wednesday', ]
Thursday = gate_traffic[gate_traffic$Day == 'Thursday', ]
Friday = gate_traffic[gate_traffic$Day == 'Friday', ]
Saturday = gate_traffic[gate_traffic$Day == 'Saturday', ]

total_sun = sum(Sunday$Total_by_Day)
total_mon = sum(Monday$Total_by_Day)
total_tues = sum(Tuesday$Total_by_Day)
total_wed = sum(Wednesday$Total_by_Day)
total_thurs = sum(Thursday$Total_by_Day)
total_fri = sum(Friday$Total_by_Day)
total_sat = sum(Saturday$Total_by_Day)

gate_traffic_by_day = c(total_sun, total_mon, total_tues, total_wed, total_thurs, total_fri, total_sat)

for (i in 1:7){
  
  gate_traffic_by_day$Day[i] = day[i]
  
}

colnames(gate_traffic_by_day) = c('Count', 'Day')

gate_traffic_by_day$Day = factor(gate_traffic_by_day$Day, levels = gate_traffic_by_day$Day)
gt = as.data.frame(gate_traffic_by_day)

for (i in 1:448) {
  
  if ((gate_traffic$Date[i] >= '2018-08-01' & gate_traffic$Date[i] < '2018-09-01') | (gate_traffic$Date[i] >= '2019-08-01' & gate_traffic$Date[i] < '2019-09-01')) {
    
    gate_traffic$Month[i] = 'Aug'
    
  }
  
  else if (gate_traffic$Date[i] >= '2018-09-01' & gate_traffic$Date[i] < '2018-10-01') {
    
    gate_traffic$Month[i] = 'Sep'
    
  }
  
  else if (gate_traffic$Date[i] >= '2018-10-01' & gate_traffic$Date[i] < '2018-11-01') {
    
    gate_traffic$Month[i] = 'Oct'
    
  }
  
  else if (gate_traffic$Date[i] >= '2018-11-01' & gate_traffic$Date[i] < '2018-12-01') {
    
    gate_traffic$Month[i] = 'Nov'
    
  }
  
  else if (gate_traffic$Date[i] >= '2018-12-01' & gate_traffic$Date[i] < '2019-01-01') {
    
    gate_traffic$Month[i] = 'Dec'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-01-01' & gate_traffic$Date[i] < '2019-02-01') {
    
    gate_traffic$Month[i] = 'Jan'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-02-01' & gate_traffic$Date[i] < '2019-03-01') {
    
    gate_traffic$Month[i] = 'Feb'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-03-01' & gate_traffic$Date[i] < '2019-04-01') {
    
    gate_traffic$Month[i] = 'Mar'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-04-01' & gate_traffic$Date[i] < '2019-05-01') {
    
    gate_traffic$Month[i] = 'Apr'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-05-01' & gate_traffic$Date[i] < '2019-06-01') {
    
    gate_traffic$Month[i] = 'May'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-06-01' & gate_traffic$Date[i] < '2019-07-01') {
    
    gate_traffic$Month[i] = 'Jun'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-07-01' & gate_traffic$Date[i] < '2019-08-01') {
    
    gate_traffic$Month[i] = 'Jul'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-09-01' & gate_traffic$Date[i] < '2019-10-01') {
    
    gate_traffic$Month[i] = 'Sep'
    
  }
  
  else {
    
    gate_traffic$Month[i] = 'Oct'
    
  }
  
}

for (i in 1:448) {
  
  if (gate_traffic$Date[i] >= '2018-08-01' & gate_traffic$Date[i] < '2018-09-01') {
    
    gate_traffic$Month_Year[i] = 'Aug_2018'
    
  }
  
  else if (gate_traffic$Date[i] >= '2018-09-01' & gate_traffic$Date[i] < '2018-10-01') {
    
    gate_traffic$Month_Year[i] = 'Sep_2018'
    
  }
  
  else if (gate_traffic$Date[i] >= '2018-10-01' & gate_traffic$Date[i] < '2018-11-01') {
    
    gate_traffic$Month_Year[i] = 'Oct_2018'
    
  }
  
  else if (gate_traffic$Date[i] >= '2018-11-01' & gate_traffic$Date[i] < '2018-12-01') {
    
    gate_traffic$Month_Year[i] = 'Nov_2018'
    
  }
  
  else if (gate_traffic$Date[i] >= '2018-12-01' & gate_traffic$Date[i] < '2019-01-01') {
    
    gate_traffic$Month_Year[i] = 'Dec_2018'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-01-01' & gate_traffic$Date[i] < '2019-02-01') {
    
    gate_traffic$Month_Year[i] = 'Jan_2019'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-02-01' & gate_traffic$Date[i] < '2019-03-01') {
    
    gate_traffic$Month_Year[i] = 'Feb_2019'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-03-01' & gate_traffic$Date[i] < '2019-04-01') {
    
    gate_traffic$Month_Year[i] = 'Mar_2019'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-04-01' & gate_traffic$Date[i] < '2019-05-01') {
    
    gate_traffic$Month_Year[i] = 'Apr_2019'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-05-01' & gate_traffic$Date[i] < '2019-06-01') {
    
    gate_traffic$Month_Year[i] = 'May_2019'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-06-01' & gate_traffic$Date[i] < '2019-07-01') {
    
    gate_traffic$Month_Year[i] = 'Jun_2019'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-07-01' & gate_traffic$Date[i] < '2019-08-01') {
    
    gate_traffic$Month_Year[i] = 'Jul_2019'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-08-01' & gate_traffic$Date[i] < '2019-09-01') {
    
    gate_traffic$Month_Year[i] = 'Aug_2019'
    
  }
  
  else if (gate_traffic$Date[i] >= '2019-09-01' & gate_traffic$Date[i] < '2019-10-01') {
    
    gate_traffic$Month_Year[i] = 'Sep_2019'
    
  }
  
  else {
    
    gate_traffic$Month_Year[i] = 'Oct_2019'
    
  }
  
}

for (i in 1:448) {
  
  if (gate_traffic$Date[i] >= '2018-01-01' & gate_traffic$Date[i] < '2019-01-01') {
    
    gate_traffic$Year[i] = '2018'
    
  }
  
  else {
    
    gate_traffic$Year[i] = '2019'
    
  }
}

HE8 = read.csv('C:/Users/Librom/Downloads/HE8_performance_daily_update_1.csv')

HE8$Time_In = as.POSIXct(strptime(HE8_performance$loginstamp,"%m/%d/%Y %H:%M"))
HE8$Time_Out = as.POSIXct(strptime(HE8_performance$logoutstamp,"%m/%d/%Y %H:%M"))

HE8 = subset(HE8, select = c(1, 4, 5, 6))

test = HE8[hour(HE8$Time_In) >= 10 & hour(HE8$Time_In) < 11, ]
View(test)

x = HE8[month(HE8$Time_In) == 1, ]

PCs_monthly = read_xlsx('C:/Users/Librom/Downloads/all_science_PCs_monthly.xlsx', range = "D2:P7", na = 'NA')
View(PCs_monthly)

HE8$Month = strftime(as.POSIXlt(HE8$Time_In, format = "%Y-%m-%d %H:%M:%S"), format="%b")
HE8$Year = strftime(as.POSIXlt(HE8$Time_In, format = "%Y-%m-%d %H:%M:%S"), format="%Y")
HE8$Hour_In = strftime(as.POSIXlt(HE8$Time_In, format = "%Y-%m-%d %H:%M:%S"), format="%H")
HE8$Day_In = strftime(as.POSIXlt(HE8$Time_In, format = "%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d")
class(HE8$Time_In)

View(HE8)

x = HE8$machine == 'PWS190' | HE8$machine == 'PWS191' | HE8$machine == 'PWS192' | HE8$machine == 'PWS193' | HE8$machine == 'PWS194' | HE8$machine == 'PWS195' | HE8$machine == 'PWS196' | HE8$machine == 'PWS197'
View(x)
y = HE8[x, ]
View(y)
nrow(na.omit(y))
nrow(na.omit(HE8))
HE8 = na.omit(y)

machine_counts = as.data.frame(table(HE8$machine))
View(machine_counts)
machine_counts = machine_counts[13:20, ]
barplot(machine_counts$Freq)

month_counts = as.data.frame(table(HE8$Month))
barplot(month_counts$Freq, xlab = 'Month')

hour_counts = as.data.frame(table(HE8$Hour_In))

PWS190 = HE8[HE8$machine == 'PWS190', ]
PWS191 = HE8[HE8$machine == 'PWS191', ]
PWS192 = HE8[HE8$machine == 'PWS192', ]
PWS193 = HE8[HE8$machine == 'PWS193', ]
PWS194 = HE8[HE8$machine == 'PWS194', ]
PWS195 = HE8[HE8$machine == 'PWS195', ]
PWS196 = HE8[HE8$machine == 'PWS196', ]
PWS197 = HE8[HE8$machine == 'PWS197', ]

HE8$machine = as.character(HE8$machine)
nrow(HE8)

for (i in 1:5140) {
  
  if (HE8$machine[i] == 'PWS190') {
    
    HE8$machine[i] = 'PC_0'
    
  }
  
  else if (HE8$machine[i] == 'PWS191') {
    
    HE8$machine[i] = 'PC_1'
    
  }
  
  else if (HE8$machine[i] == 'PWS192') {
    
    HE8$machine[i] = 'PC_2'
    
  }
  
  else if (HE8$machine[i] == 'PWS193') {
    
    HE8$machine[i] = 'PC_3'
    
  }
  
  else if (HE8$machine[i] == 'PWS194') {
    
    HE8$machine[i] = 'PC_4'
    
  }
  
  else if (HE8$machine[i] == 'PWS195') {
    
    HE8$machine[i] = 'PC_5'
    
  }
  
  else if (HE8$machine[i] == 'PWS196') {
    
    HE8$machine[i] = 'PC_6'
    
  }
  
  else {
    
    HE8$machine[i] = 'PC_7'
    
  }
}

gate_2 = read.csv('C:/Users/Librom/Downloads/gate_traffic_full_update.csv', header = TRUE)
View(gate_2)

gt_data = gate_2[gate_2$Recalculated_Total_by_Day >= 30, ]
View(gt_data)
class(gt_data$Date)
gt_datacopy = gt_data
HE8_copy = HE8
HE8$Day_In = as.POSIXct(strptime(HE8$Day_In, "%Y-%m-%d"))
class(HE8$Day_In)
View(HE8_copy)
gt_data$Date = as.POSIXct(strptime(as.character(gt_data$Date), format = '%Y-%m-%d', tz = ''))

rownames(gt_data) = seq(length = nrow(gt_data))
View(HE8)
HE8_copy1 = HE8
HE8 = HE8_copy

ifelse(HE8$Day_In[33] == gt_data$Date[35], print('lkersjnfke'), print('yikes'))

x = count(HE8, HE8$Day_In)
comp_date_counts = as.data.frame(x)
View(comp_date_counts)
colnames(comp_date_counts) = c('Date', 'Total_Comp_Count')
nrow(x)

x = gt_data[gt_data$Date == HE8$Day_In, ]
View(x)
test = merge(gt_data, comp_date_counts, by = "Date")
View(test)
nrow(comp_date_counts)

plot(test$Recalculated_Total_by_Day, test$Total_Comp_Count)

g = lm(Total_Comp_Count ~ Recalculated_Total_by_Day, data = test)
summary(g)
plot(g)

Mons = test[test$Day == 'Monday', ]
Tues = test[test$Day == 'Tuesday', ]
Weds = test[test$Day == 'Wednesday', ]
Thurs = test[test$Day == 'Thursday', ]
Fris = test[test$Day == 'Friday', ]
Sats = test[test$Day == 'Saturday', ]
Suns = test[test$Day == 'Sunday', ]

hour_gate_plot = ggplot(data = hour_gate) +
  geom_bar(mapping = aes(x = hour_names, y = Total_by_Hour), stat = 'identity', fill = 'darkorchid1', alpha = .8, show.legend = FALSE, width = .8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5)) +
  xlab('Hour of Day') +
  ylab('Number of People') +
  labs(title = 'Gate Traffic', subtitle = 'Total People per Hour')

hour_gate_plot

month_gate_plot = ggplot(data = gate_traffic) +
  geom_bar(mapping = aes(x = Month_Year, y = Total_by_Day), stat = 'identity', fill = 'deepskyblue', show.legend = FALSE, width = .8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab('Month and Year') +
  ylab('Number of People') +
  labs(title = 'Gate Traffic', subtitle = 'Total People per Month') +
  coord_flip()

month_gate_plot

month_plot = ggplot(data = HE8) + 
  geom_bar(mapping = aes(x = Month, fill = factor(Month)), color = 'gray', show.legend = FALSE, width = .9) +
  xlab('Month') +
  ylab('Count') +
  labs(title = 'Total Computers Used', subtitle = 'Per Month') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

month_plot

hour_count_plots = ggplot(data = gate_traffic_by_day) +
  geom_bar(mapping = aes(x = Day, y = Count), stat = 'identity', fill = 'deepskyblue2', show.legend = FALSE, width = .95) +
  xlab('Day') +
  ylab('Count') +
  labs(title = 'Total Numbers of Entries', subtitle = 'By Day of the Week') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(hjust = .5, vjust = .5))

hour_count_plots

machine_count_plots = ggplot(data = HE8) +
  geom_bar(mapping = aes(x = machine), fill = 'darkgoldenrod1', show.legend = FALSE, width = .5) +
  xlab('Computer') +
  ylab('Count') +
  labs(title = 'Computer Usage', subtitle = 'By Machine') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(hjust = .5, vjust = .5))

machine_count_plots

time_boxplot = ggplot(data = HE8) +
  geom_boxplot(mapping = aes(x = machine, y = Min_Logged_in), fill = 'chartreuse3', outlier.shape = NA, show.legend = FALSE) +
  ylim(0, 325) +
  xlab('Computer') +
  ylab('Minutes') +
  labs(title = 'Distribution of Time Logged In', subtitle = 'By Machine') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

time_boxplot

histo = ggplot(data = HE8) +
  geom_histogram(mapping = aes(x = Min_Logged_in), binwidth = 50, color = 'darkorange1', fill = 'darkorange1', alpha = .5)  +
  xlab('Time Spent Logged In') +
  ylab('Count') +
  labs(title = 'Distribution of Time', subtitle = 'Spent Logged In at a Computer') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

histo

histo2 = ggplot(data = HE8) +
  geom_histogram(mapping = aes(x = Min_Logged_in), binwidth = 10, color = 'darkorange', fill = 'darkorange1', alpha = .5)  +
  xlab('Time Spent Logged In') +
  ylab('Count') +
  xlim(0, 345) +
  labs(title = 'Distribution of Time', subtitle = 'Spent Logged In at a Computer, Without Outliers') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

histo2

logged_in_boxplot = ggplot(data = HE8) +
  geom_boxplot(mapping = aes(x = '', y = Min_Logged_in), fill = 'firebrick1') +
  labs(title = 'Login Durations', subtitle = 'Without Outliers') +
  ylim(0, 270) +
  xlab('') +
  ylab('Minutes Logged In') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.ticks = element_blank())

logged_in_boxplot

counts_boxplot = ggplot(machine_counts, aes(x = '', y = Freq)) + 
  geom_boxplot(fill = 'firebrick1') +
  labs(title = 'Login Counts') +
  xlab('') +
  ylab('Frequency') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.ticks = element_blank())

counts_boxplot

fall_2018 = test_2018[date(test_2018$Date) >= '2018-08-13' & date(test_2018$Date) <= '2018-12-14', ]
spring_2019 = test_2019[date(test_2019$Date) >= '2019-01-09' & date(test_2019$Date) <= '2019-05-10', ]
summer_2019 = test_2019[date(test_2019$Date) >= '2019-05-15' & date(test_2019$Date) <= '2019-08-02', ]
fall_2019 = test_2019[date(test_2019$Date) >= '2019-08-14' & date(test_2019$Date) <= '2019-12-13', ]

s1 = ggplot(data = fall_2018) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5)) +
  xlab('Number of People') +
  ylab('Number of Computers Used') +
  labs(title = 'Gate Traffic v Computer Use', subtitle = 'For the 2018 Fall Semester') +
  geom_smooth(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), color = 'orange')

s1

s2 = ggplot(data = spring_2019) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5)) +
  xlab('Number of People') +
  ylab('Number of Computers Used') +
  labs(title = 'Gate Traffic v Computer Use', subtitle = 'For the 2019 Spring Semester') +
  geom_smooth(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), color = 'orange')

s2

s3 = ggplot(data = summer_2019) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5)) +
  xlab('Number of People') +
  ylab('Number of Computers Used') +
  labs(title = 'Gate Traffic v Computer Use', subtitle = 'For the 2019 Summer Semester') +
  geom_smooth(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), color = 'orange')

s3

s4 = ggplot(data = fall_2019) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5)) +
  xlab('Number of People') +
  ylab('Number of Computers Used') +
  labs(title = 'Gate Traffic v Computer Use', subtitle = 'For the 2019 Fall Semester') +
  geom_smooth(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), color = 'orange')

s4

b1 = ggplot(test, aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count)) +
  geom_line(aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count, color = Day), stat = 'smooth', method = 'loess', se = FALSE, alpha = .7, size = 2) + 
  scale_colour_manual(name = '', breaks = c("Monday", 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'), values = c('Monday' = '#7FC97F', 'Tuesday' = '#BEAED4', 'Wednesday' = '#FDC086', 'Thursday' = '#FB9A99', 'Friday' = '#386CB0', 'Saturday' = '#F0027F', 'Sunday' = '#666666')) +
  labs(title = 'Loess Lines of Gate Traffic vs Computer Use') +
  xlab('Number of People') +
  ylab('Number of Computers Used') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.ticks = element_blank())

b1

?loess



































