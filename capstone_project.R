library(readxl) #needed to load in the .xls and .xlsx (that I can find)
library(tidyverse) #no need to load this in, but it will make pretty graphs if we want

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
hist(time[time <= 100], breaks = seq(0, 100, by=10), col = 'orchid', main = 'Counts of Time Spent Logged in (<100 Mins.)', xlab = 'Time')
barplot(hourly_gate$Total, names.arg = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 0), cex.names = .8, xlab = 'Hour', ylab = 'Number of People', main = 'Hourly Numbers of Entries', col = 'royalblue4')
matplot(PCs_monthly$Year, PCs_monthly[, -1], type = 'l', col = 1:length(months), main = 'Change in PCs Used', xlab = 'Year', ylab = 'Amount')
legend('topright', months, col = 1:length(months), lty = 1:length(months), title = "Month", cex = .5, ncol = 4)
months = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
total_PCs = apply(PCs_monthly, 1, sum, na.rm = TRUE)
total_PCs
plot(PCs_monthly$Year, total_PCs, 'l', main = 'Total Numbers of PCs Used', xlab = 'Year', ylab = 'Number')
boxplot(z$time ~ z$machine, horizontal = TRUE, xlab = 'Time', ylab = 'Machine', main = 'Time Spent at Each Machine, with Outliers')
boxplot(z$time ~ z$machine, outline = FALSE, las = 0, col = c(0, 2, 3, 4, 5, 6, 7, 8), xlab = 'Machine', ylab = 'Time', main = 'Time Spent at Computer (Without Outliers)')
barplot(counts, names.arg = c('PWS190', 'PWS191', 'PWS192', 'PWS193', 'PWS194', 'PWS195', 'PWS196', 'PWS197'), cex.names = .8, las = 2, col = 'skyblue', main = 'Number of People Who Used Each Computer', ylab = 'Number of People')
barplot(counts, names.arg = c('PWS190', 'PWS191', 'PWS192', 'PWS193', 'PWS194', 'PWS195', 'PWS196', 'PWS197'), cex.names = .8, las = 2, col = 'skyblue', main = 'Number of People Who Used Each Computer', xlab = 'Computer', ylab = 'Number of People')

y1 = ggplot(data = test_2018) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5))

y1

gt_v_comp = write.csv(test, 'gt_v_comp_full.csv')

m8 = ggplot(data = aug_2018) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5))

year_2019 = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct')

y = c()

m1 = ggplot(data = jan) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5)) +
  xlab('Number of People') +
  ylab('Number of Computers Used') +
  labs(title = 'Gate Traffic v Computer Use', subtitle = 'For January') +
  geom_smooth(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), color = 'orange')

m1

m2 = ggplot(data = feb) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5))

m3 = ggplot(data = mar) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5))

m4 = ggplot(data = apr) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5))

m5 = ggplot(data = may) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5))

m6 = ggplot(data = jun) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5))

m7 = ggplot(data = jul) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5))

m8 = ggplot(data = aug) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5))

m9 = ggplot(data = sep) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5))

m10 = ggplot(data = oct) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5))

plot_grid(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, nrow = 10)

m8.2 = ggplot(data = aug_2018) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5))

m9.2 = ggplot(data = sep_2018) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5))

m10.2 = ggplot(data = oct_2018) + geom_jitter(mapping = aes(x = Recalculated_Total_by_Day, y = Total_Comp_Count), alpha = .5, color = 'blue') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5))

plot_grid(m8.2, m9.2, m10.2, nrow = 3)