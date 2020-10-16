View(gt_data)
Sun_gt = gt_data[gt_data$Day == 'Sunday', ]
Mon_gt = gt_data[gt_data$Day == 'Monday', ]
Tues_gt = gt_data[gt_data$Day == 'Tuesday', ]
Wed_gt = gt_data[gt_data$Day == 'Wednesday', ]
Thurs_gt = gt_data[gt_data$Day == 'Thursday', ]
Fri_gt = gt_data[gt_data$Day == 'Friday', ]
Sat_gt = gt_data[gt_data$Day == 'Saturday', ]
t = t1 / 2
View(t1)
x = as.data.frame(apply(gt_data[, 3:26], 2, sum))
colnames(x) = 'V1'
factor(x$V1)
View(x)
t1 = as.data.frame(apply(Sun_gt[, 3:26], 2, sum))
colnames(t1) = 'V1'
t2 = as.data.frame(apply(Mon_gt[, 3:26], 2, sum))
colnames(t2) = 'V1'
t3 = as.data.frame(apply(Tues_gt[, 3:26], 2, sum))
colnames(t3) = 'V1'
t4 = as.data.frame(apply(Wed_gt[, 3:26], 2, sum))
colnames(t4) = 'V1'
t5 = as.data.frame(apply(Thurs_gt[, 3:26], 2, sum))
colnames(t5) = 'V1'
t6 = as.data.frame(apply(Fri_gt[, 3:26], 2, sum))
colnames(t6) = 'V1'
t7 = as.data.frame(apply(Sat_gt[, 3:26], 2, sum))
colnames(t7) = 'V1'

ggplot(data = x) + 
  geom_bar(mapping = aes(x=hour_names, y=V1), stat = 'identity', color = 'gray', show.legend = FALSE, width = .9)

ggplot(data = t) +
  geom_bar(mapping = aes(x = hour_names, y = V1), stat = 'identity', fill = 'steelblue1', alpha = .8, show.legend = FALSE, width = .8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5)) +
  xlab('Hour of Day') +
  ylab('Number of People') +
  labs(title = 'Gate Traffic', subtitle = 'Total People per Hour on Mondays') +
  scale_x_discrete(limits = hour_names)
nrow(gt_data)
x = colSums(gt_data[, 3:26])
View(x)
class(x$V1)
v = as.data.frame(t(x))
hour_names

for (i in 1:24){
  
  x$V2[i] = x$V1[i] / 420
  
}

View(gt_data_copy)
gt_data_copy = gt_data
nrow(gt_data)

for (i in 1:420) {
  
  if (gt_data$Date[i] >= '2018-08-01' & gt_data$Date[i] < '2018-09-01') {
    
    gt_data$Month_Year[i] = 'Aug_2018'
    
  }
  
  else if (gt_data$Date[i] >= '2018-09-01' & gt_data$Date[i] < '2018-10-01') {
    
    gt_data$Month_Year[i] = 'Sep_2018'
    
  }
  
  else if (gt_data$Date[i] >= '2018-10-01' & gt_data$Date[i] < '2018-11-01') {
    
    gt_data$Month_Year[i] = 'Oct_2018'
    
  }
  
  else if (gt_data$Date[i] >= '2018-11-01' & gt_data$Date[i] < '2018-12-01') {
    
    gt_data$Month_Year[i] = 'Nov_2018'
    
  }
  
  else if (gt_data$Date[i] >= '2018-12-01' & gt_data$Date[i] < '2019-01-01') {
    
    gt_data$Month_Year[i] = 'Dec_2018'
    
  }
  
  else if (gt_data$Date[i] >= '2019-01-01' & gt_data$Date[i] < '2019-02-01') {
    
    gt_data$Month_Year[i] = 'Jan_2019'
    
  }
  
  else if (gt_data$Date[i] >= '2019-02-01' & gt_data$Date[i] < '2019-03-01') {
    
    gt_data$Month_Year[i] = 'Feb_2019'
    
  }
  
  else if (gt_data$Date[i] >= '2019-03-01' & gt_data$Date[i] < '2019-04-01') {
    
    gt_data$Month_Year[i] = 'Mar_2019'
    
  }
  
  else if (gt_data$Date[i] >= '2019-04-01' & gt_data$Date[i] < '2019-05-01') {
    
    gt_data$Month_Year[i] = 'Apr_2019'
    
  }
  
  else if (gt_data$Date[i] >= '2019-05-01' & gt_data$Date[i] < '2019-06-01') {
    
    gt_data$Month_Year[i] = 'May_2019'
    
  }
  
  else if (gt_data$Date[i] >= '2019-06-01' & gt_data$Date[i] < '2019-07-01') {
    
    gt_data$Month_Year[i] = 'Jun_2019'
    
  }
  
  else if (gt_data$Date[i] >= '2019-07-01' & gt_data$Date[i] < '2019-08-01') {
    
    gt_data$Month_Year[i] = 'Jul_2019'
    
  }
  
  else if (gt_data$Date[i] >= '2019-08-01' & gt_data$Date[i] < '2019-09-01') {
    
    gt_data$Month_Year[i] = 'Aug_2019'
    
  }
  
  else if (gt_data$Date[i] >= '2019-09-01' & gt_data$Date[i] < '2019-10-01') {
    
    gt_data$Month_Year[i] = 'Sep_2019'
    
  }
  
  else {
    
    gt_data$Month_Year[i] = 'Oct_2019'
    
  }
  
}
gt_copy = gt_data
factor(gt_data$Month_Year)

m = tapply(gt_data$Recalculated_Total_by_Day, gt_data$Month_Year, FUN=sum)
View(m)
months_and_year

mm = as.data.frame(m)
colnames(mm) = 'Total'
rownames(mm) 
barplot(mm$Total)

ggplot(data = mm) +
  geom_bar(mapping = aes(x = rownames(mm), y = Total), stat = 'identity', fill = 'steelblue1', alpha = .8, show.legend = FALSE, width = .8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 5)) +
  xlab('Month and Year\n') +
  ylab('\nNumber of People') +
  labs(title = 'Gate Traffic', subtitle = 'Total People per Month') + 
  coord_flip() + 
  scale_x_discrete(limits = rev(months_and_year))


rev(months_and_year)

avg = tapply(gt_data$Recalculated_Total_by_Day, gt_data$Day, FUN = mean)
View(avg)

quantile(HE8$Min_Logged_in)
quantile(HE8[HE8$machine == 'PC_7', ]$Min_Logged_in)

tapply(HE8$Min_Logged_in, HE8$machine, FUN = quantile)

avg_spend = HE8[HE8$Min_Logged_in <= 30, ]
nrow(avg_spend)
avg_spend_1 = HE8[HE8$Min_Logged_in > 30 & HE8$Min_Logged_in <= 60, ]
nrow(avg_spend_1)
avg_spend_2 = HE8[HE8$Min_Logged_in > 60 & HE8$Min_Logged_in <= 90, ]
nrow(avg_spend_2)
avg_spend_3 = HE8[HE8$Min_Logged_in > 90 & HE8$Min_Logged_in <= 120, ]
nrow(avg_spend_3)
avg_spend_4 = HE8[HE8$Min_Logged_in > 120 & HE8$Min_Logged_in <= 150, ]
nrow(avg_spend_4)
avg_spend_5 = HE8[HE8$Min_Logged_in > 150 & HE8$Min_Logged_in <= 180, ]
nrow(avg_spend_5)
nrow(HE8[HE8$Min_Logged_in > 420 & HE8$Min_Logged_in <= 450, ])
1270 + 845 + 725 + 551 + 388 + 312 + 1049
nrow(HE8)

nrow(HE8[HE8$Min_Logged_in > 420, ])
24*60

420/60

View(HE8[HE8$Min_Logged_in > 180, ])

tapply(HE8$Min_Logged_in, HE8$machine, FUN = mean)
min(HE8$Time_In)

tapply(gt_data$Recalculated_Total_by_Day, gt_data$Day, FUN = mean)
nrow(Sun_gt)

write.csv(HE8, file = 'clean_HE8')
write.csv(gate_2, "C:/Users/Librom/Downloads/Capstone_Project/gate_traffic_consol.csv", row.names = FALSE)
View(gate_traffic)
View(gt_data)

hist(as.numeric(HE8$Min_Logged_in))
nrow(gate_traffic)
nrow(gt_data)

gt_noweekends = gate_traffic[gate_traffic$Day != 'Sunday' & gate_traffic$Day != 'Saturday' & gate_traffic$Day != 'Friday', ]
View(gt_noweekends)

ggplot(data = gt_noweekends) + 
  geom_line(mapping = aes(x = Date, y = Total_by_Day, color = factor(Month_Year))) +
  xlab('Date') +
  ylab('Total Number of People') +
  labs(title = 'Gate Traffic', subtitle = 'Total People per Day by Date') +
  scale_fill_manual(name = 'Month and Year') +
  labs(color = "Month and Year") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_discrete(limits = months_and_year)

ggplot(data = gate_traffic) + 
  geom_line(mapping = aes(x = Date, y = Total_by_Day), color = 'royalblue') +
  xlab('Date') +
  ylab('Total Number of People') +
  labs(title = 'Gate Traffic', subtitle = 'Total People per Day') +
  scale_fill_manual(name = 'Month and Year') +
  labs(color = "Month and Year") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


outlierKD(HE8, HE8$Min_Logged_in)

quantile(HE8$Min_Logged_in)

no_out = HE8$Min_Logged_in[HE8$Min_Logged_in <= 345]
quantile(no_out)
mean(HE8$Min_Logged_in)
mean(no_out)

View(gate_2)
nrow(gate_2)
nrow(gate_traffic)

























