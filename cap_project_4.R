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
  geom_bar(mapping = aes(x = machine), fill = 'gold', show.legend = FALSE, width = .5) +
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















