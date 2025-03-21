data = read_csv("https://raw.githubusercontent.com/JIHONGKING/Data_Analysis/refs/heads/main/carbon.csv") %>% 
  mutate(Year = strtoi(substring(Date, 7,10)))

summary = data %>% 
  group_by(Region, Year) %>% 
  summarise(`Metric Tons Per Capita` = mean(`Metric Tons Per Capita`))

ggplot(summary)+
  geom_line(aes(Year, `Metric Tons Per Capita`, col = Region))+
  theme_bw()+
  ggtitle('Average Metric Tons per Capita of CO2', subtitle = 'Seperated by Region')
