ind_data<-fread("~/Desktop/ShinyApps/int_ind/output.csv") #output
ind_data$Date<-as.Date(ind_data$Date)
sp500_data<-fread("~/Desktop/ShinyApps/int_ind/sp500.csv")
sp500_data$Date<-as.Date(sp500_data$Date, "%m/%d/%y")

ind_data_withSP<-merge(ind_data,sp500_data,by="Date",all.x = T)
library(plotly)
library(ggplot2)
ind_data_withSP %>% 
  plot_ly(x = ~ Date) %>% 
  add_lines(y = ~  mst_length) %>%
  add_lines(y = ~ mst_str_change_ratio) %>%
  add_lines(y =~Close,yaxis = "y2") %>%
  layout(yaxis2 = list(overlaying = "y", side = "right"))
