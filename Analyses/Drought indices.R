source("Analyses/WQindices.R")

vars<-c("Temperature", "Secchi")
WQ_list<-map(vars, WQindices, region="All")

yearseasons<-expand_grid(Season=c("Winter", "Spring", "Summer", "Fall"),
                         Year=1970:2021)

WQ_data<-map(WQ_list, ~left_join(yearseasons, .x, by=c("Season", "Year"))%>%
               select(contains(vars)))%>%
  bind_cols(yearseasons, .)%>%
  arrange(Year, Season)

WQ_data_n<-WQ_data%>%
  select(all_of(paste0("N_", vars)), Season, Year)%>%
  pivot_longer(all_of(paste0("N_", vars)), names_prefix="N_", names_to="Variable", values_to="N")%>%
  mutate(Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")))

WQ_data_n_plot<-ggplot(WQ_data_n, aes(x=Year, y=N, color=Variable))+
  geom_line(size=1)+
  facet_wrap(~Season)+
  coord_cartesian(expand = F)+
  ylab("Sample size")+
  scale_color_viridis_d(breaks=c("Temperature", "Secchi"))+
  theme_bw()
WQ_data_n_plot

WQ_data%>%
  select(Year, Season, Temperature, Secchi)%>%
  arrange(Season, Year)%>%
  write_csv("Outputs/Drought.csv")