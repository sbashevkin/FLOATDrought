source("Analyses/WQindices.R")

vars<-c("Salinity", "Temperature", "Secchi", "Chlorophyll")
WQ_list<-map(vars, WQindices)

yearseasons<-expand_grid(Season=c("Winter", "Spring", "Summer", "Fall"),
                         Year=1975:2021)

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
  scale_color_viridis_d(breaks=c("Temperature", "Secchi", "Salinity", "Chlorophyll"))+
  theme_bw()
WQ_data_n_plot

WQ_data%>%
  select(-contains("N_"))%>%
  pivot_wider(names_from=Season, values_from=contains(vars))%>%
  select(Year, Chlorophyll_Fall, Temperature_Summer, Temperature_Fall, Secchi_Fall)%>%
  write_csv("Outputs/FLOAT.csv")