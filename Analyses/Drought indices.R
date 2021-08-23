source("Analyses/WQindices.R")

vars<-c("Temperature", "Secchi", "Salinity")

# Seasonal averages for each year
WQ_list_season<-map(vars, WQindices, region="all", month.na="relaxed", type="season") # Nor relaxing the requirement of 3 months being present in all years

yearseasons<-expand_grid(Season=c("Winter", "Spring", "Summer", "Fall"),
                         Year=1975:2021)

WQ_data_season<-map(WQ_list_season, ~left_join(yearseasons, .x, by=c("Season", "Year"))%>%
               select(contains(vars)))%>%
  bind_cols(yearseasons, .)%>%
  arrange(Year, Season)

WQ_data_season_n<-WQ_data_season%>%
  select(all_of(paste0("N_", vars)), Season, Year)%>%
  pivot_longer(all_of(paste0("N_", vars)), names_prefix="N_", names_to="Variable", values_to="N")%>%
  mutate(Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")))

WQ_data_n_plot<-ggplot(WQ_data_season_n, aes(x=Year, y=N, color=Variable))+
  geom_line(size=1)+
  facet_wrap(~Season)+
  coord_cartesian(expand = F)+
  ylab("Sample size")+
  scale_color_viridis_d(breaks=c("Temperature", "Secchi"))+
  theme_bw()
WQ_data_n_plot

WQ_data_season%>%
  select(Year, Season, Temperature, Secchi, Salinity)%>%
  arrange(Season, Year)%>%
  write_csv("Outputs/Drought_Season_Temp_Secchi_Salinity.csv")

# Yearly averages for each region

WQ_list_year<-map(vars, WQindices, region="all", month.na="relaxed", type="year") # Nor relaxing the requirement of 3 months being present in all years

yearregion<-expand_grid(Region=unique(Regions$Region),
                         Year=1975:2021)

WQ_data_year<-map(WQ_list_year, ~left_join(yearregion, .x, by=c("Region", "Year"))%>%
                      select(contains(vars)))%>%
  bind_cols(yearregion, .)%>%
  arrange(Year, Region)

WQ_data_year_n<-WQ_data_year%>%
  select(all_of(paste0("N_", vars)), Region, Year)%>%
  pivot_longer(all_of(paste0("N_", vars)), names_prefix="N_", names_to="Variable", values_to="N")

WQ_data_n_plot<-ggplot(WQ_data_year_n, aes(x=Year, y=N, color=Variable))+
  geom_line(size=1)+
  facet_wrap(~Region)+
  coord_cartesian(expand = F)+
  ylab("Sample size")+
  scale_color_viridis_d(breaks=c("Temperature", "Secchi"))+
  theme_bw()
WQ_data_n_plot

WQ_data_year%>%
  select(Year, Region, Temperature, Secchi, Salinity)%>%
  arrange(Region, Year)%>%
  write_csv("Outputs/Drought_Year_Temp_Secchi_Salinity.csv")
