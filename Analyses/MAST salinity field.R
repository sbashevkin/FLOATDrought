# Winter starts in Dec, push to next year
require(discretewq) # water quality data https://github.com/sbashevkin/discretewq
require(deltamapr) # SubRegions dataset https://github.com/InteragencyEcologicalProgram/deltamapr
require(dplyr)
require(sf)
require(lubridate)
require(hms)
require(tidyr)
require(readr)
require(dtplyr) # To speed things up
require(ggplot2) # Plotting
require(geofacet) # plotting


# Subregions organized on a geographic grid for geofacetted plots
mygrid <- data.frame(
  name = c("Upper Sacramento River Ship Channel", "Cache Slough and Lindsey Slough", "Lower Sacramento River Ship Channel", "Liberty Island", "Upper Sacramento River", "Suisun Marsh", "Middle Sacramento River", "Lower Cache Slough", "Steamboat and Miner Slough", "Upper Mokelumne River", "Lower Mokelumne River", "Sacramento River near Ryde", "Sacramento River near Rio Vista", "Grizzly Bay", "West Suisun Bay", "Mid Suisun Bay", "Honker Bay", "Confluence", "Lower Sacramento River", "San Joaquin River at Twitchell Island", "San Joaquin River at Prisoners Pt", "Disappointment Slough", "Lower San Joaquin River", "Franks Tract", "Holland Cut", "San Joaquin River near Stockton", "Mildred Island", "Middle River", "Old River", "Rock Slough and Discovery Bay", "Upper San Joaquin River", "Grant Line Canal and Old River", "Victoria Canal"),
  row = c(2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 8, 8, 8),
  col = c(7, 4, 6, 5, 8, 2, 8, 6, 7, 9, 8, 7, 6, 2, 1, 2, 3, 4, 5, 6, 8, 9, 5, 6, 7, 9, 8, 8, 7, 6, 9, 8, 7),
  code = c(" 1", " 1", " 2", " 3", " 32", " 8", " 4", " 5", " 6", " 7", " 9", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "33", "30", "29", "31"),
  stringsAsFactors = FALSE
)


## Load Delta Shapefile from Brian
Delta<-deltamapr::R_EDSM_Subregions_Mahardja%>%
  filter(!SubRegion%in%c("South Bay", "San Francisco Bay", "San Pablo Bay", "Upper Yolo Bypass", # Remove regions outside our domain of interest
                         "Upper Napa River", "Lower Napa River", "Carquinez Strait", # Remove regions outside our domain of interest
                         "Rock Slough and Discovery Bay", "Upper Sacramento River"))%>%  # Exclude 2 regions with none or almost no samples
  dplyr::select(SubRegion)%>%
  mutate(SubRegion=recode(SubRegion, `Georgiana Slough`="Sacramento River near Ryde"))%>% # Merge Georgiana Slough with Sacramento River near Ryde since Georgiana has only 1 station
  group_by(SubRegion)%>%
  summarise()%>%
  ungroup()

Data<-wq(End_year=2021,
         Sources = c("EMP", "STN", "FMWT", "DJFMP", "SKT", "20mm", "Suisun",
                     "Baystudy", "USGS"))%>% # Select all long-term surveys (excluding EDSM and the USBR Sacramento ship channel study)
  filter(!is.na(Salinity) & !is.na(Latitude) & !is.na(Datetime) & !is.na(Longitude) & !is.na(Date))%>% #Remove any rows with NAs in our key variables
  mutate(Datetime = with_tz(Datetime, tz="America/Phoenix"), #Convert to a timezone without daylight savings time
         Date = with_tz(Date, tz="America/Phoenix"), # Calculate difference from noon for each data point for later filtering
         Station=paste(Source, Station),
         Time=as_hms(Datetime), # Create variable for time-of-day, not date. 
         Noon_diff=abs(hms(hours=12)-Time))%>% # Calculate difference from noon for each data point for later filtering
  lazy_dt()%>% # Use dtplyr to speed up operations
  group_by(Station, Source, Date)%>%
  filter(Noon_diff==min(Noon_diff))%>% # Select only 1 data point per station and date, choose data closest to noon
  filter(Time==min(Time))%>% # When points are equidistant from noon, select earlier point
  ungroup()%>%
  as_tibble()%>% # End dtplyr operation
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=FALSE)%>% # Convert to sf object
  st_transform(crs=st_crs(Delta))%>% # Change to crs of Delta
  st_join(Delta, join=st_intersects)%>% # Add subregions
  filter(!is.na(SubRegion))%>% # Remove any data outside our subregions of interest
  st_drop_geometry()%>% # Drop sf geometry column since it's no longer needed
  lazy_dt()%>% # Use dtplyr to speed up operations
  group_by(SubRegion)%>%
  mutate(Sal_sd=sd(Salinity), Sal_ext=(Salinity-mean(Salinity))/Sal_sd)%>% # Calculate salinity SD for each subregion, then the number of SD's each data point exceeds the mean
  ungroup()%>%
  as_tibble()%>% # End dtplyr operation
  filter(Sal_ext<10)%>% # Filter out any data points that are more than 10 SDs away from the mean of each subregion
  mutate(Year=if_else(Month==12, Year+1, Year), # Move Decembers to the following year
         Season=case_when(Month%in%3:5 ~ "Spring", # Create seasonal variables
                          Month%in%6:8 ~ "Summer",
                          Month%in%9:11 ~ "Fall",
                          Month%in%c(12, 1, 2) ~ "Winter",
                          TRUE ~ NA_character_))%>%
  lazy_dt()%>% # Use dtplyr to speed up operations
  group_by(Month, Season, SubRegion, Year)%>%
  summarise(Sal_month_mean=mean(Salinity), Sal_month_var=sd(Salinity)^2, N=n())%>% # Calculate monthly mean, variance, sample size
  ungroup()%>%
  as_tibble()%>% # End dtplyr operation
  complete(nesting(Month, Season), SubRegion, Year, fill=list(N=0))%>% # Fill in NAs for salinity (and 0 for N) for any missing month, subregion, year combinations to make sure all months are represented in each season
  lazy_dt()%>% # Use dtplyr to speed up operations
  group_by(Season, SubRegion, Year)%>%
  summarise(Sal_mean=mean(Sal_month_mean), Sal_sd=sqrt(mean(Sal_month_var)), N=sum(N))%>% # Calculate seasonal mean salinity, seasonal sd as the sqrt of the mean monthly variance, total seasonal sample size
  ungroup()%>%
  group_by(SubRegion, Season)%>% # Some Sal_sd were NA when there was only 1 sample per month, so here I'm calculating and filling in the average SD for that region and season
  mutate(Sal_sd_mean=mean(Sal_sd, na.rm=T))%>%
  ungroup()%>%
  as_tibble()%>% # End dtplyr operation
  mutate(Sal_sd=if_else(is.na(Sal_sd), Sal_sd_mean, Sal_sd))%>%
  select(-Sal_sd_mean)%>%
  mutate(Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")))

p_effort<-ggplot(filter(Data, N>0))+
  geom_tile(aes(x=Year, y=Season, fill=log10(N)))+
  scale_fill_viridis_c(breaks=c(0,1,2), labels=function(x) 10^x, guide=guide_colorbar(barheight=15, barwidth = 3),
                       name="Total N")+
  scale_x_continuous(breaks=seq(1970, 2020, by=10))+
  scale_y_discrete(limits=rev)+
  facet_geo(~SubRegion, grid=mygrid, labeller=label_wrap_gen(width=18))+
  ylab("Month")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1), panel.grid=element_blank(), text=element_text(size=16), legend.position=c(0.4, 0.65), 
        legend.background = element_rect(color="black"), panel.background = element_rect(color="black"), legend.margin=margin(10,10,15,10))

ggsave(p_effort, file="Outputs/Salinity sampling effort.png",
       device="png", width=15, height=18, units="in")

Low_salinity_zone<-Data%>%
  mutate(Low_sal=if_else(Sal_mean<=6 & Sal_mean>=0.5, TRUE, FALSE),
         Low_sal_sd=if_else((Sal_mean-Sal_sd)<=6 & (Sal_mean+Sal_sd)>=0.5, TRUE, FALSE))

p_low_sal_zone<-ggplot(Low_salinity_zone)+
  geom_tile(aes(x=Year, y=Season, fill=Low_sal))+
  scale_fill_viridis_d()+
  scale_x_continuous(breaks=seq(1970, 2020, by=10))+
  scale_y_discrete(limits=rev)+
  facet_geo(~SubRegion, grid=mygrid, labeller=label_wrap_gen(width=18))+
  ylab("Month")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1), panel.grid=element_blank(), text=element_text(size=16), legend.position=c(0.4, 0.65), 
        legend.background = element_rect(color="black"), panel.background = element_rect(color="black"), legend.margin=margin(10,10,15,10))
ggsave(p_low_sal_zone, file="Outputs/Low salinity zone.png",
       device="png", width=15, height=18, units="in")

p_low_sal_zone_sd<-ggplot(Low_salinity_zone)+
  geom_tile(aes(x=Year, y=Season, fill=Low_sal_sd))+
  scale_fill_viridis_d()+
  scale_x_continuous(breaks=seq(1970, 2020, by=10))+
  scale_y_discrete(limits=rev)+
  facet_geo(~SubRegion, grid=mygrid, labeller=label_wrap_gen(width=18))+
  ylab("Month")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1), panel.grid=element_blank(), text=element_text(size=16), legend.position=c(0.4, 0.65), 
        legend.background = element_rect(color="black"), panel.background = element_rect(color="black"), legend.margin=margin(10,10,15,10))
ggsave(p_low_sal_zone_sd, file="Outputs/Low salinity zone within sd.png",
       device="png", width=15, height=18, units="in")

# Save csv of low salinity zone

Rosies_regions<-tibble(
  SubRegion=c(rep(c("Suisun Marsh",
                    "Grizzly Bay", 
                    "West Suisun Bay", 
                    "Mid Suisun Bay", 
                    "Honker Bay", 
                    "Confluence",
                    "Lower Sacramento River",
                    "San Joaquin River at Twitchell Island",
                    "San Joaquin River at Prisoners Pt",
                    "Disappointment Slough",
                    "Lower San Joaquin River",
                    "Franks Tract", 
                    "Holland Cut",
                    "Mildred Island",
                    "San Joaquin River near Stockton",
                    "Old River",
                    "Middle River",
                    "Victoria Canal",
                    "Upper San Joaquin River",
                    "Sacramento River near Rio Vista",
                    "Lower Mokelumne River",
                    "Middle Sacramento River",
                    
                    "Cache Slough and Lindsey Slough",
                    "Liberty Island",
                    "Lower Sacramento River Ship Channel",
                    "Upper Sacramento River Ship Channel",
                    "Lower Cache Slough",
                    "Steamboat and Miner Slough",
                    "Upper Mokelumne River",
                    "Sacramento River near Ryde"), each=4)),
  Season = rep(levels(Data$Season), n_distinct(SubRegion))
)%>%
  mutate(Long_term=case_when(SubRegion%in%c("Cache Slough and Lindsey Slough",
                                            "Liberty Island",
                                            "Lower Sacramento River Ship Channel",
                                            "Upper Sacramento River Ship Channel",
                                            "Lower Cache Slough",
                                            "Steamboat and Miner Slough",
                                            "Upper Mokelumne River",
                                            "Sacramento River near Ryde") ~ FALSE,
                             SubRegion%in%c("Franks Tract", 
                                            "Holland Cut",
                                            "Mildred Island") & Season!="Summer" ~ FALSE,
                             SubRegion%in%"Middle River" & Season!="Fall" ~ FALSE,
                             SubRegion%in%"Victoria Canal" & !Season%in%c("Spring", "Summer") ~ FALSE,
                             TRUE ~ TRUE),
         Short_term=if_else(Long_term | SubRegion%in%c("Cache Slough and Lindsey Slough",
                                                       "Liberty Island",
                                                       "Lower Sacramento River Ship Channel",
                                                       "Upper Sacramento River Ship Channel",
                                                       "Lower Cache Slough",
                                                       "Steamboat and Miner Slough",
                                                       "Upper Mokelumne River",
                                                       "Sacramento River near Ryde"), TRUE, FALSE))

write_csv(Rosies_regions, "Outputs/Rosies_regions.csv")

Low_salinity_zone%>%
  filter(Low_sal)%>%
  select(Season, SubRegion, Year, N)%>%
  left_join(Rosies_regions)%>%
  mutate(across(c(Long_term, Short_term), ~replace_na(.x, FALSE)))%>%
  write_csv("Outputs/Low_salinity_zone.csv")
