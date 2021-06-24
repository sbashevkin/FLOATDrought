require(tidyverse)
require(sf)

data<-discretewq::wq()%>%
  filter(!is.na(Latitude) & !is.na(Longitude))%>%
  group_by(StationID, Latitude, Longitude)%>%
  summarise(N=n(), .groups="drop")

data_4326<-data%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
  st_transform(st_crs(deltamapr::R_EDSM_Subregions_Mahardja_FLOAT))%>%
  st_join(deltamapr::R_EDSM_Subregions_Mahardja_FLOAT%>%select(SubRegion))

data_4269<-data%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4269)%>%
  st_transform(st_crs(deltamapr::R_EDSM_Subregions_Mahardja_FLOAT))%>%
  st_join(deltamapr::R_EDSM_Subregions_Mahardja_FLOAT%>%select(SubRegion))


ggplot(data_4326)+
  geom_sf(data=deltamapr::R_EDSM_Subregions_Mahardja_FLOAT, fill=NA)+
  geom_sf(data=deltamapr::WW_Delta%>%st_transform(crs=st_crs(deltamapr::R_EDSM_Subregions_Mahardja_FLOAT)), color="slategray")+
  geom_sf(alpha=0.1)+
  geom_sf(data=filter(data_4326, is.na(SubRegion)), color="red")+
  theme_bw()

ggplot(data_4269)+
  geom_sf(data=deltamapr::R_EDSM_Subregions_Mahardja_FLOAT, fill=NA)+
  geom_sf(data=deltamapr::WW_Delta%>%st_transform(crs=st_crs(deltamapr::R_EDSM_Subregions_Mahardja_FLOAT)), color="slategray")+
  geom_sf(alpha=0.1)+
  geom_sf(data=filter(data_4269, is.na(SubRegion)), color="red")+
  theme_bw()
