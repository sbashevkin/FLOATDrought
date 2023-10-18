
#replace zoop processing chunk from CSAMP code with this

zoop_data_mass<-zoop_data%>%
  mutate(Taxlifestage=str_remove(Taxlifestage, fixed("_UnID")))%>%
  filter(
    !(SizeClass=="Meso" & #eliminating species which are counted in meso and micro and retained better in the micro net from the meso calcs
        
        Taxlifestage%in%c("Asplanchna Adult", "Copepoda Larva","Cyclopoida Juvenile", "Eurytemora Larva", "Harpacticoida Undifferentiated",
                          "Keratella Adult", "Limnoithona Adult", "Limnoithona Juvenile", "Limnoithona sinenesis Adult", "Limnoithona tetraspina
                                    Adult", "Oithona Adult", "Oithona Juvenile", "Oithona davisae Adult", "Polyarthra Adult","Pseudodiaptomus Larva", 
                          "Rotifera Adult", "Sinocalanus doerrii Larva", "Synchaeta Adult", "Synchaeta bicornis Adult", "Trichocerca Adult")) &
      
      !(SizeClass=="Micro" &Taxlifestage%in%c("Cirripedia Larva", "Cyclopoida Adult", "Oithona similis")) & #removing categories better retained in meso net from micro net matrix
      Order!="Amphipoda" & # Remove amphipods
      (Order!="Mysida" | Taxlifestage=="Hyperacanthomysis longirostris Adult"))%>% #Only retain Hyperacanthomysis longirostris
  mutate(Taxlifestage=recode(Taxlifestage, `Synchaeta bicornis Adult`="Synchaeta Adult", # Change some names to match to biomass conversion dataset
                             `Pseudodiaptomus Adult`="Pseudodiaptomus forbesi Adult",
                             `Acanthocyclops vernalis Adult`="Acanthocyclops Adult"))%>%
  left_join(zoop_mass_conversions, by="Taxlifestage")%>% # Add biomass conversions
  left_join(zoop_mysid, by=c("SampleID", "Taxlifestage", "SizeClass"))%>% # Add mysid biomass
  left_join(zoop_groups, by="Taxlifestage")%>% # Add IBMR categories
  mutate(BPUE=if_else(Taxlifestage=="Hyperacanthomysis longirostris Adult", BPUE_mysid, CPUE*CarbonWeight_ug))%>% # Create 1 BPUE variable
  filter(!is.na(BPUE) & !is.na(Latitude) & !is.na(Longitude) & !is.na(SalSurf))%>% # Removes any data without BPUE, which is currently restricted to Decapod Larvae, and H. longirostris from STN. Also removes 20mm and EMP EZ stations without coordinates
  group_by(IBMR)%>%
  mutate(flag=if_else(all(c("Micro", "Meso")%in%SizeClass), "Remove", "Keep"))%>% # This and the next 2 lines are meant to ensure that all categories are consistent across the surveys. Since only EMP samples microzoops, only EMP data can be used for categories that include both micro and mesozoops.
  ungroup()%>%
  filter(!(flag=="Remove" & Source!="EMP"))%>%
  mutate(Category=case_when(IBMR%in%c("acartela", "eurytem", "othcalad", 
                                      "othcaljuv", "pdiapfor") ~ "Calanoid",
                            IBMR %in%c("limno", "othcyc") ~ "Cyclopoid",
                            IBMR%in%c("daphnia", "othclad") ~ "Cladoceran",
                            IBMR=="other" ~ "Rotifer",
                            IBMR=="mysid" ~ "Mysid",
                            IBMR=="allcopnaup" ~ "Copepod"))%>%
  select(SampleID, Station, Latitude, Longitude, SalSurf, Date, Year, Category, BPUE)%>%
  group_by(across(-BPUE))%>%
  summarise(BPUE=sum(BPUE), .groups="drop")%>% # Sum each category
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
  st_transform(crs=st_crs(deltamapr::R_DSIBM)) %>% 
  st_join(deltamapr::R_DSIBM %>%
            select(SUBREGION)) %>%
  st_drop_geometry() %>% 
  mutate(Month=month(Date))%>%
  filter(Year>=2002)%>%
  group_by(Month, Category, SUBREGION)%>%
  summarise(BPUE=sum(BPUE), .groups="drop")%>% # Sum each category
  group_by(Month, SUBREGION)%>%
  mutate(prop=BPUE/sum(BPUE))%>%
  ungroup()

ggplot(zoop_data_mass, aes(x=Month, y=BPUE, fill=Category))+
  geom_bar(stat="identity")+
  facet_wrap(~SUBREGION, scales="free")+
  scale_x_continuous(breaks=1:12)+
  theme_bw()

ggplot(zoop_data_mass, aes(x=Month, y=prop, fill=Category))+
  geom_bar(stat="identity")+
  facet_wrap(~SUBREGION, scales="free")+
  scale_x_continuous(breaks=1:12)+
  theme_bw()

