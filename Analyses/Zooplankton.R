require(dplyr)
require(readxl)
require(readr)
require(tidyr)
require(purrr)
require(ggplot2)

WY<-read_excel("Data/Water years.xlsx", sheet="yearassignments")

data<-read_csv("Data/Drought_taxa_BPUEmatrix.csv")%>%
  left_join(WY, by=c("water_year"="Year"))%>%
  mutate(Yr_type=factor(Yr_type, levels=c("Wet", "Above Normal", "Below Normal", "Dry", "Critical")),
         Index_s=(Index-mean(Index))/sd(Index))%>%
  pivot_longer(cols = c(-water_year, -Index, -Index_s, -Yr_type, -Drought), names_to="Taxa", values_to="BPUE")
  

ggplot(data, aes(x=Index, y=BPUE))+
  geom_point()+
  facet_wrap(~Taxa, scales = "free_y")+
  theme_bw()

ggplot(data, aes(x=Yr_type, y=BPUE))+
  geom_boxplot()+
  facet_wrap(~Taxa, scales = "free_y")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggplot(data, aes(x=Drought, y=BPUE))+
  geom_boxplot()+
  facet_wrap(~Taxa, scales = "free_y")+
  theme_bw()

# distributions
ggplot(data, aes(x=BPUE))+
  geom_histogram(bins=10)+
  facet_wrap(~Taxa, scales = "free")+
  theme_bw()
  
drought_tester<-function(taxa){
  if(taxa%in%c("Acartiella sinensis Adult", "Lomnoithona tetraspina Adult")){
    lm(BPUE~Index_s, data=filter(data, Taxa==taxa))
  }else{
    lm(log(BPUE)~Index_s, data=filter(data, Taxa==taxa))
  }
}

drought_models<-map(set_names(unique(data$Taxa)), drought_tester)
drought_models_resids<-map_dfr(drought_models, function(x) resid(x)%>%as_tibble(), .id = "Taxa")%>%
  rename(Resid=value)

ggplot(drought_models_resids, aes(x=Resid))+
  geom_histogram(bins=15)+
  facet_wrap(~Taxa, scales = "free")+
  theme_bw()                               

map(drought_models, summary)
