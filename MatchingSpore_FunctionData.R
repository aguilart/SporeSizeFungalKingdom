#
rm(list=ls())

library(tidyverse)

#Joining spore size data with fungal functional group

#Loading the spore database
AllFungi<-read.csv('output/Spore_Database_Fungi.csv', header = T,stringsAsFactors = F)
AllFungi$SporeArea<-AllFungi$spore_width*AllFungi$spore_length*(pi/4)
AllFungi$Q_ratio<-AllFungi$spore_length/AllFungi$spore_width
AllFungi$class<-trimws(AllFungi$class)
AllFungi$order<-trimws(AllFungi$order)
AllFungi$family<-trimws(AllFungi$family)

#Loading the current taxonomy of the entire kingdom

FungalTaxanomy_col<-read.csv('output/FungalTaxanomy_col.csv', header = T, stringsAsFactors = F)

#Loading functional group data

FunGuildData<-
  read.csv("GuildData.csv",header = T,stringsAsFactors = F)

l<-sapply(strsplit(FunGuildData$taxon, " "), length)
FunGuildData$taxonomicLevel<-"Species"

unique(FunGuildData$Guild_1)

#Joining spore data and functional groups
Spore_functions<-
  left_join(
    
    AllFungi#%>%
    # group_by(phylum,names_to_use,SporeName)%>%
    #mutate(SporeArea=spore_width*spore_length*(pi/4))%>% 
    ,#summarise_at(c("spore_width","spore_length","SporeArea"),mean),
    FunGuildData%>%
      filter(taxonomicLevel=="Species")%>%
      rename(names_to_use=taxon)%>%
      select(names_to_use,trophicMode,guild,host,substrate,Function,Number_of_guilds,Guild_1),
      by="names_to_use")
      

# Spore_functions2<-
#   left_join(AllFungi,
#             FunGuildData %>% 
#               rename(base_name=taxon)%>%
#               select(base_name,trophicMode,guild,host,substrate,Function,Number_of_guilds,Guild_1),
#             by="base_name")
# length(Spore_functions$names_to_use%in%Spore_functions2$names_to_use);
# Spore_functions2<-Spore_functions2[-which(is.na(Spore_functions2$trophicMode)),]
# length(which(Spore_functions2$base_name%in%Spore_functions$base_name))

Spore_functions[which(Spore_functions$phylum=="Glomeromycota"),
                #c("names_to_use",
                c("trophicMode","host","substrate","Function","Number_of_guilds",
                  "guild","Guild_1")]<-
  FunGuildData[which(FunGuildData$taxon=="Glomus intraradices"),
               c("trophicMode","host","substrate","Function","Number_of_guilds",
                 "guild","Guild_1")]


Spore_functions[grep("Geosiphon",Spore_functions$names_to_use),
                #c("names_to_use","trophicMode","host","substrate","Function","Number_of_guilds",
                c("guild","Guild_1")]<-
  c("Arbuscular Mycorrhizal","Arbuscular Mycorrhizal")

#As for many species I do not know whether they are licheninzed or lichenicolous, I will use anything that has
#"lichen" as a generic term
Spore_functions$host[which(Spore_functions$host=="Lichen-Algae")]<-"Lichen"


#Spore size 
Spore_functions$Life_style<-Spore_functions$host
Spore_functions$Life_style[Spore_functions$trophicMode=="Saprotroph"]<-"AFree living"
#To add whether animal and human should be united


Spore_functions$SporeType<-NA
Spore_functions$SporeType[Spore_functions$SporeName%in%c("Ascospores","Basidiospores","Zygospores")]<-"Sexual"
Spore_functions$SporeType[Spore_functions$SporeName%in%c("Conidia","Sporangiospores","Chlamydospores",
                                                         "Azygospores","Teliospores")]<-"Asexual"


#Subset for tha analysis

To_Analysis<-
  left_join(
    Spore_functions %>% 
      filter(!is.na(Life_style)) %>%
      filter(!grepl("-",host))%>%#The issue is that we only have data for 2503 species
      filter(!grepl("Fungi",host))%>%
      filter(!grepl("Animal",host))%>%
      #filter(!SporeName%in%c("Azygospores","Teliospores"))%>%
      
      group_by(phylum,names_to_use,SporeType,SporeName,Specific_sporeName,Life_style)%>%
      summarise_at(c("spore_length","spore_width","SporeArea","Q_ratio"),mean),
    
    Spore_functions%>%
      filter(!duplicated(names_to_use)) %>% 
      filter(!is.na(Life_style)) %>%
      filter(!grepl("-",host))%>%#The issue is that we only have data for 2503 species
      filter(!grepl("Fungi",host))%>%
      filter(!grepl("Animal",host))%>%
      #filter(!SporeName%in%c("Azygospores","Teliospores"))%>%
      #group_by(phylum,guild) %>% 
      select(names_to_use,guild,Function,trophicMode,class,order,family,genus,Number_of_guilds));
To_Analysis$simpleFunct<-To_Analysis$Life_style
To_Analysis$simpleFunct[grep("Plant Pathogen",To_Analysis$guild)]<-"Plant Pathogen"
To_Analysis$simpleFunct[which(To_Analysis$guild=="Bryophyte Parasite")]<-"Plant Pathogen"
To_Analysis$simpleFunct[which(To_Analysis$guild=="Endophyte")]<-"Plant Endophyte"
To_Analysis$simpleFunct[which(To_Analysis$guild=="Ectomycorrhizal")]<-"Plant Ectomycorrhizal"
To_Analysis$simpleFunct[which(To_Analysis$guild=="Arbuscular Mycorrhizal")]<-"Plant AMF"
To_Analysis$simpleFunct[which(To_Analysis$simpleFunct=="Plant")]<-"Plant Endophyte"
To_Analysis$simpleFunct[which(To_Analysis$names_to_use%in%c("Lyophyllum decastes","Rhodocollybia butyracea"))]<-"Plant Ectomycorrhizal"

#this line will need to be removed
Conidia<-To_Analysis[which(To_Analysis$SporeName=="Conidia"),]


my_theme<-
  theme(title = element_text(size = 18),
        #axis.title.x=element_blank(),
        #axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 25),
        legend.position = "none")


my_theme2<-
  theme(title = element_text(size = 18),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 25),
        legend.position = "none")

my_theme3<-
  theme(title = element_text(size = 18),
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 25)#,
        #legend.position = "none"
        )