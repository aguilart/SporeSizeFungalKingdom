#
rm(list=ls())

library(tidyverse)

#Joining spore size data with fungal functional group

#Loading the spore database
AllFungi<-read.csv('output/Spore_Database_Fungi.csv', header = T,stringsAsFactors = F)
AllFungi$SporeArea<-AllFungi$spore_width*AllFungi$spore_length*(pi/4)
AllFungi$Q_ratio<-AllFungi$spore_length/AllFungi$spore_width

#Loading the current taxonomy of the entire kingdom

FungalTaxanomy_col<-read.csv('output/FungalTaxanomy_col.csv', header = T, stringsAsFactors = F)

#Loading functional group data

FunGuildData<-
  read.csv("GuildData.csv",header = T,stringsAsFactors = F)

l<-sapply(strsplit(FunGuildData$taxon, " "), length)
FunGuildData$taxonomicLevel<-"Species"

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
  c("arbuscular mycorrhizal","arbuscular mycorrhizal")

#As for many species I do not know whether they are licheninzed or lichenicolous, I will use anything that has
#"lichen" as a generic term
Spore_functions$host[which(Spore_functions$host=="Lichen-Algae")]<-"Lichen"


#Spore size 
Spore_functions$Life_style<-Spore_functions$host
Spore_functions$Life_style[Spore_functions$trophicMode=="Saprotroph"]<-"AFree living"

Spore_functions$SporeType<-NA
Spore_functions$SporeType[Spore_functions$SporeName%in%c("Ascospores","Basidiospores","Zygospores")]<-"Sexual"
Spore_functions$SporeType[Spore_functions$SporeName%in%c("Conidia","Sporangiospores","Chlamydospores",
                                                         "Azygospores","Teliospores")]<-"Asexual"


