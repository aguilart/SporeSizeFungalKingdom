########################################################################
## Building data set to map the distribution of spore size across the ##
## fungal kingdom and (potentially) fungi like organisms ###############
########################################################################

#Data, so far, come from three sources:

#1. FunToFun Database, specifically from basidiospore of basidiomycota
#data from Bassler etal 2015.

#2. Data from Aguilar etal 2018 (submitted) on Azygospore size
#of Glomeromycotina.

#3. Data extracted from "Domsch, K., et al. (2007). Compendium of soil 
#fungi, IHW-Verlag Eching.". This correspond mainly to different types
#of conidia, ascospospores and chlamydospores from Ascomycota. There is
#also data on Mucoromycotina (both sexual an asexual spores), some basid
#diomycota and oomycota.


#1. Fun to Fun Data

devtools::install_github("ropenscilabs/datastorr")
devtools::install_github("traitecoevo/fungaltraits")
library(fungaltraits)

FunToFun_sporeInfo<-fungal_traits()[!is.na(fungal_traits()$spore_length),
                                    c(1,2,3,68,69,70,75,76)]

                    #In this dataset, spore length is always larger than spore width. Thus,
                    #Equatorial axis = width; polar axis = length.


#2. AMF data

AMF_All_Copy<-read.csv("C:\\Users\\Carlos\\Documents\\Professional\\Spore CommunityAnalysis\\SporeSize\\AMF_Spore_Database_Volume.csv",
         header = T, stringsAsFactors = F)

              #UDATING THE SPORE AREA COLUMN given the previous updates in data entry
              AMF_All_Copy$SporeArea<-
                ((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/4)*
                ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/4)*
                pi
              
              #Caluculating Volume 
              #For this it is assumed that fungal spores are prolate spheroids, which means
              #two things. First, that the smallest diameter will be consider the "equatorial axis" and the largest
              #is the "polar axis"; second, that the there are two equatorial axis of the same length. This is 
              #important because the formula to calculate volume make distinction between the two type of axis.
              #In this way, AMF spore can be only of two types, perfect spheres (in which case equatorial and 
              #polar axis are of the same lenght), corrsponding to "globose" spores. And, spheroids similar to an
              #american football.  A morphology like a "flying soucer" is not possible following this rules.
              
              
              AMF_All_Copy$EquatorialAxis<-NaN
              AMF_All_Copy$EquatorialAxis[
                which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
                        ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
                ((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)[
                  which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
                          ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
              AMF_All_Copy$EquatorialAxis[which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
                                                  ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
                ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
                  which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
                          ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
              AMF_All_Copy$EquatorialAxis[which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
                                                  ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
                ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
                  which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
                          ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
              
              AMF_All_Copy$PolarAxis<-NaN
              AMF_All_Copy$PolarAxis[
                which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
                        ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
                ((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)[
                  which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
                          ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
              AMF_All_Copy$PolarAxis[
                which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
                        ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
                ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
                  which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
                          ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
              AMF_All_Copy$PolarAxis[
                which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
                        ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
                ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
                  which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
                          ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
              
              which(AMF_All_Copy$EquatorialAxis>AMF_All_Copy$PolarAxis)#This should always be zero (0)
              
              #Now calculating volume
              AMF_All_Copy$SporeVolume<-(pi*(AMF_All_Copy$EquatorialAxis^2)*(AMF_All_Copy$PolarAxis))/6
              #Adding the phylum
              AMF_All_Copy$Phylum<-"Glomeromycota"
              

#3. Compendium of soil fungi data

CompendiumData<-read.csv("CompSoilFungData.csv",header = T,stringsAsFactors = F)
                #Fixing some typos and errors
                CompendiumData$Phylum[grep("Mortierella",CompendiumData$Genus)]<-"Mortierellomycotina"
                CompendiumData$Phylum[grep("Mucor",CompendiumData$Genus)]<-"Mucoromycotina"
                CompendiumData$Phylum[grep("Cunninghamella",CompendiumData$Genus)]<-"Mucoromycotina"
                CompendiumData$Phylum[grep("Rhizomucor",CompendiumData$Genus)]<-"Mucoromycotina"
                CompendiumData$Phylum[grep("Rhizopus",CompendiumData$Genus)]<-"Mucoromycotina"
                CompendiumData$Phylum[grep("Syncephalastrum",CompendiumData$Genus)]<-"Mucoromycotina"
                CompendiumData$Phylum[grep("Thamnidium",CompendiumData$Genus)]<-"Mucoromycotina"
                CompendiumData$Phylum[grep("Zygorhynchus",CompendiumData$Genus)]<-"Mucoromycotina"
                CompendiumData$Phylum[grep("Fusarium",CompendiumData$Genus)]<-"Ascomycota"
                CompendiumData$Phylum[grep("Conidiobolus",CompendiumData$Genus)]<-"Ascomycota"
                CompendiumData$Phylum[grep("Conidiobolus",CompendiumData$Genus)]<-"Entomophthoromycotina"

                #Getting spore widht
                CompendiumData$spore_width<-NaN
                      CompendiumData$spore_width[which(
                        CompendiumData$spore_d2<=CompendiumData$spore_d1)]<-CompendiumData$spore_d2[which(
                        CompendiumData$spore_d2<=CompendiumData$spore_d1)]
                      #or
                      CompendiumData$spore_width[which(
                          CompendiumData$spore_d1<CompendiumData$spore_d2)]<-CompendiumData$spore_d1[which(
                          CompendiumData$spore_d1<CompendiumData$spore_d2)]
                #Getting spore length
                CompendiumData$spore_length<-NaN
                      CompendiumData$spore_length[which(
                        CompendiumData$spore_d2>=CompendiumData$spore_d1)]<-CompendiumData$spore_d2[which(
                          CompendiumData$spore_d2>=CompendiumData$spore_d1)]
                      #or
                      CompendiumData$spore_length[which(
                        CompendiumData$spore_d1>CompendiumData$spore_d2)]<-CompendiumData$spore_d1[which(
                          CompendiumData$spore_d1>CompendiumData$spore_d2)]

#4. Merging all data sources into one "AllFungi"            

AllFungi<-rbind(
            data.frame(Species_names=AMF_All_Copy[,1],
                       spore_length=AMF_All_Copy[,34],
                       spore_width=AMF_All_Copy[,33],
                       SporeArea=AMF_All_Copy[,15],
                       SporeName="Azygospores",
                       Phylum=AMF_All_Copy[,36],
                       Source="Aguilar_etal_2018"),
            data.frame(Species_names=CompendiumData[,4],
                       spore_length=CompendiumData[,31],
                       spore_width=CompendiumData[,30],
                      SporeArea=CompendiumData[,17],
                      SporeName=CompendiumData[,7],
                      Phylum=CompendiumData[,27],
                      Source="CompendiumSoilFungi"),
            data.frame(Species_names=FunToFun_sporeInfo[
                        FunToFun_sporeInfo$studyName=="Bassler_etal_2015",2],
                      spore_length=FunToFun_sporeInfo[
                        FunToFun_sporeInfo$studyName=="Bassler_etal_2015",4],
                      spore_width=FunToFun_sporeInfo[
                        FunToFun_sporeInfo$studyName=="Bassler_etal_2015",6],
                      SporeArea=FunToFun_sporeInfo[
                        FunToFun_sporeInfo$studyName=="Bassler_etal_2015",5],
                      SporeName="Basidiospores",
                      Phylum="Basidiomycota",
                      Source="Bassler_etal_2015")       )

        #Correcting some typos
        AllFungi$SporeName<-as.character(AllFungi$SporeName)
        AllFungi$SporeName[
        grep("conidia",AllFungi$SporeName)]<-"conidia"
        AllFungi$SporeName[
          grep("basidiospores",AllFungi$SporeName)]<-"Basidiospores"
        AllFungi$SporeName[
          grep("chlamydospore",AllFungi$SporeName)]<-"chlamydospores"

#Storing the data

write.csv(AllFungi,"AllFungi.csv",row.names = F)


###Leftovers


(is.na(AllFungi$Phylum))

write.csv(
sub("_"," ",
FunToFun_sporeInfo[
  FunToFun_sporeInfo$studyName=="Bassler_etal_2015",2]),
"BasslerNames.csv",row.names = F,col.names = F)

library(tidyverse)

AllFungi%>%
  ggplot()+
  aes(Phylum,SporeArea,fill=Phylum)+
  geom_jitter(size=0.5, width = 0.3,alpha=1)+
  geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75))+
  #facet_grid(. ~ Organism, scales = "free")+
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(y=expression("Spore size as area ("*mu*"m²)"))+
  theme(title = element_text(size = 25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.position = "none")