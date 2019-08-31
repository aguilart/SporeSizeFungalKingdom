########################################################################
## Building data set to map the distribution of spore size across the ##
## fungal kingdom and (potentially) fungi like organisms ###############
########################################################################

#Data, so far, come from four sources:

#1. FunToFun Database, specifically from basidiospore of basidiomycota
#data from Bassler, Claus, et al. "Ectomycorrhizal fungi have larger 
#fruit bodies than saprotrophic fungi." Fungal Ecology 17 (2015): 205-212.

#2. Data from Aguilar etal 2019 (ISME) on Azygospore size
#of Glomeromycotina.

#3. Data extracted from "Domsch, K., et al. (2007). Compendium of soil 
#fungi, IHW-Verlag Eching.". This correspond mainly to different types
#of conidia, ascospospores and chlamydospores from Ascomycota. There is
#also data on Mucoromycotina (both sexual an asexual spores), some basid
#diomycota and oomycota.

#4. Data extracted from the mycobank database. As for August 2019, this data
#comes from the extraction and cleaning of the data by Jeff, Carlos, Coline and
#Haiyang

#5. Mycobank dataset. This is composed of the datasets extracted for Ascospores,
#Conidia, Basidiospores, Chlaymdospores, (A)Zygospores and Teliospores

rm(list=ls())

library(tidyverse)

#1. Fun to Fun Data, Bassler data
Bassler_Data<-read.csv('output/Basidiospores_Bassler2015.csv', header = T,
                       stringsAsFactors = F)

#2. AMF data
AMF_All_Copy<-read.csv('output/AzygosporesAMF_Aguilar2019ISME.csv', header = T,
         stringsAsFactors = F)

#3. Compendium of soil fungi data

CompendiumData<-read.csv('output/Spores_CompendiumSoilFungi.csv', header = T,
                         stringsAsFactors = F)
                      
#4. Purhonen Dataset 
#This dataset correspond to species names of fungi with spores from 
#Jenna Purhonen (sent on March 2019 to Carlos Aguilar)
                      
Purhonen_species<-read.csv("output/BasidioAscospores_JennaPurhonen.csv", header = T,
                           stringsAsFactors = F)

#Merging the above data sources into one "Spores_allSources"            

Spores_allSources<-rbind(
            data.frame(Species_names=AMF_All_Copy[,1],
                       spore_length=AMF_All_Copy[,15],
                       spore_width=AMF_All_Copy[,14],
                       #SporeArea=AMF_All_Copy[,23],
                       SporeName="Azygospores",
                       Phylum=AMF_All_Copy[,22],
                       Source="Aguilar_etal_2019"),
            data.frame(Species_names=CompendiumData[,4],
                       spore_length=CompendiumData[,29],
                       spore_width=CompendiumData[,28],
                      #SporeArea=CompendiumData[,17],
                      SporeName=CompendiumData[,7],
                      Phylum=CompendiumData[,27],
                      Source="CompendiumSoilFungi"),
            data.frame(Species_names=Purhonen_species[,1],
                       spore_length=Purhonen_species[,2],
                       spore_width=Purhonen_species[,3],
                       #SporeArea=Purhonen_species[,17],
                       SporeName=Purhonen_species[,13],
                       Phylum=Purhonen_species[,12],
                       Source="Jenna Purhonen"),
            data.frame(Species_names=Bassler_Data[,1],
                      spore_length=Bassler_Data[,2],
                      spore_width=Bassler_Data[,3],
                      SporeName="Basidiospores",
                      Phylum="Basidiomycota",
                      Source="Bassler_etal_2015")       )

        #Correcting some typos
        Spores_allSources$SporeName<-as.character(Spores_allSources$SporeName)
        Spores_allSources$Specific_sporeName<-Spores_allSources$SporeName
        Spores_allSources$SporeName[
        grep("conidia",Spores_allSources$SporeName)]<-"Conidia"
        Spores_allSources$SporeName[
          grep("basidiospores",Spores_allSources$SporeName)]<-"Basidiospores"
        Spores_allSources$SporeName[
          grep("chlamydospore",Spores_allSources$SporeName)]<-"Chlamydospores"
        
        Spores_allSources$Species_names<-sub("_"," ",Spores_allSources$Species_names)
        
        #Alternaria tenuissima has incorrect entry in the All Fungi database.
        #The values for the conidia should be 22-95 x 8-19 um (as stated in the Compendium
        #of soil fungi). Tim got confused with other entry values in the description
        #so, to not run again the whole code for All Fungi, I will replace manually the 
        #entry where it correspond:
        
        #conidia length
        Spores_allSources[grep("Alternaria tenuissima",Spores_allSources$Species_names),2]<-((22+95)/2)
        #conidia length
        Spores_allSources[grep("Alternaria tenuissima",Spores_allSources$Species_names),3]<-((8+19)/2)
        
        #Making sure that Spore size refers always to spore projected area
        #(that is the area obtained by: length x width x pi/4)
        
        Spores_allSources$spore_length<-as.numeric(Spores_allSources$spore_length)
        Spores_allSources$spore_width<-as.numeric(Spores_allSources$spore_width)
        
        Spores_allSources$SporeArea<-Spores_allSources$spore_length*Spores_allSources$spore_width*
                            (pi/4)
        
        
        #Conida Area
        #Spores_allSources[grep("Alternaria tenuissima",Spores_allSources$Species_names),4]<-((22+95)/2)*((8+19)/2)*(pi/4)
        
        Spores_allSources<-Spores_allSources[-grep("\\?",Spores_allSources$Species_names),]#I discarded 4 names that have a question mark
        
        library(taxize)
        Col_IDs<-get_colid_(Spores_allSources$Species_names)
        Col_IDs<-do.call("rbind",Col_IDs)
        Col_IDs$Species_names<-rownames(Col_IDs)
        Col_IDs$Species_names<-
          sub("\\.[0-9]+","",Col_IDs$Species_names)
        
        Spores_allSources<-left_join(Spores_allSources,Col_IDs,
                                     by="Species_names")
        
        #Standardizing the accepted names
        Spores_allSources$Col_acc_names<-NA
        Spores_allSources$Col_acc_names[which(Spores_allSources$status=="accepted name")]<-
          Spores_allSources$name[which((Spores_allSources$status=="accepted name"))]
        Spores_allSources$Col_acc_names[which(Spores_allSources$status=="synonym")]<-
          Spores_allSources$acc_name[which((Spores_allSources$status=="synonym"))]
        Spores_allSources$Col_acc_names[which(Spores_allSources$status=="ambiguous synonym")]<-
          Spores_allSources$acc_name[which((Spores_allSources$status=="ambiguous synonym"))]
        Spores_allSources$Col_acc_names[which(Spores_allSources$status=="misapplied name")]<-
          Spores_allSources$acc_name[which((Spores_allSources$status=="misapplied name"))]
        
        #Standardizing the accepted id
        Spores_allSources$Col_ID_acc_names<-NA
        Spores_allSources$Col_ID_acc_names[which(Spores_allSources$status=="accepted name")]<-
          Spores_allSources$id[which((Spores_allSources$status=="accepted name"))]
        Spores_allSources$Col_ID_acc_names[which(Spores_allSources$status=="synonym")]<-
          Spores_allSources$acc_id[which((Spores_allSources$status=="synonym"))]
        Spores_allSources$Col_ID_acc_names[which(Spores_allSources$status=="ambiguous synonym")]<-
          Spores_allSources$acc_id[which((Spores_allSources$status=="ambiguous synonym"))]
        Spores_allSources$Col_ID_acc_names[which(Spores_allSources$status=="misapplied name")]<-
          Spores_allSources$acc_id[which((Spores_allSources$status=="misapplied name"))]
        Spores_allSources$Kingdom<-"Fungi"
        
        Spores_allSources<-Spores_allSources[c(20,19,1,9,10:13,4,6,2,3,21,5)]
        names(Spores_allSources)[3]<-"base_name"
        #names(Spores_allSources)[4]<-"Mycobank_base__id"
        names(Spores_allSources)[4]<-"Col_name_id"
        names(Spores_allSources)[5]<-"Col_name"
        names(Spores_allSources)[8]<-"source_acc_names"
        names(Spores_allSources)[10]<-"source_base_name"
        
        Spores_allSources<-Spores_allSources[order(Spores_allSources$Phylum,
                                                   Spores_allSources$Col_acc_names,
                                                   #Spores_allSources$Mycobank_base__id,
                                                   #Spores_allSources$Mycobank_base_name,
                                                   Spores_allSources$SporeName,
                                                   Spores_allSources$source_base_name),]
        
        names(Spores_allSources)[11]<-"Dim1"
        names(Spores_allSources)[12]<-"Dim2"
        Spores_allSources$description__id<-Spores_allSources$source_base_name
        
#5. Mycobank Dataset (as extracted and manually checked on August 2019)
Ascospores<-read.csv('output/ascospores_mycobank.csv',header = T, stringsAsFactors = F)
Ascospores_names<- strsplit(Ascospores$spec,"_")
Ascospores_names<- do.call("rbind",Ascospores_names)
Ascospores<-cbind(Ascospores,Ascospores_names)
names(Ascospores)[10]<-"base_name"
names(Ascospores)[11]<-"base__id"
names(Ascospores)[12]<-"description__id"
Ascospores$SporeName<-"Ascospores"
Ascospores$base_name<-as.character(Ascospores$base_name)
rm(Ascospores_names)

Basidiospores<-read.csv('output/basidiospores_mycobank.csv',header = T, stringsAsFactors = F)
Basidiospores_names<- strsplit(Basidiospores$spec,"_")
Basidiospores_names<- do.call("rbind",Basidiospores_names)
Basidiospores<-cbind(Basidiospores,Basidiospores_names)
names(Basidiospores)[9]<-"base_name"
names(Basidiospores)[10]<-"base__id"
names(Basidiospores)[11]<-"description__id"
Basidiospores$SporeName<-"Basidiospores"
Basidiospores$base_name<-as.character(Basidiospores$base_name)
rm(Basidiospores_names)

Conidia<-read.csv('output/conidia_mycobank.csv',header = T, stringsAsFactors = F)
Conidia_names<- strsplit(Conidia$spec,"_")
Conidia_names<- do.call("rbind",Conidia_names)
Conidia<-cbind(Conidia,Conidia_names)
names(Conidia)[11]<-"base_name"
names(Conidia)[12]<-"base__id"
names(Conidia)[13]<-"description__id"
Conidia$SporeName<-"Conidia"
Conidia$base_name<-as.character(Conidia$base_name)
rm(Conidia_names)

Zygospores<-read.csv('output/zygospores_mycobank.csv',header = T, stringsAsFactors = F)
Zygospores_names<- strsplit(Zygospores$spec,"_")
Zygospores_names<- do.call("rbind",Zygospores_names)
Zygospores<-cbind(Zygospores,Zygospores_names)
names(Zygospores)[8]<-"base_name"
names(Zygospores)[9]<-"base__id"
names(Zygospores)[10]<-"description__id"
Zygospores$SporeName<-NA
Zygospores$SporeName[grep("Zygospores",Zygospores$spore_type)]<-"Zygospores"
Zygospores$SporeName[grep("zygospores",Zygospores$spore_type)]<-"Zygospores"
Zygospores$SporeName[grep("Azygospores",Zygospores$spore_type)]<-"Azygospores"
Zygospores$SporeName[grep("azygospores",Zygospores$spore_type)]<-"Azygospores"
Zygospores$SporeName[grep("\\(azygospores",Zygospores$spore_type)]<-"Azygospores"
Zygospores$base_name<-as.character(Zygospores$base_name)
rm(Zygospores_names)

Sporangiospores<-read.csv('output/sporangiospores_mycobank.csv',header = T, stringsAsFactors = F)
Sporangiospores_names<- strsplit(Sporangiospores$spec,"_")
Sporangiospores_names<- do.call("rbind",Sporangiospores_names)
Sporangiospores<-cbind(Sporangiospores,Sporangiospores_names)
names(Sporangiospores)[9]<-"base_name"
names(Sporangiospores)[10]<-"base__id"
names(Sporangiospores)[11]<-"description__id"
Sporangiospores$SporeName<-"Sporangiospores"
Sporangiospores$base_name<-as.character(Sporangiospores$base_name)
rm(Sporangiospores_names)

Teliospores<-read.csv('output/Teliospores_mycobank.csv',header = T, stringsAsFactors = F)
Teliospores_names<- strsplit(Teliospores$spec,"_")
Teliospores_names<- do.call("rbind",Teliospores_names)
Teliospores<-cbind(Teliospores,Teliospores_names)
names(Teliospores)[8]<-"base_name"
names(Teliospores)[9]<-"base__id"
names(Teliospores)[10]<-"description__id"
Teliospores$SporeName<-"Teliospores"
Teliospores$base_name<-as.character(Teliospores$base_name)
rm(Teliospores_names)

Chlamydospores<-read.csv('output/Chlamydospores_mycobank.csv',header = T, stringsAsFactors = F)
Chlamydospores_names<- strsplit(Chlamydospores$spec,"_")
Chlamydospores_names<- do.call("rbind",Chlamydospores_names)
Chlamydospores<-cbind(Chlamydospores,Chlamydospores_names)
names(Chlamydospores)[8]<-"base_name"
names(Chlamydospores)[9]<-"base__id"
names(Chlamydospores)[10]<-"description__id"
Chlamydospores$SporeName<-"Chlamydospores"
Chlamydospores$base_name<-as.character(Chlamydospores$base_name)
rm(Chlamydospores_names)

Mycobank_SporeData<-
  rbind(
    Ascospores[,c(10:13,5,6)],
    Basidiospores[,c(9:12,5,6)],
    Conidia[,c(11:14,5,6)],
    Zygospores[,c(8:11,5,6)],
    Sporangiospores[,c(9:12,5,6)],
    Teliospores[,c(8:11,5,6)],
    Chlamydospores[,c(8:11,5,6)]
  )
Mycobank_SporeData$base__id<-as.character(Mycobank_SporeData$base__id)
Mycobank_SporeData$base__id<-as.numeric(Mycobank_SporeData$base__id)

Mycobank_Taxonomy <- read.csv('Mycobank_Taxonomy.csv', stringsAsFactors=F)#This dataframe was made on the "Checking_Taxonomy.R" code and 
Mycobank_SporeData<-left_join(Mycobank_SporeData,Mycobank_Taxonomy[c(1,12:29)],
                              by="base__id")

Mycobank_SporeData$base_name<-as.character(Mycobank_SporeData$base_name)

#Standardizing the accepted names
Mycobank_SporeData$Col_acc_names<-NA
Mycobank_SporeData$Col_acc_names[which(Mycobank_SporeData$status=="accepted name")]<-
  Mycobank_SporeData$name[which((Mycobank_SporeData$status=="accepted name"))]
Mycobank_SporeData$Col_acc_names[which(Mycobank_SporeData$status=="synonym")]<-
  Mycobank_SporeData$acc_name[which((Mycobank_SporeData$status=="synonym"))]
Mycobank_SporeData$Col_acc_names[which(Mycobank_SporeData$status=="ambiguous synonym")]<-
  Mycobank_SporeData$acc_name[which((Mycobank_SporeData$status=="ambiguous synonym"))]
Mycobank_SporeData$Col_acc_names[which(Mycobank_SporeData$status=="misapplied name")]<-
  Mycobank_SporeData$acc_name[which((Mycobank_SporeData$status=="misapplied name"))]

#Standardizing the accepted id
Mycobank_SporeData$Col_ID_acc_names<-NA
Mycobank_SporeData$Col_ID_acc_names[which(Mycobank_SporeData$status=="accepted name")]<-
  Mycobank_SporeData$id[which((Mycobank_SporeData$status=="accepted name"))]
Mycobank_SporeData$Col_ID_acc_names[which(Mycobank_SporeData$status=="synonym")]<-
  Mycobank_SporeData$acc_id[which((Mycobank_SporeData$status=="synonym"))]
Mycobank_SporeData$Col_ID_acc_names[which(Mycobank_SporeData$status=="ambiguous synonym")]<-
  Mycobank_SporeData$acc_id[which((Mycobank_SporeData$status=="ambiguous synonym"))]
Mycobank_SporeData$Col_ID_acc_names[which(Mycobank_SporeData$status=="misapplied name")]<-
  Mycobank_SporeData$acc_id[which((Mycobank_SporeData$status=="misapplied name"))]

Mycobank_SporeData<-Mycobank_SporeData[c(26,25,1,2,15:19,4,3,5:13)]
names(Mycobank_SporeData)[3]<-"base_name"
names(Mycobank_SporeData)[4]<-"Mycobank_base__id"
names(Mycobank_SporeData)[5]<-"Col_name_id"
names(Mycobank_SporeData)[6]<-"Col_name"
names(Mycobank_SporeData)[9]<-"source_acc_names"

Mycobank_SporeData$Phylum<-trimws(Mycobank_SporeData$Phylum)
Mycobank_SporeData$Phylum<-as.character(Mycobank_SporeData$Phylum)
Mycobank_SporeData$description__id<-as.character(Mycobank_SporeData$description__id)

Mycobank_SporeData<-Mycobank_SporeData[order(Mycobank_SporeData$Phylum,
                                             Mycobank_SporeData$Col_acc_names,
                                             Mycobank_SporeData$Mycobank_base__id,
                                             Mycobank_SporeData$base_name,
                                             Mycobank_SporeData$SporeName,
                                             Mycobank_SporeData$description__id),]

#For the moment I am deleting some entries
Mycobank_SporeData$source_base_name<-"Mycobank"
Mycobank_SporeData$Subkingdom<-NULL
Mycobank_SporeData$Uncertain_classification<-NULL
Mycobank_SporeData$Subphylum<-NULL
Mycobank_SporeData$Class<-NULL
Mycobank_SporeData$Order<-NULL

Mycobank_SporeData$Dim1<-as.numeric(Mycobank_SporeData$Dim1)
Mycobank_SporeData$Dim2<-as.numeric(Mycobank_SporeData$Dim2)

#Checking variation in spore


#Merging all the data
Spores_allSources<-bind_rows(Mycobank_SporeData,Spores_allSources)
Spores_allSources<-Spores_allSources[order(Spores_allSources$Phylum,
                                           Spores_allSources$Col_acc_names,
                                           #Spores_allSources$Mycobank_base__id,
                                           #Spores_allSources$base_name,
                                           Spores_allSources$SporeName,
                                           Spores_allSources$description__id),]

#Assuming that spore can be either ellipses or circles, I am duplicating dim1 when dim2 is NA
Spores_allSources$Dim2[which(is.na(Spores_allSources$Dim2))]<-
  Spores_allSources$Dim1[which(is.na(Spores_allSources$Dim2))]

#Getting length>width
#Getting spore widht
Spores_allSources$spore_width<-NA
Spores_allSources$spore_width[which(
  Spores_allSources$Dim2<=Spores_allSources$Dim1)]<-Spores_allSources$Dim2[which(
    Spores_allSources$Dim2<=Spores_allSources$Dim1)]
#or
Spores_allSources$spore_width[which(
  Spores_allSources$Dim1<Spores_allSources$Dim2)]<-Spores_allSources$Dim1[which(
    Spores_allSources$Dim1<Spores_allSources$Dim2)]
#Getting spore length
Spores_allSources$spore_length<-NA
Spores_allSources$spore_length[which(
  Spores_allSources$Dim2>=Spores_allSources$Dim1)]<-Spores_allSources$Dim2[which(
    Spores_allSources$Dim2>=Spores_allSources$Dim1)]
#or
Spores_allSources$spore_length[which(
  Spores_allSources$Dim1>Spores_allSources$Dim2)]<-Spores_allSources$Dim1[which(
    Spores_allSources$Dim1>Spores_allSources$Dim2)]

#Removing cases where Dim1 has NA (this needs to be checked early on)
Spores_allSources<-Spores_allSources[-which(is.na(Spores_allSources$Dim1)),]

all(Spores_allSources$spore_width<=Spores_allSources$spore_length)#This is TRUE!!!
# 
# IDs<-unique(Spores_allSources$Col_ID_acc_names)
# IDs<-IDs[-which(is.na(IDs))]
# 
# t1<-as.colid(IDs)#This takes 40 min
# #t2<-as.colid(unique(Spores_allSources$Col_ID_acc_names)[30001:60000])#This takes 40 min
# #classification(unique(All_names$nombres_id)[1:10], db = 'col')#it takes 40 minutes
# t2<-classification(t1, db = 'col')#it takes 40 minutes
# #t2<-t2[-which(is.na(t2))]
# t3<-cbind(t2)
# names(t3)[19]<-"Col_ID_acc_names"
# saveRDS(IDs,"output/IDs.RDS")
# saveRDS(t1,"output/t1.RDS")
# saveRDS(t2,"output/t2.RDS")
# saveRDS(t3,"output/COL_Taxonomy.RDS")

COL_Taxonomy<-readRDS("output/COL_Taxonomy.RDS")

names(Spores_allSources)[15]<-"Phylum_baseNameSource"
names(Spores_allSources)[14]<-"Kingdom_baseNameSource"

Spores_allSources<-left_join(Spores_allSources,COL_Taxonomy[,c(1:7,15,19)],
                             by="Col_ID_acc_names")
Spores_allSources$Taxonomy<-NA
Spores_allSources$Taxonomy[
  -which(is.na(Spores_allSources$Col_ID_acc_names))]<-"Catalogue_of_Life"

Spores_allSources$Taxonomy[
  which(is.na(Spores_allSources$Col_ID_acc_names))]<-"Mycobank"

Spores_allSources$kingdom[
  which(is.na(Spores_allSources$Col_ID_acc_names))]<-
  Spores_allSources$Kingdom_baseNameSource[
    which(is.na(Spores_allSources$Col_ID_acc_names))]

Spores_allSources$phylum[
  which(is.na(Spores_allSources$Col_ID_acc_names))]<-
  Spores_allSources$Phylum_baseNameSource[
    which(is.na(Spores_allSources$Col_ID_acc_names))]

Spores_allSources$Kingdom_baseNameSource<-NULL
Spores_allSources$Phylum_baseNameSource<-NULL

Spores_allSources<-Spores_allSources[order(Spores_allSources$phylum,
                                           Spores_allSources$Col_acc_names,
                                           #Spores_allSources$Mycobank_base__id,
                                           #Spores_allSources$base_name,
                                           Spores_allSources$SporeName,
                                           Spores_allSources$description__id),]


### write to file
write.csv(Spores_allSources, 'output/Spores_All_Sources.csv', row.names=F)

#Checking variation among descriptions of the same structure and the same name

split(Ascospores[1:100,],Ascospores$spec[1:100])


# COMPARING DATASETS
#1. Mycobank vs Purhonnen
library(VennDiagram)
venn.diagram(list(
  Purhonen_data=unique(Purhonen_species$species),
  Mycobank_Ascos=unique(Ascospores$base_name),
  Mycobank_Basidios=unique(Basidiospores$base_name)),
  height = 3480 , 
  width =3480 ,
  cat.pos = c(-23, 23,180),
  cex=1.5,
  cat.cex= 1.5,
  lty = 'blank',
  main = "Overlap of sps with Basidio_Ascosspores from Jenna Purhonnen",
  main.cex = 2,
  main.pos = c(0.5,0.8),
  margin=0.3,
  filename = "BasidiosAscos_spores_overlap_Purhonnen_AllFungi.png",
  fill=c("red","green","blue"))

#2.Venn diagram showing overlap between the basidiomycetes with basidiospores
#Bassler etal 2015 and the ones Franz extracted from Mycobank
venn.diagram(list(
  Bassler_etal_15=unique(Spores_allSources$Species_names[Spores_allSources$Source=="Bassler_etal_2015"]),
  Mycobank=unique(Basidiospores$base_name)),
  height = 3480 , 
  width = 3480 ,
  cat.pos = c(-23, 23),
  cex=1.5,
  cat.cex= 1.5,
  lty = 'blank',
  main = "Overlap of sps with Basidiospores",
  main.cex = 2,
  main.pos = c(0.5,0.8),
  margin=0.3,
  filename = "Basidiospores_overlap_Mycobank_Bassler.png",
  fill=c("red","blue"))

#3. Compendium of soil fungi (Ascospores)
venn.diagram(list(
  Compendium=unique(CompendiumData$Name[CompendiumData$SporeName=="ascospores"]),
  Mycobank_Ascos=unique(Ascospores$base_name)
  #Mycobank_Basidios=unique(Basidiospores$base_name),
  #Mycobank_Conidia=unique(Conidia$base_name)
  #Mycobank_Zygos=unique(Zygospores$base_name)
  ),
  height = 3480 , 
  width = 3480 ,
  #cat.pos = c(-23, 23),
  cex=1.5,
  cat.cex= 1.5,
  lty = 'blank',
  main = "Overlap of sps with Ascospores",
  main.cex = 2,
  main.pos = c(0.5,0.8),
  margin=0.3,
  filename = "Ascospores_overlap_Mycobank_Compendium.png",
  fill=c("red","blue"))

#4.Venn diagram showing overlap between the ascomycetes with conidia in the
#Compendium of Soil Fungi and the ones Franz extracted from Mycobank
venn.diagram(list(
  Compendium=unique(CompendiumData$Name[grep("onidia",CompendiumData$SporeName)]),
  Mycobank_Conidia=unique(Conidia$base_name)),
  height = 3480 , 
  width = 3480 ,
  cat.pos = c(-23, 23),
  cex=1.5,
  cat.cex= 1.5,
  lty = 'blank',
  main = "Overlap of sps with Conidia",
  main.cex = 2,
  main.pos = c(0.5,0.8),
  margin=0.3,
  filename = "Conidia_overlap_Mycobank_Compendium.png",
  fill=c("red","blue"))

#5.Venn diagram showing overlap between Glomeromycota from my paper and Mycobank
venn.diagram(list(
  Aguilar=unique(Spores_allSources$Species_names[which(Spores_allSources$Source=="Aguilar_etal_2019")]),
  Mycobank_AMF=unique(Zygospores$base_name[which(Zygospores$SporeName=="Azygospores")])),
  height = 3480 , 
  width = 3480 ,
  cat.pos = c(-23, 23),
  cex=1.5,
  cat.cex= 1.5,
  lty = 'blank',
  main = "Overlap of sps with Conidia",
  main.cex = 2,
  main.pos = c(0.5,0.8),
  margin=0.3,
  filename = "AMF_overlap_Mycobank_Aguilar.png",
  fill=c("red","blue"))
        
#####################################################################################################################
#### OLD DATABASE BASED ON DATASET EXTRACTED BY FRANZ KRAH: THIS DATASET WAS REPLACED ON AUGUST 2019  ###############        

        
# #4. Spore dimensions extracted by Franz Krah on January 2019 from mycobank descriptions. This data is
# #contained in the object: FranzKrah_sporeValues. This object is based on a csv he sent me to my email on 
# #the 21st of January 2019. He said that he manually checked each entry after building it up in R in his computer.
# #I imported this csv, created the object FranzKrah_sporeValues, checked it and made slight changes on it.
# #All this is reported in the script CheckSporeData_FromFranzExtc_on_Jan19.R located in this environment. In that
# #script I also check how much overlap in sps there are between Spores_allSources and Franz_Krah_sporeValues, and the correlation of spore values 
# #between the shared sps. I found two things: 1. Low overlap between the datasets. For basidiomycetes we have 
# #64 sps in common between both datasets; for Ascos only 14 sps overlap, for conidia 52 sps. 
# #2. There is Good correlation between values of shared species! It is actually an almost 1:1 relationship in 
# #the case of basidiospores and ascospores. For conidia is slightly more variable, however I realized that 
# #a lot of the differences are because several species produce more than one size class of conidia (usually depending on septation).
# #As for zygos or sps with chlamydospores the overlap is essentially 0. For more details one has to look at that script.
#         
#         
#         #Based on the low overlap between dataset but good correlation in spore size among shared sps
#         #I decided to keep the sps in Spores_allSources over the ones in Franz data. My logic is that Spores_allSources reflects 
#         #better curated data than Franz. The compendium data itself reflects an authority editing sps names
#         #and entries plus the data was entered manually. I am assumign that the data of Bassler et al 15 was also
#         #better curated.
#         
#         
#         trial<-rbind(
#           Spores_allSources%>%
#             mutate(measure_orig=NA),
#           FranzKrah_sporeValues[which(!FranzKrah_sporeValues$Species_names%in%Spores_allSources$Species_names),]%>%
#           rename(SporeName=spore_type)%>%
#           rename(Phylum=phylum)%>%
#           rename(SporeArea=spore_surf)%>%
#           rename(spore_length=length_mean)%>%
#           rename(spore_width=width_mean)%>%
#           mutate(Source="Mycobank extraction (Jan19_FKrah")%>%
#           mutate(Specific_sporeName=NA)%>%
#           select(Species_names,spore_length,spore_width,SporeArea,SporeName,Phylum,Source,Specific_sporeName,measure_orig)
#         )
#         
#                 #The "wrong" entries are:"Basidiospore"   "Ascospore"      "Conidia" and "Chlamydospore"
#         #They should be  "conidia"        "ascospores" "Basidiospores"  "chlamydospores"
#         trial$SporeName[trial$SporeName=="Basidiospore"]<-"Basidiospores"
#         trial$SporeName[trial$SporeName=="Ascospore"]<-"ascospores"
#         trial$SporeName[trial$SporeName=="Conidia"]<-"conidia"
#         trial$SporeName[trial$SporeName=="Chlamydospore"]<-"chlamydospores"
#         
# #Finally putting all data together:
#         
#         
#         Spores_allSources<-trial;rm(trial)
#         Spores_allSources$Phylum[Spores_allSources$Phylum=="Mucoromycota"]<-"Mucoromycotina"
#         Spores_allSources$Phylum<-droplevels(Spores_allSources$Phylum)
#         Spores_allSources$Phylum<-as.character(Spores_allSources$Phylum)#I added this on 27th March 2019
#         
#         Spores_allSources%>%
#           filter(Phylum=="Basidiomycota")%>%
#           filter(SporeName=="conidia")
#         
#         #There are 192 sps of basidiomycota with conidia, I check the followings:
#         #Tomentella tulasnelloidea is correct but has actually two entries
#         #Auricularia pulvurenta (anamorph=Postia ptychogaster): this entry is correct
#         #Tremella moriformis  this entry is correct (although there are two descriptions)
#         #of conidia in this case
#         #Haplotrichum ramosissimum this entry is correct (anamorph=Oidium ramosissimum)
#         #One would need to double check all these cases to see how many are correct
#         #and which ones are not
#         
#         #I found that the entry for Pseudocercospora clematidis is unsually large (for
#         #spore lenght). I checked in Mycobank and the entry says actulla 40100um. However
#         #I think it is almost impossible to have a conida of 4cm. The same for
#         #Mycovellosiella clerodendri and its synonym (Passalora clerodendri), I found in the reference
#         #New species of Mycovellosiella associated with foliar spots in Nepal, Mycol. Res. 100 (6): 689-692 (1996)
#         #the spore range is actually 10-160 x 2.5-4 um. That means that the text in mycobank is wrong
#         #Similar situation for Rimaconus jamaicensis (check Huhndorf, S. M., Fernández, F. A., Taylor, J. E., & Hyde, K. D. (2001). Two pantropical ascomycetes: Chaetosphaeria cylindrospora sp. nov. and Rimaconus, a new genus for Lasiosphaeria jamaicensis. Mycologia, 1072-1080.)
#         #The same for Cercospora allophyli (second description reveals the typo), the same for Pseudocercospora macarangae
#         #Melanomphalia thermophila there is typo in the entry of the code,
#         #the same for Alternaria hordeiaustralica, Helicobasidium longisporum 
#         
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Pseudocercospora clematidis"]<-(40+100)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Mycovellosiella clerodendri"]<-(10+160)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Passalora clerodendri"]<-(10+160)/2
#         
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Rimaconus jamaicensis"]<-(55+73)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Melanomphalia thermophila"]<-(7+10.7)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Cercospora allophyli"]<-(45+80)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Alternaria hordeiaustralica"]<-(35+55)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Helicobasidium longisporum"]<-(16+21)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Pseudocercospora macarangae"]<-(20+75)/2
#         
#         #Simmilar problems in these ones (either the code made an error or the mycobank descripton
#         #has a typo happens in the following ones):
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Pseudocercosporella bambusae"]<-(20+70)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Alternaria citricancri"]<-(20+35)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Polyporus unilaterus"]<-(15+17)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Sebacina dubia"]<-(4+8.5)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Ascochyta lobikii"]<-(13+15)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Rhabdospora viciae"]<-(12+17)/2#Mycobank description (tranlated from russian) states 1217
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Ascochyta cytisi"]<-(9+15)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Ascochyta tarda"]<-(9+14)/2
#         Spores_allSources$spore_length[21127]<-(8+13)/2#For Subramaniula obscura, chlamydospore
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Ascochyta greenei"]<-(8+10)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Ascochyta solidaginis"]<-(8+10)/2
#         Spores_allSources[9290,c(2,3,5)]<-c(NA,NA,NA)#There are no ascospores for Penicillium manginii
#         Spores_allSources[16510,c(2,3,5)]<-c(NA,NA,NA)#There are no condia for Lophodermium jiangnanense
#         
#         #It could be that all entries from Ascochyta are wrong, however I could not be certain
#         #if this is the case for all Ascochyta´s above 500um remaining as spore length. I am changing them
#         #but one would need to double check:
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Ascochyta valerianae"]<-(7+12)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Ascochyta boni-henrici"]<-(6+12)/2
#         Spores_allSources$spore_length[Spores_allSources$Species_names=="Ascochyta zonata"]<-(6+10)/2
#         
#         
#         
#         #After checking a couple of times, I will also remove all instances of Penicillium sps having
#         #ascospores higher 100um lenght
#         Spores_allSources[which(grepl("Penicillium",Spores_allSources$Species_names)&Spores_allSources$spore_length>100),]
#         Spores_allSources[which(grepl("Penicillium",Spores_allSources$Species_names)&Spores_allSources$spore_length>100),3]<-c(rep(NA,6))
#         Spores_allSources[which(grepl("Penicillium",Spores_allSources$Species_names)&Spores_allSources$spore_length>100),5]<-c(rep(NA,6))
#         Spores_allSources[which(grepl("Penicillium",Spores_allSources$Species_names)&Spores_allSources$spore_length>100),2]<-c(rep(NA,6))
#         
#         #Similar problems with spore width:
#         #For Phaeodimeriella ctenotricha (and the other that seems like a typo), there is again a mistake. Check: Revision of genera in Perisporiopsidaceae
#         #and Pseudoperisporiaceae and other Ascomycota genera incertae sedis Mycosphere 8(10): 1695–1801 (2017
#         Spores_allSources$spore_width[Spores_allSources$Species_names=="Phaeodimeriella cetotricha"]<-(12+15)/2
#         Spores_allSources$spore_width[Spores_allSources$Species_names=="Phaeodimeriella ctenotricha"]<-(12+15)/2
#         Spores_allSources$spore_width[Spores_allSources$Species_names=="Rhinotrichum pulchrum"]<-(11+12.5)/2#Clearly there is a missing dash in the description on the website
#         Spores_allSources$spore_width[Spores_allSources$Species_names=="Alternaria herbiphorbicola"]<-(8+11)/2
#         Spores_allSources$spore_width[Spores_allSources$Species_names=="Ascochyta sesleriae"]<-(8+10)/2
#         
#         #I could not check Microsphaera miurae and Erysiphe miurae but I will also modify them
#         Spores_allSources$spore_width[Spores_allSources$Species_names=="Microsphaera miurae"]<-(11+15)/2#Clearly there is a missing dash in the description on the website
#         Spores_allSources$spore_width[Spores_allSources$Species_names=="Erysiphe miurae"]<-(11+15)/2#Clearly there is a missing dash in the description on the website
#         
#         
#         #Updating the SporeArea entry:
#         Spores_allSources$SporeArea<-Spores_allSources$spore_length*Spores_allSources$spore_width*
#           (pi/4)
#         
#         #Further checks
#         Spores_allSources%>%
#           filter(!is.na(spore_length))%>%
#           #filter(Phylum!="Glomeromycota")%>%
#           #filter(grepl("Penicillium",Species_names))%>%
#           #arrange(desc(spore_length))
#           arrange(desc(SporeArea))
#           #arrange(desc(spore_width))%>%
#           head
#         
#         summary(Spores_allSources$spore_length)
#         
#         hist(Spores_allSources$spore_length)
#         boxplot(Spores_allSources$spore_length)
#         boxplot(Spores_allSources$SporeArea)
#         
#         
#         FranzKrah_sporeValues%>%
#           arrange(desc())
#         #I am still not conviced that the all fungi (not AMF) that are in the top 20 are all good entries
#         #one would need to double check these cases
#         
#         #Also, words that indicate that the entry is wrong: absent, not observed
#         
#         Spores_allSources[which(Spores_allSources$spore_length==max(Spores_allSources$spore_length,na.rm = T)),]
#         
# #Storing the data
# #On March 7th 2019 I save a version of All fungi that includes all 21455 entries from 
# #Franz_Krah_sporeValues. I did that because I need to send a version to Moises 
# #for getting the matches in FunGuild since he has a code for that. I think that for
# #that purpose it makes sense to use all names there since I have no 
# 
#         
# #Changes in the dataset after March 7th
# #On March 12th I decided to add genera to the dataset in order to assing 
# t<-strsplit(Spores_allSources$Species_names," ")
# Spores_allSources$Genus<-sapply(t,function(x)x[[1]]);rm(t)
# 
# #The fungi with teliospores belonging to the Ascomycota are weird. This should be checked!:        
# Spores_allSources[Spores_allSources$SporeName=="Teliospore"&
#            Spores_allSources$Phylum=="Ascomycota",]#They are Bulgaria pura,Torula carbonaria,Gloeosporium succineum, Peziza pura
#         
# 
# #Adding the codes 
# 
# 
# write.csv(Spores_allSources,"Spores_allSources.csv",row.names = F)
# saveRDS(Spores_allSources, file = "SporeData_Carlos.rds")
# 
# 
# 
# 
# 
# 
# 
# ###
# ##Random stuff
# ###############################################################################
# #Can u run:
#   
# RCurl ::url.exists("r-project.org")
# RCurl::getURL("http://nt.ars-grin.gov/fungaldatabases/index.cfm")
# 
# RCurl::getURL("https://www.google.com")
# 
# xml2::read_html("https://nt.ars-grin.gov/fungaldatabases/index.cfm")
# 
# pack <- available.packages()
# pack[grep("rusda", rownames(pack)), "Imports"]
# library(XML)
# library(httr)
# library(plyr)
# library(foreach)
# library(stringr)
# library(taxize)
# 
# #Type
# fix(associations) 
# # Outcomment or delete
# # "if (!is.character(getURL("https://nt.ars-grin.gov/fungaldatabases/index.cfm"))) 
# # stop(" Database is not available : http://nt.ars-grin.gov/fungaldatabases/index.cfm”)"
# 
# 
# ##Old import of AMF data. I stopped using this one because it is better to use the one
# #used in the ISME paper for AMF size.
# 
# #AMF_All_Copy<-read.csv("C:\\Users\\Carlos\\Documents\\Professional\\Spore CommunityAnalysis\\SporeSize\\AMF_Spore_Database_Volume.csv",
# #header = T, stringsAsFactors = F)
# 
# #UDATING THE SPORE AREA COLUMN given the previous updates in data entry
# # AMF_All_Copy$SporeArea<-
# #   ((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/4)*
# #   ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/4)*
# #   pi
# # 
# # #Caluculating Volume 
# # #For this it is assumed that fungal spores are prolate spheroids, which means
# # #two things. First, that the smallest diameter will be consider the "equatorial axis" and the largest
# # #is the "polar axis"; second, that the there are two equatorial axis of the same length. This is 
# # #important because the formula to calculate volume make distinction between the two type of axis.
# # #In this way, AMF spore can be only of two types, perfect spheres (in which case equatorial and 
# # #polar axis are of the same lenght), corrsponding to "globose" spores. And, spheroids similar to an
# # #american football.  A morphology like a "flying soucer" is not possible following this rules.
# # 
# # 
# # AMF_All_Copy$EquatorialAxis<-NaN
# # AMF_All_Copy$EquatorialAxis[
# #   which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
# #           ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
# #   ((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)[
# #     which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
# #             ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
# # AMF_All_Copy$EquatorialAxis[which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
# #                                     ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
# #   ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
# #     which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
# #             ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
# # AMF_All_Copy$EquatorialAxis[which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
# #                                     ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
# #   ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
# #     which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
# #             ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
# # 
# # AMF_All_Copy$PolarAxis<-NaN
# # AMF_All_Copy$PolarAxis[
# #   which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
# #           ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
# #   ((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)[
# #     which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
# #             ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
# # AMF_All_Copy$PolarAxis[
# #   which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
# #           ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
# #   ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
# #     which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
# #             ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
# # AMF_All_Copy$PolarAxis[
# #   which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
# #           ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
# #   ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
# #     which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
# #             ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
# # 
# # which(AMF_All_Copy$EquatorialAxis>AMF_All_Copy$PolarAxis)#This should always be zero (0)
# # 
# # #Now calculating volume
# # AMF_All_Copy$SporeVolume<-(pi*(AMF_All_Copy$EquatorialAxis^2)*(AMF_All_Copy$PolarAxis))/6
# #Adding the phylum
# #AMF_All_Copy$Phylum<-"Glomeromycota"
# 
# 
# #The bit to rbind:
# # data.frame(Species_names=AMF_All_Copy[,1],
# #            spore_length=AMF_All_Copy[,34],
# #            spore_width=AMF_All_Copy[,33],
# #            SporeArea=AMF_All_Copy[,15],
# #            SporeName="Azygospores",
# #            Phylum=AMF_All_Copy[,36],
# #            Source="Aguilar_etal_2018"),