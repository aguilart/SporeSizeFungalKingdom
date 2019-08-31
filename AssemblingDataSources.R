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



