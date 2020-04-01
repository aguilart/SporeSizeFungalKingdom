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
#Haiyang. This is composed of the datasets extracted for Ascospores,
#Conidia, Basidiospores, Chlaymdospores, (A)Zygospores and Teliospores

#5. Finally taxonomic data comes (moslty) from the Catalogue of Life and 
#from mycobank itself (when a given name is not present in the Catalogue of
#Life)

rm(list=ls())

library(tidyverse)

#1. Fun to Fun Data, Bassler data
Bassler_Data<-read.csv('output/Basidiospores_Bassler2015.csv', header = T,
                       stringsAsFactors = F)
Bassler_Data$Species_names<-gsub("\\?","Volvariella",Bassler_Data$Species_names)

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
                       #spore_type="Azygospores",
                       Phylum=AMF_All_Copy[,22],
                       Source="Aguilar_etal_2019"),
            data.frame(Species_names=CompendiumData[,4],
                       spore_length=CompendiumData[,29],
                       spore_width=CompendiumData[,28],
                      #SporeArea=CompendiumData[,17],
                      SporeName=CompendiumData[,7],
                      #spore_type=CompendiumData[,7],
                      Phylum=CompendiumData[,27],
                      Source="CompendiumSoilFungi"),
            data.frame(Species_names=Purhonen_species[,1],
                       spore_length=Purhonen_species[,2],
                       spore_width=Purhonen_species[,3],
                       #SporeArea=Purhonen_species[,17],
                       SporeName=Purhonen_species[,13],
                       #spore_type=Purhonen_species[,13],
                       Phylum=Purhonen_species[,12],
                       Source="Jenna Purhonen"),
            data.frame(Species_names=Bassler_Data[,1],
                      spore_length=Bassler_Data[,2],
                      spore_width=Bassler_Data[,3],
                      SporeName="Basidiospores",
                      #spore_type="Basidiospores",
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
        
        #Spores_allSources<-Spores_allSources[-grep("\\?",Spores_allSources$Species_names),]#I discarded 4 names that have a question mark
        
        #Adding taxonomic data drom the Catalogue of Life (CoL)
        # library(taxize)
        # Col_IDs<-get_colid_(Spores_allSources$Species_names)
        # Col_IDs<-do.call("rbind",Col_IDs)
        # Col_IDs$Species_names<-rownames(Col_IDs)
        # Col_IDs$Species_names<-
        #   sub("\\.[0-9]+","",Col_IDs$Species_names)
        # 
        # Spores_allSources<-left_join(Spores_allSources,Col_IDs,
        #                              by="Species_names")
        # 
        # #Standardizing the accepted names
        # Spores_allSources$Col_acc_names<-NA
        # Spores_allSources$Col_acc_names[which(Spores_allSources$status=="accepted name")]<-
        #   Spores_allSources$name[which((Spores_allSources$status=="accepted name"))]
        # Spores_allSources$Col_acc_names[which(Spores_allSources$status=="synonym")]<-
        #   Spores_allSources$acc_name[which((Spores_allSources$status=="synonym"))]
        # Spores_allSources$Col_acc_names[which(Spores_allSources$status=="ambiguous synonym")]<-
        #   Spores_allSources$acc_name[which((Spores_allSources$status=="ambiguous synonym"))]
        # Spores_allSources$Col_acc_names[which(Spores_allSources$status=="misapplied name")]<-
        #   Spores_allSources$acc_name[which((Spores_allSources$status=="misapplied name"))]
        # 
        # #Standardizing the accepted id
        # Spores_allSources$Col_ID_acc_names<-NA
        # Spores_allSources$Col_ID_acc_names[which(Spores_allSources$status=="accepted name")]<-
        #   Spores_allSources$id[which((Spores_allSources$status=="accepted name"))]
        # Spores_allSources$Col_ID_acc_names[which(Spores_allSources$status=="synonym")]<-
        #   Spores_allSources$acc_id[which((Spores_allSources$status=="synonym"))]
        # Spores_allSources$Col_ID_acc_names[which(Spores_allSources$status=="ambiguous synonym")]<-
        #   Spores_allSources$acc_id[which((Spores_allSources$status=="ambiguous synonym"))]
        # Spores_allSources$Col_ID_acc_names[which(Spores_allSources$status=="misapplied name")]<-
        #   Spores_allSources$acc_id[which((Spores_allSources$status=="misapplied name"))]
        # Spores_allSources$Kingdom<-"Fungi"
        # 
        # Spores_allSources<-Spores_allSources[c(20,19,1,9,10:13,4,6,2,3,21,5)]
        # names(Spores_allSources)[3]<-"base_name"
        # #names(Spores_allSources)[4]<-"Mycobank_base__id"
        # names(Spores_allSources)[4]<-"Col_name_id"
        # names(Spores_allSources)[5]<-"Col_name"
        # names(Spores_allSources)[8]<-"source_acc_names"
        # names(Spores_allSources)[10]<-"source_base_name"
        
        #
        
        rm(AMF_All_Copy,Bassler_Data,CompendiumData,Purhonen_species)
        
        #
        # temporal<-
        #   AllFungi_Oct19[which(AllFungiOct19$source_base_name!="Mycobank"),
        #                  c("Col_ID_acc_names",  "Col_acc_names",
        #                    "Col_name_id","Col_name","rank","status",
        #                    "source_acc_names","base_name","source_base_name")]
        # unique(temporal$source_base_name)
        # temporal$source_base_name<-NULL
        # names(temporal)[8]<-"Species_names"
        # 
        # Col_IDs<-temporal
        #write.csv(Col_IDs,"output/Col_IDs_AllButMycobank.csv",row.names = F)
        Col_IDs<-read.csv('output/Col_IDs_AllButMycobank.csv',header = T, stringsAsFactors = F)
        #I made this file at the end of Nov 19 (while in Australia). It turns out that the Catalogue of Life
        #now restrict the amount of data that can be downloaded at the time. Thus, I subset the catalogue of Life
        #ID´s from the dataset that I had created on October 19 (output/Spores_All_Sources_Oct19.csv). In theory,
        #they should be the same. At some moment I should try to do again the get_colid_ function
        
        Spores_allSources<-left_join(Spores_allSources,Col_IDs,
                                     by="Species_names")
        
        names(Spores_allSources)[1]<-"base_name"
        names(Spores_allSources)[6]<-"source_base_name"
        Spores_allSources$Kingdom<-"Fungi"
        
        #Spores_allSources<-Spores_allSources[c(20,19,1,9,10:13,4,6,2,3,21,5)]
        Spores_allSources<-Spores_allSources[c(9,10,1,11,12:15,4,6,2,3,16,5,7)]
        
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

# Mycobank_SporeData<-
#   rbind(
#     Ascospores[,c(10:13,5,6)],
#     Basidiospores[,c(9:12,5,6)],
#     Conidia[,c(11:14,5,6)],
#     Zygospores[,c(8:11,5,6)],
#     Sporangiospores[,c(9:12,5,6)],
#     Teliospores[,c(8:11,5,6)],
#     Chlamydospores[,c(8:11,5,6)]
#   )

Mycobank_SporeData<-
  rbind(
    Ascospores[,c(10:13,5,6,3)],
    Basidiospores[,c(9:12,5,6,3)],
    Conidia[,c(11:14,5,6,3)],
    Zygospores[,c(8:11,5,6,3)],
    Sporangiospores[,c(9:12,5,6,3)],
    Teliospores[,c(8:11,5,6,3)],
    Chlamydospores[,c(8:11,5,6,3)]
  )
names(Mycobank_SporeData)[7]<-"Specific_sporeName"
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

Mycobank_SporeData<-Mycobank_SporeData[c(1:6,8:27,7)]#I executed until here
Mycobank_SporeData<-Mycobank_SporeData[c(26,25,1,2,15:19,4,3,5:13,27)]
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

# Adding higher order taxonomy from the Catalogue of Life

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
#

Spores_allSources<-Spores_allSources[,c(1:13,15:26,14)]

#Fixing some wrong entries

Spores_allSources[which(Spores_allSources$phylum=="Mollusca"),c(1,2,9,10,17:25)]<-
  Spores_allSources[which(Spores_allSources$Col_acc_names=="Pseudocolus fusiformis"),c(1,2,9,10,17:25)]



Spores_allSources[which(Spores_allSources$phylum=="Foraminifera"),c(1,2,5,6,9,17:25)]<-c(NA,NA,NA,NA,NA,"Fungi","Ascomycota","Lecanoromycetes",
                                                                                         "Ostropales","Graphidaceae","Fissurina",
                                                                                         "Fissurina radiata",NA,"Mycobank")

Spores_allSources[which(Spores_allSources$base_name=="Graphis japonica"),c(1,2,5,6,9,17:25)]<-c(NA,NA,NA,NA,NA,"Fungi","Ascomycota","Lecanoromycetes",
                                                                                                "Ostropales","Graphidaceae","Graphis",
                                                                                                "Graphis japonica",NA,"Mycobank")

#Removing cases where the virus that affect a fungus is also reported
#Spores_allSources<-Spores_allSources[-which(Spores_allSources$kingdom=="Viruses"),]



length(which(is.na(Spores_allSources$Col_acc_names)))
length(which(!is.na(Spores_allSources$Col_acc_names)))

unique(Spores_allSources[which(Spores_allSources$phylum=="Zygomycota"),]$class)
unique(Spores_allSources[which(Spores_allSources$phylum_=="Zygomycota"),]$order)
#The lower fungi: Catalogue of Life still uses Zygomycota for the following
#groups: Mucoromycetes,Zoopagomycetes,Entomophthoromycetes,Mortierellomycetes.
#For the moment, my plan is to place them individual phylum. Here I am following
#the classification found in Wijayawardene, N. N., et al. (2018).

Spores_allSources$phylum_<-Spores_allSources$phylum
Spores_allSources$phylum_[which(Spores_allSources$class=="Mucoromycetes")]<-"Mucoromycota"
Spores_allSources$phylum_[which(Spores_allSources$order=="Endogonales")]<-"Mucoromycota"
Spores_allSources$phylum_[which(Spores_allSources$order=="Umbelopsidales")]<-"Mucoromycota"

Spores_allSources$phylum_[which(Spores_allSources$order=="Zoopagales")]<-"Zoopagomycota"

Spores_allSources$phylum_[which(Spores_allSources$class=="Entomophthoromycetes")]<-"Entomophthoromycota"
Spores_allSources$phylum_[which(Spores_allSources$class=="Mortierellomycetes")]<-"Mortierellomycota"

Spores_allSources$phylum_[which(Spores_allSources$order=="Kickxellales")]<-"Kickxellomycota"
Spores_allSources$phylum_[which(Spores_allSources$order=="Harpellales")]<-"Kickxellomycota"
Spores_allSources$phylum_[which(Spores_allSources$order=="Dimargaritales")]<-"Kickxellomycota"

Spores_allSources$phylum_[which(Spores_allSources$order=="Basidiobolales")]<-"Basidiobolomycota"

### write to file
#write.csv(Spores_allSources, 'output/Spores_All_Sources_Oct19.csv', row.names=F)
#In theory the october version is only different
#in not having the column "SpecificSporeName". But for some reason it has less entries
write.csv(Spores_allSources, 'output/Spores_All_Sources_Nov19.csv', row.names=F)



#########################################
### Fixing issues with names, taxonmy ###
###            and typos              ###
#########################################

##On Nov 2019, I realized some issues with the data. Here I am fixing them.

#Loading spore dataset (as in created in AssemblingDataSourcesR):

AllFungi<-read.csv('output/Spores_All_Sources_Nov19.csv',header = T, stringsAsFactors = F)

#It turns out that several species were not found in the catalogue of Life
head(AllFungi[which(is.na(AllFungi$Col_acc_names)),])

#So, for the moment my solution is to create a new column with the "names to use" based on 
#mix of col_acc_names and base name

AllFungi$names_to_use<-AllFungi$Col_acc_names
AllFungi[which(is.na(AllFungi$names_to_use)),28]<-AllFungi[which(is.na(AllFungi$names_to_use)),3]

unique(AllFungi$phylum_)
unique(AllFungi$phylum)
#There are left 335 cases where no phylum is assigned. In almost all of these cases no other 
#piece of information is left, except for 64 species which they are reported as Fungi.
#Aside from those 64, looking at the genera, it seems there are a lot of Oomycetes
length(which(is.na(AllFungi$phylum_)))
all(is.na(AllFungi[which(is.na(AllFungi$phylum_)),]$Col_acc_names))
length(AllFungi[which(is.na(AllFungi$phylum_)&AllFungi$kingdom=="Fungi"),1])

#Fixing typos
AllFungi$SporeName<-gsub("ascospores","Ascospores",AllFungi$SporeName)
AllFungi$SporeName<-gsub("zygospores","Zygospores",AllFungi$SporeName)
AllFungi$SporeName<-gsub("sporangiospore","Sporangiospores",AllFungi$SporeName)
AllFungi$SporeName<-gsub("AZygospores","Azygospores",AllFungi$SporeName)

#This could be check out
AllFungi$SporeName[which(AllFungi$phylum=="Glomeromycota"&AllFungi$SporeName=="Chlamydospores"
)]<-"Azygospores"

AllFungi$Specific_sporeName[AllFungi$SporeName=="Basidiospores"]<-"basidiospores"#I checked this!

AllFungi$Specific_sporeName[AllFungi$SporeName=="Ascospores"&
                              !grepl("art",AllFungi$Specific_sporeName)]<-"ascospores"#I checked this!

AllFungi$Specific_sporeName[AllFungi$SporeName=="Sporangiospores"]<-"sporangiospores"#I checked this!

AllFungi$Specific_sporeName[AllFungi$SporeName=="Teliospores"]<-"teliospores"#I checked this!

AllFungi$Specific_sporeName[AllFungi$SporeName=="Zygospores"]<-"zygospores"#I checked this!

AllFungi$Specific_sporeName<-
  gsub("µ?[[:alpha:]]+\\.\\-","",AllFungi$Specific_sporeName)

AllFungi$Specific_sporeName<-
  gsub("µm?[[:punct:]]+","",AllFungi$Specific_sporeName)

AllFungi$Specific_sporeName<-
  gsub("\\(?[[:digit:]]+\\)","",AllFungi$Specific_sporeName)

AllFungi$Specific_sporeName<-
  gsub("\\(?[[:digit:]]+\\)?[[:punct:]]+","",AllFungi$Specific_sporeName)

AllFungi$Specific_sporeName<-
  gsub("µ?[[:alpha:]]+\\)?\\.\\;?","",AllFungi$Specific_sporeName)

gsub("\\(","",
     AllFungi$Specific_sporeName[grep("\\(",AllFungi$Specific_sporeName)]
)

AllFungi$Specific_sporeName<-
  gsub("\\[","",AllFungi$Specific_sporeName)

AllFungi$Specific_sporeName<-
  gsub("\\.\\-","",AllFungi$Specific_sporeName)

AllFungi$Specific_sporeName<-tolower(AllFungi$Specific_sporeName)

AllFungi$Specific_sporeName[which(AllFungi$Specific_sporeName=="(c)chlamydospores")]<-"chlamydospores"

AllFungi$Specific_sporeName<-
  gsub("\\(","",AllFungi$Specific_sporeName)

AllFungi$Specific_sporeName[grep("‘conidia",AllFungi$Specific_sporeName)]<-"conidia"

AllFungi$Specific_sporeName[grep("\"conidia",AllFungi$Specific_sporeName)]<-"conidia"

AllFungi[grep("bloxami)-conidia",AllFungi$Specific_sporeName),c("spore_width","spore_length")]<-c(9.5,9.5,9.5,18.5,18.5,18.5)

AllFungi$Specific_sporeName[grep("bloxami)-conidia",AllFungi$Specific_sporeName)]<-"conidia"

AllFungi[grep("\\?\\-conidia",AllFungi$Specific_sporeName),c("Specific_sporeName","spore_width","spore_length")]
AllFungi$Specific_sporeName[grep("\\?\\-conidia",AllFungi$Specific_sporeName)]<-c("fusiform_conidia","filiform_conidia")

AllFungi$Specific_sporeName[grep("\\?\\conidia",AllFungi$Specific_sporeName)]<-"conidia"

AllFungi$Specific_sporeName[which(AllFungi$Specific_sporeName=="-conidia")]<-"conidia"

AllFungi$Specific_sporeName[which(AllFungi$Specific_sporeName=="cconidia")]<-"conidia"
AllFungi$Specific_sporeName[which(AllFungi$Specific_sporeName=="microcconidia")]<-"microconidia"

AllFungi<-AllFungi[-which(AllFungi$Specific_sporeName=="gonidia"),]#This is not a typo. It seems it is an obsolete term
#to describe some structure of a lichenized fungus (I checked in the Dictionary of the fungi)

rownames(AllFungi)<-NULL
#Microascus trigonosporus is a good example where everything worked great despite the fact that the same descriptions
#contains actually multiple descriptions.

AllFungi$Specific_sporeName[which(AllFungi$Specific_sporeName=="??chlamydospores")]<-"chlamydospores"
AllFungi$Specific_sporeName[which(AllFungi$Specific_sporeName==".chlamydospores")]<-"chlamydospores"
AllFungi$Specific_sporeName[which(AllFungi$Specific_sporeName=="chlamydospore")]<-"chlamydospores"

unique(AllFungi$SporeName)
unique(AllFungi$Specific_sporeName[which(AllFungi$SporeName=="Conidia")])
unique(AllFungi$Specific_sporeName)



#########################################
###   Tyding up the database and      ###
###  identifying extreme values       ###
#########################################



#FIRST. Discarding duplicated entries that are similar in everything, but for some reason got duplicated

AllFungi<-AllFungi[-which(
  duplicated(
    paste(AllFungi$Col_ID_acc_names,AllFungi$Col_acc_names,AllFungi$base_name,AllFungi$Mycobank_base__id,
          AllFungi$Col_name_id,AllFungi$Col_name,AllFungi$description__id,
          AllFungi$SporeName,AllFungi$Specific_sporeName,
          AllFungi$spore_length,AllFungi$spore_width,sep = "_"))
),
];rownames(AllFungi)<-NULL;AllFungi$row_number<-as.character(c(1:length(AllFungi$base_name)))

#SECOND. Identifying which values for a given species are extreme values.

#Extreme values are considered any value that is 2 times larger than the median or half the media
#for either spore width and spore (explained below).

#For doing this I am using "codigo" as the splitting factor. This is codigo:
AllFungi$codigo<-paste(AllFungi$names_to_use,
                       AllFungi$SporeName,
                       AllFungi$Specific_sporeName,
                       AllFungi$description__id,
                       sep = "_")

#In this case the median will be calculated across all entries for the accepted name (or the base name, where
#no accepted names was found in the Catalogue of life) as follows: 
#  -all values for a given spore type within a description
#  -all values for a given spore type across synonyms IF they share the same description. 
#If I do not include description_id, I would get that median across all synonyms regardless of whether they share
#a description (that is, across descriptions)

extremos<-function(x,column=c("spore_width","spore_length")){
  if(length(unique(x[,column]))>1&
     any(x[,column]>2*median(x[,column])|x[,column]<(1/2)*median(x[,column]))
  ){x<-x[which(x[,column]>2*median(x[,column])|
                 x[,column]<(1/2)*median(x[,column])),]}#else{x<-x}
}

For_width<-
  do.call("rbind",
          lapply(
            split(AllFungi,AllFungi$codigo),
            extremos,column="spore_width")
  )

AllFungi$width_extreme<-rep(FALSE,length(AllFungi$base_name))
AllFungi$width_extreme[as.numeric(For_width$row_number)]<-TRUE

For_length<-
  do.call("rbind",
          lapply(
            split(AllFungi,AllFungi$codigo),
            extremos,column="spore_length")
  )

AllFungi$length_extreme<-rep(FALSE,length(AllFungi$base_name))
AllFungi$length_extreme[as.numeric(For_length$row_number)]<-TRUE
rm(For_length,For_width)

#THIRD. MISSING TAXONOMY

#There are around 9k entries with no higher rank taxonomy. All this entries come from fungi that were not found in the
#catalogue of life but are reported in Mycobank (a small fraction also come from other sources)

#To solve this, I am using two approaches:

#-a) Copying the same higher taxonomy for species that share genera with taxonomy from the Catalogue of Life

No_taxonomy<-AllFungi[which(is.na(AllFungi$family)),c("base_name","row_number","Mycobank_base__id")]

Genera<-
  do.call("rbind",  
          lapply(
            strsplit(
              No_taxonomy$base_name,split = " "
            ),
            function(x){y<-data.frame(genus=as.character(x[1]),
                                      base_name=as.character(paste(x[1:length(x)],collapse = " "),
                                                             stringsAsFactors = F))}
          ));Genera$base_name<-as.character(Genera$base_name);Genera$genus<-as.character(Genera$genus)
all(Genera$base_name==No_taxonomy$base_name)
Genera$row_number<-No_taxonomy$row_number;Genera$Mycobank_base__id<-No_taxonomy$Mycobank_base__id

Replacing_taxonomy<-
  AllFungi[which(AllFungi$genus%in%Genera$genus),
           c("kingdom","phylum","class","order","family","genus")]
Replacing_taxonomy<-Replacing_taxonomy[-which(duplicated(Replacing_taxonomy$genus)),]

Genera<-left_join(Genera,Replacing_taxonomy,by="genus")

class(Genera$Mycobank_base__id)

Genera$name_test<-NA

#b) For the ones not even the genus has higher taxonomy I will use the higher taxonomy as reported in Mycobank
Mycobank_Taxonomy<-read.csv("Mycobank_Taxonomy.csv", stringsAsFactors = F)

Genera[is.na(Genera$family),c("kingdom","phylum","class","order","family","name_test")]<-
  Mycobank_Taxonomy[match(Genera$Mycobank_base__id[is.na(Genera$family)],Mycobank_Taxonomy$base__id),
                    c("Kingdom","Phylum","Class","Order","Family","base_name")]

#just checking whether the data imported matches the names
all(
  Genera$base_name[which(!is.na(Genera$name_test))]==Genera$name_test[which(!is.na(Genera$name_test))])

Genera$Taxonomy<-NA
Genera$Taxonomy[which(!is.na(Genera$name_test))]<-"Mycobank"
Genera$Taxonomy[which(is.na(Genera$name_test))]<-"Catalogue_of_Life_borrowed"
Genera$Taxonomy[which(
  rowSums(
    is.na(Genera[c("kingdom","phylum","class","order","family")]))==4)]<-NA

Genera$name_test<-NULL

# tabla[-which(
#   rowSums(is.na(tabla[c("Var1","Var2","Var3")]))==3)]<-NA
# 
# 
# tabla[-which(
#   rowSums(
#     (tabla[c("Var1","Var2","Var3")])==0)==3),
#   ]

#Finally puting this taxonomy into the database
AllFungi[which(is.na(AllFungi$family)),c("kingdom","phylum","class","order","family","genus","Taxonomy")]<-
  Genera[match(AllFungi$row_number[which(is.na(AllFungi$family))],Genera$row_number),
         c("kingdom","phylum","class","order","family","genus","Taxonomy")]

#FOUTH. Standardizing (again) the taxonomy

AllFungi$phylum<-trimws(AllFungi$phylum)
#AllFungi2<-AllFungi

#The lower fungi: Catalogue of Life still uses Zygomycota for the following
#groups: Mucoromycetes,Zoopagomycetes,Entomophthoromycetes,Mortierellomycetes.
#For the moment, my plan is to place them in individual phyla. Here I am following
#the classification found in Wijayawardene, N. N., et al. (2018). In phylum_
#I am depositing this new classification

AllFungi$phylum_<-AllFungi$phylum
AllFungi$phylum_[which(AllFungi$class=="Mucoromycetes")]<-"Mucoromycota"
AllFungi$phylum_[which(AllFungi$order=="Endogonales")]<-"Mucoromycota"
AllFungi$phylum_[which(AllFungi$order=="Umbelopsidales")]<-"Mucoromycota"

AllFungi$phylum_[which(AllFungi$order=="Zoopagales")]<-"Zoopagomycota"

AllFungi$phylum_[which(AllFungi$class=="Entomophthoromycetes")]<-"Entomophthoromycota"
AllFungi$phylum_[which(AllFungi$class=="Mortierellomycetes")]<-"Mortierellomycota"

AllFungi$phylum_[which(AllFungi$order=="Kickxellales")]<-"Kickxellomycota"
AllFungi$phylum_[which(AllFungi$order=="Harpellales")]<-"Kickxellomycota"
AllFungi$phylum_[which(AllFungi$order=="Dimargaritales")]<-"Kickxellomycota"

AllFungi$phylum_[which(AllFungi$order=="Basidiobolales")]<-"Basidiobolomycota"

#And in phylum I will keep only the Zygomycota classification

AllFungi$phylum<-gsub("Entomophthoromycota","Zygomycota",AllFungi$phylum)
AllFungi$phylum<-gsub("Kickxellomycota","Zygomycota",AllFungi$phylum)
AllFungi$phylum<-gsub("Mucoromycota","Zygomycota",AllFungi$phylum)
AllFungi$phylum<-gsub("Mucoromycota","Zygomycota",AllFungi$phylum)
AllFungi$phylum<-gsub("Zoopagomycota","Zygomycota",AllFungi$phylum)
AllFungi$phylum<-gsub("Calcarisporiellomycota","Zygomycota",AllFungi$phylum)

#Now removing fossils and things that are not fungi
AllFungi<-
  AllFungi%>%
  filter(phylum!="Oomycota")%>%
  filter(phylum!="Choanozoa")%>%#This should not even be here in the first place! This ones here are Trichomycetes. They are protozoan that were placed some time ago as Fungi
  filter(phylum!="Deuteromycota")%>%
  filter(phylum!="Fossil Ascomycota")


unique(AllFungi$phylum)
unique(AllFungi$phylum_)

#Finally removing odd cases that need to be checked

AllFungi_public<-
  AllFungi%>%
  # filter(phylum!="Oomycota")%>%
  # filter(phylum!="Choanozoa")%>%#This should not even be here in the first place!
  # filter(phylum!="Deuteromycota")%>%
  # filter(phylum!="Fossil Ascomycota")%>%
  filter(!(phylum=="Ascomycota"&SporeName=="Basidiospores"))%>% #Eight entries that need to be checked
  filter(!(phylum=="Ascomycota"&SporeName=="Teliospores"))%>% #weird  cases
  filter(!(phylum=="Basidiomycota"&SporeName=="Ascospores")) #just one



#Saving this file. This is the one others will use
write.csv(AllFungi_public,'output/Spore_Database_Fungi_March2020.csv',row.names = F)
rm(AllFungi_public)


#Doing final cleaning on March 2020

AllFungi<-read.csv('output/Spore_Database_Fungi_March2020.csv', header = T,stringsAsFactors = F)

#Here I check what are wrong very large values:
To_check_temp<-
AllFungi[which(!AllFungi$SporeName%in%c("Azygospores","Zygospores")&AllFungi$Dim1>400),
         c("names_to_use","Dim1","Dim2","SporeName")]

To_check_temp2<-
  AllFungi[which(!AllFungi$SporeName%in%c("Azygospores","Zygospores")&AllFungi$Dim2>400),
           c("names_to_use","Dim1","Dim2","SporeName")]

To_check_temp2$names_to_use[which(!To_check_temp2$names_to_use%in%To_check_temp$names_to_use)]
#Given that there are so many entries and the changes are quite complex (compare to other values)
#reported from other descriptions or synonyms, looking at both dim1 and dim2
#I decided to change these values by hand in excell on March 2020. These are the species
#that were changed then:
# [1] "Alternaria argyranthemi"  c(115-225 x 20-28)     
# [2] "Alternaria cerealis"      c(35-50 x 10-12)#the one with dim 6.5	3.5 should be deleted
# [3] "Alternaria longipes"           
# [4] "Alternaria longipes"           
# [5] "Alternaria nitrimali"          
# [6] "Alternaria novae-guineensis"   
# [7] "Aspergillus angustatus"        
# [8] "Aspergillus cretensis"         
# [9] "Aspergillus cvjetkovicii"      
# [10] "Aspergillus dromiae"           
# [11] "Aspergillus flavus"            
# [12] "Aspergillus flavus"            
# [13] "Aspergillus peyronelii"        
# [14] "Aspergillus restrictus"        
# [15] "Aspergillus sulphureoviridis"  
# [16] "Aspergillus terreus"           
# [17] "Aspergillus transmontanensis"  
# [18] "Bipolaris oryzae"              
# [19] "Botryosphaeria fabicerciana"   
# [20] "Calonectria quinqueseptata"#this one is ok    
# [21] "Chaetospermum malipoense"      
# [22] "Cladobotryum tchimbelense"     
# [23] "Clonostachys coccicola"        
# [24] "Coniella macrospora"           
# [25] "Coniella macrospora"           
# [26] "Diplodina volubilis"           
# [27] "Geosmithia lavendula"          
# [28] "Geosmithia lavendula"          
# [29] "Kalamarospora multiflagellata" 
# [30] "Lauriomyces cylindricus"       
# [31] "Ophiostoma ulmi"               
# [32] "Ophiostoma ulmi"               
# [33] "Passalora lepistemonis"        
# [34] "Penicillium annulatum"         
# [35] "Penicillium cartierense"       
# [36] "Penicillium novae-zeelandiae"  
# [37] "Penicillium singorense"        
# [38] "Penicillium sucrivorum"        
# [39] "Plagiostoma rubrosporum"       
# [40] "Polycephalomyces ramosus"      
# [41] "Polycephalomyces ramosus"      
# [42] "Polycephalomyces ramosus"      
# [43] "Polycephalomyces ramosus"      
# [44] "Sammeyersia grandispora"       
# [45] "Solicorynespora fici"          
# [46] "Solicorynespora fici"          
# [47] "Thozetella queenslandica"      
# [48] "Chaenothecopsis perforata"     
# [49] "Metarhizium kalasinense"       
# [50] "Aspergillus terrestris"        
# [51] "Hymenoscyphus varicosporioides"
# [52] "Hymenoscyphus varicosporoides" 
# [53] "Ellisembia hainanensis"        
# [54] "Hermatomyces reticulatus"      
# [55] "Hermatomyces sphaericoides"    
# [56] "Hermatomyces verrucosus"       
# [57] "Castanedospora pachyanthicola" 
# [58] "Pleurotus cystidiosus"         
# [59] "Pleurotus cystidiosus"
# 
# "Ascochyta sesleriae"
# "Ascochyta sesleriae"
# "Microidium phyllanthi"  
# "Oidium ramakrishnanii"
# "Colacosiphon filiformis"

#Phyllactinia roboris
#Cosmospora flammea
#Aspergillus rugulosus
#Cercospora canavaliicola
#"Alternaria americana"            
# [2] "Alternaria astragali"            
# [3] "Alternaria crassa"               
# [4] "Alternaria cucumerina"           
# [5] "Alternaria ipomoeae"             
# [6] "Alternaria iranica"              
# [7] "Alternaria lallemantiae"         
# [8] "Alternaria multirostrata"        
# [9] "Alternaria neoipomoeae"          
# [10] "Alternaria passiflorae"          
# [11] "Alternaria porri"                
# [12] "Alternaria porri f.sp. solani"   
# [13] "Alternaria simsimi"              
# [14] "Alternaria solani"               
# [15] "Alternaria vaccinii"             
# [16] "Arthrographis kalrae"            
# [17] "Aspergillus rugulosus"           
# [18] "Bipolaris heveae"                
# [19] "Bipolaris maydis"                
# [20] "Cercospora canavaliicola"        
# [21] "Cercospora catalpae"             
# [22] "Cercospora jatrophiphila"        
# [23] "Cercospora wulffiae"             
# [24] "Cochliobolus stenospilus"        
# [25] "Coniella eucalyptorum"           
# [26] "Cosmospora flammea"              
# [27] "Erysiphe ceibae"                 
# [28] "Erysiphe corylicola"             
# [29] "Erysiphe kusanoi"                
# [30] "Erysiphe sengokui"               
# [31] "Fusarium secorum"                
# [32] "Penicillium implicatum"          
# [33] "Penicillium purpurescens"        
# [34] "Phaeophleospora stonei"          
# [35] "Phyllactinia corylea var. rigida"
# [36] "Phyllactinia rigida"             
# [37] "Phyllactinia roboris"            
# [38] "Pyrenophora tritici-repentis"    
# [39] "Trichoconiella padwickii"        
# [40] "Zasmidium genipae-americanae"

#Then on "output/Spore_Database_Fungi_March2020.csv" I manually changed those entries in excel and saved it
#as a new file called output/Spore_Database_Fungi_ManualCheck_March2020

AllFungi<-read.csv('output/Spore_Database_Fungi_ManualCheck_March2020.csv', header = T,stringsAsFactors = F)

AllFungi[which(AllFungi$names_to_use=="Oidium bixae"),c("Dim2")]<-(13+19)/2

AllFungi[which(AllFungi$names_to_use%in%c("Melanconium dendrocalami","Oidium heveae",
                                          "Chevalieropsis ctenotricha","Phaeodimeriella cetotricha",
                                          "Ascochyta tarda")&AllFungi$Dim2>50),c("Dim2")]<-
  c((12+15)/2,(12+15)/2,(12+15)/2,(12+15)/2,(14+20)/2,(12+17)/2,(12+17)/2,(12+15)/2)

AllFungi[which(AllFungi$names_to_use%in%c("Ascochyta tarda")&AllFungi$Dim1>50),c("Dim1")]<-c((9+14)/2)

AllFungi[which(AllFungi$names_to_use%in%c("Microidium phyllanthi")&AllFungi$Dim2>50),c("Dim2")]<-c((7+10)/2)


#Clean sporangiospores entries
#Typhula ishikariensis from Compendium is wrong. The basidiospores should be 6-8 x 3-4 um
#Cladophialophora minutissima is wrong (conidia entry). It has the format d+error x d+error
#Aphanoascella galapagosensis	is wrong. It has sub-structures
#Also check bigger sizes
#the entry for :Cosmospora flammea is wrong
##########################################################################################################
#Trying to fixing the entries "long ... wide"
Conidia<-read.csv('output/conidia_mycobank.csv',header = T, stringsAsFactors = F)
Conidia_names<- strsplit(Conidia$spec,"_")
Conidia_names<- do.call("rbind",Conidia_names)
Conidia<-cbind(Conidia,Conidia_names)
names(Conidia)[11]<-"base_name"
names(Conidia)[12]<-"base__id"
names(Conidia)[13]<-"description__id"
Conidia$SporeName<-"Conidia"
Conidia$base_name<-as.character(Conidia$base_name)

#trial<-Conidia[grep("up to \\d",Conidia$text_entry),c("text_entry","spec","Dim1","Dim2","description__id")]
trial<-Conidia[-grep("\\sx\\s",Conidia$text_entry),]
trial<-trial[-grep("x",trial$measure_orig),]
trial<-trial[which(is.na(trial$Dim2)),]

#Loading again the texts
spore.dat<- readRDS("mycobank_descriptions_mod.RDS")#This dataset
#contains 117,481 species); the entry called base_mycobank_nr is 
#the mycobank code as it is found when checking in the website and it is the table
#that Will sent us on November 2019

#source of taxonomic data:
Mycobank_Taxonomy <- read.csv('Mycobank_Taxonomy.csv', stringsAsFactors=F)#This dataframe was made on the "Checking_Taxonomy.R" code and 
#reflects the the higher rank taxonomy of species and infraspecies of true fungi

spore.dat$base_mycobanknr_ <- as.numeric(spore.dat$base_mycobanknr_)

spore.dat<-left_join(spore.dat,Mycobank_Taxonomy[c(4,12:19)],
                     by="base_mycobanknr_")

#I. SELECT THE DESCRIPTIONS WITH PROBLEMS

spore.dat<-spore.dat[which(spore.dat$description__id%in%trial$description__id),]
# spore.dat1<-spore.dat[grep("up to [^a-wy-zA-WY-Z]+\\s?µm\\s?long and [^a-wy-zA-WY-Z]+\\s?µm wide",
#                spore.dat$description_description_),]
# 
# spore.dat2<-spore.dat[grep("up to [^a-wy-zA-WY-Z]+\\s?µm\\s?long\\s?[[:punct:]]\\s?[^a-wy-zA-WY-Z]+\\s?µm wide",
#                           spore.dat$description_description_),]

spore.dat3<-spore.dat[grep("[^a-wy-zA-WY-Z]+\\s?µm\\s?long\\s?[[:punct:]]\\s?[^a-wy-zA-WY-Z]+\\s?µm wide",
                           spore.dat$description_description_),]
spore.dat4<-spore.dat[grep("[^a-wy-zA-WY-Z]+\\s?µm\\s?long and [^a-wy-zA-WY-Z]+\\s?µm wide",
                           spore.dat$description_description_),]
#µm long, 15-19 µm wide

spore.dat<-rbind(spore.dat3,spore.dat4)

spore.dat$description_description_<-gsub("\\s?µm\\s?long and ","x",spore.dat$description_description_)
spore.dat$description_description_<-gsub("\\s?µm\\s?long\\s?[[:punct:]]\\s?","x",spore.dat$description_description_)


spore.dat$description_description_<-gsub("wide","\\.",spore.dat$description_description_)

textos<-spore.dat$description_description_
names(textos)<-paste(spore.dat$base_name, spore.dat$base__id, spore.dat$description__id, sep ="_")

#Changing the name of the old Conidia values (this is because it will make easier run the code later)
Conidia_old<-Conidia

#II. EXTRACT THE REGIONS OF TEXT WITH SPORE DIMENSIONS  Extract the regions of the text with the spore dimensions

#Now I can extract Conidia out of these subset

Conidia_text<-
  lapply(textos,get_text,
         start.regex="onidia$|ONIDIA$",
         end.regex="µm"#,
  )

#III. STANDARDINZING ISSUES WITH DASHES, X, OR UM SYMBOL

# temp <- Conidia_text

Conidia_text<-lapply(Conidia_text, 
                     function(x)gsub('\\([a-zA-Z]+\\. [0-9]{+}-[0-9]{1,}\\)', '', x))
Conidia_text<-lapply(Conidia_text, 
                     function(x)gsub('\\([a-zA-Z]+\\. [0-9]+\\,[0-9]+\\)', '', x))
Conidia_text<-lapply(Conidia_text, 
                     function(x)gsub('\\([a-zA-Z]+\\. [0-9]+\\)', '', x))
Conidia_text<-lapply(Conidia_text, 
                     function(x)gsub('－', '-', x))
Conidia_text<-lapply(Conidia_text, 
                     function(x)gsub('−', '-', x))
Conidia_text<-lapply(Conidia_text, 
                     function(x)gsub(' x ca ', ' x ', x))
Conidia_text<-lapply(Conidia_text, 
                     function(x)gsub(' x c\\. ', ' x ', x))
Conidia_text<-lapply(Conidia_text, 
                     function(x)gsub(' x ca\\. ', ' x ', x))
Conidia_text<-lapply(Conidia_text, 
                     function(x)gsub('\\-c\\. ', ' x ', x))
Conidia_text<-lapply(Conidia_text, 
                     function(x)gsub("\\<U\\+F0B4\\>","x",x))
Conidia_text<-lapply(Conidia_text, 
                     function(x)gsub("\\<U\\+2012\\>","-",x))


# Conidia_text<-lapply(Conidia_text, 
#                      function(x)gsub("\\s?µm\\s?long and ","x",x))
# Conidia_text<-lapply(Conidia_text, 
#                      function(x)gsub("\\s?µm\\s?long\\s?[[:punct:]]\\s?","x",x))
# 
# Conidia_text<-lapply(Conidia_text, 
#                      function(x)gsub("wide","\\.",x))
#IV. EXTRACTING THE FORMATS "DIGIT x DIGIT um" CONTAINED IN THE EXTRACTED TEXT

#Extracting Conidia values
Conidia_values<-#Conidia_text[[632]] Conidia$text_entry_temp[128]
  lapply(Conidia_text,get_dimensions2,
         extract.regex="[^a-wy-zA-WY-Z]+\\s?µm")

values<- lapply(Conidia_values, function(x) x[[1]])

## restructure data as table
values2 <- list()#it took ~30 seconds to run it
for(i in 1:length(values)){
  x <- plyr::rbind.fill(lapply(values[[i]], function(y) { as.data.frame(t(y), stringsAsFactors=FALSE) }))
  x <- cbind(spore_type = names(values[[i]]), x)
  values2[[i]] <- x
}
names(values2) <- names(values)

#It seems one does not need this for Conidia:
#q<-which(sapply(values2,is.null))#for some reasons some entries are empty

## combine all spore results in one table
#values_df <- plyr::rbind.fill(lapply(values2[-q], function(y) { as.data.frame(y) }))
values_df <- plyr::rbind.fill(lapply(values2, function(y) { as.data.frame(y) }))
#values_df<- cbind(spec=rep(names(values2[-q]),lapply(values2[-q], nrow)),values_df)
values_df<- cbind(spec=rep(names(values2),lapply(values2, nrow)),values_df)
#values_df <- values_df[,-length(values_df)]
names(values_df)[3] <- "measure_orig"


#Merge the text with the values and spore data info
text<-lapply(Conidia_text,plyr::ldply, rbind)
text<-plyr::rbind.fill(text)
names(text)[1]<-"text_entry"

# temp_text<-lapply(temp,plyr::ldply, rbind)
# temp_text<-plyr::rbind.fill(temp_text)
# names(temp_text)[1]<-"text_entry_temp"

#Creation of the object "Conidia" containing all the data
# Conidia<-cbind(temp_text,text,values_df)#For some reason the transformations above return 47032, instead of 45416 elements that has Conidia_text and Conidia_values. However, it seems fine!
Conidia<-cbind(text,values_df)#For some reason the transformations above return 47032, instead of 45416 elements that has Conidia_text and Conidia_values. However, it seems fine!
Conidia<-data.frame(
  sapply(Conidia, as.character),
  stringsAsFactors = F)


#V. STANDARDIZING ISSUES WITH "x" WITHIN THE "DIGIT x DIGIT um"

#Just standarizing the "x"

Conidia$measure_orig<-gsub("X","x",Conidia$measure_orig)
Conidia$measure_orig <- gsub('\\<U\\+F02D>', ' - ', Conidia$measure_orig)
Conidia$measure_orig <- gsub(' [[:punct:]] ', ' x ', Conidia$measure_orig)
Conidia$measure_orig <- gsub('\\s+x\\s+', ' x ', Conidia$measure_orig)
Conidia$measure_orig <- gsub('[[:punct:]]  ', '', Conidia$measure_orig)
Conidia$measure_orig <- gsub('  ', ' x ', Conidia$measure_orig)
Conidia$measure_orig <- gsub(' ', ' x ', Conidia$measure_orig)
Conidia$measure_orig <- gsub(' ◊ ', ' x ', Conidia$measure_orig)
Conidia$measure_orig <- gsub('∑', '.', Conidia$measure_orig)
Conidia$measure_orig <- gsub('[[:space::]]{2,}', ' x ', Conidia$measure_orig)
Conidia$measure_orig <- gsub('^x', '', Conidia$measure_orig)

Conidia$measure_orig[grep("av\\. \\= \\d+\\.?\\d?\\-",Conidia$text_entry)]<-
  gsub("\\-"," x ",
       Conidia$measure_orig[grep("av\\. \\= \\d+\\.?\\d?\\-",Conidia$text_entry)])

Conidia$measure_orig[grep("^\\. x ",Conidia$measure_orig)] <- 
  gsub('^\\. x ', '', Conidia$measure_orig[grep("^\\. x ",Conidia$measure_orig)])

Conidia$measure_orig <- gsub('\\?', '-', Conidia$measure_orig)
Conidia$measure_orig <- gsub('·', '.', Conidia$measure_orig)
Conidia$measure_orig <- gsub("À\u008d","x",Conidia$measure_orig)
Conidia$measure_orig <- gsub("\\'",".",Conidia$measure_orig)
Conidia$measure_orig <- gsub("\\= 11: 66: 23 \\%\\)","",Conidia$measure_orig)
Conidia$measure_orig <- gsub(" x x x "," x ",Conidia$measure_orig)
Conidia$measure_orig <- gsub(" x x "," x ",Conidia$measure_orig)

Conidia$measure_orig <- gsub(" x  x  x ","-",Conidia$measure_orig)
Conidia$measure_orig <- gsub(" x  x ","-",Conidia$measure_orig)
Conidia$measure_orig[grepl("\\)\\-\\(",Conidia$measure_orig)&grepl("x",Conidia$measure_orig)]<-
  gsub("\\s?x\\s?\\s?x?","-", Conidia$measure_orig[grepl("\\)\\-\\(",Conidia$measure_orig)&grepl("x",Conidia$measure_orig)])
Conidia$measure_orig <- gsub("\\)\\-\\(","\\) x \\(",Conidia$measure_orig)


#VI. SPLITTING THE "DIGIT x DIGIT" BY THE "x" AND PLACING THEM IN SEPARATE COLUMNS


####  Extracting the spore ranges  ######

t<-strsplit(Conidia$measure_orig,"x")

s<-sapply(t,length)
temp<-plyr::rbind.fill(lapply(t, function(y) { as.data.frame(t(y)) }))


#VIII. STANDARDIZING THE DASH

temp <- apply(temp, 2, function(x)gsub(',', '.', x))
temp <- apply(temp, 2, function(x)gsub('\\(', '-', x))
temp <- apply(temp, 2, function(x)gsub('\\)', '-', x))
temp <- apply(temp, 2, function(x)gsub('--', '-', x))
temp <- apply(temp, 2, function(x)gsub(' ', '', x))
temp <- apply(temp, 2, function(x)gsub('^-', '', x))
temp <- apply(temp, 2, function(x)gsub('-$', '', x))
temp <- apply(temp, 2, function(x)gsub('+-', 'plmn', x, fixed=T))
temp <- apply(temp, 2, function(x)gsub('plmn[0-9]+.[0-9]+', '', x))
temp <- apply(temp, 2, function(x)gsub('plmn[0-9].', '', x))
temp <- apply(temp, 2, function(x)gsub('--', '-', x))
temp <- apply(temp, 2, function(x)gsub('^[[:punct:]]+', '', x))
temp <- apply(temp, 2, function(x)gsub('[[:punct:]]+$', '', x))
temp <- apply(temp, 2, function(x)gsub('\\[', '-', x))
temp <- apply(temp, 2, function(x)gsub('\\]', '-', x))
temp <- apply(temp, 2, function(x)gsub('--', '-', x))
temp <- apply(temp, 2, function(x)gsub('<->', '-', x))
temp <- apply(temp, 2, function(x)gsub("\\*\\*","-",x))
temp <- apply(temp, 2, function(x)gsub("~","-",x))
temp <- apply(temp, 2, function(x)gsub("\\:\\s?","-",x))
temp <- apply(temp, 2, function(x)gsub('--', '-', x))

temp <- apply(temp, 2, str_trim)


#IX. SPLITTING BY THE HYPHEN AND STORING THE OUTUPUT IN DIFFERENT COLUMNS

temp <- apply(temp, 2, function(x){
  t <- strsplit(x, '-')
  t1 <- c()
  sapply(t, function(x){
    if(is.na(x[1])) t1<-NA else if(length(x) %in% seq(1, 21, 2)) t1<-as.numeric(x[length(x)/2 + 0.5]) else t1<-mean(as.numeric(c(x[length(x)/2], x[length(x)/2 + 1])))
    return(t1)
  })
})
############


#X. PUTTING ALL THE DATA INTO A SINGLE DATAFRAME TOGETHER WITH THE ORIGINAL EXTRACTED TEXT 

Conidia <- cbind(Conidia, temp)
Conidia <- Conidia %>% 
  rename(Dim1 = V1, 
         Dim2 = V2, 
         Dim3 = V3, 
         Dim4 = V4,
         Dim5 = V5,
         Dim6 = V6)

# Conidia_<-
# Conidia[grep("up\\s?to\\s?\\d",Conidia$text_entry),]

Conidia_<-Conidia[-which(is.na(Conidia$V2)),]
Conidia_$to_replace<-paste(Conidia_$spec,Conidia_$spore_type,Conidia_$V1)

# Conidia_old$to_replace<-paste(Conidia_old$spec,Conidia_old$spore_type,Conidia_old$Dim1)
# Conidia_$to_replace[which(!Conidia_$to_replace%in%Conidia_old$to_replace)]
# Conidia_[which(!Conidia_$to_replace%in%Conidia_old$to_replace),]$spec%in%Conidia_old$spec
# Conidia_old[which(Conidia_old$to_replace%in%Conidia_$to_replace),
#             c("spore_type","measure_orig","Dim1","Dim2")]<-
#   Conidia_[match(Conidia_old$to_replace[which(Conidia_old$to_replace%in%Conidia_$to_replace)],
#                  Conidia_$to_replace),c("spore_type","measure_orig", "Dim1", "Dim2")]

Conidia_$spore_type<-tolower(Conidia_$spore_type)

Conidia_$spore_type<-
  gsub("5\\.5\\-15x","",Conidia_$spore_type)

Conidia_$spore_type<-
  gsub("2\\.5\\-3\\.5x","",Conidia_$spore_type)

Conidia_$spore_type<-
  gsub("15x","",Conidia_$spore_type)

Conidia_$spore_type<-
  gsub("schizolytic\\.\\-","",Conidia_$spore_type)

names(Conidia_)[c(5,6)]<-c("Dim1","Dim2")

Conidia_$to_replace<-paste(Conidia_$spec,Conidia_$spore_type,Conidia_$Dim1)
Conidia_$to_replace<-gsub("_"," ",Conidia_$to_replace)


AllFungi$to_replace<-paste(AllFungi$base_name,AllFungi$Mycobank_base__id,
                          AllFungi$description__id,AllFungi$Specific_sporeName,AllFungi$Dim1)

Conidia_$to_replace[
which(!Conidia_$to_replace%in%AllFungi$to_replace)]


AllFungi[which(AllFungi$to_replace%in%Conidia_$to_replace&AllFungi$Dim1==AllFungi$Dim2),
         c("Dim1","Dim2")]<-
  Conidia_[match(AllFungi$to_replace[which(AllFungi$to_replace%in%Conidia_$to_replace&AllFungi$Dim1==AllFungi$Dim2)],
                 Conidia_$to_replace),c("Dim1", "Dim2")]


#Now I need to update entries

#Getting spore widht
AllFungi$spore_width<-NA
AllFungi$spore_width[which(
  AllFungi$Dim2<=AllFungi$Dim1)]<-AllFungi$Dim2[which(
    AllFungi$Dim2<=AllFungi$Dim1)]
#or
AllFungi$spore_width[which(
  AllFungi$Dim1<AllFungi$Dim2)]<-AllFungi$Dim1[which(
    AllFungi$Dim1<AllFungi$Dim2)]
#Getting spore length
AllFungi$spore_length<-NA
AllFungi$spore_length[which(
  AllFungi$Dim2>=AllFungi$Dim1)]<-AllFungi$Dim2[which(
    AllFungi$Dim2>=AllFungi$Dim1)]
#or
AllFungi$spore_length[which(
  AllFungi$Dim1>AllFungi$Dim2)]<-AllFungi$Dim1[which(
    AllFungi$Dim1>AllFungi$Dim2)]

#IDs
AllFungi$row_number<-as.character(c(1:length(AllFungi$base_name)))
AllFungi$codigo<-paste(AllFungi$names_to_use,
                       AllFungi$SporeName,
                       AllFungi$Specific_sporeName,
                       #AllFungi$description__id,
                       sep = "_")

#Spore extremes
extremos<-function(x,column=c("spore_width","spore_length")){
  if(length(unique(x[,column]))>1&
     any(x[,column]>2*median(x[,column])|x[,column]<(1/2)*median(x[,column]))
  ){x<-x[which(x[,column]>2*median(x[,column])|
                 x[,column]<(1/2)*median(x[,column])),]}#else{x<-x}
}

For_width<-
  do.call("rbind",
          lapply(
            split(AllFungi,AllFungi$codigo),
            extremos,column="spore_width")
  )

AllFungi$width_extreme<-rep(FALSE,length(AllFungi$base_name))
AllFungi$width_extreme[as.numeric(For_width$row_number)]<-TRUE

For_length<-
  do.call("rbind",
          lapply(
            split(AllFungi,AllFungi$codigo),
            extremos,column="spore_length")
  )

AllFungi$length_extreme<-rep(FALSE,length(AllFungi$base_name))
AllFungi$length_extreme[as.numeric(For_length$row_number)]<-TRUE
rm(For_length,For_width)

#The entry for Mucor ardhlaengiktus, azygospore of 600 um is an error. This entry should be deleted
#The entry for Umbelopsis westeae for sporangiospores of 0.25um is an error. The sporangiospores are actually  (4-)5-6(-8) x 2-2.5(-3) µm



#Clean sporangiospores entries
#Typhula ishikariensis from Compendium is wrong. The basidiospores should be 6-8 x 3-4 um
#Cladophialophora minutissima is wrong (conidia entry). It has the format d+error x d+error
#Aphanoascella galapagosensis	is wrong. It has sub-structures
#Also check bigger sizes
#the entry for :Cosmospora flammea is wrong


#Saving the updated dataset
write.csv(AllFungi,'output/Spore_Database_Fungi.csv',row.names = F)

#OPTIONAL Checking how many have the same name, same description and same values but differ only the ID´s

#It turns out that several exact same entries for a given species have multiple ID´s (either mycobank ID or
#description ID). Since this information might be useful later (why Mycobank assings different ID´s ?) but at the same time
#it just inflates the dataset, I will create a new dataset where only one ID is kept (a good example is http://www.mycobank.org/quicksearch.aspx)
#This could also be, because two different range values for a diameter can end up in the same mean estimate
#of either length and width. In such case, although the range is different among descriptions, the value we get in the dataset
#is the same (I am not 100% sure whether such number combination exists though)


counting<-function(x){length(unique(x))}

Same_all_multiple_ID<-
  AllFungi%>%
  group_by(Col_acc_names,base_name,Col_name,
           SporeName,Specific_sporeName,
           spore_width,spore_length)%>%
  summarise_at(c("Mycobank_base__id","description__id"),counting)#%>%

summary(Same_all_multiple_ID$Mycobank_base__id)
summary(Same_all_multiple_ID$description__id)

length(
  unique(
    Same_all_multiple_ID$base_name[which(Same_all_multiple_ID$Mycobank_base__id>1)]
  )
)

########################################
### Understanding "intraspcific"     ###
###         sources of variation     ###
########################################


#1. Same species, same spore type, same name, same description, DIFFERENT VALUES

Testing<-
  AllFungi%>%
  group_by(Col_ID_acc_names,Col_acc_names,
           base_name,Mycobank_base__id,
           Col_name_id,Col_name,
           description__id,SporeName,Specific_sporeName)%>%
  summarise_at(c("spore_width","spore_length"),counting)#%>%

hist(Testing$spore_width)
hist(Testing$spore_length)

#Good news is that there are not so many in this case!!
#I mean, larger than 1 there are 4505 entries.... 
Testing[which(Testing$spore_width>1),]
#The most honest solution would be to revise those cases... but at this moment I want to minimize the effort
#so I will take some logical decissions to save time
#The majority of cases are when there are 2 entries per description
Testing[which(Testing$spore_width==2),]#3,480 cases
#Assuming that only one of those values is true a solution would to pick only the first one
#Actually the same logic can be done for cases when there are 3, because they are quite a few
Testing[which(Testing$spore_width==3),]#676
#Leaving a managable number of entries larger than 3 to check by hand 
Testing[which(Testing$spore_width>3),]#279
rm(Testing)


#This cases is the trickiest because it means that the functions for extracting spores size caught mutiple
#values within the same description. This could happen because of serveral reasons as I found that:

#a)It turns out that some descriptions contain actually descriptions from multiple authors! A good example is this one
#http://www.mycobank.org/BioloMICS.aspx?TableKey=14682616000000063&Rec=38268&Fields=All
#http://www.mycobank.org/BioloMICS.aspx?TableKey=14682616000000063&Rec=23267&Fields=All

#b) There is indeed multiple values for a given spore type depending on ontogenetic or environmental causes that are
#reported in the description. The following is an example (not the best one), where the latin description reports 
#a different spore size in the plant host (hospite):
#http://www.mycobank.org/BioloMICS.aspx?TableKey=14682616000000063&Rec=23087&Fields=All

#This one is a better example with the same fungus as above but different descriptions, where there are different conidia
#depending on being 6-8 celled or 6-10 celled
#http://www.mycobank.org/BioloMICS.aspx?TableKey=14682616000000063&Rec=38375&Fields=All

#c) The algorithm picked up something that it is not conida. The same fungus and description as above is a good example where
#the phrase "then white to greenish-white as conidiophores and conidia develop, finally amber-colored ("melleus"), cushion-like,
#hemispherical, about 200 µm diam" was caught as the size of conida
#http://www.mycobank.org/BioloMICS.aspx?TableKey=14682616000000063&Rec=38375&Fields=All
#or (this one is a mess!)
#http://www.mycobank.org/BioloMICS.aspx?TableKey=14682616000000063&Rec=17575&Fields=All

#I cannot see a way to differiate the cases. The most problematic is "c". As I explained below, "c" cases would report either 
#very small or very big values (like outliers). I will use a cut-off to distinguish them


#2. Same species, same spore type, same name,DIFFERENT DESCRIPTIONS, DIFFERENT VALUES

#This case might represent "real" intraspecific variation because different authors (I guess)
#examine different material. 

#3. Same species, same spore type, DIFFERENT NAME (SYNONYM), DIFFERENT DESCRIPTIONS, DIFFERENT VALUES
#This case is the most likely to reflect real intraspecific variation.

#Thus to reduce the changes of getting case 1.c, the easiest (and logic) thing to do would be to discard 
#the largest and smallest values assuming that those are actually wrong entries. This can be done easily as
#with the columns width_extreme and lenght_extreme I created above following the criteria 
#of considering extrmes values larger than 2 times the median and smaler than half the median!!!!


#In this case the median will be calculated across: 
#  -all values for a given spore type within a description
#  -all values for a given spore type across synonyms IF they share the same description. 
# NOTE. If I do not include description_id, I would get that median across all synonyms regardless of whether they share
#a description


#to check package janitor; look at function pluck (purr)

#################################################################################

