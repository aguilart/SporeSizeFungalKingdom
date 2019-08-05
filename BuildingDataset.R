########################################################################
## Building data set to map the distribution of spore size across the ##
## fungal kingdom and (potentially) fungi like organisms ###############
########################################################################

install.packages("installr")

library(installr)

updateR()
install.packages("tidyverse")


#Data, so far, come from four sources:

#1. FunToFun Database, specifically from basidiospore of basidiomycota
#data from Bassler, Claus, et al. "Ectomycorrhizal fungi have larger 
#fruit bodies than saprotrophic fungi." Fungal Ecology 17 (2015): 205-212.

#2. Data from Aguilar etal 2018 (ISME) on Azygospore size
#of Glomeromycotina.

#3. Data extracted from "Domsch, K., et al. (2007). Compendium of soil 
#fungi, IHW-Verlag Eching.". This correspond mainly to different types
#of conidia, ascospospores and chlamydospores from Ascomycota. There is
#also data on Mucoromycotina (both sexual an asexual spores), some basid
#diomycota and oomycota.

#4. Data extracted from the mycobank database. As for March 24th, this data
#comes from the extraction of Franz in december18-january19


#1. Fun to Fun Data

devtools::install_github("ropenscilabs/datastorr")
devtools::install_github("traitecoevo/fungaltraits")
library(fungaltraits)

FunToFun_sporeInfo<-fungal_traits()[!is.na(fungal_traits()$spore_length),
                                    c(1,2,3,68,69,70,75,76)]

                    #In this dataset, spore length is always larger than spore width. Thus,
                    #Equatorial axis = width; polar axis = length.


#2. AMF data
probando<-read.csv("C:\\Users\\Carlos\\Documents\\Professional\\Spore CommunityAnalysis\\SporeSize\\Bridging_Rep_Micro_Ecology_ISME_paper\\BridgingReproductiveEcology-MicrobialEcology\\Aguilar_etal_SuppMatt_AMF_Spore_Database.csv")
          which(probando$EquatorialAxis>probando$PolarAxis)#This should always be zero (0)
          probando$Phylum<-"Glomeromycota";probando$SporeArea<-(probando$EquatorialAxis*probando$PolarAxis)*(1/4)*pi
AMF_All_Copy<-probando;rm(probando)

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

#Merging the above data sources into one "AllFungi"            

AllFungi<-rbind(
            
            data.frame(Species_names=AMF_All_Copy[,1],
                       spore_length=AMF_All_Copy[,15],
                       spore_width=AMF_All_Copy[,14],
                       SporeArea=AMF_All_Copy[,23],
                       SporeName="Azygospores",
                       Phylum=AMF_All_Copy[,22],
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
        AllFungi$Specific_sporeName<-AllFungi$SporeName
        AllFungi$SporeName[
        grep("conidia",AllFungi$SporeName)]<-"conidia"
        AllFungi$SporeName[
          grep("basidiospores",AllFungi$SporeName)]<-"Basidiospores"
        AllFungi$SporeName[
          grep("chlamydospore",AllFungi$SporeName)]<-"chlamydospores"
        
        AllFungi$Species_names<-sub("_"," ",AllFungi$Species_names)
        
        #Making sure that Spore size refers always to spore projected area
        #(that is the area obtained by: length x width x pi/4)
        
        AllFungi$SporeArea<-AllFungi$spore_length*AllFungi$spore_width*
                            (pi/4)
        
        #Alternaria tenuissima has incorrect entry in the All Fungi database.
        #The values for the conidia should be 22-95 x 8-19 um (as stated in the Compendium
        #of soil fungi). Tim got confused with other entry values in the description
        #so, to not run again the whole code for All Fungi, I will replace manually the 
        #entry where it correspond:
        
        #conidia length
        AllFungi[grep("Alternaria tenuissima",AllFungi$Species_names),2]<-((22+95)/2)
        #conidia length
        AllFungi[grep("Alternaria tenuissima",AllFungi$Species_names),3]<-((8+19)/2)
        #Conida Area
        AllFungi[grep("Alternaria tenuissima",AllFungi$Species_names),4]<-((22+95)/2)*((8+19)/2)*(pi/4)
        
#4. Spore dimensions extracted by Franz Krah on January 2019 from mycobank descriptions. This data is
#contained in the object: FranzKrah_sporeValues. This object is based on a csv he sent me to my email on 
#the 21st of January 2019. He said that he manually checked each entry after building it up in R in his computer.
#I imported this csv, created the object FranzKrah_sporeValues, checked it and made slight changes on it.
#All this is reported in the script CheckSporeData_FromFranzExtc_on_Jan19.R located in this environment. In that
#script I also check how much overlap in sps there are between AllFungi and Franz_Krah_sporeValues, and the correlation of spore values 
#between the shared sps. I found two things: 1. Low overlap between the datasets. For basidiomycetes we have 
#64 sps in common between both datasets; for Ascos only 14 sps overlap, for conidia 52 sps. 
#2. There is Good correlation between values of shared species! It is actually an almost 1:1 relationship in 
#the case of basidiospores and ascospores. For conidia is slightly more variable, however I realized that 
#a lot of the differences are because several species produce more than one size class of conidia (usually depending on septation).
#As for zygos or sps with chlamydospores the overlap is essentially 0. For more details one has to look at that script.
        
        
        #Based on the low overlap between dataset but good correlation in spore size among shared sps
        #I decided to keep the sps in AllFungi over the ones in Franz data. My logic is that AllFungi reflects 
        #better curated data than Franz. The compendium data itself reflects an authority editing sps names
        #and entries plus the data was entered manually. I am assumign that the data of Bassler et al 15 was also
        #better curated.
        
        
        trial<-rbind(
          AllFungi%>%
            mutate(measure_orig=NA),
          FranzKrah_sporeValues[which(!FranzKrah_sporeValues$Species_names%in%AllFungi$Species_names),]%>%
          rename(SporeName=spore_type)%>%
          rename(Phylum=phylum)%>%
          rename(SporeArea=spore_surf)%>%
          rename(spore_length=length_mean)%>%
          rename(spore_width=width_mean)%>%
          mutate(Source="Mycobank extraction (Jan19_FKrah")%>%
          mutate(Specific_sporeName=NA)%>%
          select(Species_names,spore_length,spore_width,SporeArea,SporeName,Phylum,Source,Specific_sporeName,measure_orig)
        )
        
                #The "wrong" entries are:"Basidiospore"   "Ascospore"      "Conidia" and "Chlamydospore"
        #They should be  "conidia"        "ascospores" "Basidiospores"  "chlamydospores"
        trial$SporeName[trial$SporeName=="Basidiospore"]<-"Basidiospores"
        trial$SporeName[trial$SporeName=="Ascospore"]<-"ascospores"
        trial$SporeName[trial$SporeName=="Conidia"]<-"conidia"
        trial$SporeName[trial$SporeName=="Chlamydospore"]<-"chlamydospores"
        
#Finally putting all data together:
        
        
        AllFungi<-trial;rm(trial)
        AllFungi$Phylum[AllFungi$Phylum=="Mucoromycota"]<-"Mucoromycotina"
        AllFungi$Phylum<-droplevels(AllFungi$Phylum)
        AllFungi$Phylum<-as.character(AllFungi$Phylum)#I added this on 27th March 2019
        
        AllFungi%>%
          filter(Phylum=="Basidiomycota")%>%
          filter(SporeName=="conidia")
        
        #There are 192 sps of basidiomycota with conidia, I check the followings:
        #Tomentella tulasnelloidea is correct but has actually two entries
        #Auricularia pulvurenta (anamorph=Postia ptychogaster): this entry is correct
        #Tremella moriformis  this entry is correct (although there are two descriptions)
        #of conidia in this case
        #Haplotrichum ramosissimum this entry is correct (anamorph=Oidium ramosissimum)
        #One would need to double check all these cases to see how many are correct
        #and which ones are not
        
        #I found that the entry for Pseudocercospora clematidis is unsually large (for
        #spore lenght). I checked in Mycobank and the entry says actulla 40100um. However
        #I think it is almost impossible to have a conida of 4cm. The same for
        #Mycovellosiella clerodendri and its synonym (Passalora clerodendri), I found in the reference
        #New species of Mycovellosiella associated with foliar spots in Nepal, Mycol. Res. 100 (6): 689-692 (1996)
        #the spore range is actually 10-160 x 2.5-4 um. That means that the text in mycobank is wrong
        #Similar situation for Rimaconus jamaicensis (check Huhndorf, S. M., Fernández, F. A., Taylor, J. E., & Hyde, K. D. (2001). Two pantropical ascomycetes: Chaetosphaeria cylindrospora sp. nov. and Rimaconus, a new genus for Lasiosphaeria jamaicensis. Mycologia, 1072-1080.)
        #The same for Cercospora allophyli (second description reveals the typo), the same for Pseudocercospora macarangae
        #Melanomphalia thermophila there is typo in the entry of the code,
        #the same for Alternaria hordeiaustralica, Helicobasidium longisporum 
        
        AllFungi$spore_length[AllFungi$Species_names=="Pseudocercospora clematidis"]<-(40+100)/2
        AllFungi$spore_length[AllFungi$Species_names=="Mycovellosiella clerodendri"]<-(10+160)/2
        AllFungi$spore_length[AllFungi$Species_names=="Passalora clerodendri"]<-(10+160)/2
        
        AllFungi$spore_length[AllFungi$Species_names=="Rimaconus jamaicensis"]<-(55+73)/2
        AllFungi$spore_length[AllFungi$Species_names=="Melanomphalia thermophila"]<-(7+10.7)/2
        AllFungi$spore_length[AllFungi$Species_names=="Cercospora allophyli"]<-(45+80)/2
        AllFungi$spore_length[AllFungi$Species_names=="Alternaria hordeiaustralica"]<-(35+55)/2
        AllFungi$spore_length[AllFungi$Species_names=="Helicobasidium longisporum"]<-(16+21)/2
        AllFungi$spore_length[AllFungi$Species_names=="Pseudocercospora macarangae"]<-(20+75)/2
        
        #Simmilar problems in these ones (either the code made an error or the mycobank descripton
        #has a typo happens in the following ones):
        AllFungi$spore_length[AllFungi$Species_names=="Pseudocercosporella bambusae"]<-(20+70)/2
        AllFungi$spore_length[AllFungi$Species_names=="Alternaria citricancri"]<-(20+35)/2
        AllFungi$spore_length[AllFungi$Species_names=="Polyporus unilaterus"]<-(15+17)/2
        AllFungi$spore_length[AllFungi$Species_names=="Sebacina dubia"]<-(4+8.5)/2
        AllFungi$spore_length[AllFungi$Species_names=="Ascochyta lobikii"]<-(13+15)/2
        AllFungi$spore_length[AllFungi$Species_names=="Rhabdospora viciae"]<-(12+17)/2#Mycobank description (tranlated from russian) states 1217
        AllFungi$spore_length[AllFungi$Species_names=="Ascochyta cytisi"]<-(9+15)/2
        AllFungi$spore_length[AllFungi$Species_names=="Ascochyta tarda"]<-(9+14)/2
        AllFungi$spore_length[21127]<-(8+13)/2#For Subramaniula obscura, chlamydospore
        AllFungi$spore_length[AllFungi$Species_names=="Ascochyta greenei"]<-(8+10)/2
        AllFungi$spore_length[AllFungi$Species_names=="Ascochyta solidaginis"]<-(8+10)/2
        AllFungi[9290,c(2,3,5)]<-c(NA,NA,NA)#There are no ascospores for Penicillium manginii
        AllFungi[16510,c(2,3,5)]<-c(NA,NA,NA)#There are no condia for Lophodermium jiangnanense
        
        #It could be that all entries from Ascochyta are wrong, however I could not be certain
        #if this is the case for all Ascochyta´s above 500um remaining as spore length. I am changing them
        #but one would need to double check:
        AllFungi$spore_length[AllFungi$Species_names=="Ascochyta valerianae"]<-(7+12)/2
        AllFungi$spore_length[AllFungi$Species_names=="Ascochyta boni-henrici"]<-(6+12)/2
        AllFungi$spore_length[AllFungi$Species_names=="Ascochyta zonata"]<-(6+10)/2
        
        
        
        #After checking a couple of times, I will also remove all instances of Penicillium sps having
        #ascospores higher 100um lenght
        AllFungi[which(grepl("Penicillium",AllFungi$Species_names)&AllFungi$spore_length>100),]
        AllFungi[which(grepl("Penicillium",AllFungi$Species_names)&AllFungi$spore_length>100),3]<-c(rep(NA,6))
        AllFungi[which(grepl("Penicillium",AllFungi$Species_names)&AllFungi$spore_length>100),5]<-c(rep(NA,6))
        AllFungi[which(grepl("Penicillium",AllFungi$Species_names)&AllFungi$spore_length>100),2]<-c(rep(NA,6))
        
        #Similar problems with spore width:
        #For Phaeodimeriella ctenotricha (and the other that seems like a typo), there is again a mistake. Check: Revision of genera in Perisporiopsidaceae
        #and Pseudoperisporiaceae and other Ascomycota genera incertae sedis Mycosphere 8(10): 1695–1801 (2017
        AllFungi$spore_width[AllFungi$Species_names=="Phaeodimeriella cetotricha"]<-(12+15)/2
        AllFungi$spore_width[AllFungi$Species_names=="Phaeodimeriella ctenotricha"]<-(12+15)/2
        AllFungi$spore_width[AllFungi$Species_names=="Rhinotrichum pulchrum"]<-(11+12.5)/2#Clearly there is a missing dash in the description on the website
        AllFungi$spore_width[AllFungi$Species_names=="Alternaria herbiphorbicola"]<-(8+11)/2
        AllFungi$spore_width[AllFungi$Species_names=="Ascochyta sesleriae"]<-(8+10)/2
        
        #I could not check Microsphaera miurae and Erysiphe miurae but I will also modify them
        AllFungi$spore_width[AllFungi$Species_names=="Microsphaera miurae"]<-(11+15)/2#Clearly there is a missing dash in the description on the website
        AllFungi$spore_width[AllFungi$Species_names=="Erysiphe miurae"]<-(11+15)/2#Clearly there is a missing dash in the description on the website
        
        
        #Updating the SporeArea entry:
        AllFungi$SporeArea<-AllFungi$spore_length*AllFungi$spore_width*
          (pi/4)
        
        #Further checks
        AllFungi%>%
          filter(!is.na(spore_length))%>%
          #filter(Phylum!="Glomeromycota")%>%
          #filter(grepl("Penicillium",Species_names))%>%
          #arrange(desc(spore_length))
          arrange(desc(SporeArea))
          #arrange(desc(spore_width))%>%
          head
        
        summary(AllFungi$spore_length)
        
        hist(AllFungi$spore_length)
        boxplot(AllFungi$spore_length)
        boxplot(AllFungi$SporeArea)
        
        
        FranzKrah_sporeValues%>%
          arrange(desc())
        #I am still not conviced that the all fungi (not AMF) that are in the top 20 are all good entries
        #one would need to double check these cases
        
        #Also, words that indicate that the entry is wrong: absent, not observed
        
        AllFungi[which(AllFungi$spore_length==max(AllFungi$spore_length,na.rm = T)),]
        
#Storing the data
#On March 7th 2019 I save a version of All fungi that includes all 21455 entries from 
#Franz_Krah_sporeValues. I did that because I need to send a version to Moises 
#for getting the matches in FunGuild since he has a code for that. I think that for
#that purpose it makes sense to use all names there since I have no 

        
#Changes in the dataset after March 7th
#On March 12th I decided to add genera to the dataset in order to assing 
t<-strsplit(AllFungi$Species_names," ")
AllFungi$Genus<-sapply(t,function(x)x[[1]]);rm(t)

#The fungi with teliospores belonging to the Ascomycota are weird. This should be checked!:        
AllFungi[AllFungi$SporeName=="Teliospore"&
           AllFungi$Phylum=="Ascomycota",]#They are Bulgaria pura,Torula carbonaria,Gloeosporium succineum, Peziza pura
        

#Adding the codes 


write.csv(AllFungi,"AllFungi.csv",row.names = F)
saveRDS(AllFungi, file = "SporeData_Carlos.rds")







###
##Random stuff
###############################################################################
#Can u run:
  
RCurl ::url.exists("r-project.org")
RCurl::getURL("http://nt.ars-grin.gov/fungaldatabases/index.cfm")

RCurl::getURL("https://www.google.com")

xml2::read_html("https://nt.ars-grin.gov/fungaldatabases/index.cfm")

pack <- available.packages()
pack[grep("rusda", rownames(pack)), "Imports"]
library(XML)
library(httr)
library(plyr)
library(foreach)
library(stringr)
library(taxize)

#Type
fix(associations) 
# Outcomment or delete
# "if (!is.character(getURL("https://nt.ars-grin.gov/fungaldatabases/index.cfm"))) 
# stop(" Database is not available : http://nt.ars-grin.gov/fungaldatabases/index.cfm”)"


##Old import of AMF data. I stopped using this one because it is better to use the one
#used in the ISME paper for AMF size.

#AMF_All_Copy<-read.csv("C:\\Users\\Carlos\\Documents\\Professional\\Spore CommunityAnalysis\\SporeSize\\AMF_Spore_Database_Volume.csv",
#header = T, stringsAsFactors = F)

#UDATING THE SPORE AREA COLUMN given the previous updates in data entry
# AMF_All_Copy$SporeArea<-
#   ((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/4)*
#   ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/4)*
#   pi
# 
# #Caluculating Volume 
# #For this it is assumed that fungal spores are prolate spheroids, which means
# #two things. First, that the smallest diameter will be consider the "equatorial axis" and the largest
# #is the "polar axis"; second, that the there are two equatorial axis of the same length. This is 
# #important because the formula to calculate volume make distinction between the two type of axis.
# #In this way, AMF spore can be only of two types, perfect spheres (in which case equatorial and 
# #polar axis are of the same lenght), corrsponding to "globose" spores. And, spheroids similar to an
# #american football.  A morphology like a "flying soucer" is not possible following this rules.
# 
# 
# AMF_All_Copy$EquatorialAxis<-NaN
# AMF_All_Copy$EquatorialAxis[
#   which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
#           ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
#   ((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)[
#     which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
#             ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
# AMF_All_Copy$EquatorialAxis[which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
#                                     ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
#   ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
#     which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
#             ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
# AMF_All_Copy$EquatorialAxis[which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
#                                     ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
#   ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
#     which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
#             ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
# 
# AMF_All_Copy$PolarAxis<-NaN
# AMF_All_Copy$PolarAxis[
#   which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
#           ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
#   ((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)[
#     which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)>
#             ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
# AMF_All_Copy$PolarAxis[
#   which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
#           ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
#   ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
#     which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)<
#             ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
# AMF_All_Copy$PolarAxis[
#   which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
#           ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]<-
#   ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2)[
#     which(((AMF_All_Copy$dim1.min+AMF_All_Copy$dim1.max)/2)==
#             ((AMF_All_Copy$dim2.min+AMF_All_Copy$dim2.max)/2))]
# 
# which(AMF_All_Copy$EquatorialAxis>AMF_All_Copy$PolarAxis)#This should always be zero (0)
# 
# #Now calculating volume
# AMF_All_Copy$SporeVolume<-(pi*(AMF_All_Copy$EquatorialAxis^2)*(AMF_All_Copy$PolarAxis))/6
#Adding the phylum
#AMF_All_Copy$Phylum<-"Glomeromycota"


#The bit to rbind:
# data.frame(Species_names=AMF_All_Copy[,1],
#            spore_length=AMF_All_Copy[,34],
#            spore_width=AMF_All_Copy[,33],
#            SporeArea=AMF_All_Copy[,15],
#            SporeName="Azygospores",
#            Phylum=AMF_All_Copy[,36],
#            Source="Aguilar_etal_2018"),