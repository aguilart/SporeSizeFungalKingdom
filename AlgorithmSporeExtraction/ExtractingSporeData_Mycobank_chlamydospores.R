####################################################################################
###########################  CHLAMYDOSPORES ########################################
####################################################################################

rm(list=ls())

library(tidyverse)

source('General_dimensionExtractionFunct.R')#!!!!!!!!!!!NOTE!!!!!!!!!!!!!!!!! 
#NOTE!!!!! It could be that running the functions via "source" could alter some features of the functions!!!
#Thus, better run the functions directly from the script


#Source of the spore data data:
spore.dat<- readRDS("mycobank_descriptions_mod.RDS")#This dataset
#contains 117,481 species); the entry called base_mycobank_nr is 
#the mycobank code as it is found when checking in the website and it is the table
#that Will sent us on November 2018, then slighlty modified to standardized the "µm" symbol

#source of taxonomic data:
Mycobank_Taxonomy <- read.csv('Mycobank_Taxonomy.csv', stringsAsFactors=F)#This dataframe was made on the "Checking_Taxonomy.R" code and 
#reflects the the higher rank taxonomy of species and infraspecies of true fungi

spore.dat$base_mycobanknr_ <- as.numeric(spore.dat$base_mycobanknr_)

spore.dat<-left_join(spore.dat,Mycobank_Taxonomy[c(4,12:19)],
                     by="base_mycobanknr_")

#1. Chlamydospores: This goes for all the database because conida occurs Kingdom-wise


# #Just checking what did get NA:
# p<-spore.dat[which(is.na(spore.dat$Phylum)),]
# p<-p[grep("\\w\\s\\w",p$base_name),]
# p<-p[,c(1:28)]
# p<-left_join(p,MycobankNames_list[,c(5,7)]%>%
#                rename(base_mycobanknr_=MycoBank__)%>%
#                mutate(base_mycobanknr_=as.character(base_mycobanknr_)),
#              by="base_mycobanknr_")
# p<-p[,c(1:4,29,5:28)]
# p<-p[grep("Fungi",p$Classification),]
# p<-p[-grep("Fossil",p$Classification),]
# #Out of this excericse I got that NA was attributed to only 175 species
# #all of which are Incertae sedis. For the moment I will just ignore these
# #species because they are so few.
# rm(p)
# Ascomycetes<-Ascomycetes[-which(is.na(Ascomycetes$Phylum)),]#This reduces it to 45416entries

textos<-spore.dat$description_description_
names(textos)<-paste(spore.dat$base_name, spore.dat$base__id, spore.dat$description__id, sep ="_")

#[c(315 , 821 ,1645, 1694, 1708, 1724, 1801, 2054, 2204, 2524)]
#Now I can extract Chlamydospores out of these subset

Chlamydospores_text<-
  lapply(textos,get_text,
         start.regex="hlamydospores$",
         end.regex="µm"#,
  )

# temp <- Chlamydospores_text

Chlamydospores_text<-lapply(Chlamydospores_text, 
                           function(x)gsub('\\([a-zA-Z]+\\. [0-9]{+}-[0-9]{1,}\\)', '', x))
Chlamydospores_text<-lapply(Chlamydospores_text, 
                           function(x)gsub('\\([a-zA-Z]+\\. [0-9]+\\,[0-9]+\\)', '', x))
Chlamydospores_text<-lapply(Chlamydospores_text, 
                           function(x)gsub('\\([a-zA-Z]+\\. [0-9]+\\)', '', x))
Chlamydospores_text<-lapply(Chlamydospores_text, 
                           function(x)gsub('－', '-', x))
Chlamydospores_text<-lapply(Chlamydospores_text, 
                           function(x)gsub('−', '-', x))
Chlamydospores_text<-lapply(Chlamydospores_text, 
                     function(x)gsub(' x ca ', ' x ', x))

#Extracting Chlamydospores values
Chlamydospores_values<-#Chlamydospores_text[[632]] Chlamydospores$text_entry_temp[128]
  lapply(Chlamydospores_text,get_dimensions2,
         extract.regex="[^a-wy-zA-WY-Z]+\\s?µm")
#Chlamydospores_values<-Chlamydospores_values$`1`

#textos<-Chlamydospores$text_entry_temp[128]

#Reformatting it into a dataframe

values<- lapply(Chlamydospores_values, function(x) x[[1]])

## restructure data as table
values2 <- list()#it took ~30 seconds to run it
for(i in 1:length(values)){
  x <- plyr::rbind.fill(lapply(values[[i]], function(y) { as.data.frame(t(y), stringsAsFactors=FALSE) }))
  x <- cbind(spore_type = names(values[[i]]), x)
  values2[[i]] <- x
}
names(values2) <- names(values)

#It seems one does not need this for Chlamydospores:
#q<-which(sapply(values2,is.null))#for some reasons some entries are empty

## combine all spore results in one table
#values_df <- plyr::rbind.fill(lapply(values2[-q], function(y) { as.data.frame(y) }))
values_df <- plyr::rbind.fill(lapply(values2, function(y) { as.data.frame(y) }))
#values_df<- cbind(spec=rep(names(values2[-q]),lapply(values2[-q], nrow)),values_df)
values_df<- cbind(spec=rep(names(values2),lapply(values2, nrow)),values_df)
#values_df <- values_df[,-length(values_df)]
names(values_df)[3] <- "measure_orig"


#Merge the text with the values and spore data info
text<-lapply(Chlamydospores_text,plyr::ldply, rbind)
text<-plyr::rbind.fill(text)
names(text)[1]<-"text_entry"

# temp_text<-lapply(temp,plyr::ldply, rbind)
# temp_text<-plyr::rbind.fill(temp_text)
# names(temp_text)[1]<-"text_entry_temp"

#Creation of the object "Chlamydospores" containing all the data
# Chlamydospores<-cbind(temp_text,text,values_df)#For some reason the transformations above return 47032, instead of 45416 elements that has Chlamydospores_text and Chlamydospores_values. However, it seems fine!
Chlamydospores<-cbind(text,values_df)#For some reason the transformations above return 47032, instead of 45416 elements that has Chlamydospores_text and Chlamydospores_values. However, it seems fine!
Chlamydospores<-data.frame(
  sapply(Chlamydospores, as.character),
  stringsAsFactors = F)#This creates a dataframe conatianing 47032 entries
#Removing empty entries
Chlamydospores<-Chlamydospores[-which(
  Chlamydospores$text_entry=="Result not found"),]#Which is actually the majority of cases: 37768!
#Removing them reduces the dataframe to 9264 entries


#s<-sapply(list(Chlamydospores_long$text_entry), nchar)

#Just standarizing the "x"

Chlamydospores$measure_orig<-gsub("X","x",Chlamydospores$measure_orig)
Chlamydospores$measure_orig <- gsub(' [[:punct:]] ', ' x ', Chlamydospores$measure_orig)
Chlamydospores$measure_orig <- gsub('\\s+x\\s+', ' x ', Chlamydospores$measure_orig)
Chlamydospores$measure_orig <- gsub('[[:punct:]]  ', '', Chlamydospores$measure_orig)
Chlamydospores$measure_orig <- gsub('  ', ' x ', Chlamydospores$measure_orig)
Chlamydospores$measure_orig <- gsub(' ', ' x ', Chlamydospores$measure_orig)
Chlamydospores$measure_orig <- gsub('[[:space::]]{2,}', ' x ', Chlamydospores$measure_orig)
Chlamydospores$measure_orig <- gsub('^x', '', Chlamydospores$measure_orig)


####  Extracting the spore ranges  ######

t<-strsplit(Chlamydospores$measure_orig,"x")

s<-sapply(t,length)
temp<-plyr::rbind.fill(lapply(t, function(y) { as.data.frame(t(y)) }))

# x<-Chlamydospores$text_entry[Chlamydospores$spec=="Cercospora zygophylli_113672"]
# temp[Chlamydospores$spec=="Cercospora zygophylli_113672",]


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
temp <- apply(temp, 2, str_trim)
#temp <- apply(temp, 2, function(x)gsub('µ', '', x))#For some reason, for Chlamydospores this line has to be added

temp <- apply(temp, 2, function(x){
  t <- strsplit(x, '-')
  t1 <- c()
  sapply(t, function(x){
    if(is.na(x[1])) t1<-NA else if(length(x) %in% seq(1, 21, 2)) t1<-as.numeric(x[length(x)/2 + 0.5]) else t1<-mean(as.numeric(c(x[length(x)/2], x[length(x)/2 + 1])))
    return(t1)
  })
})

Chlamydospores <- cbind(Chlamydospores, temp)
Chlamydospores <- Chlamydospores %>% 
  rename(Dim1 = V1, 
         Dim2 = V2, 
         Dim3 = V3)


### Any manual changes needed, do below ###
# for example
# Chlamydospores$Dim1[...]<- ...
# Chlamydospores$Dim2[...]<- ...


Chlamydospores$Dim1[which(Chlamydospores$spec=="Umbelopsis nana_28349_71232")]<-(4+9)/2  
Chlamydospores$Dim1[which(Chlamydospores$spec=="Umbelopsis nana_28349_71232")]<-(6+9)/2  
                          
Chlamydospores$Dim1[which(Chlamydospores$spec=="Raffaelea scolytodis_451037_26125")]<-13
  Chlamydospores$Dim2[which(Chlamydospores$spec=="Raffaelea scolytodis_451037_26125")]<-5
  
Chlamydospores<-Chlamydospores[-which(Chlamydospores$spec=="Staphylotrichum indicum_551131_71816"),]

Chlamydospores<-Chlamydospores[-which(Chlamydospores$spec=="Cyrenella elegans_7697_6817")[1],]
Chlamydospores$Dim1[Chlamydospores$spec=="Cyrenella elegans_7697_6817"]<-6

#Removing cases where Chlamydospores are absent
Chlamydospores<-
Chlamydospores[-grep("hlamydospores absent",Chlamydospores$text_entry),]

Chlamydospores<-
  Chlamydospores[-grep("hlamydospores not observed",Chlamydospores$text_entry),]

# Chlamydospores<-
#   Chlamydospores[-grep("hlamydospores not present",Chlamydospores$text_entry),]

Chlamydospores<-
  Chlamydospores[-grep("hlamydospores none",Chlamydospores$text_entry),]

Chlamydospores<-
  Chlamydospores[-grep("hlamydospores lacking",Chlamydospores$text_entry),]

Chlamydospores<-
  Chlamydospores[-grep("hlamydospores are not produced",Chlamydospores$text_entry),]

Chlamydospores<-
  Chlamydospores[-grep("hlamydospores were absent",Chlamydospores$text_entry),]

Chlamydospores<-
  Chlamydospores[-grep("asidiospores",Chlamydospores$text_entry),]

Chlamydospores<-
  Chlamydospores[-grep("scoma",Chlamydospores$text_entry),]

Chlamydospores<-
  Chlamydospores[-grep("\\. Conidi",Chlamydospores$text_entry),]

Chlamydospores<-
  Chlamydospores[-grep("\\. Hyph",Chlamydospores$text_entry),]

Chlamydospores$text_entry[grep("with l/w",Chlamydospores$text_entry)]<-"Chlamydospores first noted after 6-8 d, scanty, mainly in the center around the point of inoculation, smooth, terminal on hyphae, oval to subclavate, often truncated at one end, and intercalary in hyphal cells, oval to ellipsoidal, (6.9-)7.5-10.8(-12.9) x (5-)5.6-7.4(-8.2)  with l/w = (1.2-)1.2-1.6(-1.7) (n = 12)"
Chlamydospores$measure_orig[grep("with l/w",Chlamydospores$text_entry)]<-"(6.9-)7.5-10.8(-12.9) x (5-)5.6-7.4(-8.2)"
Chlamydospores$Dim1[grep("with l/w",Chlamydospores$text_entry)]<-(7.5+10.8)/2
Chlamydospores$Dim2[grep("with l/w",Chlamydospores$text_entry)]<-(5.6+7.4)/2

#Chlamydospores$text_entry[Chlamydospores$spec=="Sporotrichum pruinosum_26227_968"]<-"chlamydospores numerous, terminal and intercalary, globose to ovoid with walls noticeably thick and often appearing sculptured, 10.5-16.5 x 7.5-12.0 µm"
# Chlamydospores$text_entry[Chlamydospores$spec=="Haploporus fraxineus_64163_674"]<-"chlamydospores numerous, terminal and intercalary, globose to ovoid with walls noticeably thick and often appearing sculptured, 10.5-16.5 x 7.5-12.0 µm"
# Chlamydospores$measure_orig[Chlamydospores$spec=="Haploporus fraxineus_64163_674"]<-"10.5-16.5 x 7.5-12.0 µ"


#Removing very long descriptions that do not contain the right description
t<-sapply(list(Chlamydospores$text_entry), nchar)
Chlamydospores<-Chlamydospores[-which(t>500),]

#Final specific changes: 
Chlamydospores$Dim1[Chlamydospores$spec== "Trichoderma aethiopicum_481130_53233"]<-5
Chlamydospores$Dim2[Chlamydospores$spec== "Trichoderma aethiopicum_481130_53233"]<-10

Chlamydospores<-Chlamydospores[-which(Chlamydospores$Dim1==0.8),]


#

### write to file
write.csv(Chlamydospores, 'output/Chlamydospores_mycobank.csv', row.names=F)

