####################################################################################
###########################   CONIDIA      ########################################
####################################################################################

rm(list=ls())

library(tidyverse)

source('General_dimensionExtractionFunct.R')

#Source of the spore data data:
spore.dat<- readRDS("mycobank_descriptions.RDS")#This dataset
#contains 117,481 species); the entry called base_mycobank_nr is 
#the mycobank code as it is found when checking in the website and it is the table
#that Will sent us on November 2019

#source of taxonomic data:
Mycobank_Taxonomy <- read.csv('Mycobank_Taxonomy.csv', stringsAsFactors=F)#This dataframe was made on the "Checking_Taxonomy.R" code and 
#reflects the the higher rank taxonomy of species and infraspecies of true fungi

spore.dat$base_mycobanknr_ <- as.numeric(spore.dat$base_mycobanknr_)

spore.dat<-left_join(spore.dat,Mycobank_Taxonomy[c(4,12:19)],
                     by="base_mycobanknr_")

#1. Conidia: This goes for all the database because conida occurs Kingdom-wise


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


#Now I can extract Conidia out of these subset

Conidia_text<-
  lapply(textos,get_text,
         start.regex="onidia$",
         end.regex="µm"#,
  )

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

#Extracting Conidia values
Conidia_values<-#Conidia_text[[632]] Conidia$text_entry_temp[128]
  lapply(Conidia_text,get_dimensions2,
         extract.regex="[^a-wy-zA-WY-Z]+\\s?µm")
#Conidia_values<-Conidia_values$`1`

#txt<-Conidia$text_entry_temp[128]

#Reformatting it into a dataframe

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

temp_text<-lapply(temp,plyr::ldply, rbind)
temp_text<-plyr::rbind.fill(temp_text)
names(temp_text)[1]<-"text_entry_temp"

#Creation of the object "Conidia" containing all the data
Conidia<-cbind(temp_text,text,values_df)#For some reason the transformations above return 47032, instead of 45416 elements that has Conidia_text and Conidia_values. However, it seems fine!
Conidia<-data.frame(
  sapply(Conidia, as.character),
  stringsAsFactors = F)#This creates a dataframe conatianing 47032 entries
#Removing empty entries
Conidia<-Conidia[-which(
  Conidia$text_entry=="Result not found"),]#Which is actually the majority of cases: 37768!
#Removing them reduces the dataframe to 9264 entries

t<-sapply(list(Conidia$text_entry), nchar)
#Here we have cases where the text is very long: 
length(Conidia$spec[t>510])#Returning 1329 entries

Conidia_long<-Conidia[t>510,]#This needs to be manually checked

#s<-sapply(list(Conidia_long$text_entry), nchar)

#Just standarizing the "x"

Conidia$measure_orig<-gsub("X","x",Conidia$measure_orig)
Conidia$measure_orig <- gsub(' [[:punct:]] ', ' x ', Conidia$measure_orig)
Conidia$measure_orig <- gsub('\\s+x\\s+', ' x ', Conidia$measure_orig)
Conidia$measure_orig <- gsub('[[:punct:]]  ', '', Conidia$measure_orig)
Conidia$measure_orig <- gsub('  ', ' x ', Conidia$measure_orig)
Conidia$measure_orig <- gsub(' ', ' x ', Conidia$measure_orig)
Conidia$measure_orig <- gsub('[[:space::]]{2,}', ' x ', Conidia$measure_orig)


####  Extracting the spore ranges  ######

t<-strsplit(Conidia$measure_orig,"x")

s<-sapply(t,length)
temp<-plyr::rbind.fill(lapply(t, function(y) { as.data.frame(t(y)) }))

# x<-Conidia$text_entry[Conidia$spec=="Cercospora zygophylli_113672"]
# temp[Conidia$spec=="Cercospora zygophylli_113672",]


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
#temp <- apply(temp, 2, function(x)gsub('µ', '', x))#For some reason, for conidia this line has to be added

temp <- apply(temp, 2, function(x){
  t <- strsplit(x, '-')
  t1 <- c()
  sapply(t, function(x){
    if(is.na(x[1])) t1<-NA else if(length(x) %in% seq(1, 21, 2)) t1<-as.numeric(x[length(x)/2 + 0.5]) else t1<-mean(as.numeric(c(x[length(x)/2], x[length(x)/2 + 1])))
    return(t1)
  })
})

Conidia <- cbind(Conidia, temp)
Conidia <- Conidia %>% 
  rename(Dim1 = V1, 
         Dim2 = V2, 
         Dim3 = V3, 
         Dim4 = V4)
 
# Conidia$Dim2[which(is.na(Conidia$Dim2))][
#   -grep("x",Conidia$measure_orig[which(is.na(Conidia$Dim2))])]<-
#     Conidia$Dim1[which(is.na(Conidia$Dim2))][
#       -grep("x",Conidia$measure_orig[which(is.na(Conidia$Dim2))])]


### Any manual changes needed, do below ###

# for example
# Conidia$Dim1[...]<- ...
# Conidia$Dim2[...]<- ...


### write to file
write.csv(Conidia, 'output/conidia_mycobank.csv', row.names=F)

