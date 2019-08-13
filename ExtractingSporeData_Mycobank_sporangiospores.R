####################################################################################
###########################   (A)SPORANGIOSPORE    #################################
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

#1. Zygospores: this applies only to the zygomycota subset of the Data

#Subsetting only non-ascomycete species producing zygospores

Zygomycetes<-spore.dat[!spore.dat$Phylum %in% c(" Ascomycota", " Basidiomycota"),]#Retruning 54011 entries

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
# Zygomycetes<-Zygomycetes[-which(is.na(Zygomycetes$Phylum)),]#This reduces it to 45416entries

textos<-Zygomycetes$description_description_
names(textos)<-paste(Zygomycetes$base_name, Zygomycetes$base__id, Zygomycetes$description__id, sep ="_")

#Now I can extract Sporangiospores out of these subset

Sporangiospores_text<-
  lapply(textos,get_text,
         start.regex="porangiospore",
         end.regex="µm"#,
  )

# temp <- Sporangiospores_text
Sporangiospores_text<-lapply(Sporangiospores_text, 
                        function(x)gsub('\\([a-zA-Z]+\\. [0-9]{+}-[0-9]{1,}\\)', '', x))
Sporangiospores_text<-lapply(Sporangiospores_text, 
                        function(x)gsub('\\([a-zA-Z]+\\. [0-9]+\\,[0-9]+\\)', '', x))
Sporangiospores_text<-lapply(Sporangiospores_text, 
                        function(x)gsub('\\([a-zA-Z]+\\. [0-9]+\\)', '', x))
Sporangiospores_text<-lapply(Sporangiospores_text, 
                        function(x)gsub('－', '-', x))
Sporangiospores_text<-lapply(Sporangiospores_text, 
                        function(x)gsub('−', '-', x))


#Extracting Sporangiospores values
Sporangiospores_values<-
  lapply(Sporangiospores_text,get_dimensions2,
         extract.regex="[^a-wy-zA-WY-Z]+\\s?µm")
#Sporangiospores_values<-Sporangiospores_values$`1`

#Reformatting it into a dataframe

values<- lapply(Sporangiospores_values, function(x) x[[1]])

## restructure data as table
values2 <- list()#it took ~30 seconds to run it
for(i in 1:length(values)){
  x <- plyr::rbind.fill(lapply(values[[i]], function(y) { as.data.frame(t(y), stringsAsFactors=FALSE) }))
  x <- cbind(spore_type = names(values[[i]]), x)
  values2[[i]] <- x
}
names(values2) <- names(values)

#It seems one does not need this for Sporangiospores:
#q<-which(sapply(values2,is.null))#for some reasons some entries are empty

## combine all spore results in one table
#values_df <- plyr::rbind.fill(lapply(values2[-q], function(y) { as.data.frame(y) }))
values_df <- plyr::rbind.fill(lapply(values2, function(y) { as.data.frame(y) }))
#values_df<- cbind(spec=rep(names(values2[-q]),lapply(values2[-q], nrow)),values_df)
values_df<- cbind(spec=rep(names(values2),lapply(values2, nrow)),values_df)
#values_df <- values_df[,-length(values_df)]
names(values_df)[3] <- "measure_orig"


#Merge the text with the values and spore data info
text<-lapply(Sporangiospores_text,plyr::ldply, rbind)
text<-plyr::rbind.fill(text)
names(text)[1]<-"text_entry"

#Creation of the object "Zygospores" containing all the data
Sporangiospores<-cbind(text,values_df)#For some reason the transformations above return 47032, instead of 45416 elements that has Sporangiospores_text and Sporangiospores_values. However, it seems fine!
Sporangiospores<-data.frame(
  sapply(Sporangiospores, as.character),
  stringsAsFactors = F)#This creates a dataframe conatianing 47032 entries
#Removing empty entries
Sporangiospores<-Sporangiospores[-which(
  Sporangiospores$text_entry=="Result not found"),]#Which is actually the majority of cases: 37768!
#Removing them reduces the dataframe to 9264 entries

t<-sapply(list(Sporangiospores$text_entry), nchar)
#Just standarizing the "x"
Sporangiospores$measure_orig<-gsub("X","x",Sporangiospores$measure_orig)
Sporangiospores$measure_orig <- gsub(' [[:punct:]] ', ' x ', Sporangiospores$measure_orig)
Sporangiospores$measure_orig <- gsub('\\s+x\\s+', ' x ', Sporangiospores$measure_orig)
Sporangiospores$measure_orig <- gsub('[[:punct:]]  ', '', Sporangiospores$measure_orig)
Sporangiospores$measure_orig <- gsub('  ', ' x ', Sporangiospores$measure_orig)
Sporangiospores$measure_orig <- gsub(' ', ' x ', Sporangiospores$measure_orig)
Sporangiospores$measure_orig <- gsub('[[:space::]]{2,}', ' x ', Sporangiospores$measure_orig)
Sporangiospores$measure_orig <- gsub('^x', '', Sporangiospores$measure_orig)


####  Extracting the spore ranges  ######

t<-strsplit(Sporangiospores$measure_orig,"x")

s<-sapply(t,length)
temp<-plyr::rbind.fill(lapply(t, function(y) { as.data.frame(t(y)) }))

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
temp <- apply(temp, 2, function(x)gsub('\\[p\\. [0-9]+\\]', '', x))
temp <- apply(temp, 2, str_trim)

temp <- apply(temp, 2, function(x){
  t <- strsplit(x, '-')
  t1 <- c()
  sapply(t, function(x){
    if(is.na(x[1])) t1<-NA else if(length(x) %in% seq(1, 21, 2)) t1<-as.numeric(x[length(x)/2 + 0.5]) else t1<-mean(as.numeric(c(x[length(x)/2], x[length(x)/2 + 1])))
    return(t1)
  })
})

Sporangiospores <- cbind(Sporangiospores, temp)
Sporangiospores <- Sporangiospores %>% 
  rename(Dim1 = V1, 
         Dim2 = V2, 
         Dim3 = V3)


### Manual changes ###
dim1<-c(11.5, 16.2)
dim2<-c(6.8,8.4)
Sporangiospores$Dim1[c(1,560)]<-mean(dim1)
Sporangiospores$Dim2[c(1,560)]<-mean(dim2)
rm(dim1,dim2)

Sporangiospores$Dim1[Sporangiospores$spec=="Circinella linderi_10326_34591"]<-(6+10)/2
Sporangiospores$Dim2[Sporangiospores$spec=="Circinella linderi_10326_34591"]<-(5+7.5)/2
Sporangiospores$Dim1[Sporangiospores$spec=="Fennellomyces linderi_10325_34591"]<-(6+10)/2
Sporangiospores$Dim2[Sporangiospores$spec=="Fennellomyces linderi_10325_34591"]<-(5+7.5)/2

#Exclude data with dimensions above 200um
Sporangiospores<-Sporangiospores[which(Sporangiospores$Dim1<=200),]

#Check spore dimensions between 100 and 200um and exclude data that do not correspond to spore size (perithecia, asci...)
b<-c(3015,3374) #rownames to exclude
Sporangiospores<-Sporangiospores[!rownames(Sporangiospores) %in% b,] #Exclude from dataframe
rm(b)

Sporangiospores$Dim1[rownames(Sporangiospores)=='360']<-'8.1';Sporangiospores$Dim2[rownames(Sporangiospores)=='360']<-'4.05'
Sporangiospores$Dim1[rownames(Sporangiospores)=='3818']<-'8.1';Sporangiospores$Dim2[rownames(Sporangiospores)=='3818']<-'4.05'

Sporangiospores$Dim1[rownames(Sporangiospores)=='2573']<-'6.5';Sporangiospores$Dim2[rownames(Sporangiospores)=='2573']<-'4.5'
Sporangiospores$Dim1[rownames(Sporangiospores)=='3716']<-'6.5';Sporangiospores$Dim2[rownames(Sporangiospores)=='3716']<-'4.5'
Sporangiospores$Dim1[rownames(Sporangiospores)=='4286']<-'6.5';Sporangiospores$Dim2[rownames(Sporangiospores)=='4286']<-'4.5'

Sporangiospores$Dim1[rownames(Sporangiospores)=='4387']<-'97.4';Sporangiospores$Dim2[rownames(Sporangiospores)=='4387']<-'6'


### write to file
write.csv(Sporangiospores, 'output/sporangiospores_mycobank.csv', row.names=F)
