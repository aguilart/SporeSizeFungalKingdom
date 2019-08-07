####################################################################################
###########################   (A)ZYGOSPORES    ########################################
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

#Now I can extract Zygospores out of these subset

Zygospores_text<-
  lapply(textos,get_text,
         start.regex="ygospores$",
         end.regex="µm"#,
  )

# temp <- Zygospores_text
Zygospores_text<-lapply(Zygospores_text, 
                           function(x)gsub('\\([a-zA-Z]+\\. [0-9]{+}-[0-9]{1,}\\)', '', x))
Zygospores_text<-lapply(Zygospores_text, 
                           function(x)gsub('\\([a-zA-Z]+\\. [0-9]+\\,[0-9]+\\)', '', x))
Zygospores_text<-lapply(Zygospores_text, 
                           function(x)gsub('\\([a-zA-Z]+\\. [0-9]+\\)', '', x))
Zygospores_text<-lapply(Zygospores_text, 
                           function(x)gsub('－', '-', x))
Zygospores_text<-lapply(Zygospores_text, 
                           function(x)gsub('−', '-', x))


#Extracting Zygospores values
Zygospores_values<-
  lapply(Zygospores_text,get_dimensions2,
         extract.regex="[^a-wy-zA-WY-Z]+\\s?µm")
#Zygospores_values<-Zygospores_values$`1`

#Reformatting it into a dataframe

values<- lapply(Zygospores_values, function(x) x[[1]])

## restructure data as table
values2 <- list()#it took ~30 seconds to run it
for(i in 1:length(values)){
  x <- plyr::rbind.fill(lapply(values[[i]], function(y) { as.data.frame(t(y), stringsAsFactors=FALSE) }))
  x <- cbind(spore_type = names(values[[i]]), x)
  values2[[i]] <- x
}
names(values2) <- names(values)

#It seems one does not need this for Zygospores:
#q<-which(sapply(values2,is.null))#for some reasons some entries are empty

## combine all spore results in one table
#values_df <- plyr::rbind.fill(lapply(values2[-q], function(y) { as.data.frame(y) }))
values_df <- plyr::rbind.fill(lapply(values2, function(y) { as.data.frame(y) }))
#values_df<- cbind(spec=rep(names(values2[-q]),lapply(values2[-q], nrow)),values_df)
values_df<- cbind(spec=rep(names(values2),lapply(values2, nrow)),values_df)
#values_df <- values_df[,-length(values_df)]
names(values_df)[3] <- "measure_orig"


#Merge the text with the values and spore data info
text<-lapply(Zygospores_text,plyr::ldply, rbind)
text<-plyr::rbind.fill(text)
names(text)[1]<-"text_entry"

#Creation of the object "Zygospores" containing all the data
Zygospores<-cbind(text,values_df)#For some reason the transformations above return 47032, instead of 45416 elements that has Zygospores_text and Zygospores_values. However, it seems fine!
Zygospores<-data.frame(
  sapply(Zygospores, as.character),
  stringsAsFactors = F)#This creates a dataframe conatianing 47032 entries
#Removing empty entries
Zygospores<-Zygospores[-which(
  Zygospores$text_entry=="Result not found"),]#Which is actually the majority of cases: 37768!
#Removing them reduces the dataframe to 9264 entries

t<-sapply(list(Zygospores$text_entry), nchar)
#Just standarizing the "x"
Zygospores$measure_orig<-gsub("X","x",Zygospores$measure_orig)
Zygospores$measure_orig <- gsub(' [[:punct:]] ', ' x ', Zygospores$measure_orig)
Zygospores$measure_orig <- gsub('\\s+x\\s+', ' x ', Zygospores$measure_orig)
Zygospores$measure_orig <- gsub('[[:punct:]]  ', '', Zygospores$measure_orig)
Zygospores$measure_orig <- gsub('  ', ' x ', Zygospores$measure_orig)
Zygospores$measure_orig <- gsub(' ', ' x ', Zygospores$measure_orig)
Zygospores$measure_orig <- gsub('[[:space::]]{2,}', ' x ', Zygospores$measure_orig)


####  Extracting the spore ranges  ######

t<-strsplit(Zygospores$measure_orig,"x")

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
temp <- apply(temp, 2, str_trim)

temp <- apply(temp, 2, function(x){
  t <- strsplit(x, '-')
  t1 <- c()
  sapply(t, function(x){
    if(is.na(x[1])) t1<-NA else if(length(x) %in% seq(1, 21, 2)) t1<-as.numeric(x[length(x)/2 + 0.5]) else t1<-mean(as.numeric(c(x[length(x)/2], x[length(x)/2 + 1])))
    return(t1)
  })
})

Zygospores <- cbind(Zygospores, temp)
Zygospores <- Zygospores %>% 
  rename(Dim1 = V1, 
         Dim2 = V2, 
         Dim3 = V3)



### Any manual changes needed, do below ###

# for example
# Zygospores$Dim1[...]<- ...
# Zygospores$Dim2[...]<- ...


### write to file
write.csv(Zygospores, 'output/zygospores_mycobank.csv', row.names=F)
