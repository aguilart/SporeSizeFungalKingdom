####################################################################################
###########################   BASIDIOSPORES    ########################################
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

#1. Basidiospores: this applies only to the ascomycota subset of the Data

#Subsetting only basidiomycetes

Basidiomycetes<-spore.dat[spore.dat$Phylum==" Basidiomycota",]#Retruning 54011 entries

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

textos<-Basidiomycetes$description_description_
names(textos)<-paste(Basidiomycetes$base_name, Basidiomycetes$base__id, Basidiomycetes$description__id, sep ="_")

#Now I can extract Basidiospores out of these subset

Basidiospores_text<-
  lapply(textos,get_text,
         start.regex="asidiospores$",
         end.regex="µm"#,
  )

# temp <- Basidiospores_text

Basidiospores_text<-lapply(Basidiospores_text, 
                           function(x)gsub('\\([a-zA-Z]+\\. [0-9]+\\-[0-9]+\\)', '', x))
Basidiospores_text<-lapply(Basidiospores_text, 
                           function(x)gsub('\\([a-zA-Z]+\\. [0-9]+\\,[0-9]+\\)', '', x))
Basidiospores_text<-lapply(Basidiospores_text, 
                           function(x)gsub('\\([a-zA-Z]+\\. [0-9]+\\)', '', x))
Basidiospores_text<-lapply(Basidiospores_text, 
                           function(x)gsub('－', '-', x))
Basidiospores_text<-lapply(Basidiospores_text, 
                           function(x)gsub('−', '-', x))


#Extracting Basidiospores values
Basidiospores_values<-
  lapply(Basidiospores_text,get_dimensions2,
         extract.regex="[^a-wy-zA-WY-Z]+\\s?µm")
#Basidiospores_values<-Basidiospores_values$`1`

#Reformatting it into a dataframe

values<- lapply(Basidiospores_values, function(x) x[[1]])

## restructure data as table
values2 <- list()#it took ~30 seconds to run it
for(i in 1:length(values)){
  x <- plyr::rbind.fill(lapply(values[[i]], function(y) { as.data.frame(t(y), stringsAsFactors=FALSE) }))
  x <- cbind(spore_type = names(values[[i]]), x)
  values2[[i]] <- x
}
names(values2) <- names(values)

#It seems one does not need this for Basidiospores:
#q<-which(sapply(values2,is.null))#for some reasons some entries are empty

## combine all spore results in one table
#values_df <- plyr::rbind.fill(lapply(values2[-q], function(y) { as.data.frame(y) }))
values_df <- plyr::rbind.fill(lapply(values2, function(y) { as.data.frame(y) }))
#values_df<- cbind(spec=rep(names(values2[-q]),lapply(values2[-q], nrow)),values_df)
values_df<- cbind(spec=rep(names(values2),lapply(values2, nrow)),values_df)
#values_df <- values_df[,-length(values_df)]
names(values_df)[3] <- "measure_orig"


#Merge the text with the values and spore data info
text<-lapply(Basidiospores_text,plyr::ldply, rbind)
text<-plyr::rbind.fill(text)
names(text)[1]<-"text_entry"

#Creation of the object "Basidiospores" containing all the data
Basidiospores<-cbind(text,values_df)#For some reason the transformations above return 47032, instead of 45416 elements that has Basidiospores_text and Basidiospores_values. However, it seems fine!
Basidiospores<-data.frame(
  sapply(Basidiospores, as.character),
  stringsAsFactors = F)#This creates a dataframe conatianing 47032 entries
#Removing empty entries
Basidiospores<-Basidiospores[-which(
  Basidiospores$text_entry=="Result not found"),]#Which is actually the majority of cases: 37768!
#Removing them reduces the dataframe to 9264 entries

t<-sapply(list(Basidiospores$text_entry), nchar)
#Just standarizing the "x"
Basidiospores$measure_orig <- gsub("X","x",Basidiospores$measure_orig)
Basidiospores$measure_orig <- gsub(' [[:punct:]] ', ' x ', Basidiospores$measure_orig)
Basidiospores$measure_orig <- gsub('\\s+x\\s+', ' x ', Basidiospores$measure_orig)
Basidiospores$measure_orig <- gsub('[[:punct:]]  ', '', Basidiospores$measure_orig)
Basidiospores$measure_orig <- gsub('  ', ' x ', Basidiospores$measure_orig)
Basidiospores$measure_orig <- gsub(' ', ' x ', Basidiospores$measure_orig)
Basidiospores$measure_orig <- gsub('[[:space::]]{2,}', ' x ', Basidiospores$measure_orig)


####  Extracting the spore ranges  ######

t<-strsplit(Basidiospores$measure_orig,"x")

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

Basidiospores <- cbind(Basidiospores, temp)
Basidiospores <- Basidiospores %>% 
  rename(Dim1 = V1, 
         Dim2 = V2, 
         Dim3 = V3, 
         Dim4 = V4)


### Any manual changes needed, do below ###

# for example
# Basidiospores$Dim1[...]<- ...
# Basidiospores$Dim2[...]<- ...
# for example
Basidiospores[grep('202783', Basidiospores$spec), c('Dim1', 'Dim2')] <- c(18.2, 17.1)
Basidiospores[grep('440865', Basidiospores$spec), c('Dim1', 'Dim2')] <- c(18.2, 17.1)
Basidiospores[grep('508322', Basidiospores$spec), c('Dim1', 'Dim2')] <- c(18.2, 17.1)
Basidiospores[grep('512359', Basidiospores$spec), c('Dim1', 'Dim2')][1,] <- c(9, 3.25)
Basidiospores[grep('88034', Basidiospores$spec), c('Dim1', 'Dim2')][1,] <- c(5.4, 2.6)
Basidiospores[grep('88034', Basidiospores$spec), c('Dim1', 'Dim2')][2,] <- c(5, 2)

Basidiospores[grep('172826', Basidiospores$spec), c('Dim1', 'Dim2')][1,] <- c(5.4, 2.6)
Basidiospores[grep('172826', Basidiospores$spec), c('Dim1', 'Dim2')][2,] <- c(5, 2)
Basidiospores[grep('74295', Basidiospores$spec), c('Dim1')] <- 16
Basidiospores[grep('86825', Basidiospores$spec), c('Dim1')] <- 16
Basidiospores[grep('74294', Basidiospores$spec), c('Dim1')] <- 16
Basidiospores[grep('82226', Basidiospores$spec), c('Dim1')] <- 16

Basidiospores[grep('71208', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(4.4, 4.3)
Basidiospores[grep('20446', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(4.4, 4.3)
Basidiospores[grep('79426', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(4.4, 4.3)
Basidiospores[grep('71207', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(4.4, 4.3)
Basidiospores[grep('80076', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(4.4, 4.3)
Basidiospores[grep('71209', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(4.4, 4.3)
Basidiospores[grep('82125', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(4.4, 4.3)


Basidiospores[grep('15229', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(6.5, 2.75)
Basidiospores[grep('15228', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(6.5, 2.75)
Basidiospores[grep('14808', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(6.5, 2.75)
Basidiospores[grep('73957', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(6.5, 2.75)
Basidiospores[grep('85051', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(6.5, 2.75)
Basidiospores[grep('410125', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(6.5, 2.75)

Basidiospores[grep('508825', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(7.5, 8)
Basidiospores[grep('508826', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(8.8, NA)
Basidiospores[grep('508828', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(8.5, 10)
Basidiospores[grep('508824', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(8.25, NA)


Basidiospores[grep('264921_17429', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(8.85, 5.35)
Basidiospores[grep('264922_17429', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(8.85, 5.35)
Basidiospores[grep('411956_17429', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(8.85, 5.35)



Basidiospores[grep('85846_5408', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(13, 11)
Basidiospores[grep('67940_5408', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(13, 11)
Basidiospores[grep('68652_5408', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(13,11)
Basidiospores[grep('82139_5408', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(13, 11)
Basidiospores[grep('73554_5408', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(13, 11)
Basidiospores[grep('71145_5408', Basidiospores$spec),  c('Dim1', 'Dim2')][1,] <- c(13, 11)


# large values:
# filter(Basidiospores, Dim1 > 500)

Basidiospores$Dim1[c(1196, 8472)] <- 16
Basidiospores[14921, c('Dim1', 'Dim2')] <- c(18.5, 6.5)
Basidiospores$Dim1[6825] <- 9.25
Basidiospores[11300, c('Dim1', 'Dim2')] <- c(4.5, 4)


### write to file
write.csv(Basidiospores, 'output/basidiospores_mycobank.csv', row.names=F)
