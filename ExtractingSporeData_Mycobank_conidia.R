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

# temp_text<-lapply(temp,plyr::ldply, rbind)
# temp_text<-plyr::rbind.fill(temp_text)
# names(temp_text)[1]<-"text_entry_temp"

#Creation of the object "Conidia" containing all the data
# Conidia<-cbind(temp_text,text,values_df)#For some reason the transformations above return 47032, instead of 45416 elements that has Conidia_text and Conidia_values. However, it seems fine!
Conidia<-cbind(text,values_df)#For some reason the transformations above return 47032, instead of 45416 elements that has Conidia_text and Conidia_values. However, it seems fine!
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
Conidia$measure_orig <- gsub('^x', '', Conidia$measure_orig)


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

## check the large value for Dim1
Conidia[grep('_10206', Conidia$spec),  c('Dim1', 'Dim2')]<- c(70,2.5)
Conidia[grep('_9981', Conidia$spec),  c('Dim1', 'Dim2')] <- c(72.5, 4)
Conidia[grep('_10225', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(85, 3.13)
Conidia[grep('_19615', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(2.85, NA)
Conidia[grep('545342_70019', Conidia$spec),  c('Dim1', 'Dim2')][3,] <-  c(13.25, NA)
Conidia[grep('_10265', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(62.5, 4.5)
Conidia[grep('_28956', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(36.5, 17.5)
Conidia[grep('_29847', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(39.5, 16.5)
Conidia[grep('_45282', Conidia$spec),  c('Dim1', 'Dim2')][1,] <-  c(28.5, 5.75)
Conidia[grep('_43602', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(32, 16.5)
Conidia[grep('_31321', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(24, 4)
Conidia[grep('_10025', Conidia$spec),  c('Dim1', 'Dim2')][c(1, 4),] <-  c(47.5, 5.25)
Conidia[grep('_10056', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(45, 3.75)
Conidia[grep('1928', Conidia$Dim1),  c('Dim1', 'Dim2')] <-  c(23.5, 15.5)
Conidia[grep('1530', Conidia$Dim1),  c('Dim1', 'Dim2')] <-  c(22.5, 10.5)
Conidia[grep('1518', Conidia$Dim1),  c('Dim1', 'Dim2')] <-  c(16.5, 6)
Conidia[grep('1924', Conidia$Dim1),  c('Dim1', 'Dim2')] <-  c(21.5, 3)
Conidia[grep('35125', Conidia$Dim2),  c('Dim1', 'Dim2')] <-  c(3.25, 80)
Conidia[grep('20100', Conidia$Dim2),  c('Dim1', 'Dim2')] <-  c(2.75, 60)
Conidia[grep('920', Conidia$Dim2),  c('Dim1', 'Dim2')] <-  c(17, 14.5)
Conidia[grep('560.75', Conidia$Dim2),  c('Dim1', 'Dim2')] <-  c(16.25, 11)


# Note1 : the "ca" issue. when the text contain ca, 
#        such as 4.0-5.0 x ca. 1.0 µm, the code only pick up 
#        the later value, i.e. 1.0 in this case  (FIXED)


### Note 2 : when the measure_orig contained x in there, 
###          it always did not pick up the value for Dim 1 
###          and only put them into Dim2

Conidia[11272,  c('Dim1', 'Dim2')] <-  c(50, NA)
Conidia[15114,  c('Dim1', 'Dim2')] <-  c(5.3, NA)
Conidia[19580,  c('Dim1', 'Dim2')] <-  c(50, NA)
Conidia[26212,  c('Dim1', 'Dim2')] <-  c(115, NA)


## Note 3 : check the small values for Dim1 and Dim2, 
## seems the code did not extract the right value 
## and just give the wrong value, all the value 
## below than 1 um should be checked.

# head(filter(Conidia, Dim1 > 200))
# head(filter(Conidia, Dim1 < 0.2))


### write to file
write.csv(Conidia, 'output/conidia_mycobank.csv', row.names=F)

