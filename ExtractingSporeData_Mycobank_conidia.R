####################################################################################
###########################   CONIDIA      ########################################
####################################################################################

#Note, This script was used to extract conidia from the Mycobank descriptions on August 2019. The result
#is the file output/conidia_mycobank.csv, which is the version currently used in the spore database.
#However, on January 2020 I run it in another computer and for some reason when loading the descriptiosns
#(that is,"mycobank_descriptions_mod.RDS" ) there are around 2k entries where the micrometer symbol is not
#read anymore. Thus, if you run this again the outcome will be different


rm(list=ls())

library(tidyverse)

source('General_dimensionExtractionFunct.R')#!!!!!!!!!!!NOTE!!!!!!!!!!!!!!!!! 
#NOTE!!!!! It could be that running the functions via "source" could alter some features of the functions!!!
#Thus, better run the functions directly from the script

#Source of the spore data data:
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

#I. SELECT THE APPROPRIATE DESCRIPTIONS

#For conidia, tis goes for all the database because conida occurs Kingdom-wise

textos<-spore.dat$description_description_
names(textos)<-paste(spore.dat$base_name, spore.dat$base__id, spore.dat$description__id, sep ="_")

ct<-grep("onidia|ONIDIA",textos)

#II. EXTRACT THE REGIONS OF TEXT WITH SPORE DIMENSIONS  Extract the regions of the text with the spore dimensions

#Now I can extract Conidia out of these subset

Conidia_text<-
  lapply(textos[ct],get_text,
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

#IV. EXTRACTING THE FORMATS "DIGIT x DIGIT um" CONTAINED IN THE EXTRACTED TEXT

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

Conidia_c<-Conidia


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

#There were some cases where the x is missing and instead there is a hyphen. This
#cannot be solved by normal gsub. So I had to change it via a bit more complicated way.
#The "x" separates two dimensions, in almost all cases the two dimensions follow a patter like:
# 1-2 x 1-2 . The problem 

temp_c<-temp
no_x<-strsplit(temp_c[,1],"-")
# Conidia$measure_orig[sapply(no_x,function(x)is.unsorted(as.numeric(x)))&!grepl("x",Conidia$measure_orig)]
# temp_c[sapply(no_x,function(x)is.unsorted(as.numeric(x)))&!grepl("x",Conidia$measure_orig),1]
# which(sapply(no_x,function(x)is.unsorted(as.numeric(x)))&!grepl("x",Conidia$measure_orig))

ordenando<-function(x){
  datos<-as.numeric(x)
  
      breaking<-function(datos){
          for (i in 1:(length(datos)-1)){
            if (datos[i+1]<datos[i]){
            first<-datos[1:i]
            second<-datos[i+1:(length(datos)-i)]
            datos_b<-list(first,second)
            }
            }
        return(datos_b)
      }
      
      nuevos_datos<-
        if(length(datos>2)&is.unsorted(datos)&!any(is.na(datos))){
        nuevos_datos<-breaking(datos)
        nuevos_datos<-lapply(nuevos_datos,paste,collapse="-")
        nuevos_datos<-unlist(nuevos_datos)}
        else{nuevos_datos<-paste(datos,collapse = "-")}
}

# x <- c(1:3, 1:4, 1:5)
# unname(split(x, cumsum(seq_along(x) %in% which(c(FALSE, diff(x) < 0)))))

z<-lapply(no_x,ordenando)
z_c<-plyr::rbind.fill(lapply(z, function(y) { as.data.frame(t(y)) }));z_c<-as.matrix(z_c)

temp[which(sapply(no_x,function(x)is.unsorted(as.numeric(x)))&!grepl("x",Conidia$measure_orig)),c(1,2)]<-
  z_c[which(sapply(no_x,function(x)is.unsorted(as.numeric(x)))&!grepl("x",Conidia$measure_orig)),c(1,2)]

###'

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

Conidia_c<-Conidia 

### Any manual changes needed, do below ###

# for example
# Conidia$Dim1[...]<- ...
# Conidia$Dim2[...]<- ...
#Sawadaea tulasnei_144619_29503
## check the large value for Dim1
Conidia[grep('_109869_10206', Conidia$spec),  c('Dim1', 'Dim2')]<- c(70,2.5)
Conidia[grep('428366_10206', Conidia$spec),  c('Dim1', 'Dim2')]<- c(70,2.5)
Conidia[grep('_9981', Conidia$spec),  c('Dim1', 'Dim2')] <- c(72.5,72.5,4, 4)
Conidia[grep('_10225', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(85, 85,85,3.25,3.25,3.25)
Conidia[grep('_19615', Conidia$spec),  c('Dim1', 'Dim2')][c(2,4),] <-  c(28.85,28.85,NA,NA)
Conidia[grep('545342_70019', Conidia$spec),  c('Dim1', 'Dim2')][3,] <-  c(13.25, NA)
Conidia[grep('_10265', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(62.5,62.5, 4.5,4.5)#
Conidia[grep('_28956', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(36.5, 17.5)
Conidia[grep('_29847', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(39.5,39.5,39.5,16.6, 16.5,16.5)
Conidia[grep('_45282', Conidia$spec),  c('Dim1', 'Dim2')][1,] <-  c(28.5, 5.75)
Conidia[grep('_43602', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(32, 16.5)
Conidia[grep('_31321', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(24, 4)
Conidia[grep('_10025', Conidia$spec),  c('Dim1', 'Dim2')][c(1, 4),] <-  c(47.5,47.5, 5.25,5.25)
Conidia[grep('_10056', Conidia$spec),  c('Dim1', 'Dim2')] <-  c(45, 3.75)
Conidia[grep('1928', Conidia$Dim1),  c('Dim1', 'Dim2')] <-  c(23.5,23.5,23.5,23.5,15.5,15.5,15.5,15.5)
Conidia[grep('1530', Conidia$Dim1),  c('Dim1', 'Dim2')] <-  c(22.5, 10.5)
Conidia[grep('1518', Conidia$Dim1),  c('Dim1', 'Dim2')] <-  c(16.5, 6)
Conidia[grep('1924', Conidia$Dim1),  c('Dim1', 'Dim2')] <-  c(21.5, 3)
Conidia[grep('35125', Conidia$Dim2),  c('Dim1', 'Dim2')] <-  c(3.25,3.25,3.25,3.25,80,80,80,80)
Conidia[grep('20100', Conidia$Dim2),  c('Dim1', 'Dim2')] <-  c(2.75,2.75, 60,60)
Conidia[grep('920', Conidia$Dim2),  c('Dim1', 'Dim2')] <-  c(17, 14.5)
Conidia[grep('560.75', Conidia$Dim2),  c('Dim1', 'Dim2')] <-  c(16.25,16.25,16.25, 11,11,11)


Conidia$text_entry[grep("Trichoderma evansii_453461_31385",Conidia$spec)][2]<-"Conidia subglobose, (2.7-)3.0-3.5(-4.2) x (2.5-)2.9-3.2(-3.7) mu;m"
Conidia$measure_orig[grep("Trichoderma evansii_453461_31385",Conidia$spec)][2]<-"(2.7-)3.0-3.5(-4.2) x (2.5-)2.9-3.2(-3.7)"
Conidia[grep("Trichoderma evansii_453461_31385",Conidia$spec),c('Dim1', 'Dim2')][2,]<-c(3.25,3.05)

Conidia$measure_orig[grep("23O",Conidia$text_entry)]<-"5-6 x 120-230"
#Conidia[grep("23O",Conidia$text_entry),c('Dim1', 'Dim2')]<-c(5.5,125)
Conidia$Dim1[grep("23O",Conidia$text_entry)]<-5.5
Conidia$Dim2[grep("23O",Conidia$text_entry)]<-125

Conidia$measure_orig[grep("5O",Conidia$text_entry)]<-"5-7 x 20-50"
#Conidia[grep("5O",Conidia$text_entry),c('Dim1', 'Dim2')]<-c(6,35)
Conidia$Dim1[grep("5O",Conidia$text_entry)]<-6
Conidia$Dim2[grep("5O",Conidia$text_entry)]<-35

Conidia$measure_orig[grep("20O",Conidia$text_entry)]<-"34.5 x 40-200"
#Conidia[grep("20O",Conidia$text_entry),c('Dim1', 'Dim2')]<-c(34.5,120)
Conidia$Dim1[grep("20O",Conidia$text_entry)]<-34.5
Conidia$Dim2[grep("20O",Conidia$text_entry)]<-120

Conidia$text_entry[grep("_9932$",Conidia$spec)]<-"Conidia pale olivaccous brown, cylindric to cylindro-obclavate, straight to slightly curved, 3-7 septate (shorter ones 0-1 septate), no distinct constriction at the septa, obtuse at the apex, obconically truncate at the base with a prominent hil µm, 30-74 x 3.54 µm"
Conidia$measure_orig[grep("_9932$",Conidia$spec)]<-"30-74 x 3.54"
#Conidia[grep("_9932$",Conidia$spec),c('Dim1', 'Dim2')]<-c(52,3.54)
Conidia$Dim1[grep("_9932$",Conidia$spec)]<-52
Conidia$Dim2[grep("_9932$",Conidia$spec)]<-3.54

Conidia$text_entry[grep("Pseudocercospora colocasiae_114572_9929",Conidia$spec)]<-"Conidia shortly clavate or turbinate, with a broadly rounded apex, pale olivaccous, guttulate, nearly all 3septate, sometimes 1- or 2- septate, very rarely 5-septate or with a longitudinal sept µm, usually slightly constricted, 20-45 x 7-11 µm"
Conidia$measure_orig[grep("Pseudocercospora colocasiae_114572_9929",Conidia$spec)]<-"20-45 x 7-11"
#Conidia[grep("Pseudocercospora colocasiae_114572_9929",Conidia$spec),c('Dim1', 'Dim2')]<-c(32.5,9)
Conidia$Dim1[grep("Pseudocercospora colocasiae_114572_9929",Conidia$spec)]<-32.5
Conidia$Dim2[grep("Pseudocercospora colocasiae_114572_9929",Conidia$spec)]<-9

Conidia$measure_orig[grep("Polycephalomyces cylindrosporus_130181_17155",Conidia$spec)]<-"2.5-4 x 1"
#Conidia[grep("Polycephalomyces cylindrosporus_130181_17155",Conidia$spec),c('Dim1', 'Dim2')]<-c(3.25,1)
Conidia$Dim1[grep("Polycephalomyces cylindrosporus_130181_17155",Conidia$spec)]<-3.25
Conidia$Dim2[grep("Polycephalomyces cylindrosporus_130181_17155",Conidia$spec)]<-1

Conidia$measure_orig[grep("Rhinotrichum album_116849_47198",Conidia$spec)][1]<-"1"
#Conidia[grep("Rhinotrichum album_116849_47198",Conidia$spec),c('Dim1', 'Dim2')]<-c(1,1)
Conidia$Dim1[grep("Rhinotrichum album_116849_47198",Conidia$spec)][1]<-1
Conidia$Dim2[grep("Rhinotrichum album_116849_47198",Conidia$spec)][1]<-1

b<-which(Conidia$spec=="Bensingtonia ingoldii_104960_18134"&Conidia$spore_type=="conidia")
Conidia<-Conidia[-b,]
Conidia<-Conidia[-which(Conidia$spec=="Taeniolina_39122_28083"),]

b<-which(Conidia$measure_orig=="15-35 x 1-2")
Conidia$text_entry[b]<-"Conidia ellipsoidal to obovoid-ellipsoidal, (3-)3.5-5(-6) x (1.5-)2-3"
Conidia$measure_orig[b]<-"(3-)3.5-5(-6) x (1.5-)2-3"
Conidia$Dim1[b]<-4.25
Conidia$Dim2[b]<-2.5

Conidia$text_entry[grep("Aspergillus floriformis_2370_24586",Conidia$spec)][1]<-"Conidia globose, finely echinulate, greenish, 4-5 um in diam"

Conidia$text_entry[grep("Sphacelotheca virens_308289_27681",Conidia$spec)]<-"conidia and at last are apt to drop off. They are hard, flat, clavate, botuliform, reniform, horse-shoe-shaped, or differently shaped, 5-13 X 2-5 mm, though small subglobose ones are 1-2 mm in diameter, black outside, white inside, pseudoparenchymatous, containing lipid-1ike granules."
Conidia$measure_orig[grep("Sphacelotheca virens_308289_27681",Conidia$spec)]<-"5-13 X 2-5"
#Conidia[grep("Sphacelotheca virens_308289_27681",Conidia$spec),c('Dim1', 'Dim2')]<-c(9,3.5)
Conidia$Dim1[grep("Sphacelotheca virens_308289_27681",Conidia$spec)]<-9
Conidia$Dim2[grep("Sphacelotheca virens_308289_27681",Conidia$spec)]<-3.5

b<-which(Conidia$spec=="Xenosporium ovatum_438169_19829"&grepl("72-101\\? 40-58",Conidia$text_entry))
Conidia$measure_orig[b]<-"72-101 x 40-58"
# Conidia$Dim1[b]<-86.5
# Conidia$Dim2[b]<-49

b<-grep("6.0 11.0",Conidia$measure_orig)
Conidia$measure_orig[b]<-"6.0-11.0 x 2.0-5.0"
Conidia$Dim1[b]<-8.5
Conidia$Dim2[b]<-3.5

b<-grep("150-350 x 70-150",Conidia$measure_orig)
Conidia$text_entry[b]<-"conidia and at last are apt to drop off. They are hard, flat, clavate, botuliform, reniform, horse-shoe-shaped, or differently shaped, 5-13 X 2-5 mm, though small subglobose ones are 1-2 mm in diameter, black outside, white inside, pseudoparenchymatous, containing lipid-1ike granules."
Conidia$measure_orig[b]<-"5-13 X 2-5"
Conidia$Dim1[b]<-9
Conidia$Dim2[b]<-3.5


b<-which(grepl("Diaporthe kochmanii_477350",Conidia$spec)&grepl("SD",Conidia$text_entry))
Conidia$measure_orig[b]<-"6.4 x 2.2"
Conidia$Dim1[b]<-6.4
Conidia$Dim2[b]<-2.2

b<-grep("= 3.34 \\+\\- 0.59",Conidia$measure_orig)
Conidia$text_entry[b]<-"Conidia produced in slime drops, one-celled; oblong to slightly tapered with rounded ends, oval; small, 2-4 (sd = 3.34 ± 0.59) µm long and 1-2 (sd = 1.38 ± 0.48) µm wide hyaline and smooth."
Conidia$measure_orig[b]<-"2-4 x 1-2"
Conidia$Dim1[b]<-3.34
Conidia$Dim2[b]<-1.38

b<-which(Conidia$spec=="Paraphoma pye_557120_75478")
Conidia$measure_orig[b]<-"3.5-6 x 1.5-3.5"
Conidia$Dim1[b]<-4.75
Conidia$Dim2[b]<-2.5

b<-which(Conidia$spec=="Paraphoma vinacea_546748_71359")
Conidia$measure_orig[b]<-"2.3-4.5 x 3.94-6.8"
Conidia$Dim1[b]<-3.4
Conidia$Dim2[b]<-5.37

b<-which(Conidia$spec=="Paraphoma chlamydocopiosa_557119_75477")
Conidia$measure_orig[b]<-"3-4 x 6-9"
Conidia$Dim1[b]<-3.5
Conidia$Dim2[b]<-7.5

b<-which(Conidia$spec=="Leohumicola lenta_117053_12051")
Conidia$measure_orig[b][2]<-"7-10 x 6.5-8.5"
Conidia$Dim2[b][2]<-7.5

b<-which(Conidia$spec=="Paraconiothyrium archidendri_508283_55946")
Conidia$text_entry[b]<-"Conidia variable in shape, subglobose or ellipsoid, more rarely obovoid, ends rounded, sometimes one end more or less blunt, initially hyaline, soon after secession olivaceous brown, contents with several small oil-droplets (< 0.5 µm diam) near each end, conidial wall at maturity relatively thick, smooth, sometimes very minutely roughened, 0-septate, 3.5–6 × 2.5–3.5(–4) µm"
Conidia$measure_orig[b]<-"3.5–6 × 2.5–3.5(–4)"
Conidia$Dim1[b]<-4.75
Conidia$Dim2[b]<-3.5

# Note1 : the "ca" issue. when the text contain ca, 
#        such as 4.0-5.0 x ca. 1.0 µm, the code only pick up 
#        the later value, i.e. 1.0 in this case  (FIXED)


### Note 2 : when the measure_orig contained x in there, 
###          it always did not pick up the value for Dim 1 
###          and only put them into Dim2

# Conidia[11272,  c('Dim1', 'Dim2')] <-  c(50, NA)
# Conidia[15114,  c('Dim1', 'Dim2')] <-  c(5.3, NA)
# Conidia[19580,  c('Dim1', 'Dim2')] <-  c(50, NA)
# Conidia[26212,  c('Dim1', 'Dim2')] <-  c(115, NA)


## Note 3 : check the small values for Dim1 and Dim2, 
## seems the code did not extract the right value 
## and just give the wrong value, all the value 
## below than 1 um should be checked.

# head(filter(Conidia, Dim1 > 200))
# head(filter(Conidia, Dim1 < 0.2))


#Checking protocol:
#1. Cases where Conidia are reported as absent (or similar expression)
#2. NA entries in Dim1
#3. Long descriptions & Cases where other structures are picked up
#4. Big values
#5. Small values
#6. Final manual checking

Conidia_c<-Conidia

#1. Cases where Conidia are reported as absent (or similar expression)
Conidia<-
  Conidia[-grep("onidia absent",Conidia$text_entry),]
not_observed<-
which(grepl("onidia not observed",Conidia$text_entry)&!grepl("Ballistoconidia",Conidia$spore_type))
Conidia<-
  Conidia[-not_observed,]
rm(not_observed)
Conidia<-
  Conidia[-grep("onidia not present",Conidia$text_entry),]
Conidia<-
  Conidia[-grep("onidia none",Conidia$text_entry),]
Conidia<-
  Conidia[-grep("onidia lacking",Conidia$text_entry),]
Conidia<-
  Conidia[-grep("onidia are not produced",Conidia$text_entry),]
not_produced<-
  which(grepl("onidia not produced",Conidia$text_entry)&!grepl("timor",Conidia$spec))
Conidia<-
  Conidia[-not_produced,]
rm(not_produced)
Conidia<-
  Conidia[-grep("onidia not seen\\.",Conidia$text_entry),]
Conidia<-
  Conidia[-grep("onidia not occurring",Conidia$text_entry),]
Conidia<-
  Conidia[-grep("onidia not formed\\.",Conidia$text_entry),]


#2. NA entries in Dim1
#After all the cleaning there are 64 NA entries that can be solved by manual checking.
#For the moment I will leave that for later. And just remove them
Conidia_NA<-Conidia[which(is.na(Conidia$Dim1)),]

Conidia<-Conidia[-which(is.na(Conidia$Dim1)),]
#Plus this changes



#3. Long descriptions or wrong descriptions
#Removing extremely long explanation that in almost all cases do not contain
#conidia description really

t<-sapply(list(Conidia$text_entry), nchar)
Conidia<-Conidia[-which(t>1000),]
#t<-sapply(list(Conidia$text_entry), nchar)

Conidia<-
  Conidia[-grep("\\. Conidiophores",Conidia$text_entry),]

Conidia<-
  Conidia[-grep("\\. \\s?Phialides",Conidia$text_entry),]

Conidia<-
  Conidia[-grep("\\. \\s?Chlamydospores",Conidia$text_entry),]

Conidia<-
  Conidia[-grep("\\. \\s?Ascospores",Conidia$text_entry),]

#Removing cases where the same entry is repeated many times because the word
#conidia appeared more than once
t<-sapply(list(Conidia$text_entry), nchar)
Conidia$n_char<-as.numeric(t)

#Order, so only the shortest description is kept for a given range of values reported 
Conidia<-
Conidia[order(Conidia$spec,Conidia$measure_orig,Conidia$n_char),]

#Removing the duplicated entries
Conidia<-
  Conidia[-which(duplicated(paste(Conidia$spec,Conidia$measure_orig,sep = "_"))),]

Conidia<-
  Conidia[-grep("\\. Vegetative",Conidia$text_entry),]
Conidia<-
  Conidia[-grep("\\.  Vegetative",Conidia$text_entry),]
Conidia<-
  Conidia[-grep("\\. Peri",Conidia$text_entry),]
Conidia<-
  Conidia[-grep("\\. Asci",Conidia$text_entry),]
Conidia<-
  Conidia[-grep("\\.  Conidiophores",Conidia$text_entry),]
Conidia<-
  Conidia[-grep("\\. \\s?Conidiogenous",Conidia$text_entry),]
Conidia<-
  Conidia[-grep("\\. \\s?Asco",Conidia$text_entry),]
Conidia<-
  Conidia[-grep("\\. \\s?Colon",Conidia$text_entry),]
Conidia<-
  Conidia[-grep("\\. \\s?Hyph",Conidia$text_entry),]
Conidia<-
  Conidia[-grep("\\. \\s?Mycel",Conidia$text_entry),]

# #To test
# Conidia<-
#   Conidia[-grep("\\. {+}[A-Z]",Conidia$text_entry),]



#To remove also: Cleistothecia
t<-sapply(list(Conidia$text_entry), nchar)
Conidia_long<-Conidia[t>510,]#This needs to be manually checked, but it seems most of them
#are wrong entries. For the moment I will just remove them:

Conidia<-Conidia[-which(t>510),]

#Small values
#There are still many entries that actually report substructures of the conidia,
#these are a bit problematic because, unlike the other spore types where a minimmum
#spore size can be easily detected, conidia can be indeed very small. I think a potential
#solution for this problem is to modify the get_dimensions2 function to extend the range
#of words included in text entry, I was thining adding up to 30 words after spore.end. In
#that way it would be more likely to detect the real value. This would be a major change
#so, for the moment I will just remove really small values where most entries are not the whole
#conidia

Conidia<-Conidia[-which(Conidia$Dim1<=1&!grepl("x",Conidia$measure_orig)),]

# Big values
sumando<-
  function(x){
    listado<-as.numeric(strsplit(as.character(x),"")[[1]])
    if(length(listado)==3){
      (listado[1]+as.numeric(paste(listado[2:3],collapse = "")))/2
    }else{(as.numeric(paste(listado[1:2],collapse = ""))+as.numeric(paste(listado[3:4],collapse = "")))/2}
  }


hgt<-strsplit(Conidia$measure_orig[which(grepl("Ascochyta",Conidia$spec)&Conidia$Dim1>300)],"x")
hgt<-sapply(hgt,function(x)x[[1]])
hgt<-gsub("\\s?\\s?\\(\\d\\d?\\.?\\d?)","",hgt)
hgt<-str_trim(hgt)
hgt<-sapply(hgt,sumando);names(hgt)<-NULL
Conidia$Dim1[which(grepl("Ascochyta",Conidia$spec)&Conidia$Dim1>300)]<-hgt

b<-grep("1217",Conidia$Dim1)
Conidia$Dim1[b]<-sapply(Conidia$Dim1[b],sumando)

b<-grep("\\(1545\\)7\\-15\\(-25\\)",Conidia$measure_orig)
Conidia$Dim1[b]<-11
Conidia$Dim2[b]<-11

Conidia<-
  Conidia[-grep("\\. Conidiophor",Conidia$text_entry),]

Conidia<-Conidia[-which(Conidia$Dim1>999),]

Conidia$Dim1[grep("1221 \\(26\\) x 3\\-5",Conidia$measure_orig)]<-16.5

Conidia$Dim1[grep("1213\\-30 x 4\\-5",Conidia$measure_orig)]<-12.5
#solve cases with % in measure orig
Conidia$n_char<-NULL

### write to file
#write.csv(Conidia, 'output/conidia_mycobank.csv', row.names=F)


#Ideas to solve the issue of really small values:
#number of words
# prueba<-"(10–)25–45(–70) x (2–)2.5–3 µm."
# prueba2<-"near each end, conidial wall at maturity relatively thick, smooth, sometimes very minutely roughened, 0-septate, 3.5–6 × 2.5–3.5(–4) µm,"
# str_count(prueba2,"\\w+")
# nchar(prueba2)
# 
# prueba3<-"Conidia variable in shape, subglobose or ellipsoid, more rarely obovoid, ends rounded, sometimes one end more or less blunt, initially hyaline, soon after secession olivaceous brown, contents with several small oil-droplets (< 0.5 µm diam) near each end, conidial wall at maturity relatively thick, smooth, sometimes very minutely roughened, 0-septate, 3.5–6 × 2.5–3.5(–4) µm, "
# names(prueba3)<-"Conidia"
# prueba3<-Conidia$text_entry[Conidia$spec=="Palaeoclaviceps parasiticus_559173_66030"]
# 
# prueba_val<-get_dimensions2(prueba3,extract.regex="[^a-wy-zA-WY-Z]+\\s?µm")
# 
# gsub("\"","",prueba_val_df$measure_orig)
# 
# strsplit(prueba_val_df$measure_orig,"\"")
# 
#   str_count("and smoothed-walled, subglobose to oblong, often distinctly bent in phaseoliform or sigmoid, 3.6–9.0 × 2.1–3.2 μm."," ")