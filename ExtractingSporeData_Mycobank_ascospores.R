####################################################################################
###########################   ASCOSPORES    ########################################
####################################################################################

rm(list=ls())

library(tidyverse)

source('General_dimensionExtractionFunct.R')


# spore.dat<- readRDS("mycobank_descriptions.RDS")#This dataset
# write.csv(spore.dat,"spore_dat.csv",row.names = F)
# spore.dat<-read.csv("spore_dat.csv",header = T,stringsAsFactors = F)
# saveRDS(spore.dat,"mycobank_descriptions_mod.RDS")

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


#1. Ascospores: this applies only to the ascomycota subset of the Data

#Subsetting only ascomycetes

Ascomycetes<-spore.dat[spore.dat$Phylum==" Ascomycota",]#Retruning 54011 entries

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

textos<-Ascomycetes$description_description_
names(textos)<-paste(Ascomycetes$base_name, Ascomycetes$base__id, Ascomycetes$description__id, sep ="_")

#Now I can extract Ascospores out of these subset

Ascospores_text<-
  lapply(textos,get_text,
         start.regex="scospores$",
         end.regex="µm"#,
  )


# temp <- Ascospores_text
Ascospores_text<-lapply(Ascospores_text, 
                           function(x)gsub('\\([a-zA-Z]+\\. [0-9]{+}-[0-9]{1,}\\)', '', x))
Ascospores_text<-lapply(Ascospores_text, 
                           function(x)gsub('\\([a-zA-Z]+\\. [0-9]+\\,[0-9]+\\)', '', x))
Ascospores_text<-lapply(Ascospores_text, 
                           function(x)gsub('\\([a-zA-Z]+\\. [0-9]+\\)', '', x))
Ascospores_text<-lapply(Ascospores_text, 
                           function(x)gsub('－', '-', x))
Ascospores_text<-lapply(Ascospores_text, 
                           function(x)gsub('−', '-', x))


#Extracting Ascospores values
Ascospores_values<-
  lapply(Ascospores_text,get_dimensions2,
         extract.regex="[^a-wy-zA-WY-Z]+\\s?µm")
#Ascospores_values<-Ascospores_values$`1`

#Reformatting it into a dataframe

values<- lapply(Ascospores_values, function(x) x[[1]])

## restructure data as table
values2 <- list()#it took ~30 seconds to run it
for(i in 1:length(values)){
  x <- plyr::rbind.fill(lapply(values[[i]], function(y) { as.data.frame(t(y), stringsAsFactors=FALSE) }))
  x <- cbind(spore_type = names(values[[i]]), x)
  values2[[i]] <- x
}
names(values2) <- names(values)

#It seems one does not need this for Ascospores:
#q<-which(sapply(values2,is.null))#for some reasons some entries are empty

## combine all spore results in one table
#values_df <- plyr::rbind.fill(lapply(values2[-q], function(y) { as.data.frame(y) }))
values_df <- plyr::rbind.fill(lapply(values2, function(y) { as.data.frame(y) }))
#values_df<- cbind(spec=rep(names(values2[-q]),lapply(values2[-q], nrow)),values_df)
values_df<- cbind(spec=rep(names(values2),lapply(values2, nrow)),values_df)
#values_df <- values_df[,-length(values_df)]
names(values_df)[3] <- "measure_orig"


#Merge the text with the values and spore data info
text<-lapply(Ascospores_text,plyr::ldply, rbind)
text<-plyr::rbind.fill(text)
names(text)[1]<-"text_entry"

#Creation of the object "Ascospores" containing all the data
Ascospores<-cbind(text,values_df)#For some reason the transformations above return 47032, instead of 45416 elements that has Ascospores_text and Ascospores_values. However, it seems fine!
Ascospores<-data.frame(
  sapply(Ascospores, as.character),
  stringsAsFactors = F)#This creates a dataframe conatianing 47032 entries
#Removing empty entries
Ascospores<-Ascospores[-which(
  Ascospores$text_entry=="Result not found"),]#Which is actually the majority of cases: 37768!
#Removing them reduces the dataframe to 9264 entries

t<-sapply(list(Ascospores$text_entry), nchar)
#Just standarizing the "x"
Ascospores$measure_orig<-gsub("X","x",Ascospores$measure_orig)
Ascospores$measure_orig <- gsub(' [[:punct:]] ', ' x ', Ascospores$measure_orig)
Ascospores$measure_orig <- gsub('\\s+x\\s+', ' x ', Ascospores$measure_orig)
Ascospores$measure_orig <- gsub('[[:punct:]]  ', '', Ascospores$measure_orig)
Ascospores$measure_orig <- gsub('  ', ' x ', Ascospores$measure_orig)
Ascospores$measure_orig <- gsub(' ', ' x ', Ascospores$measure_orig)
Ascospores$measure_orig <- gsub('[[:space::]]{2,}', ' x ', Ascospores$measure_orig)
Ascospores$measure_orig <- gsub('^x', '', Ascospores$measure_orig)


####  Extracting the spore ranges  ######

t<-strsplit(Ascospores$measure_orig,"x")

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

Ascospores <- cbind(Ascospores, temp)
Ascospores <- Ascospores %>% 
  rename(Dim1 = V1, 
         Dim2 = V2, 
         Dim3 = V3, 
         Dim4 = V4)


### Manual changes ###

Ascospores<-
  Ascospores[-grep("scospores absent",Ascospores$text_entry),]

Ascospores<-
  Ascospores[-grep("scospores not observed",Ascospores$text_entry),]

Ascospores<-
  Ascospores[-grep("scospores are not observed",Ascospores$text_entry),]

Ascospores<-
  Ascospores[-grep("scospores not seen",Ascospores$text_entry),]


#b<-c(2164,6894,9991,27087)
#bc<-Ascospores[rownames(Ascospores) %in% b,]$spec
b<-c("Nectria mammoidea var. rubi_7535_17872","Nectria rubi_34178_17872",
     "Hypomyces rubi_382204_17872","Neonectria discophora var. rubi_427163_17872")
#Ascospores[Ascospores$spec %in% b,]$spec
Ascospores[Ascospores$spec %in% b,]$text_entry<-"ascospores ellipsoid, 1-septate, (11.5-)12-16(-23) x (4.5-)5.5-6.5(-8.0) µm"
Ascospores[Ascospores$spec %in% b,]$measure_orig<-"(11.5-)12-16(-23) x (4.5-)5.5-6.5(-8.0)"
Ascospores[Ascospores$spec %in% b,]$Dim1<-(12+16)/2
Ascospores[Ascospores$spec %in% b,]$Dim2<-(5.5+6.5)/2

# b<-c(16396,16402,23364,42974,51893)
# Ascospores[rownames(Ascospores) %in% b,]$spec
b<-c("Calonectria hederae_468219_9386","Calonectria hederae_175248_9386","Bionectria subgen. Bionectria_58652_109",
     "Lecanactis caceresiana_558701_76534,Gnomoniopsis castanea_508585_56284")
Ascospores<-Ascospores[!Ascospores$spec %in% b,] #Exclude from dataframe



#b<-c(9454,11933,14158,14540,20954,39375,42145)
#Ascospores[rownames(Ascospores) %in% b,]$spec###TO TEST!!!!!!!!!!!!
b<-c("Hypoxylon carabayanse_342730_52991", "Nummularia carabayanse_342731_52991","Hypoxylon carabayense_420679_52991",
     "Nummularia carabayensis_421850_52991","Penzigia carabayensis_236754_52991","Nummularia carabayense_461679_52991",
     "Xylaria carabayensis_480927_52991")
Ascospores[Ascospores$spec %in% b,]$spec
Ascospores[Ascospores$spec %in% b,]$Dim1<-(27.5+35)/2
Ascospores[Ascospores$spec %in% b,]$Dim2<-(15+19.5)/2

Ascospores$Dim1[Ascospores$spec=="Sporothrix nebularis_554058_73321"]<-(3.12+4.23)/2
Ascospores$Dim2[Ascospores$spec=="Sporothrix nebularis_554058_73321"]<-(1.42+1.79)/2

#Ascospores$spec[rownames(Ascospores)=="40112"]
Ascospores$Dim1[Ascospores$spec=="Ligninsphaeria jonesii_558906_76679"]<-(79+121)/2
Ascospores$Dim2[Ascospores$spec=="Ligninsphaeria jonesii_558906_76679"]<-(14+23)/2

#Ascospores$spec[rownames(Ascospores)=="5442"]
Ascospores$Dim1[Ascospores$spec=="Hypocrea ravenelii_326307_54215"]<-(38+42)/2
Ascospores$Dim2[Ascospores$spec=="Hypocrea ravenelii_326307_54215"]<-(4.5+5.5)/2

#Ascospores$spec[rownames(Ascospores)=="10808"]
Ascospores$Dim1[Ascospores$spec=="Broomella ravenelii_378968_54215"]<-(38+42)/2
Ascospores$Dim2[Ascospores$spec=="Broomella ravenelii_378968_54215"]<-(4.5+5.5)/2

#Ascospores$spec[rownames(Ascospores)=="26444"]
Ascospores$Dim1[Ascospores$spec=="Rimaconus jamaicensis_374615_62559"]<-(55+73)/2

#Ascospores$spec[rownames(Ascospores)=="45486"]
Ascospores$Dim1[Ascospores$spec=="Chaenothecopsis resinophila_512177_59274"]<-5.55
Ascospores$Dim2[Ascospores$spec=="Chaenothecopsis resinophila_512177_59274"]<-2.95
Ascospores$Dim3[Ascospores$spec=="Chaenothecopsis resinophila_512177_59274"]<-NA

#Ascospores$spec[rownames(Ascospores)=="53396"]
Ascospores$measure_orig[Ascospores$spec=="Graphis maomingensis_565532_80418"]<-"54-87 x 6-10"
Ascospores$Dim1[Ascospores$spec=="Graphis maomingensis_565532_80418"]<-(54+87)/2
Ascospores$Dim2[Ascospores$spec=="Graphis maomingensis_565532_80418"]<-(6+10)/2

#Ascospores$spec[rownames(Ascospores)=="44121"]
Ascospores$Dim2[Ascospores$spec=="Asterina mezonevronis_508535_56246"]<-(10+13)/2
Ascospores$Dim2[Ascospores$spec=="Lembosia garciniae_508542_56255"]<-(10+13)/2

# b<-c(41625,41633,47674,47670,30520,43185,48315)
# Ascospores[rownames(Ascospores) %in% b,]$spec
b<-c("Tubeufia asiana_445053_24639","Penicillium quebecense_478378_52917","Penicillium aurantiacobrunneum_478382_52924",
     "Bionectria subgen. Zebrinella_58655_38","Penicillium cartierense_541093_65181","Penicillium grevilleicola_541097_65185",
     "Calvitimela subgen. Severidea_543165_67591")
Ascospores<-Ascospores[!Ascospores$spec %in% b,] #Exclude from dataframe

#Ascospores$spec[rownames(Ascospores)=="19602"]
Ascospores$text_entry[Ascospores$spec=="Pyrenophora erythrospila_181220_16654"]<-"Ascospores ellipsoidal, light yellow brown, broadly rounded at both ends, muriform with 3 (sometimes up to 5) transverse septa and 0-2 vertical septa, constricted at the septa, 33-70 x 18-28 µm"
Ascospores$measure_orig[Ascospores$spec=="Pyrenophora erythrospila_181220_16654"]<-"33-70 x 18-28"
Ascospores$Dim1[Ascospores$spec=="Pyrenophora erythrospila_181220_16654"]<-(33+70)/2
Ascospores$Dim2[Ascospores$spec=="Pyrenophora erythrospila_181220_16654"]<-(18+28)/2

#Ascospores$spec[rownames(Ascospores)=="45931"]
Ascospores$text_entry[Ascospores$spec=="Ceratocystis cerberus_519450_60248"]<-"Ascospores in sheaths, hyaline, aseptate, ellipsoidal (6-)7-9(-13) x (3-)3-4(-6) µm"
Ascospores$measure_orig[Ascospores$spec=="Ceratocystis cerberus_519450_60248"]<-"(6-)7-9(-13) x (3-)3-4(-6)"
Ascospores$Dim1[Ascospores$spec=="Ceratocystis cerberus_519450_60248"]<-(7+9)/2
Ascospores$Dim2[Ascospores$spec=="Ceratocystis cerberus_519450_60248"]<-(3+4)/2

# b<-c(14804,40253)
# Ascospores[rownames(Ascospores) %in% b,]$spec
b<-c("Ramularia robusta_41459_48669","Ilyonectria robusta_476732_48669")
Ascospores[Ascospores$spec %in% b,]$text_entry<-"Ascospores medianly 1-septate, ellipsoid to oblong-ellipsoid, somewhat tapering towards both ends, smooth to finely warted, hyaline, (8.2)9.4-9.7-10.0(11.5)-(2.5)2.9-3.0-3.1(3.7) µm"
Ascospores[Ascospores$spec %in% b,]$measure_orig<-"(8.2)9.4-9.7-10.0(11.5)-(2.5)2.9-3.0-3.1(3.7)"
Ascospores[Ascospores$spec %in% b,]$Dim1<-9.7
Ascospores[Ascospores$spec %in% b,]$Dim2<-3.0

# b<-c(11672,13377,14773)
# Ascospores[rownames(Ascospores) %in% b,]$spec
b<-c("Eurotium amstelodami_9993_16253","Aspergillus amstelodami_9994_16253","Eurotium repens var. amstelodami_171588_16253")
Ascospores[Ascospores$spec %in% b,]$text_entry<-"Ascospores yellow, ellipsoidal, 4,5-5 x 3,5-4 µm"
Ascospores[Ascospores$spec %in% b,]$measure_orig<-"4.5-5 x 3.5-4"
Ascospores[Ascospores$spec %in% b,]$Dim1<-(4.5+5)/2
Ascospores[Ascospores$spec %in% b,]$Dim2<-(3.5+4)/2

#Ascospores$spec[rownames(Ascospores)=="Gaeumannomyces oryzicola_552521_72657"]
Ascospores$text_entry[Ascospores$spec=="Gaeumannomyces oryzicola_552521_72657"]<-"Ascospores faintly tinted yellowish in mass, hyaline to pale brown, vacuolated, slightly curved to sinuate, ends rounded, 92.5-120 x 4-6 µm"
Ascospores$measure_orig[Ascospores$spec=="Gaeumannomyces oryzicola_552521_72657"]<-"92.5-120 x 4-6"
Ascospores$Dim1[Ascospores$spec=="Gaeumannomyces oryzicola_552521_72657"]<-(92.5+120)/2
Ascospores$Dim2[Ascospores$spec=="Gaeumannomyces oryzicola_552521_72657"]<-(4+6)/2

#Ascospores$spec[rownames(Ascospores)=="826"]
Ascospores$text_entry[Ascospores$spec=="Cryptosphaeria pullmanensis_127227_40657"]<-"Ascospores brown, oblong to reniform, occasionally septate, 12-16(-18) x 4.5-5 µm"
Ascospores$measure_orig[Ascospores$spec=="Cryptosphaeria pullmanensis_127227_40657"]<-"12-16(-18) x 4.5-5"
Ascospores$Dim1[Ascospores$spec=="Cryptosphaeria pullmanensis_127227_40657"]<-(12+16)/2
Ascospores$Dim2[Ascospores$spec=="Cryptosphaeria pullmanensis_127227_40657"]<-(4.5+5)/2

#Ascospores$spec[rownames(Ascospores)=="48914"]
Ascospores$text_entry[Ascospores$spec=="Buellia mayrhoferae_545787_70434"]<-"Ascospores briefly Physconia-type, then of the Buellia-type, 1-septate, olive-brown to brown, ellipsoid, 10-[12.9]-17 x 5-[6.6]-8 µm"
Ascospores$measure_orig[Ascospores$spec=="Buellia mayrhoferae_545787_70434"]<-"10-[12.9]-17 x 5-[6.6]-8"
Ascospores$Dim1[Ascospores$spec=="Buellia mayrhoferae_545787_70434"]<-12.9
Ascospores$Dim2[Ascospores$spec=="Buellia mayrhoferae_545787_70434"]<-6.6

# b<-c(30120,38961)
# Ascospores[rownames(Ascospores) %in% b,]$spec
b<-c("Lecanora bicintoidea_440544_22054","Lecanora bicinctoidea_454633_46436")
Ascospores[Ascospores$spec %in% b,]$text_entry<-"ascospores hyaline, simple, ellipsoid, wall thick, 10.2-11.6-13(15) +- 5.7-6.6-7.5(8) µm"
Ascospores[Ascospores$spec %in% b,]$measure_orig<-"10.2-11.6-13(15) +- 5.7-6.6-7.5(8)"
Ascospores[Ascospores$spec %in% b,]$Dim1<-(11.6+13)/2
Ascospores[Ascospores$spec %in% b,]$Dim2<-(6.6+7.5)/2

#Ascospores$spec[rownames(Ascospores)=="51679"]
Ascospores$text_entry[Ascospores$spec=="Parmotrema screminiae_558172_76056"]<-"ascospores ellipsoid, sometimes long-ellipsoid, 12-15 x 6-8 µm"
Ascospores$measure_orig[Ascospores$spec=="Parmotrema screminiae_558172_76056"]<-"12-15 x 6-8"
Ascospores$Dim1[Ascospores$spec=="Parmotrema screminiae_558172_76056"]<-(12+15)/2
Ascospores$Dim2[Ascospores$spec=="Parmotrema screminiae_558172_76056"]<-(6+8)/2

#Ascospores$spec[rownames(Ascospores)=="52303"]
Ascospores$text_entry[Ascospores$spec=="Aspiciliella portosantana_559995_77424"]<-"Ascospores hyaline, simple, subglobose, 22-28 x 17-23 µm"
Ascospores$measure_orig[Ascospores$spec=="Aspiciliella portosantana_559995_77424"]<-"22-28 x 17-23"
Ascospores$Dim1[Ascospores$spec=="Aspiciliella portosantana_559995_77424"]<-(22+28)/2
Ascospores$Dim2[Ascospores$spec=="Aspiciliella portosantana_559995_77424"]<-(17+23)/2

#Ascospores$spec[rownames(Ascospores)=="29764"]
Ascospores$text_entry[Ascospores$spec=="Ophiocordyceps communis_438267_20043"]<-"Ascospores whole, stout, lightly pigmented (100-)120-150(-180) x 5-6 µm"
Ascospores$measure_orig[Ascospores$spec=="Ophiocordyceps communis_438267_20043"]<-"(100-)120-150(-180) x 5-6"
Ascospores$Dim1[Ascospores$spec=="Ophiocordyceps communis_438267_20043"]<-(120+150)/2
Ascospores$Dim2[Ascospores$spec=="Ophiocordyceps communis_438267_20043"]<-(5+6)/2

#Ascospores$spec[rownames(Ascospores)=="52547"]
Ascospores$text_entry[Ascospores$spec=="Ophiocordyceps pseudoacicularis_560917_77896"]<-"Ascospores hyaline, filiform, remain whole after discharge, with septation.  Asexual morph terminal, pale grey (1B1)-dark grey(1F1), 4-5 µm long"
Ascospores$measure_orig[Ascospores$spec=="Ophiocordyceps pseudoacicularis_560917_77896"]<-"4-5"
Ascospores$Dim1[Ascospores$spec=="Ophiocordyceps pseudoacicularis_560917_77896"]<-(4+5)/2
Ascospores$Dim2[Ascospores$spec=="Ophiocordyceps pseudoacicularis_560917_77896"]<-NA

#Ascospores$spec[rownames(Ascospores)=="963"]
Ascospores$text_entry[Ascospores$spec=="Diatrype whitmanensis_128459_40663"]<-"Ascospores allantoid, suballantoid to oblong, dark brown 7.5-10 (-12.5) x 1-1.5 µm"
Ascospores$measure_orig[Ascospores$spec=="Diatrype whitmanensis_128459_40663"]<-"7.5-10 (-12.5) x 1-1.5"
Ascospores$Dim1[Ascospores$spec=="Diatrype whitmanensis_128459_40663"]<-(7.5+10)/2

#Ascospores$spec[rownames(Ascospores)=="51670"]
Ascospores$text_entry[Ascospores$spec=="Ilyonectria strelitziae_558116_76037"]<-"Ascospores ellipsoidal, tapering towards both ends, divided into two equal sized cells, smooth, (8-)9-11 x 3-4 µm"

# b<-c(540,4261)
# Ascospores[rownames(Ascospores) %in% b,]$spec
b<-c("Vladracula annuliformis_144483_16081","Schizothyrium annuliforme_144484_16081")
Ascospores[Ascospores$spec %in% b,]$measure_orig[c(1,3)]<-"25-55 x 1"
Ascospores[Ascospores$spec %in% b,]$Dim1[c(1,3)]<-(25+55)/2
Ascospores[Ascospores$spec %in% b,]$Dim2[c(1,3)]<-1

# b<-c(21902,41326)
# Ascospores[rownames(Ascospores) %in% b,]$spec
b<-c("Lophodermium illiciicola_443809_55045","Terriera illiciicola_477949_55045")
Ascospores[Ascospores$spec %in% b,]$measure_orig<-"65-95 x 1"
Ascospores[Ascospores$spec %in% b,]$Dim1<-(65+95)/2
Ascospores[Ascospores$spec %in% b,]$Dim2<-1

#Ascospores$spec[rownames(Ascospores)=="18812"]
Ascospores$measure_orig[Ascospores$spec=="Naemacyclus korfii_180674_16077"]<-"58-65 x 1"
Ascospores$Dim1[Ascospores$spec=="Naemacyclus korfii_180674_16077"]<-(58+65)/2
Ascospores$Dim2[Ascospores$spec=="Naemacyclus korfii_180674_16077"]<-1

#Ascospores$spec[rownames(Ascospores)=="10055"]
Ascospores$measure_orig[Ascospores$spec=="Lophodermium mangiferae_159098_15594"]<-"70-75 x 1"
Ascospores$Dim1[Ascospores$spec=="Lophodermium mangiferae_159098_15594"]<-(70+75)/2
Ascospores$Dim2[Ascospores$spec=="Lophodermium mangiferae_159098_15594"]<-1

#Ascospores$spec[rownames(Ascospores)=="1658"]
Ascospores$measure_orig[Ascospores$spec=="Lophodermium baculiferum_133190_15591"]<-"90-130 x 1.5"
Ascospores$Dim1[Ascospores$spec=="Lophodermium baculiferum_133190_15591"]<-(90+130)/2
Ascospores$Dim2[Ascospores$spec=="Lophodermium baculiferum_133190_15591"]<-1.5

#Ascospores$spec[rownames(Ascospores)=="13923"]
Ascospores$measure_orig[Ascospores$spec=="Lophodermium durilabrum_169620_15592"]<-"90-105 x 1.5"
Ascospores$Dim1[Ascospores$spec=="Lophodermium durilabrum_169620_15592"]<-(90+105)/2
Ascospores$Dim2[Ascospores$spec=="Lophodermium durilabrum_169620_15592"]<-1.5

Ascospores[grep("oreadum",Ascospores$spec),]$measure_orig<-"(4.7-)5.0-6.1 x (0.8-)0.9-1.1(-1.3)µ"
Ascospores[grep("oreadum",Ascospores$spec),]$Dim1<-(5.0+6.1)/2
Ascospores[grep("oreadum",Ascospores$spec),]$Dim2<-(0.9+1.1)/2

t<-sapply(list(Ascospores$text_entry), nchar)
Ascospores<-Ascospores[-which(t>800),]


#Removing the few cases where measurments correspond to Acervuli or Sporodochia and not Ascospores
Ascospores<-Ascospores[-grep("cervuli",Ascospores$text_entry),]
Ascospores<-Ascospores[-grep("porodochia",Ascospores$text_entry),]
Ascospores<-Ascospores[-grep("\\. Conidia",Ascospores$text_entry),]
Ascospores<-Ascospores[-grep("conidia",Ascospores$text_entry),]
Ascospores<-Ascospores[-grep("onidiogenous",Ascospores$text_entry),]
Ascospores<-Ascospores[-grep("onidiophores",Ascospores$text_entry),]
Ascospores<-Ascospores[-grep("Ascomata",Ascospores$text_entry),]
Ascospores<-Ascospores[-grep("\\. Perithecia",Ascospores$text_entry),]

#Removing Ascospore values that are too small (these ones were checked manually and correspond to substructure of the ascospore)
Ascospores<-Ascospores[which(Ascospores$Dim1>=0.72),]

write.csv(Ascospores, 'output/ascospores_mycobank.csv', row.names=F)



##############################################################################################################
# Ascospores_1<-read.csv('output/ascospores_mycobank.csv',header = ,stringsAsFactors = F)
# Ascospores$Dim1[Ascospores$spec=="Rinodina nugrae_551144_71831"]==
#   Ascospores_1$Dim1[Ascospores_1$spec=="Rinodina nugrae_551144_71831"]
# 
# comp<-data.frame(New=Ascospores[which(Ascospores$Dim1!=Ascospores_1$Dim1),]$Dim1,
#                  Old=Ascospores_1[which(Ascospores$Dim1!=Ascospores_1$Dim1),]$Dim1)



#Exclude data with dimensions above 200um
#Ascospores<-Ascospores[which(Ascospores$Dim1<=200),]

#Check spore dimensions between 100 and 200um and exclude data that do not correspond to spore size (perithecia, asci...)
# b<-c(136,2767,2961,3131,5354, 5525,7581,11095,13102,13232, 13233,13234,13235,13890,
#      13891,13892,13893, 13897,13898,13899,13900,14368,14375,14729:14732,15160,18499,
#      21054,21055,21140:21143,25369,26116,28069,28299,28609,29511,29747:29749,
#      31746,33107,33752,33789,33817,34918,35933, 39924,41411,43085,44268,45002,
#      45003,46765,47246,47334,48349,48642,49504,49505,49736,50137,53347,53351,1880,
#      3044,13486,14303,38849) #rownames to exclude

# Ascospores<-Ascospores[!rownames(Ascospores) %in% b,] #Exclude from dataframe
# rm(b)
# 
# Ascospores_b<-Ascospores[rownames(Ascospores) %in% b,] #Exclude from dataframe
# #Remove entry with Dim1 = 0 and <0.2
# Ascospores<-Ascospores[which(Ascospores$Dim1>0.2),]
# 
# #Changing last odd entries (really large values)
# b<-c(940,1076,1243,1707,6600)
# Ascospores[rownames(Ascospores) %in% b,]$Dim2<-(12+15)/2
# 
# #Removing cases when sppores are absent
# Ascospores<-
#   Ascospores[-grep("scospores absent",Ascospores$text_entry),]
# 
# Ascospores<-
#   Ascospores[-grep("scospores not observed",Ascospores$text_entry),]
# 
# b<-c(27683,27684,29010, 30091, 30095, 30098, 30100, 30103, 30113, 30114, 30129,30126, 30130,
#      13894,13895,13901,13902,16469,16470,20808,20809,21144,21145,21163,21164,31177,37315,49771,
#      48801)
# Ascospores<-Ascospores[!rownames(Ascospores) %in% b,] #Exclude from dataframe
# 
# Ascospores[rownames(Ascospores)=="1840",]$Dim1<-(3.5+4)/2
# Ascospores[rownames(Ascospores)=="17931",]$Dim1<-(3.5+4)/2
# 
# # Ascospores[rownames(Ascospores)=="46269",]$Dim1<-14.3
# # Ascospores[rownames(Ascospores)=="46269",]$Dim2<-7.5
# 
# # Ascospores[rownames(Ascospores)=="46271",]$Dim1<-14.7
# # Ascospores[rownames(Ascospores)=="46271",]$Dim2<-7.7
# 
# Ascospores[rownames(Ascospores)=="46275",]$Dim1<-15.1
# Ascospores[rownames(Ascospores)=="46275",]$Dim2<-6.6
# 
# # Ascospores[rownames(Ascospores)=="48128",]$Dim1<-(17+19)/2
# # Ascospores[rownames(Ascospores)=="48128",]$Dim2<-(7+8)/2
# 
# # Ascospores[rownames(Ascospores)=="48700",]$Dim1<-32
# # Ascospores[rownames(Ascospores)=="48700",]$Dim2<-32
# 
# # Ascospores[rownames(Ascospores)=="48705",]$Dim1<-32
# # Ascospores[rownames(Ascospores)=="48705",]$Dim2<-32
# 
# # Ascospores[rownames(Ascospores)=="53612",]$Dim1<-(49+57)/2
# # Ascospores[rownames(Ascospores)=="53612",]$Dim2<-(19.3+21)/2
# 
# # Ascospores[rownames(Ascospores)=="53613",]$Dim1<-(50+59)/2
# # Ascospores[rownames(Ascospores)=="53613",]$Dim2<-(15+19)/2
# 
# w<-Ascospores[grep("onidia",Ascospores$text_entry),]
# 
# t<-sapply(list(Ascospores$text_entry), nchar)
# Ascospores<-Ascospores[-which(t>300),]
# Ascospores_long2<-Ascospores[which(t>500),]
# 
# # txt<-textos[which(names(textos)=="Arthrinium yunnanum_555884_74409")]
# txt1<-spore.dat$description_description_[which(spore.dat$base_name=="Arthrinium yunnanum")]
# txt2<-spore.dat2$description_description_[which(spore.dat2$base_name=="Arthrinium yunnanum")]
# txt3<-spore.dat$description_description_[which(spore.dat$base_name=="Ramalina carpatica")]
# txt4<-spore.dat$description_description_[which(spore.dat$description__id=="14085")[1]]
# txt5<-"Ascospores colourless, simple, narrowly ellipsoid to oblong-ellipsoid or oblong, usually straight and with rounded ends, irregularly biseriate in the ascus or obliquely arranged, (8–)11(–13) × (2.5–)3(–3.5) µm [n = 100], thin-walled, lacking a perispore at maturity (immature ascospores commonly with a perispore to 0.7 µm thick); contents clear."
# txt6<-Ascospores$text_entry[Ascospores$spec=="Paraphysalospora eucalypti_562985_79054"]
# txt<-txt5
#  txt <- gsub("\\μm", "µm", txt)
# # 
# # txt1<-
# #   txt %>%
# #   strsplit(split = " ")
# # 
# get_text(
#   txt4,start.regex="scospores$",
#   end.regex="µm")
# # "μm"
# 
# write.csv(txt,"txt.csv",row.names = F)
# txt<-read.csv("txt.csv",header = F, stringsAsFactors = F)
# txt<-txt$V1[2]
### write to file

