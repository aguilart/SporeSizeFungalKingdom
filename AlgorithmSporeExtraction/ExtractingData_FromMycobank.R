
spore.dat<- readRDS("mycobank_descriptions.RDS")#This dataset
#contains 117,481 species); the entry called base_mycobank_nr is 
#the mycobank code as it is found when checking in the website

#For later purposes I am extracting the ID´s of each entry.
#That is: the species names, the order ID and the mycobank number
Data_IDs<-spore.dat[c(1,4,8)]
# textos<-originaltextos
# names(textos)<-exampleNames
# 
# resultsFromMycobankSubset<-list(AsexualSporeBearingStructures,SexualSporeBearingStructures)

##
#To refine the search I will make separate search for the phyla
#asco-, basidiio mycota, and one general for the rest (except Glomeromycota)

#spore.dat<-left_join(spore.dat,MycobankNames_FungiwDescrpt[c(1,11:14)])

textos<-spore.dat$description_description_
names(textos)<-paste(spore.dat$base_name, spore.dat$base__id, sep ="_")


rm(spore.dat)
####################################################################################
####################spore bearing structures########################################
####################################################################################

#1. Asexual ones: this applies to conidiophores and the conidiogenous cells

#Extracting conidiophore text and the species to which the text belongs:
#Note: one has to check whether get_text has activated the lines that remove
#text with the word hymenophore.

Conidiophores_text<-#It took ~6 minutes to run it
  lapply(textos,get_text,
         start.regex="ophores$|onidiogenous$",
         end.regex="µm"#,
  )

#Extracting conidiophore values and the name of the conidiophore type
Conidiophores_values<-#It took ~1 minute to run it
  lapply(Conidiophores_text,get_dimensions2,
            extract.regex="[^a-wy-zA-WY-Z]+\\s?µm")
        #Conidiophores_values<-Conidiophores_values$`1`

##

values<- lapply(Conidiophores_values, function(x) x[[1]])

## restructure data as table
values2 <- list()#it took ~30 seconds to run it
for(i in 1:length(values)){
  x <- plyr::rbind.fill(lapply(values[[i]], function(y) { as.data.frame(t(y), stringsAsFactors=FALSE) }))
  x <- cbind(spore_type = names(values[[i]]), x)
  values2[[i]] <- x
}
names(values2) <- names(values)
q<-which(sapply(values2,is.null))#for some reasons some entries are empty

## combine all spore results in one table
values_df <- plyr::rbind.fill(lapply(values2[-q], function(y) { as.data.frame(y) }))
values_df<- cbind(spec=rep(names(values2[-q]),lapply(values2[-q], nrow)),values_df)
#values_df <- values_df[,-length(values_df)]
names(values_df)[3] <- "measure_orig"


#Merge the text with the values and spore data info
text<-lapply(Conidiophores_text,plyr::ldply, rbind)
text<-plyr::rbind.fill(text)
names(text)[1]<-"text_entry"

AsexualSporeBearingStructures<-cbind(text,values_df)
                                AsexualSporeBearingStructures<-data.frame(
                                sapply(AsexualSporeBearingStructures, as.character),
                                stringsAsFactors = F)#This creates a dataframe conatianing 132270 entries
                                #Removing empty entries
                                AsexualSporeBearingStructures<-AsexualSporeBearingStructures[-which(
                                AsexualSporeBearingStructures$text_entry=="Result not found"),]
                                #Removing them reduces the dataframe to 32258 entries

#Reviewing the cases that have conidia:
Overlap_conidia<-#This creates a dataframe with 3610 entries
  AsexualSporeBearingStructures[grep("Conidia",AsexualSporeBearingStructures$text_entry),]

#One could double check but I think most of the entries with "Conidia" should be removed
#write.csv(Overlap_conidia,"Overlap_conidia.csv")

AsexualSporeBearingStructures<-#This step produces a dataframe of 31476 entries
  AsexualSporeBearingStructures[-as.integer(row.names(Overlap_conidia)),]
AsexualSporeBearingStructures<-
  AsexualSporeBearingStructures[order(AsexualSporeBearingStructures$spec),]


#Removing duplicated values (this step reduces the dataframe to 25030:

trial<-split(AsexualSporeBearingStructures,AsexualSporeBearingStructures$spec)

t<-sapply(trial,function(x)duplicated(x$measure_orig))
t<-lapply(t,plyr::ldply, rbind)#this works like "gather"
t<-do.call("rbind",t)

AsexualSporeBearingStructures<-AsexualSporeBearingStructures[-which(t[,1]),]
rownames(AsexualSporeBearingStructures)<-NULL

#For some reason the are still a lot of entries with the word conidia, in total 2525
v<-grep("Conidia",AsexualSporeBearingStructures$text_entry)

#I remove these 2525 making a total of 22505
AsexualSporeBearingStructures<-AsexualSporeBearingStructures[-v,]
rownames(AsexualSporeBearingStructures)<-NULL

#Just to have some summary stats

length(unique(AsexualSporeBearingStructures$spec))#One gets 15104 species in the dataframe


#write.csv(AsexualSporeBearingStructures,"AsexualSporeBearingStructures.csv",row.names = F)
#one would need to make a special case for "70 um long, 5-8 um wide".

#There are weird cases where there is an appostrophe in the range of size, like:
#"Conidiogenous cells enteroblastic phialidic, subcylindrical, tapering to the apices,
#with minute collarettes, 6-8 ´ 1 µm."


#2. Sexual ones: This applies to asci and basidia

#Extracting structure text and the species to which the text belongs:

SexualStr_text<-
  lapply(textos,get_text,
         start.regex="basidia$|Basidia|asci$|Asci",
         end.regex="µm"#,
  )#Note: for running this I modified the code (silence some lines) so as not to remove
    #hymenophores entries. Those silenced lines applied to the conidiophore case before.

#Extracting structure values and the name of the structure type
SexualStr_values<-
  lapply(SexualStr_text,get_dimensions2,
         extract.regex="[^a-wy-zA-WY-Z]+\\s?µm")
#SexualStr_values<-SexualStr_values$`1`

##Arranging the data into a single dataframe

values<- lapply(SexualStr_values, function(x) x[[1]])

## restructure data as table
values2 <- list()
for(i in 1:length(values)){
  x <- plyr::rbind.fill(lapply(values[[i]], function(y) { as.data.frame(t(y), stringsAsFactors=FALSE) }))
  x <- cbind(spore_type = names(values[[i]]), x)
  values2[[i]] <- x
}
names(values2) <- names(values)

## combine all spore results in one table
values_df <- plyr::rbind.fill(lapply(values2, function(y) { as.data.frame(y) }))
values_df <- cbind(spec = rep(names(values2), lapply(values2, nrow)), values_df)
#values_df <- values_df[,-length(values_df)]
names(values_df)[3] <- "measure_orig"


#Merge the text with the values and spore data info
text<-lapply(SexualStr_text,plyr::ldply, rbind)
text<-plyr::rbind.fill(text)
names(text)[1]<-"text_entry"

#The following produces a dataframe with 122514 entries
SexualSporeBearingStructures<-cbind(text,values_df)
SexualSporeBearingStructures<-data.frame(
  sapply(SexualSporeBearingStructures, as.character),stringsAsFactors = F)

#Removing empty values (which reduces the dataframe to 42523)
SexualSporeBearingStructures<-SexualSporeBearingStructures[-which(
  SexualSporeBearingStructures$text_entry=="Result not found"
),]

#Removing the entries that refer to spores instead of basidia or asci
Overlap_spores<-
SexualSporeBearingStructures[grep("w*pores\\b",SexualSporeBearingStructures$text_entry),]
#One would need to double check whether all cases where the word spore is mentioned
#reflects the dimensions of the spore and not the spore bearing structures, for the 
#moment I am excluding all those cases:

SexualSporeBearingStructures<-#It reduces the dataframe to a 41111 entries
  SexualSporeBearingStructures[-as.integer(row.names(Overlap_spores)),]
SexualSporeBearingStructures<-
  SexualSporeBearingStructures[order(SexualSporeBearingStructures$spec),]

rownames(SexualSporeBearingStructures)<-NULL

#Removing duplicated values (which reduces the dataframe to 36475 entries:
#Note on June 2019. Maybe this step is not necessary!!
trial<-split(SexualSporeBearingStructures,SexualSporeBearingStructures$spec)
    
t<-sapply(trial,function(x)duplicated(x$measure_orig))
t<-lapply(t,plyr::ldply, rbind)
t<-do.call("rbind",t)

SexualSporeBearingStructures<-SexualSporeBearingStructures[-which(t[,1]),]

#Just some summary numbers:
length(unique(SexualSporeBearingStructures$spec))#one has info for 24376

#rm(values,values2,values_df,text)

AsexualSporeBearingStructures$measure_orig<-
paste(AsexualSporeBearingStructures$measure_orig,"µm",sep=" ")

rownames(SexualSporeBearingStructures)<-NULL
# SexualSporeBearingStructures$measure_orig<-
#   paste(SexualSporeBearingStructures$measure_orig,"µm",sep=" ")


write.table(AsexualSporeBearingStructures,"AsexualSporeBearingStructures.txt",row.names = F)
write.table(SexualSporeBearingStructures,"SexualSporeBearingStructures.txt",row.names = F)

write.csv(AsexualSporeBearingStructures,"AsexualSporeBearingStructures.csv",row.names = F)

#############################################################################

