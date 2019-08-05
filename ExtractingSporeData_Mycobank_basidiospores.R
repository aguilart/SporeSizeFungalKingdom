####################################################################################
###########################   BASIDIOSPORES    ########################################
####################################################################################

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
names(textos)<-paste(Basidiomycetes$base_name, Basidiomycetes$base__id, sep ="_")

#Now I can extract Basidiospores out of these subset

Basidiospores_text<-
  lapply(textos,get_text,
         start.regex="asidiospores$",
         end.regex="µm"#,
  )

temp <- Basidiospores_text
Basidiospores_text<-lapply(Basidiospores_text, 
                           function(x)gsub('\\([a-zA-Z]+\\. [0-9]{+}-[0-9]{1,}\\)', '', x))
Basidiospores_text<-lapply(Basidiospores_text, 
                           function(x)gsub('\\([a-zA-Z]+\\. [0-9]+\\,[0-9]+\\)', '', x))
Basidiospores_text<-lapply(Basidiospores_text, 
                           function(x)gsub('－', '', x))
Basidiospores_text<-lapply(Basidiospores_text, 
                           function(x)gsub('−', '', x))


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
Basidiospores$measure_orig<-gsub("X","x",Basidiospores$measure_orig)
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

temp$V1<-gsub(",",".",temp$V1)
temp$V2<-gsub(",",".",temp$V2)
temp$V3<-gsub(",",".",temp$V3)
# temp$V4<-gsub(",",".",temp$V4)

#('\\)','\\-)')

temp <- apply(temp, 2, function(x)gsub('\\(', '-', x))
temp <- apply(temp, 2, function(x)gsub('\\)', '-', x))
temp <- apply(temp, 2, function(x)gsub('--', '-', x))
temp <- apply(temp, 2, str_trim)
temp <- apply(temp, 2, function(x)gsub(' ', '', x))
temp <- apply(temp, 2, function(x)gsub('^-', '-', x))
temp <- apply(temp, 2, function(x)gsub('-$', '-', x))

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

# 
# #Extracting the first pattern
# library(stringr)
# s<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
# t<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
# u<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
# # v<-lapply(temp[,4], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
# 
# s_<-sapply(s,function(x){length(x[[1]])})
# t_<-sapply(t,function(x){length(x[[1]])})
# u_<-sapply(u,function(x){length(x[[1]])})
# # v_<-sapply(v,function(x){length(x[[1]])})
# 
# s<-do.call(rbind,s)
# t<-do.call(rbind,t)
# u<-do.call(rbind,u)
# # v<-do.call(rbind,v)
# 
# 
# #
# Basidiospores$Dimension_1<-NA
# Basidiospores$Dimension_1[s_==1]<-s[s_==1]
# #
# Basidiospores$Dimension_2<-NA
# Basidiospores$Dimension_2[t_==1]<-t[t_==1]
# #
# Basidiospores$Dimension_3<-NA
# Basidiospores$Dimension_3[u_==1]<-u[u_==1]
# 
# #
# #Basidiospores$Dimension_1[s_==0&t_==1]<-s[s_==0&t_==1]
# #Basidiospores$Dimension_2[w==0&v==1]<-t[w==0&v==1]
# 
# #Extracting the second pattern
# s1<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?")##
# t1<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?")
# u1<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?")
# 
# s1_<-sapply(s1,function(x){length(x[[1]])})
# t1_<-sapply(t1,function(x){length(x[[1]])})
# u1_<-sapply(u1,function(x){length(x[[1]])})
# 
# s1<-do.call(rbind,s1)
# t1<-do.call(rbind,t1)
# u1<-do.call(rbind,u1)
# #v1<-do.call(rbind,v1)
# 
# Basidiospores$Dimension_1[s1_==1]<-s1[s1_==1]
# Basidiospores$Dimension_2[t1_==1]<-t1[t1_==1]
# Basidiospores$Dimension_3[u1_==1]<-u1[u1_==1]
# 
# #Checking again
# #Basidiospores$measure_orig[is.na(Basidiospores$Dimension_1)]
# #temp$V1[is.na(Basidiospores$Dimension_1)]
# 
# #Extracting a third pattern
# s2<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
# t2<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
# u2<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
# # v2<-lapply(temp[,4], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
# 
# s2_<-sapply(s2,function(x){length(x[[1]])})
# t2_<-sapply(t2,function(x){length(x[[1]])})
# u2_<-sapply(u2,function(x){length(x[[1]])})
# 
# s2<-do.call(rbind,s2)
# t2<-do.call(rbind,t2)
# u2<-do.call(rbind,u2)
# 
# Basidiospores$Dimension_1[s2_==1]<-s2[s2_==1]
# Basidiospores$Dimension_2[t2_==1]<-t2[t2_==1]
# Basidiospores$Dimension_3[u2_==1]<-u2[u2_==1]
# 
# # #Checking again
# # Basidiospores$measure_orig[is.na(Basidiospores$Dimension_1)]
# # 
# # Basidiospores$text_entry[is.na(Basidiospores$Dimension_1)][117]
# # 
# # #Solving cases where "x" is missing and instead there is "--"
# # missing_x<-grep("--",Basidiospores$measure_orig[is.na(Basidiospores$Dimension_1)])
# # Basidiospores$measure_orig[is.na(Basidiospores$Dimension_1)][missing_x]<-
# #   gsub("--"," - ",
# #        Basidiospores$measure_orig[is.na(Basidiospores$Dimension_1)][missing_x])
# # 
# # 
# # #
# # Basidiospores$Dimension_1<-NA
# # Basidiospores$Dimension_1[s_==1]<-s[s_==1]
# # #
# # Basidiospores$Dimension_2<-NA
# # Basidiospores$Dimension_2[t_==1]<-t[t_==1]
# # #
# # Basidiospores$Dimension_3<-NA
# # Basidiospores$Dimension_3[u_==1]<-u[u_==1]
# # 
# # #
# # #Basidiospores$Dimension_1[s_==0&t_==1]<-s[s_==0&t_==1]
# # #Basidiospores$Dimension_2[w==0&v==1]<-t[w==0&v==1]
# # 
# # #Extracting the second pattern
# # s1<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?")##
# # t1<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?")
# # u1<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?")
# # 
# # s1_<-sapply(s1,function(x){length(x[[1]])})
# # t1_<-sapply(t1,function(x){length(x[[1]])})
# # u1_<-sapply(u1,function(x){length(x[[1]])})
# # 
# # s1<-do.call(rbind,s1)
# # t1<-do.call(rbind,t1)
# # u1<-do.call(rbind,u1)
# # #v1<-do.call(rbind,v1)
# # 
# # Basidiospores$Dimension_1[s1_==1]<-s1[s1_==1]
# # Basidiospores$Dimension_2[t1_==1]<-t1[t1_==1]
# # Basidiospores$Dimension_3[u1_==1]<-u1[u1_==1]
# # 
# # #Extracting a third pattern
# # s2<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
# # t2<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
# # u2<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
# # v2<-lapply(temp[,4], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
# # 
# # s2_<-sapply(s2,function(x){length(x[[1]])})
# # t2_<-sapply(t2,function(x){length(x[[1]])})
# # u2_<-sapply(u2,function(x){length(x[[1]])})
# # 
# # s2<-do.call(rbind,s2)
# # t2<-do.call(rbind,t2)
# # u2<-do.call(rbind,u2)
# # 
# # Basidiospores$Dimension_1[s2_==1]<-s2[s2_==1]
# # Basidiospores$Dimension_2[t2_==1]<-t2[t2_==1]
# # Basidiospores$Dimension_3[u2_==1]<-u2[u2_==1]
# 
# 
# #Extracting a fourth pattern
# s4<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?\\d?\\-\\d+\\.?\\d?\\d?\\-\\d+\\.?\\d?\\d?")
# t4<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?\\d?\\-\\d+\\.?\\d?\\d?\\-\\d+\\.?\\d?\\d?")
# u2<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
# 
# s4_<-sapply(s4,function(x){length(x[[1]])})
# t4_<-sapply(t4,function(x){length(x[[1]])})
# # u2_<-sapply(u2,function(x){length(x[[1]])})
# # 
# s4<-do.call(rbind,s4)
# t4<-do.call(rbind,t4)
# 
# Basidiospores$Dimension_1[s4_==1]<-s4[s4_==1]
# #Basidiospores$Dimension_2[t4_==1]<-t4[t4_==1][550]
# Basidiospores$Dimension_1<-unlist(Basidiospores$Dimension_1)
# Basidiospores$Dimension_2<-unlist(Basidiospores$Dimension_2)
# Basidiospores$Dimension_3<-unlist(Basidiospores$Dimension_3)
# 
# 
# #Extracting single values out of the ranges (for dimension 1)
# t<-strsplit(Basidiospores$Dimension_1,"-")
# s<-sapply(t,length)
# 
# #Summing the two extremes and then dividing by 2
# t[s==2]<-lapply(t[s==2],function(x){x<-(as.numeric(x[1])+as.numeric(x[2]))/2})
# s_<-lapply(t,length)
# which(s_!=1);rm(s_)#now I got only one value for every entry as desired
# 
# t1<-Basidiospores$Dimension_1[grep("\\+\\-",Basidiospores$Dimension_1)]
# t1<-strsplit(t1,"\\+-")
# t1<-lapply(t1,function(x){x<-as.numeric(x[[1]])})
# 
# #Adding these values to the dataset:
# Basidiospores$Mean_Dim1<-unlist(t)
# Basidiospores$Mean_Dim1<-as.numeric(Basidiospores$Mean_Dim1)
# Basidiospores$Mean_Dim1[grep("\\+\\-",Basidiospores$Dimension_1)]<-t1
# 
# 
# #Repating the same logic for Dimension 2
# #Extracting single values out of the ranges
# t<-strsplit(Basidiospores$Dimension_2,"-")
# s<-sapply(t,length)
# 
# #Summing the two extremes and then dividing by 2
# t[s==2]<-lapply(t[s==2],function(x){x<-(as.numeric(x[1])+as.numeric(x[2]))/2})
# s_<-lapply(t,length)
# which(s_!=1);rm(s_)#now I got only one value for every entry as desired
# 
# t1<-Basidiospores$Dimension_2[grep("\\+\\-",Basidiospores$Dimension_2)]
# t1<-strsplit(t1,"\\+-")
# t1<-lapply(t1,function(x){x<-as.numeric(x[[1]])})
# 
# 
# #Adding these values to the dataset:
# Basidiospores$Mean_Dim2<-unlist(t)
# Basidiospores$Mean_Dim2<-as.numeric(Basidiospores$Mean_Dim2)
# Basidiospores$Mean_Dim2[grep("\\+\\-",Basidiospores$Dimension_2)]<-t1
# 
# Basidiospores$Mean_Dim2[is.na(Basidiospores$Dimension_2)][
#   -grep("x",Basidiospores$measure_orig[is.na(Basidiospores$Dimension_2)])]<-
#                                                       Basidiospores$Mean_Dim1[is.na(Basidiospores$Dimension_2)][
#                                                   -grep("x",Basidiospores$measure_orig[is.na(Basidiospores$Dimension_2)])]
# 
# Basidiospores$measure_orig[is.na(Basidiospores$Mean_Dim2)]
# 
# temp1 <- filter(Basidiospores, is.na(Basidiospores$Mean_Dim2))
# 
# #Notes:Basidiospores$measure_orig[is.na(Basidiospores$Dimension_2)][558]
# 
# #Ways to measure how many spores per asci: "Asci 8-spored, 9-17 — 7-20 µm"
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ##### maybe useful #####
# # 
# # 
# # #Extracting 4th pattern:
# # # temp$V1[is.na(Basidiospores$Dimension_1)][
# # # grep("\\)\\s?\\d+\\.?\\d?\\s?\\(",temp$V1[is.na(Basidiospores$Dimension_1)])
# # # ]
# # 
# # s3<-s1[is.na(Basidiospores$Dimension_1)][
# #   grep("\\)\\s?\\d+\\.?\\d?\\s?\\(",temp$V1[is.na(Basidiospores$Dimension_1)])
# #   ]
# # 
# # # temp$V2[is.na(Basidiospores$Dimension_2)][
# # #   grep("\\)\\s?\\d+\\.?\\d?\\s?\\(",temp$V2[is.na(Basidiospores$Dimension_2)])
# # #   ]
# # 
# # t3<-t1[is.na(Basidiospores$Dimension_2)][
# #   grep("\\)\\s?\\d+\\.?\\d?\\s?\\(",temp$V2[is.na(Basidiospores$Dimension_2)])
# #   ]
# # #
# # Basidiospores$Dimension_1[is.na(Basidiospores$Dimension_1)][
# #   grep("\\)\\s?\\d+\\.?\\d?\\s?\\(",temp$V1[is.na(Basidiospores$Dimension_1)])
# #   ]<-sapply(s3,function(x){x[2]})
# # 
# # Basidiospores$Dimension_2[is.na(Basidiospores$Dimension_2)][
# #   grep("\\)\\s?\\d+\\.?\\d?\\s?\\(",temp$V2[is.na(Basidiospores$Dimension_2)])
# #   ]<-sapply(t3,function(x){x[2]})
# # 
# # #
# # 
# # #Extracting 5th pattern:
# # Basidiospores$measure_orig[is.na(Basidiospores$Dimension_1)][
# #   grep("\\[\\s?\\d+\\.?\\d?\\s?\\]",temp$V1[is.na(Basidiospores$Dimension_1)])
# #   ][-c(1:4)]
# # 
# # 
# # temp$V1[is.na(Basidiospores$Dimension_1)][
# #   grep("\\[\\s?\\d+\\.?\\d?\\s?\\]",temp$V1[is.na(Basidiospores$Dimension_1)])
# #   ]
# # 
# # s4<-s1[is.na(Basidiospores$Dimension_1)][
# #   grep("\\[\\s?\\d+\\.?\\d?\\s?\\]",temp$V1[is.na(Basidiospores$Dimension_1)])
# #   ][-c(1:4)]
# # #
# # temp$V2[is.na(Basidiospores$Dimension_2)][
# #   grep("\\[\\s?\\d+\\.?\\d?\\s?\\]",temp$V2[is.na(Basidiospores$Dimension_2)])
# #   ][-c(1:3)]
# # 
# # t4<-t1[is.na(Basidiospores$Dimension_2)][
# #   grep("\\[\\s?\\d+\\.?\\d?\\s?\\]",temp$V2[is.na(Basidiospores$Dimension_2)])
# #   ][-c(1:3)]
# # #
# # Basidiospores$Dimension_1[is.na(Basidiospores$Dimension_1)][
# #   grep("\\[\\s?\\d+\\.?\\d?\\s?\\]",temp$V1[is.na(Basidiospores$Dimension_1)])
# #   ][-c(1:4)]<-sapply(s4,function(x){x[2]})
# # 
# # Basidiospores$Dimension_2[is.na(Basidiospores$Dimension_2)][
# #   grep("\\[\\s?\\d+\\.?\\d?\\s?\\]",temp$V2[is.na(Basidiospores$Dimension_2)])
# #   ][-c(1:3)]<-sapply(t4,function(x){x[2]})
# # 
# # #Solving remaining random problems
# # problems<-which(is.na(Basidiospores$Dimension_1))
# # 
# # Basidiospores$Dimension_1[problems][2]<- "13-17"
# # Basidiospores$Dimension_2[problems][2]<- "6-7"
# # Basidiospores$Dimension_1[problems][9]<- "7.5-16"
# # Basidiospores$Dimension_2[problems][9]<- "2.5-3"
# # Basidiospores$Dimension_1[problems][11]<- "22.6-23.5"
# # Basidiospores$Dimension_2[problems][11]<- "10.6-11.2"
# # Basidiospores$Dimension_1[problems][16]<- "7.5-16"
# # Basidiospores$Dimension_2[problems][16]<- "2.5-3"
# # Basidiospores$Dimension_1[problems][17]<- "30"
# # Basidiospores$Dimension_2[problems][17]<- "11"
# # Basidiospores$Dimension_1[problems][19]<- "11.5-14"
# # Basidiospores$Dimension_2[problems][19]<- "5-6"
# # Basidiospores$Dimension_1[problems][20]<- "8.82"
# # Basidiospores$Dimension_2[problems][20]<- "3.01"
# # Basidiospores$Dimension_1[problems][23]<- "22.6-23.5"
# # Basidiospores$Dimension_2[problems][23]<- "10.6-11.2"
# # Basidiospores$Dimension_1[problems][25]<- "10"
# # Basidiospores$Dimension_2[problems][25]<- "6.1"
# # Basidiospores$Dimension_1[problems][26]<- "10"
# # Basidiospores$Dimension_2[problems][26]<- "6.1"
# # Basidiospores$Dimension_1[problems][27]<- "34.85"
# # Basidiospores$Dimension_2[problems][27]<- "14"
# # Basidiospores$Dimension_1[problems][28]<- "34.85"
# # Basidiospores$Dimension_2[problems][28]<- "14"
# # Basidiospores$Dimension_1[problems][29]<- "34.85"
# # Basidiospores$Dimension_2[problems][29]<- "14"
# # Basidiospores$Dimension_1[problems][30]<- "13.75"
# # Basidiospores$Dimension_2[problems][30]<- "4.5"
# # Basidiospores$Dimension_1[problems][31]<- "13.77"
# # Basidiospores$Dimension_2[problems][31]<- "4.0"
# # Basidiospores$Dimension_1[problems][32]<- "24"
# # Basidiospores$Dimension_2[problems][32]<- "9"
# # Basidiospores$Dimension_1[problems][34]<- "10.77-11.5"
# # Basidiospores$Dimension_2[problems][34]<- "5.53"
# # Basidiospores$Dimension_1[problems][35]<- "6.01"
# # Basidiospores$Dimension_2[problems][35]<- "2.78"
# # Basidiospores$Dimension_1[problems][36]<- "10.77-11.5"
# # Basidiospores$Dimension_2[problems][36]<- "5.53"
# # Basidiospores$Dimension_1[problems][37]<- "23.8-24.7"
# # Basidiospores$Dimension_2[problems][37]<- "5.2-5.7"
# # Basidiospores$Dimension_1[problems][38]<- "23.2-27.6"
# # Basidiospores$Dimension_2[problems][38]<- "6.1-6.8"
# # Basidiospores$Dimension_1[problems][39]<- "23.2-27.6"                         
# # Basidiospores$Dimension_2[problems][39]<- "6.1-6.8"
# # Basidiospores$Dimension_1[problems][40]<- "3.0"                                                 
# # Basidiospores$Dimension_2[problems][40]<- "3.0"                                                 
# # Basidiospores$Dimension_1[problems][41]<- "20.2-20.3"                         
# # Basidiospores$Dimension_2[problems][41]<- "5.1-5.2" 
# # Basidiospores$Dimension_1[problems][42]<- "23.0"
# # Basidiospores$Dimension_2[problems][42]<- "12.3"
# # Basidiospores$Dimension_1[problems][43]<- "16.7" 
# # Basidiospores$Dimension_2[problems][43]<- "10.2" 
# # Basidiospores$Dimension_1[problems][44]<- "7.5-16"                                                
# # Basidiospores$Dimension_2[problems][44]<- "2.5-3"
# # Basidiospores$Dimension_1[problems][45]<- "7.5-16"                                                
# # Basidiospores$Dimension_2[problems][45]<- "2.5-3"                                                
# # Basidiospores$Dimension_1[problems][46]<- "8.84"
# # Basidiospores$Dimension_2[problems][46]<- "2.91"
# # Basidiospores$Dimension_1[problems][48]<- "24"                                         
# # Basidiospores$Dimension_2[problems][48]<- "9"
# # Basidiospores$Dimension_1[problems][49]<- "8.84"
# # Basidiospores$Dimension_2[problems][49]<- "2.91"
# # Basidiospores$Dimension_1[problems][50]<- "11.5"
# # Basidiospores$Dimension_2[problems][50]<- "7.4"
# # Basidiospores$Dimension_1[problems][52]<- "30"
# # Basidiospores$Dimension_2[problems][52]<- "10"
# # Basidiospores$Dimension_1[problems][53]<- "8-10"
# # Basidiospores$Dimension_2[problems][53]<- "2.5-3.5"
# # Basidiospores$Dimension_1[problems][54]<- "120-180"
# # Basidiospores$Dimension_2[problems][54]<- "1-1.5"
# # Basidiospores$Dimension_1[problems][55]<- "6.96"
# # Basidiospores$Dimension_2[problems][55]<- "3.15"
# # Basidiospores$Dimension_1[problems][56]<- "22-28"
# # Basidiospores$Dimension_2[problems][56]<- "10-13"
# # Basidiospores$Dimension_1[problems][57]<- "25-29"
# # Basidiospores$Dimension_2[problems][57]<- "11-15"
# # Basidiospores$Dimension_1[problems][58]<- "22-30"
# # Basidiospores$Dimension_2[problems][58]<- "10-14"
# # Basidiospores$Dimension_1[problems][60]<- "4.0-4.3"
# # Basidiospores$Dimension_2[problems][60]<- "3.7-4.0"
# # Basidiospores$Dimension_1[problems][61]<- "4.0-4.3"                   
# # Basidiospores$Dimension_2[problems][61]<- "3.7-4.0"
# # Basidiospores$Dimension_1[problems][62]<- "4.4-4.5"
# # Basidiospores$Dimension_2[problems][62]<- "4.0-4.2"
# # Basidiospores$Dimension_1[problems][63]<- "4.4-4.5"
# # Basidiospores$Dimension_2[problems][63]<- "4.0-4.2"
# # Basidiospores$Dimension_1[problems][64]<- "2.5-3.5"
# # Basidiospores$Dimension_2[problems][64]<- "2.2-3.4"
# # Basidiospores$Dimension_1[problems][67]<- "5.95"
# # Basidiospores$Dimension_2[problems][67]<- "5.95"
# # Basidiospores$Dimension_1[problems][68]<- "10"
# # Basidiospores$Dimension_2[problems][68]<- "2.5-3"
# # Basidiospores$Dimension_1[problems][69]<- "33"
# # Basidiospores$Dimension_2[problems][69]<- "12"
# # Basidiospores$Dimension_1[problems][70]<- "5"                                                         
# # Basidiospores$Dimension_2[problems][70]<- "5"
# # Basidiospores$Dimension_1[problems][71]<- "31-45"
# # Basidiospores$Dimension_2[problems][71]<- "31-47"
# # Basidiospores$Dimension_1[problems][72]<- "32-46"
# # Basidiospores$Dimension_2[problems][72]<- "32-46"
# # Basidiospores$Dimension_1[problems][74]<- "11.1-13.7"
# # Basidiospores$Dimension_2[problems][74]<- "3.8-5.0"
# # Basidiospores$Dimension_1[problems][75]<- "12-15"
# # Basidiospores$Dimension_2[problems][75]<- "2-3"
# # Basidiospores$Dimension_1[problems][77]<- "6.1-9.1"
# # Basidiospores$Dimension_2[problems][77]<- "1.7-2.0"
# # Basidiospores$Dimension_1[problems][78]<- "13.5-22.1"
# # Basidiospores$Dimension_2[problems][78]<- "2.7-3.9"
# # Basidiospores$Dimension_1[problems][79]<- "9.1-12.3"
# # Basidiospores$Dimension_2[problems][79]<- "2.8-4.2"
# # Basidiospores$Dimension_1[problems][81]<- "50.0-72.5"
# # Basidiospores$Dimension_2[problems][81]<- "1.5-1.9"
# # Basidiospores$Dimension_1[problems][82]<- "9.6-12.4"
# # Basidiospores$Dimension_2[problems][82]<- "1.6-2.0"
# # Basidiospores$Dimension_1[problems][83]<- "10.4-13.0"
# # Basidiospores$Dimension_2[problems][83]<- "3.5-4.3"
# # Basidiospores$Dimension_1[problems][84]<- "16.8-22.8"
# # Basidiospores$Dimension_2[problems][84]<- "2.3-2.9"
# # Basidiospores$Dimension_1[problems][88]<- "8.9-10.4"
# # Basidiospores$Dimension_2[problems][88]<- "4.8-6.2"
# # Basidiospores$Dimension_1[problems][89]<- "20-24"
# # Basidiospores$Dimension_2[problems][89]<- "10-11"
# # Basidiospores$Dimension_1[problems][90]<- "3.5-4"
# # Basidiospores$Dimension_2[problems][90]<- "3.5-4"
# # Basidiospores$Dimension_1[problems][91]<- "4.5−5"
# # Basidiospores$Dimension_2[problems][91]<- "4.5−5"
# # Basidiospores$Dimension_1[problems][92]<- "2.5-3"
# # Basidiospores$Dimension_2[problems][92]<- "2.5-3"
# # Basidiospores$Dimension_1[problems][93]<- "3.5−4"
# # Basidiospores$Dimension_2[problems][93]<- "3.5−4"
# # Basidiospores$Dimension_1[problems][94]<- "3-3.5"
# # Basidiospores$Dimension_2[problems][94]<- "3-3.5"
# # Basidiospores$Dimension_1[problems][95]<- "3-3.5"
# # Basidiospores$Dimension_2[problems][95]<- "3-3.5"
# # Basidiospores$Dimension_1[problems][96]<- "45-100"
# # Basidiospores$Dimension_2[problems][96]<- "45-100"
# # Basidiospores$Dimension_1[problems][97]<- "30-130"
# # Basidiospores$Dimension_2[problems][97]<- "30-130"
# # Basidiospores$Dimension_1[problems][98]<- "5-5.5"
# # Basidiospores$Dimension_2[problems][98]<- "5-5.5"
# # Basidiospores$Dimension_1[problems][99]<- "4.5-5.5"
# # Basidiospores$Dimension_2[problems][99]<- "4.5-5.5"
# # Basidiospores$Dimension_1[problems][100]<- "5-5.5"
# # Basidiospores$Dimension_2[problems][100]<- "5-5.5"
# # Basidiospores$Dimension_1[problems][101]<- "53.7-73.7"
# # Basidiospores$Dimension_2[problems][101]<- "3.0-3.8"
# # Basidiospores$Dimension_1[problems][102]<- "100-150"
# # Basidiospores$Dimension_2[problems][102]<- "100-150"
# # Basidiospores$Dimension_1[problems][103]<- "45-50"
# # Basidiospores$Dimension_2[problems][103]<- "45-50"
# # Basidiospores$Dimension_1[problems][104]<- "15-17"
# # Basidiospores$Dimension_2[problems][104]<- "3-4"
# # Basidiospores$Dimension_1[problems][105]<- "12-14"
# # Basidiospores$Dimension_2[problems][105]<- "6-7"
# # Basidiospores$Dimension_1[problems][108]<- "14.7-17.9"
# # Basidiospores$Dimension_2[problems][108]<- "3.0-3.8"
# # 
# # rm(problems)
# # 
# # #Extracting the second pattern
# # 
# # problem<-which(is.na(Basidiospores$Dimension_2))
# # Basidiospores$Dimension_2[problem]
# # 
# # 
# # 
# # #Remaining 30 entries with problems (not processed)
# # Basidiospores$measure_orig[which(is.na(Basidiospores$Dimension_1))]
# # 
# # #GETTING AVERAGE DIMENSION VALUES
# # Basidiospores_copy<-Basidiospores
# # 
# # 
