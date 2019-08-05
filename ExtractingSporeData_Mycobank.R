####################################################################################
###########################   ASCOSPORES    ########################################
####################################################################################

#Source of the spore data data:
spore.dat<- readRDS("mycobank_descriptions.RDS")#This dataset
#contains 117,481 species); the entry called base_mycobank_nr is 
#the mycobank code as it is found when checking in the website and it is the table
#that Will sent us on November 2019

#source of taxonomic data:
Mycobank_Taxonomy#This dataframe was made on the "Checking_Taxonomy.R" code and 
#reflects the the higher rank taxonomy of species and infraspecies of true fungi

spore.dat<-left_join(spore.dat,Mycobank_Taxonomy[c(4,12:19)],
                     by="base_mycobanknr_")

#1. Ascospores: this applies only to the ascomycota subset of the Data

#Subsetting only ascomycetes

Ascomycetes<-spore.dat[spore.dat$Phylum==" Ascomycota",]#Retruning 54011 entries

#Just checking what did get NA:
p<-spore.dat[which(is.na(spore.dat$Phylum)),]
p<-p[grep("\\w\\s\\w",p$base_name),]
p<-p[,c(1:28)]
p<-left_join(p,MycobankNames_list[,c(5,7)]%>%
               rename(base_mycobanknr_=MycoBank__)%>%
               mutate(base_mycobanknr_=as.character(base_mycobanknr_)),
             by="base_mycobanknr_")
p<-p[,c(1:4,29,5:28)]
p<-p[grep("Fungi",p$Classification),]
p<-p[-grep("Fossil",p$Classification),]
#Out of this excericse I got that NA was attributed to only 175 species
#all of which are Incertae sedis. For the moment I will just ignore these
#species because they are so few.
rm(p)
Ascomycetes<-Ascomycetes[-which(is.na(Ascomycetes$Phylum)),]#This reduces it to 45416entries

textos<-Ascomycetes$description_description_
names(textos)<-paste(Ascomycetes$base_name, Ascomycetes$base__id, sep ="_")

#Now I can extract Ascospores out of these subset

ascospores_text<-
  lapply(textos,get_text,
         start.regex="scospores$",
         end.regex="µm"#,
  )



#Extracting ascospores values
ascospores_values<-
  lapply(ascospores_text,get_dimensions2,
         extract.regex="[^a-wy-zA-WY-Z]+\\s?µm")
#ascospores_values<-ascospores_values$`1`

#Reformatting it into a dataframe

values<- lapply(ascospores_values, function(x) x[[1]])

## restructure data as table
values2 <- list()#it took ~30 seconds to run it
for(i in 1:length(values)){
  x <- plyr::rbind.fill(lapply(values[[i]], function(y) { as.data.frame(t(y), stringsAsFactors=FALSE) }))
  x <- cbind(spore_type = names(values[[i]]), x)
  values2[[i]] <- x
}
names(values2) <- names(values)

#It seems one does not need this for ascospores:
#q<-which(sapply(values2,is.null))#for some reasons some entries are empty

## combine all spore results in one table
#values_df <- plyr::rbind.fill(lapply(values2[-q], function(y) { as.data.frame(y) }))
values_df <- plyr::rbind.fill(lapply(values2, function(y) { as.data.frame(y) }))
#values_df<- cbind(spec=rep(names(values2[-q]),lapply(values2[-q], nrow)),values_df)
values_df<- cbind(spec=rep(names(values2),lapply(values2, nrow)),values_df)
#values_df <- values_df[,-length(values_df)]
names(values_df)[3] <- "measure_orig"


#Merge the text with the values and spore data info
text<-lapply(ascospores_text,plyr::ldply, rbind)
text<-plyr::rbind.fill(text)
names(text)[1]<-"text_entry"

#Creation of the object "Ascospores" containing all the data
Ascospores<-cbind(text,values_df)#For some reason the transformations above return 47032, instead of 45416 elements that has ascospores_text and ascospores_values. However, it seems fine!
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

####  Extracting the spore ranges  ######

t<-strsplit(Ascospores$measure_orig,"x")

s<-sapply(t,length)
temp<-plyr::rbind.fill(lapply(t, function(y) { as.data.frame(t(y)) }))

temp$V1<-gsub(",",".",temp$V1)
temp$V2<-gsub(",",".",temp$V2)
temp$V3<-gsub(",",".",temp$V3)
temp$V4<-gsub(",",".",temp$V4)


#Extracting the first pattern
library(stringr)
s<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
t<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
u<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
v<-lapply(temp[,4], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")

s_<-sapply(s,function(x){length(x[[1]])})
t_<-sapply(t,function(x){length(x[[1]])})
u_<-sapply(u,function(x){length(x[[1]])})
v_<-sapply(v,function(x){length(x[[1]])})

s<-do.call(rbind,s)
t<-do.call(rbind,t)
u<-do.call(rbind,u)
v<-do.call(rbind,v)

#Solving by hand some entries mixing the "x". All these ones were thoroughly check!
Ascospores$measure_orig[which(s_==4)][1]<-"10.0-10.77-11.5(-12.0) x 5.0-5.53-6.0"
Ascospores$measure_orig[which(s_==4)][2]<-"4-6.01-7.5 x 1.5-2.78-3.5"
Ascospores$measure_orig[which(s_==4)][3]<-"21.5-23.8-24.7-27.0 x 4.5-5.2-5.7-6.5"
Ascospores$measure_orig[which(s_==4)][4]<-"21.5-23.8-24.7-27.0 x 4.5-5.2-5.7-6.5"
Ascospores$measure_orig[which(s_==4)][5]<-"21.0-23.2-27.6-29.0 x 5.0-6.1-6.8-8.0"
Ascospores$measure_orig[which(s_==4)][6]<-"21.0-23.2-27.6-29.0 x 5.0-6.1-6.8-8.0"
Ascospores$measure_orig[which(s_==4)][7]<-"18.0-20.2-20.3-22.5 x 4.5-5.1-5.2-5.5"
Ascospores$measure_orig[which(s_==4)][8]<-"(4.5-)5-6.4-8 x 2-2.7-3"
Ascospores$measure_orig[which(s_==4)][9]<-"7-7.9-9 x 2-2.9-3·5"
Ascospores$measure_orig[which(s_==4)][10]<-"7-7.9-9 x 2-2.9-3.5"
Ascospores$measure_orig[which(s_==4)][11]<-"(4.5-)5-6.4-8 x 2-2.7-3"

#Doing it again:
t<-strsplit(Ascospores$measure_orig,"x")

s<-sapply(t,length)
temp<-plyr::rbind.fill(lapply(t, function(y) { as.data.frame(t(y)) }))

temp$V1<-gsub(",",".",temp$V1)
temp$V2<-gsub(",",".",temp$V2)
temp$V3<-gsub(",",".",temp$V3)
temp$V4<-gsub(",",".",temp$V4)


#Extracting the first pattern
library(stringr)
s<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
t<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
u<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
v<-lapply(temp[,4], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")

s_<-sapply(s,function(x){length(x[[1]])})
t_<-sapply(t,function(x){length(x[[1]])})
u_<-sapply(u,function(x){length(x[[1]])})
v_<-sapply(v,function(x){length(x[[1]])})

s<-do.call(rbind,s)
t<-do.call(rbind,t)
u<-do.call(rbind,u)
v<-do.call(rbind,v)

#There were more problems with missing "x". See: Ascospores$measure_orig[s_==3]. These ones were thoroughly checked!
Ascospores$measure_orig[which(s_==3)][1]<-"3-7 x 1-2"#Look at "Ophiostoma africanum_58406" to check why the numbers are so oddly written
Ascospores$measure_orig[which(s_==3)][2]<-"(28.5-)30.0-34.85-40.0(-44) x 12.0-14.0-16.0(-16.5)"
Ascospores$measure_orig[which(s_==3)][3]<-"(28.5-)30.0-34.85-40.0(-44) x 12.0-14.0-16.0(-16.5)" 
Ascospores$measure_orig[which(s_==3)][4]<-"(28.5-)30.0-34.85-40.0(-44) x 12.0-14.0-16.0(-16.5)"
Ascospores$measure_orig[which(s_==3)][5]<-"(12.5-)13.0-13.75-14.5(-15.5) x (-3.5)4.0-4.5-5.0(-5.5)"
Ascospores$measure_orig[which(s_==3)][6]<-"(12.0-)12.5-13.77-15.0(-16.5) x (-3.0)3.5-4.0-4.5"
Ascospores$measure_orig[which(s_==3)][7]<-"(7.5-)8.5-10(-11) x (5-)6-7.5(-8) x (3-)4-5" 
Ascospores$measure_orig[which(s_==3)][8]<-"(7.5-)8.5-10(-11) x (5-)6-7.5(-8) x (3-)4-5"
Ascospores$measure_orig[which(s_==3)][9]<-"16.5-20.5 x 13.5-15 x 11-12.5"
Ascospores$measure_orig[which(s_==3)][10]<-"16.5-21 x 10.5-13 x 9-11.5"
Ascospores$measure_orig[which(s_==3)][11]<-"17.5-21.5 x 11-13 x 9-11"
Ascospores$measure_orig[which(s_==3)][12]<-"18-22 x 11.5-14 x 9.5-11"
Ascospores$measure_orig[which(s_==3)][13]<-"28-33.0-38 x 5.0-6.0-7.0"
Ascospores$measure_orig[which(s_==3)][14]<-"28-33.0-38 x 5.0-6.0-7.0"
Ascospores$measure_orig[which(s_==3)][15]<-"22-26-30 x 5.0-5.9-7.0" 
Ascospores$measure_orig[which(s_==3)][16]<-"22-26-30 x 5.0-5.9-7.0"
Ascospores$measure_orig[which(s_==3)][17]<-"18.0-22.5 x 5.0-7.5"
Ascospores$measure_orig[which(s_==3)][18]<-"23-26.3-28 x 10-11.6-12.5"
Ascospores$measure_orig[which(s_==3)][19]<-"23-26.3-28 x 10-11.6-12.5"
Ascospores$measure_orig[which(s_==3)][20]<-"5-8 x 4-5.5 x 3-4"
Ascospores$measure_orig[which(s_==3)][21]<-"(21-22) 22.4-24.5 (25-26) x (10.5) 11.4-12 (13)"

#Doing it again:
t<-strsplit(Ascospores$measure_orig,"x")

s<-sapply(t,length)
temp<-plyr::rbind.fill(lapply(t, function(y) { as.data.frame(t(y)) }))

temp$V1<-gsub(",",".",temp$V1)
temp$V2<-gsub(",",".",temp$V2)
temp$V3<-gsub(",",".",temp$V3)
temp$V4<-gsub(",",".",temp$V4)


#Extracting the first pattern
library(stringr)
s<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
t<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
u<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
v<-lapply(temp[,4], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")

s_<-sapply(s,function(x){length(x[[1]])})
t_<-sapply(t,function(x){length(x[[1]])})
u_<-sapply(u,function(x){length(x[[1]])})
v_<-sapply(v,function(x){length(x[[1]])})

s<-do.call(rbind,s)
t<-do.call(rbind,t)
u<-do.call(rbind,u)
v<-do.call(rbind,v)

#
Ascospores$Dimension_1<-NA
Ascospores$Dimension_1[s_==1]<-s[s_==1]
#
Ascospores$Dimension_2<-NA
Ascospores$Dimension_2[t_==1]<-t[t_==1]
#
Ascospores$Dimension_3<-NA
Ascospores$Dimension_3[u_==1]<-u[u_==1]

#
#Ascospores$Dimension_1[s_==0&t_==1]<-s[s_==0&t_==1]
#Ascospores$Dimension_2[w==0&v==1]<-t[w==0&v==1]

#Extracting the second pattern
s1<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?")##
t1<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?")
u1<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?")


s1_<-sapply(s1,function(x){length(x[[1]])})
t1_<-sapply(t1,function(x){length(x[[1]])})
u1_<-sapply(u1,function(x){length(x[[1]])})

s1<-do.call(rbind,s1)
t1<-do.call(rbind,t1)
u1<-do.call(rbind,u1)
#v1<-do.call(rbind,v1)

Ascospores$Dimension_1[s1_==1]<-s1[s1_==1]
Ascospores$Dimension_2[t1_==1]<-t1[t1_==1]
Ascospores$Dimension_3[u1_==1]<-u1[u1_==1]

#Revising cases where no could be extracted with the above patterns

#The ones that have missing "x"
missing_x<-grep("\\)\\-\\(",Ascospores$measure_orig[is.na(Ascospores$Dimension_1)])

Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][missing_x]<-
  gsub("\\)\\-\\(",") x (",
       Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][missing_x])

#Doing it again:
t<-strsplit(Ascospores$measure_orig,"x")

s<-sapply(t,length)
temp<-plyr::rbind.fill(lapply(t, function(y) { as.data.frame(t(y)) }))

temp$V1<-gsub(",",".",temp$V1)
temp$V2<-gsub(",",".",temp$V2)
temp$V3<-gsub(",",".",temp$V3)
temp$V4<-gsub(",",".",temp$V4)


#Extracting the first pattern
#library(stringr)
s<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
t<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
u<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
v<-lapply(temp[,4], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")

s_<-sapply(s,function(x){length(x[[1]])})
t_<-sapply(t,function(x){length(x[[1]])})
u_<-sapply(u,function(x){length(x[[1]])})
v_<-sapply(v,function(x){length(x[[1]])})


s<-do.call(rbind,s)
t<-do.call(rbind,t)
u<-do.call(rbind,u)
v<-do.call(rbind,v)

#
Ascospores$Dimension_1<-NA
Ascospores$Dimension_1[s_==1]<-s[s_==1]
#
Ascospores$Dimension_2<-NA
Ascospores$Dimension_2[t_==1]<-t[t_==1]
#
Ascospores$Dimension_3<-NA
Ascospores$Dimension_3[u_==1]<-u[u_==1]

#
#Ascospores$Dimension_1[s_==0&t_==1]<-s[s_==0&t_==1]
#Ascospores$Dimension_2[w==0&v==1]<-t[w==0&v==1]

#Extracting the second pattern
s1<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?")##
t1<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?")
u1<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?")

s1_<-sapply(s1,function(x){length(x[[1]])})
t1_<-sapply(t1,function(x){length(x[[1]])})
u1_<-sapply(u1,function(x){length(x[[1]])})

s1<-do.call(rbind,s1)
t1<-do.call(rbind,t1)
u1<-do.call(rbind,u1)
#v1<-do.call(rbind,v1)

Ascospores$Dimension_1[s1_==1]<-s1[s1_==1]
Ascospores$Dimension_2[t1_==1]<-t1[t1_==1]
Ascospores$Dimension_3[u1_==1]<-u1[u1_==1]

#Checking again
#Ascospores$measure_orig[is.na(Ascospores$Dimension_1)]


#temp$V1[is.na(Ascospores$Dimension_1)]

#Solving cases where "x" is missing and instead there is "´"
missing_x<-grep("´",Ascospores$measure_orig[is.na(Ascospores$Dimension_1)])
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][missing_x]<-
  gsub("´"," x ",
       Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][missing_x])

#Doing it again:
t<-strsplit(Ascospores$measure_orig,"x")

s<-sapply(t,length)
temp<-plyr::rbind.fill(lapply(t, function(y) { as.data.frame(t(y)) }))

temp$V1<-gsub(",",".",temp$V1)
temp$V2<-gsub(",",".",temp$V2)
temp$V3<-gsub(",",".",temp$V3)
temp$V4<-gsub(",",".",temp$V4)


#Extracting the first pattern
#library(stringr)
s<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
t<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
u<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
v<-lapply(temp[,4], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")

s_<-sapply(s,function(x){length(x[[1]])})
t_<-sapply(t,function(x){length(x[[1]])})
u_<-sapply(u,function(x){length(x[[1]])})
v_<-sapply(v,function(x){length(x[[1]])})


s<-do.call(rbind,s)
t<-do.call(rbind,t)
u<-do.call(rbind,u)
v<-do.call(rbind,v)

#
Ascospores$Dimension_1<-NA
Ascospores$Dimension_1[s_==1]<-s[s_==1]
#
Ascospores$Dimension_2<-NA
Ascospores$Dimension_2[t_==1]<-t[t_==1]
#
Ascospores$Dimension_3<-NA
Ascospores$Dimension_3[u_==1]<-u[u_==1]

#
#Ascospores$Dimension_1[s_==0&t_==1]<-s[s_==0&t_==1]
#Ascospores$Dimension_2[w==0&v==1]<-t[w==0&v==1]

#Extracting the second pattern
s1<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?")##
t1<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?")
u1<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?")

s1_<-sapply(s1,function(x){length(x[[1]])})
t1_<-sapply(t1,function(x){length(x[[1]])})
u1_<-sapply(u1,function(x){length(x[[1]])})

s1<-do.call(rbind,s1)
t1<-do.call(rbind,t1)
u1<-do.call(rbind,u1)
#v1<-do.call(rbind,v1)

Ascospores$Dimension_1[s1_==1]<-s1[s1_==1]
Ascospores$Dimension_2[t1_==1]<-t1[t1_==1]
Ascospores$Dimension_3[u1_==1]<-u1[u1_==1]

#Checking again
#Ascospores$measure_orig[is.na(Ascospores$Dimension_1)]
#temp$V1[is.na(Ascospores$Dimension_1)]

#Fixing problems:
#The problems are so heterogenous that I fixed by hand on July 2019. The 
#changes can be found in the script Fixing_Missing_x_Ascospores.R

#Doing it again:
t<-strsplit(Ascospores$measure_orig,"x")

s<-sapply(t,length)
temp<-plyr::rbind.fill(lapply(t, function(y) { as.data.frame(t(y)) }))

temp$V1<-gsub(",",".",temp$V1)
temp$V2<-gsub(",",".",temp$V2)
temp$V3<-gsub(",",".",temp$V3)
temp$V4<-gsub(",",".",temp$V4)


#Extracting the first pattern
#library(stringr)
s<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
t<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
u<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
v<-lapply(temp[,4], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")

s_<-sapply(s,function(x){length(x[[1]])})
t_<-sapply(t,function(x){length(x[[1]])})
u_<-sapply(u,function(x){length(x[[1]])})
v_<-sapply(v,function(x){length(x[[1]])})


s<-do.call(rbind,s)
t<-do.call(rbind,t)
u<-do.call(rbind,u)
v<-do.call(rbind,v)

#
Ascospores$Dimension_1<-NA
Ascospores$Dimension_1[s_==1]<-s[s_==1]
#
Ascospores$Dimension_2<-NA
Ascospores$Dimension_2[t_==1]<-t[t_==1]
#
Ascospores$Dimension_3<-NA
Ascospores$Dimension_3[u_==1]<-u[u_==1]

#
#Ascospores$Dimension_1[s_==0&t_==1]<-s[s_==0&t_==1]
#Ascospores$Dimension_2[w==0&v==1]<-t[w==0&v==1]

#Extracting the second pattern
s1<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?")##
t1<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?")
u1<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?")

s1_<-sapply(s1,function(x){length(x[[1]])})
t1_<-sapply(t1,function(x){length(x[[1]])})
u1_<-sapply(u1,function(x){length(x[[1]])})

s1<-do.call(rbind,s1)
t1<-do.call(rbind,t1)
u1<-do.call(rbind,u1)
#v1<-do.call(rbind,v1)

Ascospores$Dimension_1[s1_==1]<-s1[s1_==1]
Ascospores$Dimension_2[t1_==1]<-t1[t1_==1]
Ascospores$Dimension_3[u1_==1]<-u1[u1_==1]

#Checking again
#Ascospores$measure_orig[is.na(Ascospores$Dimension_1)]
#temp$V1[is.na(Ascospores$Dimension_1)]

#Extracting a third pattern
s2<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
t2<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
u2<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
v2<-lapply(temp[,4], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")

s2_<-sapply(s2,function(x){length(x[[1]])})
t2_<-sapply(t2,function(x){length(x[[1]])})
u2_<-sapply(u2,function(x){length(x[[1]])})

s2<-do.call(rbind,s2)
t2<-do.call(rbind,t2)
u2<-do.call(rbind,u2)

Ascospores$Dimension_1[s2_==1]<-s2[s2_==1]
Ascospores$Dimension_2[t2_==1]<-t2[t2_==1]
Ascospores$Dimension_3[u2_==1]<-u2[u2_==1]

#Checking again
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)]

Ascospores$text_entry[is.na(Ascospores$Dimension_1)][117]

#Solving cases where "x" is missing and instead there is "--"
missing_x<-grep("--",Ascospores$measure_orig[is.na(Ascospores$Dimension_1)])
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][missing_x]<-
  gsub("--"," - ",
       Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][missing_x])

#Fixing the few ones that still have no X  "(8-)10-[11.6]-14(-15)  4-[5.4]-7"  
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][206]<- "(8-)10-[11.6]-14(-15) x 4-[5.4]-7" 
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][257]<- "12-[13.9]-16 x 6-[7.9]-10"
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][260]<- "12-13 x 4-4.5"                                                 
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][261]<- "30-36 x 13-17"                                                 
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][262]<- "(15.2-)15.8-18.3(-19.6) x (5.8-)6.2-6.9(-7.1)"                 
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][263]<- "(12.5-)16-21(-25.5) x (4.5-)5.5-6.5(-7)"                       
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][264]<- "(15-)17.5-21.5(-24.5) x (5-)5.5-6.5(-8)"                       
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][265]<- "(13-)14.5-18(-22) x (4-)5-6(-7)"                               
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][266]<- "(13-)14.5-18(-22) x (4-)5-6(-7)"                               
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][267]<- "(13-)15-19(-21) x (5-)5.5-6(-7)"                               
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][268]<- "(14-)15.5-23(-33.5) x (3.5-)4-5"                               
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][269]<- "(18.5-)20-27(-37) x (4-)4.5-5"                                 
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][270]<- "(18.5-)20-27(-37) x (4-)4.5-5"                                 
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][271]<- "(12-)14-16.5(-17.5) x (4.5-)5-5.5(-6)"                         
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][272]<- "(11.5-)14.5-19(-23.5) x (4-)4.5-5.5(-6)"                       

#Doing it again:
t<-strsplit(Ascospores$measure_orig,"x")

s<-sapply(t,length)
temp<-plyr::rbind.fill(lapply(t, function(y) { as.data.frame(t(y)) }))

temp$V1<-gsub(",",".",temp$V1)
temp$V2<-gsub(",",".",temp$V2)
temp$V3<-gsub(",",".",temp$V3)
temp$V4<-gsub(",",".",temp$V4)


#Extracting the first pattern
#library(stringr)
s<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
t<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
u<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")
v<-lapply(temp[,4], str_extract_all, pattern = "\\d+\\.?\\d?\\-\\d+\\.?\\d?")

s_<-sapply(s,function(x){length(x[[1]])})
t_<-sapply(t,function(x){length(x[[1]])})
u_<-sapply(u,function(x){length(x[[1]])})
v_<-sapply(v,function(x){length(x[[1]])})


s<-do.call(rbind,s)
t<-do.call(rbind,t)
u<-do.call(rbind,u)
v<-do.call(rbind,v)

#
Ascospores$Dimension_1<-NA
Ascospores$Dimension_1[s_==1]<-s[s_==1]
#
Ascospores$Dimension_2<-NA
Ascospores$Dimension_2[t_==1]<-t[t_==1]
#
Ascospores$Dimension_3<-NA
Ascospores$Dimension_3[u_==1]<-u[u_==1]

#
#Ascospores$Dimension_1[s_==0&t_==1]<-s[s_==0&t_==1]
#Ascospores$Dimension_2[w==0&v==1]<-t[w==0&v==1]

#Extracting the second pattern
s1<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?")##
t1<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?")
u1<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?")

s1_<-sapply(s1,function(x){length(x[[1]])})
t1_<-sapply(t1,function(x){length(x[[1]])})
u1_<-sapply(u1,function(x){length(x[[1]])})

s1<-do.call(rbind,s1)
t1<-do.call(rbind,t1)
u1<-do.call(rbind,u1)
#v1<-do.call(rbind,v1)

Ascospores$Dimension_1[s1_==1]<-s1[s1_==1]
Ascospores$Dimension_2[t1_==1]<-t1[t1_==1]
Ascospores$Dimension_3[u1_==1]<-u1[u1_==1]

#Extracting a third pattern
s2<-lapply(temp[,1], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
t2<-lapply(temp[,2], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
u2<-lapply(temp[,3], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")
v2<-lapply(temp[,4], str_extract_all, pattern = "\\d+\\.?\\d?\\s?\\+\\-\\s?\\d+\\.?\\d?")

s2_<-sapply(s2,function(x){length(x[[1]])})
t2_<-sapply(t2,function(x){length(x[[1]])})
u2_<-sapply(u2,function(x){length(x[[1]])})

s2<-do.call(rbind,s2)
t2<-do.call(rbind,t2)
u2<-do.call(rbind,u2)

Ascospores$Dimension_1[s2_==1]<-s2[s2_==1]
Ascospores$Dimension_2[t2_==1]<-t2[t2_==1]
Ascospores$Dimension_3[u2_==1]<-u2[u2_==1]


#Extracting 4th pattern:
# temp$V1[is.na(Ascospores$Dimension_1)][
# grep("\\)\\s?\\d+\\.?\\d?\\s?\\(",temp$V1[is.na(Ascospores$Dimension_1)])
# ]

s3<-s1[is.na(Ascospores$Dimension_1)][
  grep("\\)\\s?\\d+\\.?\\d?\\s?\\(",temp$V1[is.na(Ascospores$Dimension_1)])
  ]

# temp$V2[is.na(Ascospores$Dimension_2)][
#   grep("\\)\\s?\\d+\\.?\\d?\\s?\\(",temp$V2[is.na(Ascospores$Dimension_2)])
#   ]

t3<-t1[is.na(Ascospores$Dimension_2)][
  grep("\\)\\s?\\d+\\.?\\d?\\s?\\(",temp$V2[is.na(Ascospores$Dimension_2)])
  ]
#
Ascospores$Dimension_1[is.na(Ascospores$Dimension_1)][
  grep("\\)\\s?\\d+\\.?\\d?\\s?\\(",temp$V1[is.na(Ascospores$Dimension_1)])
  ]<-sapply(s3,function(x){x[2]})

Ascospores$Dimension_2[is.na(Ascospores$Dimension_2)][
  grep("\\)\\s?\\d+\\.?\\d?\\s?\\(",temp$V2[is.na(Ascospores$Dimension_2)])
  ]<-sapply(t3,function(x){x[2]})

#

#Extracting 5th pattern:
Ascospores$measure_orig[is.na(Ascospores$Dimension_1)][
  grep("\\[\\s?\\d+\\.?\\d?\\s?\\]",temp$V1[is.na(Ascospores$Dimension_1)])
  ][-c(1:4)]


temp$V1[is.na(Ascospores$Dimension_1)][
  grep("\\[\\s?\\d+\\.?\\d?\\s?\\]",temp$V1[is.na(Ascospores$Dimension_1)])
  ]

s4<-s1[is.na(Ascospores$Dimension_1)][
  grep("\\[\\s?\\d+\\.?\\d?\\s?\\]",temp$V1[is.na(Ascospores$Dimension_1)])
  ][-c(1:4)]
#
temp$V2[is.na(Ascospores$Dimension_2)][
  grep("\\[\\s?\\d+\\.?\\d?\\s?\\]",temp$V2[is.na(Ascospores$Dimension_2)])
  ][-c(1:3)]

t4<-t1[is.na(Ascospores$Dimension_2)][
  grep("\\[\\s?\\d+\\.?\\d?\\s?\\]",temp$V2[is.na(Ascospores$Dimension_2)])
  ][-c(1:3)]
#
Ascospores$Dimension_1[is.na(Ascospores$Dimension_1)][
  grep("\\[\\s?\\d+\\.?\\d?\\s?\\]",temp$V1[is.na(Ascospores$Dimension_1)])
  ][-c(1:4)]<-sapply(s4,function(x){x[2]})

Ascospores$Dimension_2[is.na(Ascospores$Dimension_2)][
  grep("\\[\\s?\\d+\\.?\\d?\\s?\\]",temp$V2[is.na(Ascospores$Dimension_2)])
  ][-c(1:3)]<-sapply(t4,function(x){x[2]})

#Solving remaining random problems
problems<-which(is.na(Ascospores$Dimension_1))

Ascospores$Dimension_1[problems][2]<- "13-17"
Ascospores$Dimension_2[problems][2]<- "6-7"
Ascospores$Dimension_1[problems][9]<- "7.5-16"
Ascospores$Dimension_2[problems][9]<- "2.5-3"
Ascospores$Dimension_1[problems][11]<- "22.6-23.5"
Ascospores$Dimension_2[problems][11]<- "10.6-11.2"
Ascospores$Dimension_1[problems][16]<- "7.5-16"
Ascospores$Dimension_2[problems][16]<- "2.5-3"
Ascospores$Dimension_1[problems][17]<- "30"
Ascospores$Dimension_2[problems][17]<- "11"
Ascospores$Dimension_1[problems][19]<- "11.5-14"
Ascospores$Dimension_2[problems][19]<- "5-6"
Ascospores$Dimension_1[problems][20]<- "8.82"
Ascospores$Dimension_2[problems][20]<- "3.01"
Ascospores$Dimension_1[problems][23]<- "22.6-23.5"
Ascospores$Dimension_2[problems][23]<- "10.6-11.2"
Ascospores$Dimension_1[problems][25]<- "10"
Ascospores$Dimension_2[problems][25]<- "6.1"
Ascospores$Dimension_1[problems][26]<- "10"
Ascospores$Dimension_2[problems][26]<- "6.1"
Ascospores$Dimension_1[problems][27]<- "34.85"
Ascospores$Dimension_2[problems][27]<- "14"
Ascospores$Dimension_1[problems][28]<- "34.85"
Ascospores$Dimension_2[problems][28]<- "14"
Ascospores$Dimension_1[problems][29]<- "34.85"
Ascospores$Dimension_2[problems][29]<- "14"
Ascospores$Dimension_1[problems][30]<- "13.75"
Ascospores$Dimension_2[problems][30]<- "4.5"
Ascospores$Dimension_1[problems][31]<- "13.77"
Ascospores$Dimension_2[problems][31]<- "4.0"
Ascospores$Dimension_1[problems][32]<- "24"
Ascospores$Dimension_2[problems][32]<- "9"
Ascospores$Dimension_1[problems][34]<- "10.77-11.5"
Ascospores$Dimension_2[problems][34]<- "5.53"
Ascospores$Dimension_1[problems][35]<- "6.01"
Ascospores$Dimension_2[problems][35]<- "2.78"
Ascospores$Dimension_1[problems][36]<- "10.77-11.5"
Ascospores$Dimension_2[problems][36]<- "5.53"
Ascospores$Dimension_1[problems][37]<- "23.8-24.7"
Ascospores$Dimension_2[problems][37]<- "5.2-5.7"
Ascospores$Dimension_1[problems][38]<- "23.2-27.6"
Ascospores$Dimension_2[problems][38]<- "6.1-6.8"
Ascospores$Dimension_1[problems][39]<- "23.2-27.6"                         
Ascospores$Dimension_2[problems][39]<- "6.1-6.8"
Ascospores$Dimension_1[problems][40]<- "3.0"                                                 
Ascospores$Dimension_2[problems][40]<- "3.0"                                                 
Ascospores$Dimension_1[problems][41]<- "20.2-20.3"                         
Ascospores$Dimension_2[problems][41]<- "5.1-5.2" 
Ascospores$Dimension_1[problems][42]<- "23.0"
Ascospores$Dimension_2[problems][42]<- "12.3"
Ascospores$Dimension_1[problems][43]<- "16.7" 
Ascospores$Dimension_2[problems][43]<- "10.2" 
Ascospores$Dimension_1[problems][44]<- "7.5-16"                                                
Ascospores$Dimension_2[problems][44]<- "2.5-3"
Ascospores$Dimension_1[problems][45]<- "7.5-16"                                                
Ascospores$Dimension_2[problems][45]<- "2.5-3"                                                
Ascospores$Dimension_1[problems][46]<- "8.84"
Ascospores$Dimension_2[problems][46]<- "2.91"
Ascospores$Dimension_1[problems][48]<- "24"                                         
Ascospores$Dimension_2[problems][48]<- "9"
Ascospores$Dimension_1[problems][49]<- "8.84"
Ascospores$Dimension_2[problems][49]<- "2.91"
Ascospores$Dimension_1[problems][50]<- "11.5"
Ascospores$Dimension_2[problems][50]<- "7.4"
Ascospores$Dimension_1[problems][52]<- "30"
Ascospores$Dimension_2[problems][52]<- "10"
Ascospores$Dimension_1[problems][53]<- "8-10"
Ascospores$Dimension_2[problems][53]<- "2.5-3.5"
Ascospores$Dimension_1[problems][54]<- "120-180"
Ascospores$Dimension_2[problems][54]<- "1-1.5"
Ascospores$Dimension_1[problems][55]<- "6.96"
Ascospores$Dimension_2[problems][55]<- "3.15"
Ascospores$Dimension_1[problems][56]<- "22-28"
Ascospores$Dimension_2[problems][56]<- "10-13"
Ascospores$Dimension_1[problems][57]<- "25-29"
Ascospores$Dimension_2[problems][57]<- "11-15"
Ascospores$Dimension_1[problems][58]<- "22-30"
Ascospores$Dimension_2[problems][58]<- "10-14"
Ascospores$Dimension_1[problems][60]<- "4.0-4.3"
Ascospores$Dimension_2[problems][60]<- "3.7-4.0"
Ascospores$Dimension_1[problems][61]<- "4.0-4.3"                   
Ascospores$Dimension_2[problems][61]<- "3.7-4.0"
Ascospores$Dimension_1[problems][62]<- "4.4-4.5"
Ascospores$Dimension_2[problems][62]<- "4.0-4.2"
Ascospores$Dimension_1[problems][63]<- "4.4-4.5"
Ascospores$Dimension_2[problems][63]<- "4.0-4.2"
Ascospores$Dimension_1[problems][64]<- "2.5-3.5"
Ascospores$Dimension_2[problems][64]<- "2.2-3.4"
Ascospores$Dimension_1[problems][67]<- "5.95"
Ascospores$Dimension_2[problems][67]<- "5.95"
Ascospores$Dimension_1[problems][68]<- "10"
Ascospores$Dimension_2[problems][68]<- "2.5-3"
Ascospores$Dimension_1[problems][69]<- "33"
Ascospores$Dimension_2[problems][69]<- "12"
Ascospores$Dimension_1[problems][70]<- "5"                                                         
Ascospores$Dimension_2[problems][70]<- "5"
Ascospores$Dimension_1[problems][71]<- "31-45"
Ascospores$Dimension_2[problems][71]<- "31-47"
Ascospores$Dimension_1[problems][72]<- "32-46"
Ascospores$Dimension_2[problems][72]<- "32-46"
Ascospores$Dimension_1[problems][74]<- "11.1-13.7"
Ascospores$Dimension_2[problems][74]<- "3.8-5.0"
Ascospores$Dimension_1[problems][75]<- "12-15"
Ascospores$Dimension_2[problems][75]<- "2-3"
Ascospores$Dimension_1[problems][77]<- "6.1-9.1"
Ascospores$Dimension_2[problems][77]<- "1.7-2.0"
Ascospores$Dimension_1[problems][78]<- "13.5-22.1"
Ascospores$Dimension_2[problems][78]<- "2.7-3.9"
Ascospores$Dimension_1[problems][79]<- "9.1-12.3"
Ascospores$Dimension_2[problems][79]<- "2.8-4.2"
Ascospores$Dimension_1[problems][81]<- "50.0-72.5"
Ascospores$Dimension_2[problems][81]<- "1.5-1.9"
Ascospores$Dimension_1[problems][82]<- "9.6-12.4"
Ascospores$Dimension_2[problems][82]<- "1.6-2.0"
Ascospores$Dimension_1[problems][83]<- "10.4-13.0"
Ascospores$Dimension_2[problems][83]<- "3.5-4.3"
Ascospores$Dimension_1[problems][84]<- "16.8-22.8"
Ascospores$Dimension_2[problems][84]<- "2.3-2.9"
Ascospores$Dimension_1[problems][88]<- "8.9-10.4"
Ascospores$Dimension_2[problems][88]<- "4.8-6.2"
Ascospores$Dimension_1[problems][89]<- "20-24"
Ascospores$Dimension_2[problems][89]<- "10-11"
Ascospores$Dimension_1[problems][90]<- "3.5-4"
Ascospores$Dimension_2[problems][90]<- "3.5-4"
Ascospores$Dimension_1[problems][91]<- "4.5−5"
Ascospores$Dimension_2[problems][91]<- "4.5−5"
Ascospores$Dimension_1[problems][92]<- "2.5-3"
Ascospores$Dimension_2[problems][92]<- "2.5-3"
Ascospores$Dimension_1[problems][93]<- "3.5−4"
Ascospores$Dimension_2[problems][93]<- "3.5−4"
Ascospores$Dimension_1[problems][94]<- "3-3.5"
Ascospores$Dimension_2[problems][94]<- "3-3.5"
Ascospores$Dimension_1[problems][95]<- "3-3.5"
Ascospores$Dimension_2[problems][95]<- "3-3.5"
Ascospores$Dimension_1[problems][96]<- "45-100"
Ascospores$Dimension_2[problems][96]<- "45-100"
Ascospores$Dimension_1[problems][97]<- "30-130"
Ascospores$Dimension_2[problems][97]<- "30-130"
Ascospores$Dimension_1[problems][98]<- "5-5.5"
Ascospores$Dimension_2[problems][98]<- "5-5.5"
Ascospores$Dimension_1[problems][99]<- "4.5-5.5"
Ascospores$Dimension_2[problems][99]<- "4.5-5.5"
Ascospores$Dimension_1[problems][100]<- "5-5.5"
Ascospores$Dimension_2[problems][100]<- "5-5.5"
Ascospores$Dimension_1[problems][101]<- "53.7-73.7"
Ascospores$Dimension_2[problems][101]<- "3.0-3.8"
Ascospores$Dimension_1[problems][102]<- "100-150"
Ascospores$Dimension_2[problems][102]<- "100-150"
Ascospores$Dimension_1[problems][103]<- "45-50"
Ascospores$Dimension_2[problems][103]<- "45-50"
Ascospores$Dimension_1[problems][104]<- "15-17"
Ascospores$Dimension_2[problems][104]<- "3-4"
Ascospores$Dimension_1[problems][105]<- "12-14"
Ascospores$Dimension_2[problems][105]<- "6-7"
Ascospores$Dimension_1[problems][108]<- "14.7-17.9"
Ascospores$Dimension_2[problems][108]<- "3.0-3.8"

rm(problems)

#Extracting the second pattern

problem<-which(is.na(Ascospores$Dimension_2))
Ascospores$Dimension_2[problem]



#Remaining 30 entries with problems (not processed)
Ascospores$measure_orig[which(is.na(Ascospores$Dimension_1))]

#GETTING AVERAGE DIMENSION VALUES
Ascospores_copy<-Ascospores


Ascospores$Dimension_1<-unlist(Ascospores$Dimension_1)
Ascospores$Dimension_2<-unlist(Ascospores$Dimension_2)
Ascospores$Dimension_3<-unlist(Ascospores$Dimension_3)

#Extracting single values out of the ranges (for dimension 1)
t<-strsplit(Ascospores$Dimension_1,"-")
s<-sapply(t,length)

#Summing the two extremes and then dividing by 2
t[s==2]<-lapply(t[s==2],function(x){x<-(as.numeric(x[1])+as.numeric(x[2]))/2})
s_<-lapply(t,length)
which(s_!=1);rm(s_)#now I got only one value for every entry as desired

t1<-Ascospores$Dimension_1[grep("\\+\\-",Ascospores$Dimension_1)]
t1<-strsplit(t1,"\\+-")
t1<-lapply(t1,function(x){x<-as.numeric(x[[1]])})

#Adding these values to the dataset:
Ascospores$Mean_Dim1<-unlist(t)
Ascospores$Mean_Dim1<-as.numeric(Ascospores$Mean_Dim1)
Ascospores$Mean_Dim1[grep("\\+\\-",Ascospores$Dimension_1)]<-t1


#Repating the same logic for Dimension 2
#Extracting single values out of the ranges
t<-strsplit(Ascospores$Dimension_2,"-")
s<-sapply(t,length)

#Summing the two extremes and then dividing by 2
t[s==2]<-lapply(t[s==2],function(x){x<-(as.numeric(x[1])+as.numeric(x[2]))/2})
s_<-lapply(t,length)
which(s_!=1);rm(s_)#now I got only one value for every entry as desired

t1<-Ascospores$Dimension_2[grep("\\+\\-",Ascospores$Dimension_2)]
t1<-strsplit(t1,"\\+-")
t1<-lapply(t1,function(x){x<-as.numeric(x[[1]])})


#Adding these values to the dataset:
Ascospores$Mean_Dim2<-unlist(t)
Ascospores$Mean_Dim2<-as.numeric(Ascospores$Mean_Dim2)
Ascospores$Mean_Dim2[grep("\\+\\-",Ascospores$Dimension_2)]<-t1

Ascospores$Mean_Dim2[is.na(Ascospores$Dimension_2)][
  -grep("x",Ascospores$measure_orig[is.na(Ascospores$Dimension_2)])]<-
                                                      Ascospores$Mean_Dim1[is.na(Ascospores$Dimension_2)][
                                                  -grep("x",Ascospores$measure_orig[is.na(Ascospores$Dimension_2)])]

Ascospores$measure_orig[is.na(Ascospores$Mean_Dim2)]


#Notes:Ascospores$measure_orig[is.na(Ascospores$Dimension_2)][558]

#Ways to measure how many spores per asci: "Asci 8-spored, 9-17 — 7-20 µm"