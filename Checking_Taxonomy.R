#Checking taxonomy.

#Mycobank taxonomy

#The dataset called "MycobankNames_list" does not come from the data that
#Will provided out of scrapping Mycobank. Instead this comes directly
#from the species list Mycobank provides in the website. My idea is to
#compare how many of the things they say they have got scrapped by Will.
#This dataset contains 499255 entries

MycobankNames_list<-read.csv("MycobankNames_list.csv",
                             stringsAsFactors = F,header = T)


#Like the data of Will, they both use the same ID and Mycobank number to
#the fungi, so it would be easier to identify them

# I. GETTING MYCOBANK TAXONOMY
#Out of this list I got the taxonomy hierarchy that comes with it. It took
#long because is not consistent across all names

t<-strsplit(MycobankNames_list$Classification,",")
s<-plyr::rbind.fill(lapply(t, function(y) { as.data.frame(t(y)) }))


table(s$V1)#This shows that almost everything in the first colum refer to kigndom
#there are only 6 cases of true fungi where genera are given in this column

MycobankNames_list<-cbind(MycobankNames_list,s)
names(MycobankNames_list)[1]<-"base__id";MycobankNames_list$base__id<-as.character(MycobankNames_list$base__id)
MycobankNames_list[9:20]<-sapply(MycobankNames_list[9:20],as.character)

MycobankNames_fungiMisplaced<-
  MycobankNames_list%>%
  filter(V1=="Fuckelia"|V1=="Helicocentralis"|V1=="Desmaziera"|V1=="Mitrophora"|
           V1=="Macowanites")

#These two lines of code are optional. They would be useful when interested to study other groups
#that are not fungi:

# MycobankNames_wDescrpt<-
#   left_join(Data_IDs,MycobankNames_list)#Data_IDs correspond to the the species names, the order ID and the mycobank number of the data scrapped by Will

#Now working the taxonomy. The problem is that taxonomical affiliation is inconsisten
#in Mycobank. In some cases up to 12 taxon levels are given, but in other only 2.
#So I am fixing this by hand and then merging what is needed

#1. Select only the fungi. This retrieves 489271 entries
MycobankNames_list_Fungi<-MycobankNames_list[MycobankNames_list$V1=="Fungi",];rownames(MycobankNames_list_Fungi)<-NULL
MycobankNames_list_Fungi$Kingdom<-"Fungi"

#t<-strsplit(MycobankNames_list_Fungi$Classification,",")
#w<-sapply(t,length);table(w)

#2.Assign standardize taxon categories

#According to  Hibbet et al 2007 this is the "composition" of the fungal kingdom (I am assuming, phyla):
#Rozella, Microsporidia, Aphelida, Chytridiomycota, Neocallimastigomycota,
#Blastocladiomycota, Mucoromycota, Zoopagomycota, Ascomycota and Basidiomycota
#Dykaria is subkingdom (apparently following Hibbet etal 2007)
#The ending -cotina refers to sub-phylum; -mycetes is class

#Subkingdom should only contain one: Dikarya. 
MycobankNames_list_Fungi$Subkingdom<-NA
MycobankNames_list_Fungi$Subkingdom[MycobankNames_list_Fungi$V2==" Dikarya"]<-"Dikarya"

#Uncertain categories: "Incertae sedis" and "Fossils will also get included
MycobankNames_list_Fungi$Uncertain_classification<-NA
MycobankNames_list_Fungi$Uncertain_classification[MycobankNames_list_Fungi$V2==" Fossil Fungi"]<-
  "Fossil Fungi"

MycobankNames_list_Fungi$Uncertain_classification[MycobankNames_list_Fungi$V2==" Incertae sedis"]<-
  "Incertae sedis"


#Phlya have ending -mycota, this I found in V2 to V3; I will also
#add the Microsporidia case (as in Hibbet etal 2007 it is recognized as Phylum), in mycobank for some reason
#the entry is called Microsporidia in some cases while in others Microsporidiomycota, I left both versions of
#the name

MycobankNames_list_Fungi$Phylum<-NA
MycobankNames_list_Fungi$Phylum[grep("mycota$",MycobankNames_list_Fungi$V2)]<-
  MycobankNames_list_Fungi$V2[grep("mycota$",MycobankNames_list_Fungi$V2)]

MycobankNames_list_Fungi$Phylum[grep("mycota$",MycobankNames_list_Fungi$V3)]<-
  MycobankNames_list_Fungi$V3[grep("mycota$",MycobankNames_list_Fungi$V3)]

MycobankNames_list_Fungi$Phylum[MycobankNames_list_Fungi$V2==" Microsporidia"]<-
  "Microsporidia"

#Sub-phyla with the ending -mycotina. These are in V3,V4
MycobankNames_list_Fungi$Subphylum<-NA
MycobankNames_list_Fungi$Subphylum[grep("mycotina$",MycobankNames_list_Fungi$V3)]<-
  MycobankNames_list_Fungi$V3[grep("mycotina$",MycobankNames_list_Fungi$V3)]

MycobankNames_list_Fungi$Subphylum[grep("mycotina$",MycobankNames_list_Fungi$V4)]<-
  MycobankNames_list_Fungi$V4[grep("mycotina$",MycobankNames_list_Fungi$V4)]

#Class with the ending mycetes.These are found in V3,V4 and V5
MycobankNames_list_Fungi$Class<-NA
MycobankNames_list_Fungi$Class[grep("mycetes$",MycobankNames_list_Fungi$V3)]<-
  MycobankNames_list_Fungi$V3[grep("mycetes$",MycobankNames_list_Fungi$V3)]

MycobankNames_list_Fungi$Class[grep("mycetes$",MycobankNames_list_Fungi$V4)]<-
  MycobankNames_list_Fungi$V4[grep("mycetes$",MycobankNames_list_Fungi$V4)]

MycobankNames_list_Fungi$Class[grep("mycetes$",MycobankNames_list_Fungi$V5)]<-
  MycobankNames_list_Fungi$V5[grep("mycetes$",MycobankNames_list_Fungi$V5)]

#Order with ending -ales. these are found in V3 to V7
MycobankNames_list_Fungi$Order<-NA
MycobankNames_list_Fungi$Order[grep("ales$",MycobankNames_list_Fungi$V3)]<-
  MycobankNames_list_Fungi$V3[grep("ales$",MycobankNames_list_Fungi$V3)]

MycobankNames_list_Fungi$Order[grep("ales$",MycobankNames_list_Fungi$V4)]<-
  MycobankNames_list_Fungi$V4[grep("ales$",MycobankNames_list_Fungi$V4)]

MycobankNames_list_Fungi$Order[grep("ales$",MycobankNames_list_Fungi$V5)]<-
  MycobankNames_list_Fungi$V5[grep("ales$",MycobankNames_list_Fungi$V5)]

MycobankNames_list_Fungi$Order[grep("ales$",MycobankNames_list_Fungi$V6)]<-
  MycobankNames_list_Fungi$V6[grep("ales$",MycobankNames_list_Fungi$V6)]

MycobankNames_list_Fungi$Order[grep("ales$",MycobankNames_list_Fungi$V7)]<-
  MycobankNames_list_Fungi$V7[grep("ales$",MycobankNames_list_Fungi$V7)]

#Family with ending -aceae. These are found in V2 to V9

#This is the chunk of code I use to check where to find info for all taxon
#levels described above. In this case as example with the ending for family
MycobankNames_list_Fungi%>%
  filter(grepl("aceae$",V12))%>%
  #select(V8)%>%
  filter(!is.na(Family))%>%
  select(V9)%>%
  unique

MycobankNames_list_Fungi$Family<-NA
MycobankNames_list_Fungi$Family[grep("aceae$",MycobankNames_list_Fungi$V2)]<-
  MycobankNames_list_Fungi$V2[grep("aceae$",MycobankNames_list_Fungi$V2)]

MycobankNames_list_Fungi$Family[grep("aceae$",MycobankNames_list_Fungi$V3)]<-
  MycobankNames_list_Fungi$V3[grep("aceae$",MycobankNames_list_Fungi$V3)]

MycobankNames_list_Fungi$Family[grep("aceae$",MycobankNames_list_Fungi$V4)]<-
  MycobankNames_list_Fungi$V4[grep("aceae$",MycobankNames_list_Fungi$V4)]

MycobankNames_list_Fungi$Family[grep("aceae$",MycobankNames_list_Fungi$V5)]<-
  MycobankNames_list_Fungi$V5[grep("aceae$",MycobankNames_list_Fungi$V5)]

MycobankNames_list_Fungi$Family[grep("aceae$",MycobankNames_list_Fungi$V6)]<-
  MycobankNames_list_Fungi$V6[grep("aceae$",MycobankNames_list_Fungi$V6)]

MycobankNames_list_Fungi$Family[grep("aceae$",MycobankNames_list_Fungi$V7)]<-
  MycobankNames_list_Fungi$V7[grep("aceae$",MycobankNames_list_Fungi$V7)]

MycobankNames_list_Fungi$Family[grep("aceae$",MycobankNames_list_Fungi$V8)]<-
  MycobankNames_list_Fungi$V8[grep("aceae$",MycobankNames_list_Fungi$V8)]
MycobankNames_list_Fungi$Family[which(MycobankNames_list_Fungi$V8==" Asterina melastomaceae")]<-" Asterinaceae"

MycobankNames_list_Fungi$Family[grep("aceae$",MycobankNames_list_Fungi$V9)]<-
  MycobankNames_list_Fungi$V9[grep("aceae$",MycobankNames_list_Fungi$V9)]
MycobankNames_list_Fungi$Family[which(MycobankNames_list_Fungi$V9==" Lactarius salicis-herbaceae")]<-" Russulaceae"
MycobankNames_list_Fungi$Family[which(MycobankNames_list_Fungi$V9==" Lactarius salis-herbaceae")]<-" Russulaceae"
MycobankNames_list_Fungi$Family[which(MycobankNames_list_Fungi$V9==" Puccinia jaceae")]<-" Pucciniaceae"

#Remaining fixes. These are based after going through all unique entries in V2, which were:
#Aphelidiomyceta, Basidiobolomyceta, Blastocladiomyceta, Chytridiomyceta, Microsporidiomycota,
#Mucoromyceta, Olpidiomyceta, Zoopagomyceta, Zygomycota, Ascomycota

#This chunck of code is what I used to check those entries:
table(MycobankNames_list_Fungi$V2)
unique(MycobankNames_list_Fungi$V2)
MycobankNames_list_Fungi%>%
  filter(V2==" Deuteromycota")%>%
  #filter(grepl("Fossil",V2))%>%
  #select(V8)%>%
  filter(is.na(Phylum))%>%
  select(Phylum)%>%
  unique

#The only change I made is adding "Dikarya" to the cases where V2 has Ascomycota
MycobankNames_list_Fungi$Subkingdom[MycobankNames_list_Fungi$V2==" Ascomycota"]<-
  "Dikarya"


#Merging with description data
MycobankNames_FungiwDescrpt<-
  left_join(Data_IDs,MycobankNames_list_Fungi)#Joining, by = "base__id"
#Now I left joing this MycobankNames_list_Fungi (representing the mycobank table I downloaded)
#with the Data_IDs corresponding to the the species names, ID and the mycobank number 
#of the data scrapped by Will

#Having some dimension of the data scrapped by Will:

length(Data_IDs$base__id)#117481
length(unique(Data_IDs$base__id))#72633
length(unique(Data_IDs$base_name))#71467
length(unique(Data_IDs$base_mycobanknr_))#72633

#Selecting only fungi:
MycobankNames_FungiwDescrpt<-
  MycobankNames_FungiwDescrpt[which(MycobankNames_FungiwDescrpt$Kingdom=="Fungi"),]
rownames(MycobankNames_FungiwDescrpt)<-NULL

length(unique(MycobankNames_FungiwDescrpt$base__id))#71633

MycobankNames_FungiwDescrpt$V1<-NULL
MycobankNames_FungiwDescrpt$V2<-NULL
MycobankNames_FungiwDescrpt$V3<-NULL
MycobankNames_FungiwDescrpt$V4<-NULL
MycobankNames_FungiwDescrpt$V5<-NULL
MycobankNames_FungiwDescrpt$V6<-NULL
MycobankNames_FungiwDescrpt$V7<-NULL
MycobankNames_FungiwDescrpt$V8<-NULL
MycobankNames_FungiwDescrpt$V9<-NULL
MycobankNames_FungiwDescrpt$V10<-NULL
MycobankNames_FungiwDescrpt$V11<-NULL
MycobankNames_FungiwDescrpt$V12<-NULL



#Visualizing what I got
table(MycobankNames_FungiwDescrpt$Phylum)
table(MycobankNames_list_Fungi$Phylum)
# Mycobank_phyla_all<-
# MycobankNames_list%>%
MycobankNames_FungiwDescrpt%>%
  #MycobankNames_list_Fungi%>%
  count(Phylum)%>%
  filter(!grepl("Fossil",Phylum))%>%
  ggplot()+
  aes(x=Phylum,y=n)+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

length(which(is.na(MycobankNames_list_Fungi$Phylum)))
length(which(is.na(MycobankNames_FungiwDescrpt$Phylum)))


MycobankNames_FungiwDescrpt%>%
  #MycobankNames_list_Fungi%>%
  #filter(grepl("Fossil",Phylum))%>%
  filter(is.na(Phylum))

#Having some dimension of the fungal names with descriptions:
#In total there are 116,119 entries

sapply(MycobankNames_FungiwDescrpt,function(x){length(unique(x))})#retrieving:
# base__id                    base_name             base_mycobanknr_ 
# 71633                        70479                        71633 
# Taxon_name  MycoBank__      Current.name.Taxon_name             
# 70479         71633                        30592                   


# Kingdom                 Subkingdom 
# 1                            2 
# Uncertain_classification   Phylum                    Subphylum 
# 3                           26                           25 
# Class                      Order                       Family 
# 80                          235                          772 
# base_name_edited 
# 70472 


##################################################################################

#According to Hawksworth and Lücking 2017. The number of existing fungal names
#is 2 to 3 times the number of current accepted names. These numbe of accepted
#fungi it is the number of existing names recognized as "good" in each genus.
#These good names are supposed to be found in the Species funguorum inputs in teh
#Catalogue of Life: http://www.catalogueoflife.org/annual-checklist/2018/

#They stated that the number of good fungal speceis currently stands at around 
#12000 species.
#This number is very similar to the numbers I get from the Mycobank data.


#In total there are 467929 unique species names for fungi in Mycobank:
length(unique(MycobankNames_list_Fungi$Taxon_name))

#If one assumes that the correct names are the fungal names with a description, then
#the number of 120,000 gets close. Well, what I got is actually 70479
length(MycobankNames_FungiwDescrpt$base_mycobanknr_)
length(unique(MycobankNames_FungiwDescrpt$Taxon_name))
length(unique(MycobankNames_FungiwDescrpt$base_mycobanknr_))

#To be sure of the actual names, the package taxize can give the taxonomy both in Index Fungorum (which it is
#related to the Species Fungorum) and the Catalogue of life

library(taxize)

#So, I will run get_colid_ with the names from MycobankNames_FungiwDescrpt. 

#Erysiphe aquilegiae var. ranunculi

#First I need to correct the writing of some names:
#1. Option using the "Current taxon name". I did not follow with this one because it turns out that unique "Current.name.Taxon_name" are around ~30K entries
MycobankNames_FungiwDescrpt$currName_edited<-MycobankNames_FungiwDescrpt$Current.name.Taxon_name
MycobankNames_FungiwDescrpt$currName_edited<-gsub(" f.sp.","",MycobankNames_FungiwDescrpt$currName_edited)
MycobankNames_FungiwDescrpt$currName_edited<-gsub(" var.","",MycobankNames_FungiwDescrpt$currName_edited)
Mycobank_species_names<-
  MycobankNames_FungiwDescrpt$currName_edited[grep("\\w\\s\\w",MycobankNames_FungiwDescrpt$currName_edited)]
#Because there are multiple descriptions for one species in some cases, I will only get names only once:
#Mycobank_species_names<-unique(Mycobank_species_names)#this retrives howwever only 26874 species
MycobankNames_FungiwDescrpt$currName_edited<-NULL

#2. Option using the "base name". This what I will be using. It seems there are a lot of names that actually
#are of infra species (variaties) which if I use the current taxon names are all collapsed at the species
#level

MycobankNames_FungiwDescrpt$base_name_edited<-MycobankNames_FungiwDescrpt$base_name
MycobankNames_FungiwDescrpt$base_name_edited<-gsub(" f.sp.","",MycobankNames_FungiwDescrpt$base_name_edited)
MycobankNames_FungiwDescrpt$base_name_edited<-gsub(" var.","",MycobankNames_FungiwDescrpt$base_name_edited)

#write.csv(MycobankNames_FungiwDescrpt, "MycobankNames_FungiwDescript.csv")


Mycobank_species_names<-
  MycobankNames_FungiwDescrpt$base_name_edited[grep("\\w\\s\\w",MycobankNames_FungiwDescrpt$base_name_edited)]
#Because there are multiple descriptions for one species in some cases, I will only get names only once:
Mycobank_species_names<-unique(Mycobank_species_names)#this 65542 entries out of 108838


#Random: how to create a vector from 1 to 15 by steps of 4 in r? seq(from = 1, to = 14, by = 5)

Mycobank_Names_status_col_1<-get_colid_(Mycobank_species_names[1:15000])
Mycobank_Names_status_col_2<-get_colid_(Mycobank_species_names[15001:30000])
Mycobank_Names_status_col_3<-get_colid_(Mycobank_species_names[30001:45000])
Mycobank_Names_status_col_4<-get_colid_(Mycobank_species_names[45001:65542])

saveRDS(Mycobank_Names_status_col_1,"CatalogueOfLifeData\\Mycobank_Names_status_col_1")#
saveRDS(Mycobank_Names_status_col_2,"CatalogueOfLifeData\\Mycobank_Names_status_col_2")
saveRDS(Mycobank_Names_status_col_3,"CatalogueOfLifeData\\Mycobank_Names_status_col_3")
saveRDS(Mycobank_Names_status_col_4,"CatalogueOfLifeData\\Mycobank_Names_status_col_4")


# Mycobank_Names_status_col_1<-readRDS("CatalogueOfLifeData\\Mycobank_Names_status_col_1")
# Mycobank_Names_status_col_2<-readRDS("CatalogueOfLifeData\\Mycobank_Names_status_col_2")
# Mycobank_Names_status_col_3<-readRDS("CatalogueOfLifeData\\Mycobank_Names_status_col_3")
# Mycobank_Names_status_col_4<-readRDS("CatalogueOfLifeData\\Mycobank_Names_status_col_4")

Mycobank_Names_status_col_1<-do.call("rbind",Mycobank_Names_status_col_1)
Mycobank_Names_status_col_2<-do.call("rbind",Mycobank_Names_status_col_2)
Mycobank_Names_status_col_3<-do.call("rbind",Mycobank_Names_status_col_3)#For some reason this might not be necessary. It seems that when I read this file (readRDS) it comes by default as a dataframe while the others come as lists
Mycobank_Names_status_col_4<-do.call("rbind",Mycobank_Names_status_col_4)

Mycobank_Names_status_col<-
  rbind(Mycobank_Names_status_col_1,
        Mycobank_Names_status_col_2,
        Mycobank_Names_status_col_3,
        Mycobank_Names_status_col_4)

rm(Mycobank_Names_status_col_1,
   Mycobank_Names_status_col_2,
   Mycobank_Names_status_col_3,
   Mycobank_Names_status_col_4)

names(Mycobank_Names_status_col)
#The dataframe Mycobank_Names_status_col contains the data obtained through
#the function get_colid_. It contains 10 column, the first one is the id
#of the col and the name associated. The rownames of the dataframe are the 
#the names I submitted. That is, they are the names as reported in Mycobank
#(Mycobank_species_name). So, I decided to have those
#names in a separte column so I can matched easier with the info of the 
#Mycobank dataset:
Mycobank_Names_status_col$name_as_in_MB<-row.names(Mycobank_Names_status_col)

#WHIY THERE ARE MULTIPLE ENTRIES FOR A GIVEN NAME SUBMITTED?
#FIRST, get_colid_ will not only give the status
#of the exact name submitted but also all synonyms associated to that name. 
#Thus multiple entries will be asssigned to a name,
#For example get_colid_("Rhizopogon roseolus") will retrieve 7 entries.
#BTW, all infraspecies are considered synonyms of a species name!!!
#SECOND, one also can get the same ID more than once. This happens
#because in the data submitted to get_colid_ 
#(that is, MycobankNames_FungiwDescrpt$base_name_edited), one entry reports the name of a 
#species, but another entry uses a synonym of that name (usually the case of infraspecies)
#For example MycobankNames_FungiwDescrpt$base_name_edited has Catacauma aspideum but also
#Catacauma aspideum f. fici-fulvae. Since get_colid_("Catacauma aspideum") will return the
#id of Catacauma aspideum f. fici-fulvae by the time get_colid_ hits the entry of this 
#infraspecies the info will be already there
#THIRD, for some reason the exact name has two ID´s. While the previous two resons,
#are managable, This issue I cannot control. So, as I follow later I will pick arbitrarily
#the first ID given.


#To match the status with the Mycobank data, I will use the original names as the 
#the joining variable. First, I will need to remove the number 1 to only get data
#on the names submitted and not the synonyms (when synonyms are reported)
Mycobank_Names_status_col$name_as_in_MB<-
  sub("\\.[1]","",Mycobank_Names_status_col$name_as_in_MB)
Mycobank_Names_status_col<-Mycobank_Names_status_col[c(11,1:10)]


#I am going to work on a new table just with the taxonomy, so I can get in an
#easier way the status

Mycobank_Taxonomy<-MycobankNames_FungiwDescrpt#So far it has #116,119 variables
#A given name AND mycobank ID can occur more than once, this is mainly because
#multiple decription can be given to a single mycobank ID. Because of this reason
#I can easily just remove duplicated Mycobank ID´s
Mycobank_Taxonomy<-Mycobank_Taxonomy[!duplicated(Mycobank_Taxonomy$base_mycobanknr_),]
#this retrieves a table with 71633 entries which is exactly the same amount of unique
#mycobanknr_ in MycobankNames_FungiwDescrpt

#Now only selecting the species names (that is dropping just genera or names of higher
#taxonomic ranks):
Mycobank_Taxonomy<-Mycobank_Taxonomy[grep("\\w\\s\\w",Mycobank_Taxonomy$base_name),]
#This produces 66822 entries of mostly unique entries of species names or infraspecies
#check : Mycobank_Taxonomy$base_name_edited[which(duplicated(Mycobank_Taxonomy$base_name_edited))]
#Now one can see that for some reason the exact name has more than one Mycobank Id, 
#this I cannot control and I have to live with this fact

Mycobank_Taxonomy<-Mycobank_Taxonomy[,c(1,19,2:18)]

Mycobank_Taxonomy<-left_join(Mycobank_Taxonomy,Mycobank_Names_status_col%>%
                                          #select(id,name,rank,status,source,acc_id,
                                           #      acc_name,acc_rank,acc_status,acc_source)%>%
                                          rename(base_name_edited=name_as_in_MB),
              by="base_name_edited")#As in the original, this keeps the entries to 66621
                                          

table(Mycobank_Taxonomy$Phylum)

write.csv(Mycobank_Taxonomy,"Mycobank_Taxonomy.csv", row.names = F)

##############################
#Older stuff


#Downloading all names within Fungi down to species recorded
#in the catalogue of life.
prueba<-downstream("Fungi", downto = "Species", db = "col")
#This retrieves a database of accepted species names and their id fron the catalogue of life
length(unique(prueba$Fungi$childtaxa_name))#135101 names according to the catalogue of life
#It seems these would be accedpted names but I am not 100% sure. The number is similar to what
#the paper of Hawksworth and Lücking report in their paper in Microbioloy Spectrum (2017)

#Extracting the column with just the species names or ID
col_acceptedFungalNames<-prueba$Fungi$childtaxa_name
col_acceptedFungalNames_ID<-prueba$Fungi$childtaxa_id

#Now I have to transform (for some reason) these ID´s as a "colid" object:
col_acceptedFungalNames_ID_01<-as.colid(col_acceptedFungalNames_ID[1:30000])#This takes 40 min
col_acceptedFungalNames_ID_02<-as.colid(col_acceptedFungalNames_ID[30001:60000])#This takes 40 min
col_acceptedFungalNames_ID_03<-as.colid(col_acceptedFungalNames_ID[60001:90000])#This takes 40 min
col_acceptedFungalNames_ID_04<-as.colid(col_acceptedFungalNames_ID[90001:120000])#This takes 40 min
col_acceptedFungalNames_ID_05<-as.colid(col_acceptedFungalNames_ID[120001:135101])#This takes 40 min
#I will save this to avoid doing it again (btw, I did all this in April 2019)
saveRDS(col_acceptedFungalNames_ID_01,"CatalogueOfLifeData\\col_acceptedFungalNames_ID_01")
saveRDS(col_acceptedFungalNames_ID_02,"CatalogueOfLifeData\\col_acceptedFungalNames_ID_02")
saveRDS(col_acceptedFungalNames_ID_03,"CatalogueOfLifeData\\col_acceptedFungalNames_ID_03")
saveRDS(col_acceptedFungalNames_ID_04,"CatalogueOfLifeData\\col_acceptedFungalNames_ID_04")
saveRDS(col_acceptedFungalNames_ID_05,"CatalogueOfLifeData\\col_acceptedFungalNames_ID_05")

#Now, using the function classification to get all the higher taxonomic rankings of the species
#according to the catalogue of life:
s_01<-classification(col_acceptedFungalNames_ID_01, db = 'col')#it takes 40 minutes
s_02<-classification(col_acceptedFungalNames_ID_02, db = 'col')#it takes 40 minutes
s_03<-classification(col_acceptedFungalNames_ID_03, db = 'col')#it takes 40 minutes
s_04<-classification(col_acceptedFungalNames_ID_04, db = 'col')#it takes 40 minutes
s_05<-classification(col_acceptedFungalNames_ID_05, db = 'col')#it takes 40 minutes
#I should save all these ones as RDS to avoid doing this again
saveRDS(s_01,"CatalogueOfLifeData\\s_01.rds")
saveRDS(s_02,"CatalogueOfLifeData\\s_02.rds")
saveRDS(s_03,"CatalogueOfLifeData\\s_03.rds")
saveRDS(s_04,"CatalogueOfLifeData\\s_04.rds")
saveRDS(s_05,"CatalogueOfLifeData\\s_05.rds")

rm(s_01,s_02,s_03,s_04,s_05)
#I can then transform this into a dataframe format
Group_01<-cbind(s_01);saveRDS(Group_01,"CatalogueOfLifeData\\Group_01")
Group_02<-cbind(s_02);saveRDS(Group_02,"CatalogueOfLifeData\\Group_02")
Group_03<-cbind(s_03);saveRDS(Group_03,"CatalogueOfLifeData\\Group_03")
Group_04<-cbind(s_04);saveRDS(Group_04,"CatalogueOfLifeData\\Group_04")
Group_05<-cbind(s_05);saveRDS(Group_05,"CatalogueOfLifeData\\Group_05")

FungalTaxanomy_col<-rbind(Group_01,Group_02,Group_03,Group_04,Group_05)
rm(Group_01,Group_02,Group_03,Group_04,Group_05)
FungalTaxanomy_col$query<-NULL

table(FungalTaxanomy_col$phylum)

###########################################
rm(col_acceptedFungalNames_ID_01,
   col_acceptedFungalNames_ID_02,
   col_acceptedFungalNames_ID_03,
   col_acceptedFungalNames_ID_04,
   col_acceptedFungalNames_ID_05)
###########################################

#Now, I found later that one could explicitly download accpeted names using the 
#get_colid function. So, because I want to be extra sure, I will do that

col_acceptedFungalNames_doubleChecked_01<-get_colid(col_acceptedFungalNames[1:21300],rank = "species",status = "accepted name",ask = T)
col_acceptedFungalNames_doubleChecked_02<-get_colid(col_acceptedFungalNames[21301:21308],kingdom = "Fungi",rank = "species",status = "accepted name",ask = T)
#There is a problem with col_acceptedFungalNames[21309] which is Phoma herbarum
col_acceptedFungalNames_doubleChecked_03<-get_colid(col_acceptedFungalNames[21310:25053],rank = "species",status = "accepted name",ask = T)
#There is a problem with col_acceptedFungalNames[25054] which is Hendersonia sarmentorum
col_acceptedFungalNames_doubleChecked_04<-get_colid(col_acceptedFungalNames[25055:30000],rank = "species",status = "accepted name",ask = T)
col_acceptedFungalNames_doubleChecked_05<-get_colid(col_acceptedFungalNames[30001:40000],rank = "species",status = "accepted name",ask = T)
col_acceptedFungalNames_doubleChecked_06<-get_colid(col_acceptedFungalNames[40001:50000],rank = "species",status = "accepted name",ask = T)
col_acceptedFungalNames_doubleChecked_07<-get_colid(col_acceptedFungalNames[50001:60000],rank = "species",status = "accepted name",ask = T)
col_acceptedFungalNames_doubleChecked_08<-get_colid(col_acceptedFungalNames[60001:70000],rank = "species",status = "accepted name",ask = T)
col_acceptedFungalNames_doubleChecked_09<-get_colid(col_acceptedFungalNames[70001:80000],rank = "species",status = "accepted name",ask = T)
col_acceptedFungalNames_doubleChecked_10<-get_colid(col_acceptedFungalNames[80001:83938],rank = "species",status = "accepted name",ask = T)
# Problem with col_acceptedFungalNames[83939] which is "Agaricus campestris"
col_acceptedFungalNames_doubleChecked_11<-get_colid(col_acceptedFungalNames[83940:87188],rank = "species",status = "accepted name",ask = T)
# Problem with col_acceptedFungalNames[87189] which is "Amanita vaginata"
col_acceptedFungalNames_doubleChecked_12<-get_colid(col_acceptedFungalNames[87190:100000],rank = "species",status = "accepted name",ask = T)
col_acceptedFungalNames_doubleChecked_13<-get_colid(col_acceptedFungalNames[100001:107690],rank = "species",status = "accepted name",ask = T)
#Problem with col_acceptedFungalNames[107691] which is Boletus edulis
col_acceptedFungalNames_doubleChecked_14<-get_colid(col_acceptedFungalNames[107692:120000],rank = "species",status = "accepted name",ask = T)
col_acceptedFungalNames_doubleChecked_15<-get_colid(col_acceptedFungalNames[120001:135101],rank = "species",status = "accepted name",ask = T)
#For fusarium oxysporum there is a weird entry for chrysvirus: fusarium oxsporum or a metavirus. so I have to remove eveything that is not fungi

saveRDS(col_acceptedFungalNames_doubleChecked_01,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_01")
saveRDS(col_acceptedFungalNames_doubleChecked_02,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_02")
saveRDS(col_acceptedFungalNames_doubleChecked_03,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_03")
saveRDS(col_acceptedFungalNames_doubleChecked_04,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_04")
saveRDS(col_acceptedFungalNames_doubleChecked_05,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_05")
saveRDS(col_acceptedFungalNames_doubleChecked_06,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_06")
saveRDS(col_acceptedFungalNames_doubleChecked_07,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_07")
saveRDS(col_acceptedFungalNames_doubleChecked_08,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_08")
saveRDS(col_acceptedFungalNames_doubleChecked_09,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_09")
saveRDS(col_acceptedFungalNames_doubleChecked_10,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_10")
saveRDS(col_acceptedFungalNames_doubleChecked_11,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_11")
saveRDS(col_acceptedFungalNames_doubleChecked_12,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_12")
saveRDS(col_acceptedFungalNames_doubleChecked_13,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_13")
saveRDS(col_acceptedFungalNames_doubleChecked_14,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_14")
saveRDS(col_acceptedFungalNames_doubleChecked_15,"CatalogueOfLifeData\\col_acceptedFungalNames_doubleChecked_15")

rm(col_acceptedFungalNames_doubleChecked_01,
   col_acceptedFungalNames_doubleChecked_02,
   col_acceptedFungalNames_doubleChecked_03,
   col_acceptedFungalNames_doubleChecked_04,
   col_acceptedFungalNames_doubleChecked_05,
   col_acceptedFungalNames_doubleChecked_06,
   col_acceptedFungalNames_doubleChecked_07,
   col_acceptedFungalNames_doubleChecked_08,
   col_acceptedFungalNames_doubleChecked_09,
   col_acceptedFungalNames_doubleChecked_10,
   col_acceptedFungalNames_doubleChecked_11,
   col_acceptedFungalNames_doubleChecked_12,
   col_acceptedFungalNames_doubleChecked_13,
   col_acceptedFungalNames_doubleChecked_14,
   col_acceptedFungalNames_doubleChecked_15)

#Now I will get the synonyms

col_FungalNames_synonyms_01<-get_colid_(col_acceptedFungalNames[1:30000])
col_FungalNames_synonyms_02<-get_colid_(col_acceptedFungalNames[30001:60000])
col_FungalNames_synonyms_03<-get_colid_(col_acceptedFungalNames[60001:90000])
col_FungalNames_synonyms_04<-get_colid_(col_acceptedFungalNames[90001:120000])
col_FungalNames_synonyms_05<-get_colid_(col_acceptedFungalNames[120001:135101])

saveRDS(col_FungalNames_synonyms_01,"CatalogueOfLifeData\\col_FungalNames_synonyms_01")
saveRDS(col_FungalNames_synonyms_02,"CatalogueOfLifeData\\col_FungalNames_synonyms_02")
saveRDS(col_FungalNames_synonyms_03,"CatalogueOfLifeData\\col_FungalNames_synonyms_03")
saveRDS(col_FungalNames_synonyms_04,"CatalogueOfLifeData\\col_FungalNames_synonyms_04")
saveRDS(col_FungalNames_synonyms_05,"CatalogueOfLifeData\\col_FungalNames_synonyms_05")


FungalSynonyms_col_01<-do.call("rbind",col_FungalNames_synonyms_01)
FungalSynonyms_col_02<-do.call("rbind",col_FungalNames_synonyms_02)
FungalSynonyms_col_03<-do.call("rbind",col_FungalNames_synonyms_03)
FungalSynonyms_col_04<-do.call("rbind",col_FungalNames_synonyms_04)
FungalSynonyms_col_05<-do.call("rbind",col_FungalNames_synonyms_05)

FungalSynonyms_col<-
  rbind(FungalSynonyms_col_01,
        FungalSynonyms_col_02,
        FungalSynonyms_col_03,
        FungalSynonyms_col_04,
        FungalSynonyms_col_05)

rm(col_FungalNames_synonyms_01,
   col_FungalNames_synonyms_02,
   col_FungalNames_synonyms_03,
   col_FungalNames_synonyms_04,
   col_FungalNames_synonyms_05)

rm(FungalSynonyms_col_01,
   FungalSynonyms_col_02,
   FungalSynonyms_col_03,
   FungalSynonyms_col_04,
   FungalSynonyms_col_05)


saveRDS(FungalTaxanomy_col,"CatalogueOfLifeData\\FungalTaxanomy_col")
saveRDS(FungalSynonyms_col,"CatalogueOfLifeData\\FungalSynonyms_col")

FungalTaxanomy_col$kingdom_id<-NULL
FungalTaxanomy_col$phylum_id<-NULL
FungalTaxanomy_col$class_id<-NULL
FungalTaxanomy_col$order_id<-NULL
FungalTaxanomy_col$family_id<-NULL
FungalTaxanomy_col$genus_id<-NULL

FungalTaxanomy_col<-#Here I am only adding status to the accepted names
  left_join(FungalTaxanomy_col,FungalSynonyms_col%>%
              select(id,status,source)%>%
              rename(species_id=id))

FungalTaxanomy_col<-#Here I am adding the synonyms (that is why it is joined by acc_id)
  left_join(FungalTaxanomy_col,FungalSynonyms_col%>%
              select(acc_id,name,status,source)%>%
              rename(species_id=acc_id)%>%
              rename(status_syn=status)%>%
              rename(source_syn=source)%>%
              rename(synonym=name))


###


All_names<-c(FungalTaxanomy_col$species,FungalSynonyms_col$name)
length(unique(All_names))==length(unique(FungalSynonyms_col$name))
All_names[!All_names%in%FungalSynonyms_col$name]#This means that FungalSynonyms_col contains all names in FungalTaxonomy_col

All_names[!All_names%in%Mycobank_Taxonomy$name[-which(is.na(Mycobank_Taxonomy$status))]]#But for some reason there is a bunch of names that are in col but I could not get them throuhg the get_colid_

All_names<-rbind(FungalSynonyms_col[c(1,2,4,6,7)],
                 Mycobank_Taxonomy[-which(is.na(Mycobank_Taxonomy$status)),c(20,21,23,25,26)])

#So, I am adding all the names in the same bin
All_names$nombres<-NA
All_names$nombres[which(All_names$status=="accepted name")]<-All_names$name[which((All_names$status=="accepted name"))]
All_names$nombres[which(All_names$status=="synonym")]<-All_names$acc_name[which((All_names$status=="synonym"))]
All_names$nombres[which(All_names$status=="ambiguous synonym")]<-All_names$acc_name[which((All_names$status=="ambiguous synonym"))]
All_names$nombres[which(All_names$status=="misapplied name")]<-All_names$acc_name[which((All_names$status=="misapplied name"))]
All_names$nombres[which(All_names$status=="provisionally accepted name")]<-
  All_names$acc_name[which((All_names$status=="provisionally accepted name"))]

All_names$nombres_id<-NA
All_names$nombres_id[which(All_names$status=="accepted name")]<-All_names$id[which((All_names$status=="accepted name"))]
All_names$nombres_id[which(All_names$status=="synonym")]<-All_names$acc_id[which((All_names$status=="synonym"))]
All_names$nombres_id[which(All_names$status=="ambiguous synonym")]<-All_names$acc_id[which((All_names$status=="ambiguous synonym"))]
All_names$nombres_id[which(All_names$status=="misapplied name")]<-All_names$acc_id[which((All_names$status=="misapplied name"))]
All_names$nombres_id[which(All_names$status=="provisionally accepted name")]<-
  All_names$acc_id[which((All_names$status=="provisionally accepted name"))]

All_names$dataset<-NA
All_names$dataset[1:length(FungalSynonyms_col$id)]<-"COL_COL"
All_names$dataset[(1+length(FungalSynonyms_col$id)):length(All_names$id)]<-"COL_Myco"

All_names$nombres_id[All_names$dataset=="COL_Myco"][!All_names$name[All_names$dataset=="COL_Myco"]%in%All_names$name[All_names$dataset=="COL_COL"]]

t1<-as.colid(unique(All_names$nombres_id)[1:30000])#This takes 40 min
t2<-as.colid(unique(All_names$nombres_id)[30001:60000])#This takes 40 min
#classification(unique(All_names$nombres_id)[1:10], db = 'col')#it takes 40 minutes
classification(t1, db = 'col')#it takes 40 minutes

table(Mycobank_SporeData$status)
length(which(is.na(Mycobank_SporeData$status)))
length(unique(Mycobank_SporeData$base_name))
length(which(is.na(Mycobank_SporeData$status)))

prueba<-Mycobank_SporeData[c(16:24)]

prueba$nombres<-NA
prueba$nombres[which(prueba$status=="accepted name")]<-prueba$name[which((prueba$status=="accepted name"))]
prueba$nombres[which(prueba$status=="synonym")]<-prueba$acc_name[which((prueba$status=="synonym"))]
prueba$nombres[which(prueba$status=="ambiguous synonym")]<-prueba$acc_name[which((prueba$status=="ambiguous synonym"))]
prueba$nombres[which(prueba$status=="misapplied name")]<-prueba$acc_name[which((prueba$status=="misapplied name"))]
length(which(is.na(prueba$nombres)))

#"misapplied name"
#Comparing taxonomy of mycobank with the taxonomy in the catalogue of life
library(VennDiagram)
venn.diagram(list(
  #Mycobank=unique(Mycobank_Taxonomy$name[!is.na(Mycobank_Taxonomy$name)]),
  #Mycobank=unique(prueba$name[which((prueba$status=="accepted name"))]),#Out of this comparison all fungal accepted names from which we have spore data are included in the accepted names of Catalogue of life
  Mycobank=unique(prueba$nombres[-which(is.na(prueba$nombres))]),
  #Cataloge_Of_Life=unique(FungalSynonyms_col$name[FungalSynonyms_col$status=="accepted name"])
  Cataloge_Of_Life=unique(All_names$nombres[-which(is.na(All_names$nombres))])
  ),
  height = 3480 , 
  width = 3480 ,
  cat.pos = c(-23, 23),
  cex=1.5,
  cat.cex= 1.5,
  lty = 'blank',
  #main = "Overlap of Taxonomy mycobank and col (accepted names_by_name)",
  main = "Overlap of Taxonomy mycobank and col (accepted names_byNombres)",
  main.cex = 2,
  main.pos = c(0.5,0.8),
  margin=0.3,
  filename = "Taxonomy_overlap_Mycobank_CoL3.png",
  fill=c("blue","red"))

#Looking at this diagram, there is little overlap (only 27010 sps)

#why?

Mycobank_Taxonomy$name[!Mycobank_Taxonomy$name%in%FungalSynonyms_col$name]

#It turns out that get_colid_ does not always report all synonyms associated
#to a name. It seems that when there is conflict, it does not report them. 
#For example, get_colid_("Ganoderma orbiforme") only retrieves one entry. However
#in the mycobank data I have a "Ganoderma boninense", however when I run 
#get_colid_("Ganoderma boninense") I get that this name is a synoym of Ganoderma
#orbiforme BUT ALSO of Ganoderma lucidum. Then, this example would explain why
#in some cases FungalSynonyms does not contain names that are in the Mycobank dataset

#What is left to do regarding the taxonomy?
#1. Filter out accepted names, this should represent the total number of 
#recognized species we have in the dataset.

p<-
Mycobank_Taxonomy%>%
  filter(status=="accepted name")%>%
  select(name)%>%
  count
rm(p)

#2. However, in the end all Mycobank_Taxonomy reflects the total amount of fungal
#species we have. It includes a mix of infraspecies and species names. 
#For claiming how much coverage of the fungal kingdom we have I have to use the accepted
#names form this dataframe. 

#Other issues: once the dataframe is sorted by accepted names and synonyms, then I will need to check how 
#much variation exist in spore size  among:
#-descriptions associated to the same name
#-synonyms associated to the same accepted name

#############################################################################################
#This is sth I found in the internet from Will Cornwell when trying to find all
#accepted names in Index Fungorum:

add.another.letter<-function(vec.in){
  vec.out <- paste(vapply(vec.in, function(x) paste0(x, letters), character(26)))
  return(vec.out)
}

get_all_fg_names<-function(){
  code3<- add.another.letter(add.another.letter(letters))
  out<-list()
  for (i in seq_along(code3)) {
    out[[i]] <- taxize::fg_name_search(q = code3[i], anywhere = FALSE, limit = 6000)
  }
  for (i in seq_along(letters)) {
    out[[length(code3)+i]] <- taxize::fg_name_search(q = paste0("Agaricus ",letters)[i], anywhere = FALSE, limit = 6000)
  }
  df.with.duplicates <- tibble::as_data_frame(dplyr::bind_rows(out))
  df.unique.names <- dplyr::distinct(df.with.duplicates,name_of_fungus, .keep_all = TRUE)
  return(df.unique.names)
}

start.time <- Sys.time()
fungal_names<-get_all_fg_names()
readr::write_csv(fungal_names,"fungal_names.csv")
end.time <- Sys.time()
time.taken <- end.time - start.time

