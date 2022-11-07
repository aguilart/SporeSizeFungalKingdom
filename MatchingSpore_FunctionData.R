#
rm(list=ls())

library(tidyverse)
library(phylolm)

#Joining spore size data with fungal functional group

######################################################################################################
#LOADING SPORE DATABASE
######################################################################################################


#This database consist mostly on spore dimensions extracted from species descriptions deposited in
#Mycobank (as downloaded on November 2018). The spore dimensions were extracted using an algorithm
#developed by Franz Krah and later modified by Carlos Aguilar and Jeff Powell. Then other sources of
#spore data were added provided by other researches (see "AssemblingDataSources.R" for details)


AllFungi<-read.csv('output/Spore_Database_Fungi.csv', header = T,stringsAsFactors = F)


#Fixing some typos on June 2020 (eventually this will have to be changed in AssemblingDataSources.R
AllFungi[which(AllFungi$Col_acc_names=="Phyllactinia roboris"&AllFungi$Dim2==350),c("Specific_sporeName")]<-"ascospores"

AllFungi[which(AllFungi$Col_acc_names=="Phyllactinia roboris"&
                 AllFungi$Dim2==350),c("Dim1","Dim2","spore_length","spore_width")]<-
  c(40,40,21,21)#5-6 x 3-4.5 µm


AllFungi[which(AllFungi$Col_acc_names=="Golovinomyces cichoracearum"&
                 AllFungi$Dim1==175),c("Dim1","Dim2","spore_length","spore_width")]<-
  c(25,15,25,15)#5-6 x 3-4.5 µm

AllFungi[which(AllFungi$Col_acc_names=="Golovinomyces orontii"&
                 AllFungi$Dim1==175),c("Dim1","Dim2","spore_length","spore_width")]<-
  c(25,15,25,15)#5-6 x 3-4.5 µm


AllFungi[which(AllFungi$Col_acc_names=="Aspergillus alliaceus"&
                 AllFungi$Dim1>8),c("Dim1","Dim2","spore_length","spore_width")]<-
  c(7.25,7.25,6,6)#5.5-9 x 5-7 µm

AllFungi[which(AllFungi$names_to_use=="Ascosphaera apis"),
         c("Dim1","spore_width")] <- 1.5
  
AllFungi[which(AllFungi$names_to_use=="Ascosphaera apis"),
         c("Dim2","spore_length")] <- 2.75


AllFungi<-
AllFungi[-which(AllFungi$Col_acc_names=="Heterogastridium pycnidioideum"),]#The description of this species does not contain conidia sizes

AllFungi<-AllFungi[-which(AllFungi$to_replace=="Penicillium pulvillorum 19190 14157 conidia 250"),]#This entry is wrong
AllFungi<-AllFungi[-which(AllFungi$to_replace=="Penicillium steckii 19334 14181 conidia 20"),]#This entry is wrong

#
AllFungi[which(AllFungi$names_to_use=="Podospora pauciseta"),
         c("Dim1","Dim2","spore_length","spore_width")]<-
  c(37.5,37.5,37.5,37.5,37.5,
    20,20,20,20,20,
    37.5,37.5,37.5,37.5,37.5,
    20,20,20,20,20)#33-42 x 19-21


AllFungi[which(AllFungi$names_to_use=="Ustilaginoidea virens"),
         c("Dim2","spore_width")]
          
AllFungi$SporeArea<-AllFungi$spore_width*AllFungi$spore_length*(pi/4)
AllFungi$Q_ratio<-AllFungi$spore_length/AllFungi$spore_width
AllFungi$SporeVolume<-(AllFungi$spore_width^2)*AllFungi$spore_length*(pi/6)
AllFungi$class<-trimws(AllFungi$class)
AllFungi$order<-trimws(AllFungi$order)
AllFungi$family<-trimws(AllFungi$family)
AllFungi$phylum[which(AllFungi$phylum=="Zygomycota")]<-'"Zygomycetous fungi"'

AllFungi$SporeType<-NA


AllFungi$SporeType[which(AllFungi$SporeName=="Ascospores"|AllFungi$SporeName=="Basidiospores")]<-"Meiospores"
AllFungi$SporeType[which(AllFungi$SporeName=="Conidia"|AllFungi$SporeName=="Sporangiospores"|AllFungi$SporeName=="Chlamydospores")]<-"Mitospores"

#Just checking what happens when we DO NOT include chlamydospores in the same category as conidia and sporangiospores
# AllFungi$SporeType[which(AllFungi$SporeName=="Conidia"|AllFungi$SporeName=="Sporangiospores")]<-"Mitospores"
# AllFungi$SporeType[which(AllFungi$SporeName=="Chlamydospores")]<-"Chlamydospores"

AllFungi$SporeType[which(AllFungi$SporeName=="Azygospores")]<-"Multinucleate asexual spores"
AllFungi$SporeType[which(AllFungi$SporeName=="Zygospores")]<-"Multinucleate sexual spores"
AllFungi$SporeType[which(AllFungi$SporeName=="Teliospores")]<-"Multinucleate sexual spores"


# ca. 20 families have multiple orders because of conflicts with Catalogue of Life (2019 version)
# and Mycobank classification. Because Catalogue of Life was used as the template I decided
# to standardize by that one.

a<- unique(paste(AllFungi$order,AllFungi$family, sep = "_"))
b<- unique(AllFungi$family)

a<-str_split(a,"_")
a<-sapply(a,function(x){x[2]})

a <- a[-which(a == "Not assigned"|a == "NA")]

a[duplicated(a)]

AllFungi$order[AllFungi$family=="Pseudeurotiaceae"] <- "Not assigned"
AllFungi$order[AllFungi$family=="Venturiaceae"] <- "Venturiales"#
AllFungi$order[AllFungi$family=="Amphisphaeriaceae"] <- "Amphisphaeriales"#

AllFungi$order[AllFungi$family=="Apiosporaceae"] <- "Not assigned"#
AllFungi$order[AllFungi$family=="Thelenellaceae"] <- "Not assigned"#
AllFungi$order[AllFungi$family=="Pseudoperisporiaceae"] <- "Not assigned"#

AllFungi$order[AllFungi$family=="Nitschkiaceae"] <- "Coronophorales"
AllFungi$order[AllFungi$family=="Hymeneliaceae"] <- "Not assigned"#
AllFungi$order[AllFungi$family=="Massariaceae"] <- "Pleosporales"#

AllFungi$order[AllFungi$family=="Physciaceae"] <- "Teloschistales"#
AllFungi$order[AllFungi$family=="Gomphillaceae"] <- "Ostropales"#
AllFungi$order[AllFungi$family=="Trapeliaceae"] <- "Baeomycetales"#

AllFungi$order[AllFungi$family=="Parmulariaceae"] <- "Asterinales"#
AllFungi$order[AllFungi$family=="Elaphomycetaceae"] <- "Eurotiales"
AllFungi$order[AllFungi$family=="Lecideaceae"] <- "Not assigned"#

AllFungi$order[AllFungi$family=="Fuscideaceae"] <- "Not assigned"#
AllFungi$order[AllFungi$family=="Melaspileaceae"] <- "Arthoniales"#
AllFungi$order[AllFungi$family=="Clypeosphaeriaceae"] <- "Amphisphaeriale"#

AllFungi$order[AllFungi$family=="Mycosphaerellaceae"] <- "Mycosphaerellales"#
AllFungi$order[AllFungi$family=="Glomerellaceae"] <- "Glomerellales"#
AllFungi$order[AllFungi$family=="Ceratostomataceae"] <- "Melanosporales"#

AllFungi$order[AllFungi$family=="Piedraiaceae"] <- "Capnodiales"#
AllFungi$order[AllFungi$family=="Beltraniaceae"] <- "Sordariales"#
AllFungi$order[AllFungi$family=="Schizothyriaceae"] <- "Not assigned"#

AllFungi$order[AllFungi$genus=="Kirschsteiniothelia"] <- "Pleosporales"#
AllFungi$family[AllFungi$genus=="Kirschsteiniothelia"] <- "Kirschsteiniotheliaceae"#

AllFungi$names_to_use <- gsub("<eb>", "e", AllFungi$names_to_use)# Problem with umlaut on e
AllFungi$genus <- gsub("<eb>", "e", AllFungi$genus)# Problem with umlaut on e

AllFungi$names_to_use <- gsub("<e7>", "c", AllFungi$names_to_use)# Problem with portuguese c


###################
#Simplifying All fungi

# Spore_data<-aggregate(cbind(SporeVolume,spore_length,spore_width,
#                             SporeArea,Q_ratio)~.,mean,
#                       data=AllFungi[,c("SporeVolume","spore_length","spore_width",
#                                        "SporeArea","Q_ratio","SporeType","phylum",
#                                        "class","order","family","genus","names_to_use")]
#                       )

Spore_data <- AllFungi[which(AllFungi$width_extreme==F&AllFungi$length_extreme==F),] %>% 
  group_by(phylum,
           class,order,family,genus,names_to_use,SporeType) %>% 
  summarize_at(c("SporeVolume","spore_length","spore_width",
                 "SporeArea","Q_ratio"),mean)
# #Including only species with complete taxonomic information
# 
# Spore_data<-Spore_data[complete.cases(Spore_data),]
# 
# nta<-
#   which(rowSums(sapply(Spore_data[,c("phylum","class", "order","family")],function(x){x=="Not assigned"}))==1)
# 
# Spore_data<-Spore_data[-nta,]
# Spore_data<-Spore_data[complete.cases(Spore_data),]

#Finally, here are the species that are present in the Li tree that we use in the analysis

# Li_tree_sps <- readRDS("Li_tree_names.RDS") # The newest version
# 
# # Some of those species are not present in the original dataset (I added those species manually)
# # more on that can be found in li_tree_update_funct&spore.R
# Li_tree_sps$orginal_tree_names <- gsub("_", " ", Li_tree_sps$orginal_tree_names)
# names(Li_tree_sps)[9] <- "names_to_use"
# 
# Li_tree_sps <- Li_tree_sps[which(!Li_tree_sps$names_to_use%in%Spore_data$names_to_use), ]
# 
# Spore_data <- bind_rows(Spore_data, Li_tree_sps[, -c(2,3)])#28508+410 # Life_style and simpleFunct
# #rm(Li_tree_sps)

saveRDS(Spore_data,"output/Spore_data_12Nov21.RDS")

######################################################################################################
#LOADING THE TAXONOMY FROM CATALOGUE OF LIFE
######################################################################################################

#This database was obtained through R package taxize.

FungalTaxanomy_col<-read.csv('output/FungalTaxanomy_col.csv', header = T, stringsAsFactors = F)


#######################################################################################################
#LOADING FUNCTIONAL GROUP DATA
######################################################################################################

#This database was obtained by merging different sources on functional information about fungal species.
#More details can be found in "Checking_FunGuild.R".

FunGuildData<-
  read.csv("output/GuildData.csv",header = T,stringsAsFactors = F)

l<-sapply(strsplit(FunGuildData$taxon, " "), length)
FunGuildData$taxonomicLevel<-"Species"


#As for many species I do not know whether they are licheninzed or lichenicolous, I will use anything that has
#"lichen" as a generic term
FunGuildData$host[which(FunGuildData$host=="Lichen-Algae")]<-"Lichen"


# Standardizing guilds to common terminology


#Creating "simpleFunct" column
FunGuildData$simpleFunct<-FunGuildData$host
FunGuildData$simpleFunct[grep("Plant Pathogen",FunGuildData$guild)]<-"Plant Pathogen"
FunGuildData$simpleFunct[which(FunGuildData$guild=="Bryophyte Parasite")]<-"Plant Pathogen"
FunGuildData$simpleFunct[which(FunGuildData$guild=="Endophyte")]<-"Plant Endophyte"
FunGuildData$simpleFunct[which(FunGuildData$guild=="Ectomycorrhizal")]<-"Plant Ectomycorrhizal"
FunGuildData$simpleFunct[which(FunGuildData$guild=="Arbuscular Mycorrhizal")]<-"Plant AMF"
FunGuildData$simpleFunct[which(FunGuildData$simpleFunct=="Plant")]<-"Plant Endophyte"

#Adding disease information for plant pathogens
#This information was obtained from the USDA fungal-host database accessed through the package RUSDA developed by Franz Krah.
#Carlos Aguilar, used the function "substrate" to target disease information instead of substrate. The resulting
#downloaded entries were curated by Carlos Aguilar and Noa Terracina to follow the terminology found in the 
#handbook of Plant diseases". More information about how the data was assembled can be found in "Checking_USDA.R"


#Biotrophic diseases
rust_fungi<-read.csv("output/rust_fungi.csv",stringsAsFactors = F)
smut_fungi<- read.csv("output/smut_fungi.csv",stringsAsFactors = F)
mildew_fungi<-read.csv("output/mildew_fungi.csv",stringsAsFactors = F)

#Necrotrophic diseases
canker_fungi<- read.csv("output/canker_fungi.csv",stringsAsFactors = F)
spot_fungi<- read.csv("output/spot_fungi.csv",stringsAsFactors = F)
scorch_fungi<-read.csv("output/scorch_fungi.csv",stringsAsFactors = F)
anthracnose_fungi<-read.csv("output/anthracnose_fungi.csv",stringsAsFactors = F)
blotch_fungi<-read.csv("output/blotch_fungi.csv",stringsAsFactors = F)
blight_fungi<-read.csv("output/blight_fungi.csv",stringsAsFactors = F)
damping_off_fungi<-read.csv("output/damping_off_fungi.csv",stringsAsFactors = F)
rot_fungi<-read.csv("output/rot_fungi.csv",stringsAsFactors = F)

plant_necrotrophs<-c(
  canker_fungi$taxon,
  spot_fungi$taxon,
  scorch_fungi$taxon,
  anthracnose_fungi$taxon,
  blotch_fungi$taxon,
  blight_fungi$taxon,
  damping_off_fungi$taxon,
  rot_fungi$taxon)

#Adding disease information to roughly 100 species that Noa Terracina assinged based on different sources on
#August 2020
Diseases_updateNoa<-read.csv("output/Pathogens_no_disease_fromgoogle_afterNoaWork.csv",header = T, 
                             stringsAsFactors = F)#;USDA_Data_updateNoa$X<-NULL

#adding diseases data as column

FunGuildData$disease<-NA
FunGuildData$disease[which(FunGuildData$taxon%in%rust_fungi$taxon)]<-"Rust"
FunGuildData$disease[which(FunGuildData$taxon%in%smut_fungi$taxon)]<-"Smut"
FunGuildData$disease[which(FunGuildData$taxon%in%mildew_fungi$taxon)]<-"Mildew"

#FunGuildData$disease[which(FunGuildData$names_to_use%in%undefined_necrotroph$taxon)]<-"undefined_necrotroph"#Here is a mix of wilts, blights, damping off diseases

FunGuildData$disease[which(FunGuildData$taxon%in%canker_fungi$taxon)]<-"Canker"
FunGuildData$disease[which(FunGuildData$taxon%in%spot_fungi$taxon)]<-"Spot"
FunGuildData$disease[which(FunGuildData$taxon%in%scorch_fungi$taxon)]<-"Scorch"
FunGuildData$disease[which(FunGuildData$taxon%in%anthracnose_fungi$taxon)]<-"anthracnose"
FunGuildData$disease[which(FunGuildData$taxon%in%blotch_fungi$taxon)]<-"blotch"
FunGuildData$disease[which(FunGuildData$taxon%in%blight_fungi$taxon)]<-"blight"
FunGuildData$disease[which(FunGuildData$taxon%in%damping_off_fungi$taxon)]<-"damping_off"
FunGuildData$disease[which(FunGuildData$taxon%in%rot_fungi$taxon)]<-"rot_fungi"

#Adding the ones from Noa
FunGuildData$disease[which(FunGuildData$taxon%in%Diseases_updateNoa$names_to_use[Diseases_updateNoa$simpleFunct.1=="Rust"])]<-"Rust"
FunGuildData$disease[which(FunGuildData$taxon%in%Diseases_updateNoa$names_to_use[Diseases_updateNoa$simpleFunct.1=="Plant necrotroph"])]<-"Necrotroph"

#Updating simpleFunct using disesase information for plant pathogens
FunGuildData$simpleFunct[which(FunGuildData$simpleFunct=="Plant Pathogen")]<-
  paste(FunGuildData$simpleFunct[which(FunGuildData$simpleFunct=="Plant Pathogen")],
        FunGuildData$disease[which(FunGuildData$simpleFunct=="Plant Pathogen")])


FunGuildData$simpleFunct[which(FunGuildData$simpleFunct=="Plant Pathogen NA")]<-"Plant Pathogen undefined"
FunGuildData$disease[which(FunGuildData$simpleFunct=="Plant Pathogen undefined")]<-"Undefined"


FunGuildData$simpleFunct[which(FunGuildData$taxon%in%plant_necrotrophs)]<-"Plant necrotroph"
FunGuildData$simpleFunct[which(FunGuildData$simpleFunct=="Plant Pathogen Necrotroph")]<-"Plant necrotroph"


# Classifying guilds as a gradient of specialization from saprotrophs (asymbiotic)
# to obligate symbiotic. This was done by adding life style column
FunGuildData$Life_style<-FunGuildData$host
FunGuildData$Life_style[which(FunGuildData$trophicMode=="Saprotroph"&is.na(FunGuildData$host))]<-"AFree living"
FunGuildData$simpleFunct[which(FunGuildData$Life_style=="AFree living")]<-"Saprotroph"


FunGuildData$Life_style[which(FunGuildData$host=="Human")]<-"B_Opportunistic association"
FunGuildData$Life_style[which(FunGuildData$simpleFunct=="Plant Endophyte")]<-"Facultative association"

FunGuildData$Life_style[which(FunGuildData$simpleFunct=="Insect")]<-"Obligate association"
FunGuildData$Life_style[which(FunGuildData$simpleFunct=="Lichen")]<-"Obligate association"
FunGuildData$Life_style[grep("Mildew",FunGuildData$simpleFunct)]<-"Obligate association"
FunGuildData$Life_style[grep("Smut",FunGuildData$simpleFunct)]<-"Obligate association"
FunGuildData$Life_style[grep("Rust",FunGuildData$simpleFunct)]<-"Obligate association"
FunGuildData$Life_style[which(FunGuildData$simpleFunct=="Plant AMF")]<-"Obligate association"
FunGuildData$Life_style[which(FunGuildData$simpleFunct=="Plant Ectomycorrhizal")]<-"Obligate association"

#FunGuildData$Life_style[grep("Plant Pathogen undefined",FunGuildData$simpleFunct)]<-"Facultative association"
#FunGuildData$Life_style[grep("Plant Pathogen undefined_necrotroph",FunGuildData$simpleFunct)]<-"Facultative association"
FunGuildData$Life_style[grep("Plant necrotroph",FunGuildData$simpleFunct)]<-"Facultative association"


#####################################################################################################
#    Loading phylogenetic tree of Li et al 2020 and functional data associted to the species in the tree
#####################################################################################################

#Genome wide phylogenetic tree
Li_tree <- read.tree("Li_etal_tree.treefile")

#Rooting the tree using the outgroups used in the Li paper. To get those there is a second
#tree in the supplementary material of the Li etal 2019. This second tree does not include
#the outgroups (not fungal clades)

Li_tree2 <- read.tree("1644taxa_290genes_bb_1_root.tre")
outgroups <- which(!Li_tree$tip.label%in%Li_tree2$tip.label)

# Root in the tree
Li_tree <- root(Li_tree, outgroup = outgroups, resolve.root = T); rm(Li_tree2)

Li_tree_names<-readRDS("Li_tree_names.RDS") 

Li_tree_names$spore_width[which(Li_tree_names$orginal_tree_names=="Ascosphaera_apis")] <- 1.5
Li_tree_names$spore_length[which(Li_tree_names$orginal_tree_names=="Ascosphaera_apis")] <- 2.75

Li_tree_names$SporeVolume <- (Li_tree_names$spore_width^2)*Li_tree_names$spore_length*(pi/6)

Li_tree_sps <- readRDS("Li_tree_names.RDS")
Li_tree_sps$spore_width[which(Li_tree_sps$orginal_tree_names=="Ascosphaera_apis")] <- 1.5
Li_tree_sps$spore_length[which(Li_tree_sps$orginal_tree_names=="Ascosphaera_apis")] <- 2.75

Li_tree_sps$SporeVolume <- (Li_tree_sps$spore_width^2)*Li_tree_sps$spore_length*(pi/6)

# Some of those species are not present in the original dataset (I added those species manually)  more on that can be found in li_tree_update_funct&spore.R
Li_tree_sps$orginal_tree_names <- gsub("_", " ", Li_tree_sps$orginal_tree_names)
names(Li_tree_sps)[9] <- "names_to_use"

#####################################################################################################
#    MERGING SPORE DATABASE AND FUNCTIONAL GROUP INFORMATION
#####################################################################################################


Spore_functions<-
  left_join(
    Spore_data,
    FunGuildData%>%
      rename(names_to_use=taxon)%>%
      select(names_to_use,trophicMode,guild,host,simpleFunct,Life_style,Number_of_guilds,Guild_1),
    by="names_to_use")


# All Glomeromycota share the same functional groups
Spore_functions[which(Spore_functions$phylum=="Glomeromycota"),
                #c("names_to_use",
                c("trophicMode","guild","host","simpleFunct","Life_style",
                  "Number_of_guilds","Guild_1")]<-
  FunGuildData[which(FunGuildData$taxon=="Glomus intraradices"),
               c("trophicMode","guild","host","simpleFunct","Life_style",
                 "Number_of_guilds","Guild_1")]


Spore_functions$SporeVolume[which(Spore_functions$names_to_use%in%Li_tree_sps$names_to_use)]


Spore_functions$simpleFunct[which(Spore_functions$names_to_use%in%c("Lyophyllum decastes","Rhodocollybia butyracea"))]<-"Plant Ectomycorrhizal"

Spore_functions[which(Spore_functions$names_to_use%in%Li_tree_sps$names_to_use),
                c("Life_style", "simpleFunct")] <- 
  Li_tree_sps[match(Spore_functions$names_to_use[which(Spore_functions$names_to_use%in%Li_tree_sps$names_to_use)], Li_tree_sps$names_to_use),
              c("Life_style", "simpleFunct")]

# Checking the overlap between sps between original functional dataset and the ones from the
# the tree of Li et al 2020
m <-
  which(Spore_functions$names_to_use%in%Li_tree_sps$names_to_use&Spore_functions$SporeType=="Mitospores")


all(Spore_functions$names_to_use[m]== Li_tree_sps$names_to_use[match(Spore_functions$names_to_use[m],
                                                                     Li_tree_sps$names_to_use)])# This is true

all(Spore_functions$SporeType[m]== Li_tree_sps$SporeType[match(Spore_functions$names_to_use[m],
                                                               Li_tree_sps$names_to_use)])# This is also true


# While most of the time I concentrated in adding species from the tree that were not in the database, in some 
# cases I updated existing entries this was done randomly (when one entry caught my eyes). In the end in most cases
# the updated entries differ little from the original, less than 2 times the original value (shown below). 

# uno <- Spore_functions[m, c("names_to_use", "SporeType", "SporeVolume")]
# dos<- Li_tree_sps[match(Spore_functions$names_to_use[m],
#                         Li_tree_sps$names_to_use), c("names_to_use", "SporeType", "SporeVolume")]
# 
# # But the ones that differ are few
# p<- which(!uno$SporeVolume==dos$SporeVolume)
# 
# # An out of those ones only 5 have differences larger than 2x
# check<-uno$SporeVolume[p]/dos$SporeVolume[p]
# which(check>2)
# 
# plot(uno$SporeVolume[p],dos$SporeVolume[p])
# text(uno$SporeVolume[p],dos$SporeVolume[p], labels = uno$names_to_use[p])
# 
# cbind(uno[p,],dos[p,])

# For consistency I will replace the entrie in Spore_functions with the ones from the Li tree (so the exact same data) is used in all analysis (at least for the shared species).
mi <-
  which(Spore_functions$names_to_use%in%Li_tree_sps$names_to_use&Spore_functions$SporeType=="Mitospores")

Spore_functions[mi, c("spore_length", "spore_width", "SporeVolume")] <- 
  Li_tree_sps[match(Spore_functions$names_to_use[mi], Li_tree_sps$names_to_use),
              c("spore_length", "spore_width", "SporeVolume")]

me <-
  which(Spore_functions$names_to_use%in%Li_tree_sps$names_to_use&Spore_functions$SporeType=="Meiospores")

Spore_functions[me, c("spore_length", "spore_width", "SporeVolume")] <- 
  Li_tree_sps[match(Spore_functions$names_to_use[me], Li_tree_sps$names_to_use),
              c("spore_length", "spore_width", "SporeVolume")]

# Now adding the species that were added from the genome tree
Spore_functions <- bind_rows(Spore_functions,
                             Li_tree_sps[which(!Li_tree_sps$names_to_use%in%Spore_data$names_to_use), ])

#According to the Mycota all Zoopagales are obligate parasites

Spore_functions$Life_style[which(Spore_functions$order == "Zoopagales")] <- "Obligate association"
Spore_functions$phylum[Spore_functions$phylum == "\"Zygomycetous fungi\""] <- "Zygomycetous fungi"


# Fixing some enries of Tilletia

Spore_functions$simpleFunct[which(Spore_functions$genus == "Tilletia")] <- "Plant Pathogen Smut"
Spore_functions$Life_style[which(Spore_functions$genus == "Tilletia")] <- "Obligate association"

Li_tree_names$simpleFunct[which(Li_tree_names$genus == "Tilletia")] <- "Plant Pathogen Smut"
Li_tree_names$Life_style[which(Li_tree_names$genus == "Tilletia")] <- "Obligate association"

#####################################################################################################
#    Subsetting all the specis from which we have spore and functional data
#####################################################################################################

To_Analysis <-
  aggregate(cbind(SporeVolume,spore_length,spore_width,
                  SporeArea,Q_ratio)~.,mean,
            data=Spore_functions[,c("SporeVolume","spore_length","spore_width",
                                    "SporeArea","Q_ratio","SporeType",
                                    "phylum","class","order","family","genus","names_to_use",
                                    "Life_style", "simpleFunct")])

# grep('\xeb', To_Analysis$names_to_use, value=T)
To_Analysis$names_to_use <- gsub('\xeb', 'e', To_Analysis$names_to_use)

rm(Li_tree_sps, mi, me, uno, dos, check)

To_Analysis$simpleFunct<-gsub("Plant-Human","Human",To_Analysis$simpleFunct)
To_Analysis$Life_style<-gsub("Plant-Human","B_Opportunistic association",To_Analysis$Life_style)

To_Analysis<-To_Analysis[-grep("-",To_Analysis$Life_style),]

To_Analysis$simpleFunct[which(To_Analysis$simpleFunct == "Saprotroph")] <- "A_Saprotroph"
To_Analysis$Life_style<-gsub("Plant","Facultative association",To_Analysis$Life_style)
To_Analysis$simpleFunct[which(To_Analysis$simpleFunct == "Plant necrotroph")] <- "Plant Pathogen Necrotroph"

#unique(To_Analysis$simpleFunct)
#unique(To_Analysis$Life_style)

#####################################################################################################
#    #All species from which we have spore, functional, geographic extent and climate data
#####################################################################################################

dat <- read_csv('output/df_species_noPrimers.csv')


#####################################################################################################
#LOADING SEED AND EGG SIZE DATA
#####################################################################################################


#BirdEgg<-read.csv("C:\\Users\\Carlos\\Documents\\Bridging_Rep_Micro_Ecology_ISME_paper\\BridgingReproductiveEcology-MicrobialEcology\\Aguilar_etal_SuppMatt_Stoddard_etal_EggData.csv",header = T,stringsAsFactors = F)
BirdEgg<-read.csv("output/Aguilar_etal_SuppMatt_Stoddard_etal_EggData.csv",header = T,stringsAsFactors = F)


BirdEgg$T_value<-1/(BirdEgg$Ellipticity+1)
BirdEgg$Lambda<-BirdEgg$Asymmetry+1
step_1<-sapply(list(-0.75,-0.5,-0.25,0,0.25,0.5,0.75),
               function(x){((BirdEgg$T_value*(1+x)^(1/(1+BirdEgg$Lambda)))*
                              ((1-x)^(BirdEgg$Lambda/(1+BirdEgg$Lambda))))^2})
step_1[,1]<-step_1[,1]*4
step_1[,2]<-step_1[,2]*2
step_1[,3]<-step_1[,3]*4
step_1[,4]<-step_1[,4]*2
step_1[,5]<-step_1[,5]*4
step_1[,6]<-step_1[,6]*2
step_1[,7]<-step_1[,7]*4
BirdEgg$Volume<-(pi*(BirdEgg$AvgLength..cm.^3)*rowSums(step_1))/96

#1.4. Seed size data. Seed mass was retrieved from the database of the Kew Botanical Gardens. Data is given
#in miligrams

#kewData<-read.csv("C:\\Users\\Carlos\\Documents\\Bridging_Rep_Micro_Ecology_ISME_paper\\BridgingReproductiveEcology-MicrobialEcology\\Aguilar_etal_SuppMat_SeedSize.csv",header = T,stringsAsFactors = F)
kewData<-read.csv("output/Aguilar_etal_SuppMat_SeedSize.csv",header = T,stringsAsFactors = F)


#1.5. AMF life history traits. The data correpond to the series of paper of Hart and Reader on greehouse 
#experiments with 14 AMF species (see manuscript for full reference details).


#####################################################################################################
#LOADING SUBSET DATA WITH ITS INFORMATION (as sent by Jeff Powell on September 2020)
#####################################################################################################

df_species_byPrimerSet<-read.csv("output/df_species_byPrimerSet.csv")




#####
#Removing objects
rm(step_1,plant_necrotrophs,Diseases_updateNoa,mildew_fungi,rust_fungi,smut_fungi,canker_fungi,spot_fungi,scorch_fungi,anthracnose_fungi,blotch_fungi,blight_fungi,damping_off_fungi,rot_fungi)
