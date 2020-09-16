#
rm(list=ls())

library(tidyverse)

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


AllFungi<-
AllFungi[-which(AllFungi$Col_acc_names=="Heterogastridium pycnidioideum"),]#The description of this species does not contain conidia sizes
          
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
AllFungi$SporeType[which(AllFungi$SporeName=="Azygospores")]<-"Multinucleate asexual spores"
AllFungi$SporeType[which(AllFungi$SporeName=="Zygospores")]<-"Multinucleate sexual spores"
AllFungi$SporeType[which(AllFungi$SporeName=="Teliospores")]<-"Multinucleate sexual spores"


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

unique(FunGuildData$Guild_1)

FunGuildData<-
  read.csv("output/GuildData.csv",header = T,stringsAsFactors = F)

l<-sapply(strsplit(FunGuildData$taxon, " "), length)
FunGuildData$taxonomicLevel<-"Species"


#As for many species I do not know whether they are licheninzed or lichenicolous, I will use anything that has
#"lichen" as a generic term
FunGuildData$host[which(FunGuildData$host=="Lichen-Algae")]<-"Lichen"


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
Diseases_updateNoa<-read.csv("output///Pathogens_no_disease_fromgoogle_afterNoaWork.csv",header = T, 
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


#Adding life style column
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
#MERGING SPORE DATABASE AND FUNCTIONAL GROUP INFORMATION
#####################################################################################################

Spore_functions<-
  left_join(
    
    AllFungi#%>%
    # group_by(phylum,names_to_use,SporeName)%>%
    #mutate(SporeArea=spore_width*spore_length*(pi/4))%>% 
    ,#summarise_at(c("spore_width","spore_length","SporeArea"),mean),
    FunGuildData%>%
      #filter(taxonomicLevel=="Species")%>%
      rename(names_to_use=taxon)%>%
      #select(names_to_use,trophicMode,guild,host,substrate,Function,Number_of_guilds,Guild_1),
      select(names_to_use,trophicMode,guild,host,simpleFunct,Life_style,Number_of_guilds,Guild_1),
      by="names_to_use")
      

#fixing some entries
Spore_functions[which(Spore_functions$phylum=="Glomeromycota"),
                #c("names_to_use",
                c("trophicMode","host","substrate","Function","Number_of_guilds",
                  "guild","Guild_1","simpleFunct","Life_style")]<-
  FunGuildData[which(FunGuildData$taxon=="Glomus intraradices"),
               c("trophicMode","host","substrate","Function","Number_of_guilds",
                 "guild","Guild_1","simpleFunct","Life_style")]



Spore_functions[grep("Geosiphon",Spore_functions$names_to_use),
                #c("names_to_use","trophicMode","host","substrate","Function","Number_of_guilds",
                c("guild","Guild_1")]<-
  c("Arbuscular Mycorrhizal","Arbuscular Mycorrhizal")

Spore_functions$simpleFunct[which(Spore_functions$names_to_use%in%c("Lyophyllum decastes","Rhodocollybia butyracea"))]<-"Plant Ectomycorrhizal"



# #fixing some entries
# Spore_functions[which(Spore_functions$phylum=="Glomeromycota"),
#                 #c("names_to_use",
#                 c("trophicMode","host","substrate","Function","Number_of_guilds",
#                   "guild","Guild_1")]<-
#   FunGuildData[which(FunGuildData$taxon=="Glomus intraradices"),
#                c("trophicMode","host","substrate","Function","Number_of_guilds",
#                  "guild","Guild_1")]
# 
# 
# Spore_functions[grep("Geosiphon",Spore_functions$names_to_use),
#                 #c("names_to_use","trophicMode","host","substrate","Function","Number_of_guilds",
#                 c("guild","Guild_1")]<-
#   c("Arbuscular Mycorrhizal","Arbuscular Mycorrhizal")
# 
# Spore_functions$simpleFunct[which(Spore_functions$names_to_use%in%c("Lyophyllum decastes","Rhodocollybia butyracea"))]<-"Plant Ectomycorrhizal"




############################################################################################################
# SUBSET DATA FOR WHICH LIFE_STYLE INFORMATION IS AVAILABLE (FOR ANALYSIS) 
######################################################################################################

To_Analysis<-
  left_join(
    Spore_functions %>% 
      filter(!is.na(Life_style)) %>%
      filter(!grepl("-",host))%>%#The issue is that we only have data for 2503 species
      filter(!grepl("Fungi",host))%>%
      filter(!grepl("Animal",host))%>%
      #filter(!SporeName%in%c("Azygospores","Teliospores"))%>%
      
      group_by(phylum,names_to_use,SporeType,SporeName,Specific_sporeName,simpleFunct)%>%
      summarise_at(c("spore_length","spore_width","SporeArea","SporeVolume","Q_ratio"),mean),
    
    Spore_functions%>%
      filter(!duplicated(names_to_use)) %>% 
      filter(!is.na(Life_style)) %>%
      filter(!grepl("-",host))%>%#The issue is that we only have data for 2503 species
      filter(!grepl("Fungi",host))%>%
      filter(!grepl("Animal",host))%>%
      #filter(!SporeName%in%c("Azygospores","Teliospores"))%>%
      #group_by(phylum,guild) %>% 
      select(names_to_use,guild,Life_style,trophicMode,class,order,family,genus,Number_of_guilds))#;

############################################################################################################
# SUBSET DATA FOR EACH TYPE OF SPORE
######################################################################################################

#Ascospores
Ascospores<-To_Analysis[which(To_Analysis$SporeName=="Ascospores"),]
Ascospores<-Ascospores[-which(Ascospores$simpleFunct=="Plant Ectomycorrhizal"),]
Ascospores<-Ascospores[-which(Ascospores$simpleFunct=="Plant Pathogen Smut"),]#This is because there is only one species that actually produces a"False Smut"
Ascospores$simpleFunct<-as.factor(Ascospores$simpleFunct)
levels(Ascospores$simpleFunct)
# contrasts(Ascospores$simpleFunct)<-cbind(
#   c(-1,-1,-1,-1,-1,-1,-1,7),#All host associated against free living
#   c(6,-1,-1,-1,-1,-1,-1,0),#Human vs other more "evolved" pathogens
#   c(0,1,1,-1,-1,1,-1,0),#Obligate symbionts vs factulatitive ones
#   c(0,-1,-1,0,0,2,0,0),#Mildews against Insects and lichens
#   c(0,-1,1,0,0,0,0,0),#Insects vs lichens
#   c(0,0,0,2,-1,0,-1,0),#Endophytes vs necrotrophic pathogens
#   c(0,0,0,0,-1,0,1,0)#Necrotroph vs undefined pathogens but this is temporal
# )

#Conidia
Conidia<-To_Analysis[which(To_Analysis$SporeName=="Conidia"),]
Conidia<-Conidia[-which(Conidia$simpleFunct=="Plant Ectomycorrhizal"),]
Conidia<-Conidia[-which(Conidia $simpleFunct=="Plant Pathogen Smut"),]#This is because there is only one species that actually produces a"False Smut"
Conidia$simpleFunct<-as.factor(Conidia$simpleFunct)
levels(Conidia$simpleFunct)
contrasts(Conidia$simpleFunct)<-cbind(
  c(-1,-1,-1,-1,-1,-1,-1,7),#All host associated against free living
  c(6,-1,-1,-1,-1,-1,-1,0),#Human vs other more "evolved" pathogens
  c(0,1,1,-1,-1,1,-1,0),#Obligate symbionts vs factulatitive ones
  c(0,-1,-1,0,0,2,0,0),#Mildews against Insects and lichens
  c(0,-1,1,0,0,0,0,0),#Insects vs lichens
  c(0,0,0,2,-1,0,-1,0),#Endophytes vs necrotrophic pathogens
  c(0,0,0,0,-1,0,1,0)#Necrotroph vs undefined pathogens but this is temporal
)

#Basidiospores. I need to get more data on the rust fungi, smuts and if possible insect pathogens
Basidiospores<-To_Analysis[which(To_Analysis$SporeName=="Basidiospores"),]
Basidiospores<-Basidiospores[-which(Basidiospores$simpleFunct=="Human"),]#I remove human because we have only 2 sps
#Basidiospores<-Basidiospores[-which(Basidiospores$Life_style=="Insect"),]
Basidiospores$simpleFunct<-as.factor(Basidiospores$simpleFunct)
levels(Basidiospores$simpleFunct)
contrasts(Basidiospores$simpleFunct)<-cbind(
  c(-1,-1,-1,-1,-1,-1,-1,7),#Saprotroph vs host associated
  c(1,1,1,-1,1,1,-1,0),#Faculative association vs obligate association
  c(-1,-1,-1,0,4,-1,0,0)#Rust vs all other symbionts
  
)

#Chlamydospores
Chlamydospores<-To_Analysis[which(To_Analysis$SporeName=="Chlamydospores"),]
Chlamydospores<-Chlamydospores[-which(Chlamydospores$simpleFunct=="Lichen"),]
Chlamydospores<-Chlamydospores[-which(Chlamydospores$simpleFunct=="Plant Ectomycorrhizal"),]

# Chlamydospores$simpleFunct<-as.factor(Chlamydospores$simpleFunct)
# levels(Chlamydospores$simpleFunct)
# table(Chlamydospores$simpleFunct)
# contrasts(Chlamydospores$simpleFunct)<-cbind(
#   c(3,-1,-1,-1),
#   c(0,2,-1,-1),
#   c(0,0,1,-1)
# )

#sporangiospores
Sporangiospores<-To_Analysis[which(To_Analysis$SporeName=="Sporangiospores"),]
Sporangiospores<-Sporangiospores[-which(Sporangiospores$simpleFunct=="AFree living"),]
Sporangiospores<-Sporangiospores[-which(Sporangiospores$simpleFunct=="Insect"),]

# Sporangiospores$simpleFunct<-as.factor(Sporangiospores$simpleFunct)
# levels(Sporangiospores$simpleFunct)
# table(Sporangiospores$simpleFunct)
# contrasts(Sporangiospores$simpleFunct)<-cbind(
#   c(-1,1)
# )

#Zygospores
Zygospores<-To_Analysis[which(To_Analysis$SporeName=="Zygospores"),]
Zygospores<-Zygospores[-which(Zygospores$simpleFunct=="AFree living"),]

# Zygospores$simpleFunct<-as.factor(Zygospores$simpleFunct)
# levels(Zygospores$simpleFunct)
# table(Zygospores$simpleFunct)
# contrasts(Zygospores$simpleFunct)<-cbind(
#   c(-1,-1,2),
#   c(-1,1,0)
# )

#####################################################################################################
#LOADING PHYLOGENETIC INFORMATION FROM LUTZONI ET AL 2019
#####################################################################################################


#Code for figure 1 (phylogenetic tree)
library(ape)
library(ggtree)

#devtools::install_github("ropenscilabs/datastorr")
#devtools::install_github("wcornwell/taxonlookup")

library(taxonlookup)

# phylogeny from https://www.nature.com/articles/s41467-018-07849-9
tt<-read.nexus("phylo/Lutzoni_fungi_timetree.nex")
vcapply<-taxonlookup:::vcapply

split<-function(str){
  str_split <- strsplit(str, "[_ ]+")
  vcapply(str_split, "[[", 2L)
}

#generate genus in order lookup table
#fungal_genera<-split(nnn)[1:197]
#write_csv(tibble(genus=fungal_genera),"phylo/genus_names.csv")

#read in genus in order lookup
orders<-read_csv("phylo/orders_phylo.csv")
orders %>% group_by(order) %>%
  summarize(genus=genus[1]) -> only_one_genera_per_order

#getting a tree with one tip per order
nnn<-tt$tip.label
tt$tip.label<-split(nnn)
dropped.tree<-drop.tip(tt,tt$tip.label[which(!tt$tip.label %in% only_one_genera_per_order$genus)])
dup.tips<-names(which(table(dropped.tree$tip.label)>1))
tips<-dropped.tree$tip.label 
dropped.tree2<-drop.tip(dropped.tree,which(duplicated(tips)))
dropped.tree2$tip.label<-orders$order[match(dropped.tree2$tip.label,orders$genus)]


#read in spore data and calculate size and shape variables
sdf<-read_csv("output/Spore_Database_Fungi.csv")
matched_data<-filter(sdf,order%in%dropped.tree2$tip.label)
matched_data$log.length<-log10(matched_data$spore_length)
#matched_data$log_spore_area<-log10(matched_data$spore_length*matched_data$spore_width * pi /4)
matched_data$log_spore_volume<-log10((matched_data$spore_width^2)*matched_data$spore_length*(pi/6))
matched_data$length_divided_by_width<-matched_data$spore_length/matched_data$spore_width

#check tree
dropped.tree2$tip.label %in% matched_data$order

#relabel to ggtree convention
matched_data$id <- matched_data$order

#drop not assigned taxa; there are multiple spots on tree with non assigned taxa
matched_data<-filter(matched_data,order!="Not assigned")
dropped.tree2<-drop.tip(dropped.tree2,"Not assigned")

#drop small spore size categories for plotting
match2<-filter(matched_data,!SporeName %in% c("bulbil","papulospore"))

#select only necessary data
#temp<-select(match2,id,log_spore_area,SporeName,length_divided_by_width)
#temp<-select(match2,id,log_spore_volume,SporeName,length_divided_by_width)

temp<-select(match2,
             id, phylum,log_spore_volume, SporeName, length_divided_by_width)

temp$SporeType<-NA
temp$SporeType[which(temp$SporeName=="Ascospores"|temp$SporeName=="Basidiospores")]<-"Meiospores"
temp$SporeType[which(temp$SporeName=="Conidia"|temp$SporeName=="Sporangiospores"|temp$SporeName=="Chlamydospores")]<-"Mitospores"
temp$SporeType[which(temp$SporeName=="Azygospores")]<-"Multinucleate asexual spores"
temp$SporeType[which(temp$SporeName=="Zygospores")]<-"Multinucleate sexual spores"
temp$SporeType[which(temp$SporeName=="Teliospores")]<-"Teliospores"




#####################################################################################################
#LOADING SUBSET DATA WITH ITS INFORMATION (as sent by Jeff Powell on September 2020)
#####################################################################################################

df_species_byPrimerSet<-read.csv("output/df_species_byPrimerSet.csv")


#####################################################################################################
#Aesthetic themes used for figures
#####################################################################################################


my_theme<-
  theme(title = element_text(size = 18),
        #axis.title.x=element_blank(),
        #axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 25),
        legend.position = "none")


my_theme2<-
  theme(title = element_text(size = 18),
        panel.background = element_blank(),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.25, linetype = 'longdash',
                                        colour = "gray"),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 25),
        legend.position = "none")

my_theme2_5<-
  theme(title = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_line(size = 0.5, linetype = 'longdash',
                                        colour = "gray"),
        # panel.grid.minor = element_line(size = 0.5, linetype = 'longdash',
        #                                 colour = "gray"),
        #axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20),#,angle = 45,hjust = 1),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        legend.position = "none")

my_theme3<-
  theme(#title = element_text(size = 30),
        axis.title.x=element_text(size= 30),
        axis.title.y=element_text(size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 25),
        strip.text.y = element_text(size = 25)#,
        #legend.position = "none"
        )

my_theme4<-
  theme(#title = element_text(size = 30),
    axis.title.x=element_text(size= 25),
    axis.title.y=element_blank(),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    strip.text.x = element_text(size = 20),
    strip.text.y = element_text(size = 20),
    legend.position = "none"
  )


my_theme5<-
  theme(title = element_text(size = 18),
        panel.background = element_blank(),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x=element_blank(),
        #axis.text.x = element_text(size = 5,angle = 45,hjust = 1),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.position = "none")

my_theme6<-
  theme(title = element_text(size = 18),
        panel.background = element_blank(),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x=element_blank(),
        #axis.text.x = element_text(size = 5,angle = 45,hjust = 1),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.position = "none")

#Getting data on type of pathogen for "undefined pathogens". I did this with the help of 
#the student Noa Terracina

# To_check<-To_Analysis[which(To_Analysis$simpleFunct=="Plant Pathogen undefined"),
#                       c("names_to_use","simpleFunct")]
# 
# To_check<-To_check[-which(duplicated(To_check$names_to_use)),]
# To_check<-left_join(To_check,AllFungi[,c("names_to_use","base_name")])
# To_check$disease<-NA
# To_check$disease[which(To_check$base_name%in%rust_fungi$taxon)]<-"Rust"
# To_check$disease[which(To_check$base_name%in%smut_fungi$taxon)]<-"Smut"
# To_check$disease[which(To_check$base_name%in%mildew_fungi$taxon)]<-"Mildew"
# 
# To_check$disease[which(To_check$base_name%in%undefined_necrotroph$taxon)]<-"undefined_necrotroph"#Here is a mix of wilts, blights, damping off diseases
# 
# To_check$disease[which(To_check$base_name%in%spot_fungi$taxon)]<-"Spot"
# To_check$disease[which(To_check$base_name%in%canker_fungi$taxon)]<-"Canker"#It can be overlap between fungi producing cankers and spots
# 
# #For Noa to check
# To_check_Noa<-To_check[which(is.na(To_check$disease)),]
# names(To_check_Noa)[3]<-"taxon"
# 
# To_check_Noa<-left_join(To_check_Noa,FunGuildData[,c("taxon","citationSource")])
# To_check_Noa<-To_check_Noa[-which(duplicated(To_check_Noa$taxon)),]
# 
# write.csv(To_check_Noa,"Pathogens_with_no_disease_info.csv",row.names = F)
# 
