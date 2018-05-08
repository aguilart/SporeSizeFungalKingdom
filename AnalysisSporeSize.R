###################################################################################
###### Mapping the distribution of spore size across the fungal kingdom and  ######
###### (potentially) other fungi like organisms                              ######
###################################################################################

#packages
library(tidyverse)

#Data
AllFungi<-read.csv("AllFungi.csv",header = T,stringsAsFactors = F)

#Data in "AllFungi" used, so far, come from three sources:

#1. FunToFun Database, specifically from basidiospore of basidiomycota
#fungi from Bassler etal 2015.

#2. Data from Aguilar etal 2018 (submitted) on Azygospore size
#of Glomeromycotina.

#3. Data extracted from "Domsch, K., et al. (2007). Compendium of soil 
#fungi, IHW-Verlag Eching.". This correspond mainly to different types
#of conidia, ascospospores and chlamydospores from Ascomycota. There is
#also data on Mucoromycotina (both sexual an asexual spores), some basidio
#diomycota and oomycota.

#################################################################################

#Number of species from which we have spore data so far (per major taxonomic group)

data.frame(table(list(AllFungi$Phylum,AllFungi$Source)))%>%
#data.frame(table(AllFungi$Phylum))%>%
  ggplot()+
  aes(x=X.1,y=Freq,fill=X.2)+
  geom_bar(stat = "identity")+
  #geom_text(aes(label=Freq),vjust=-0.3, size=5)+
  labs(y="Number of species",x="Taxonomic group")+
  theme(title = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.text =  element_text(size = 15))+
        scale_fill_discrete(name="Data source")


#Spore size distribution across major taxonomic groups and spore types (sexual
#or asexual)

AllFungi%>%
  ggplot()+
  aes(SporeName,SporeArea,fill=Phylum)+
  geom_jitter(size=0.5, width = 0.3,alpha=1)+
  geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75))+
  facet_grid(. ~ Phylum, scales = "free")+
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(y=expression("Spore size as area ("*mu*"m²)"))+
  theme(title = element_text(size = 25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.position = "none")

#Matchings with other datasets

TedersooBlastMatches<-
  read.table("hits_species.blast.out.txt",header = T,stringsAsFactors = F)

length(
  which(AllFungi$Species_names%in%TedersooBlastMatches$species))

length(unique(TedersooBlastMatches$species[
  (which(TedersooBlastMatches$species%in%AllFungi$Species_names))]))

length(unique(FunGuildSpecies$names[
  (which(FunGuildSpecies$names%in%AllFungi$Species_names))]))

length(
  which(AllFungi$Species_names%in%FunGuildSpecies$names))

library(VennDiagram)

venn.diagram(list(Tederersoo_Matches=TedersooBlastMatches$species,
                  SporeData=AllFungi$Species_names,
                  FungGuild_matches=FunGuildSpecies$names),
             height = 3480 , 
             width =3480 ,
             cat.pos = c(-23, 23,180),
             cex=2,
             cat.cex= 2,
             lty = 'blank',
             filename = "Tedersoo_FunGuild_Spores.png",
             fill=c("yellow","blue","green"))

venn.diagram(list(Tederersoo_Matches=TedersooBlastMatches$species,
                  SporeData=AllFungi$Species_names#,
                  #FungGuild_matches=FunGuildSpecies$names
                  ),
             height = 3480 , 
             width =3480 ,
             cat.pos = c(-23, 23),
             cex=2,
             cat.cex= 2,
             lty = 'blank',
             filename = "Tedersoo&Spores.png",
             fill=c("yellow","blue"))

venn.diagram(list(#Tederersoo_Matches=TedersooBlastMatches$species,
                  SporeData=AllFungi$Species_names,
                  FungGuild_matches=FunGuildSpecies$names
                  ),
                  height = 3480 , 
                  width =3480 ,
                  cat.pos = c(-23, 23),
                  cex=2,
                  cat.cex= 2,
                  lty = 'blank',
                  filename = "FungGuild&Spores.png",
                  fill=c("yellow","green"))




#FunGuild

install.packages(c("httr", "jsonlite", "lubridate"))

url  <- "http://www.stbates.org/funguild_db.php"

trial<-parse_funguild(url)

FunGuildSpecies<-trial[trial$taxonomicLevel=="Species",]
FunGuildSpecies$names<-sub(" ","_",FunGuildSpecies$taxon)

FunToFun_sporeInfo$species%in%FunGuildSpecies$names


#Function to read 

parse_funguild <- function(url = 'http://www.stbates.org/funguild_db.php', tax_name = TRUE){
  
  # require(XML)
  # require(jsonlite)
  # require(RCurl)
  
  ## Parse data
  tmp <- XML::htmlParse(url)
  tmp <- XML::xpathSApply(doc = tmp, path = "//body", fun = XML::xmlValue)
  
  ## Read url and convert to data.frame
  db <- jsonlite::fromJSON(txt=tmp)
  
  ## Remove IDs
  db$`_id` <- NULL
  
  if(tax_name == TRUE){
    
    ## Code legend
    ## Taxon Level: A numeral corresponding the correct taxonomic level for the taxon
    taxons <- c(
      "keyword",                                                       # 0
      "Phylum", "Subphylum", "Class", "Subclass", "Order", "Suborder", # 3:8
      "Family", "Subfamily", "Tribe", "Subtribe", "Genus",             # 9:13
      "Subgenus", "Section", "Subsection", "Series", "Subseries",      # 15:19
      "Species", "Subspecies", "Variety", "Subvariety", "Form",        # 20:24
      "Subform", "Form Species")
    
    ## Table with coding
    taxmatch <- data.frame(
      TaxID = c(0, 3:13, 15:26),
      Taxon = factor(taxons, levels = taxons))
    
    ## Match taxon codes
    db$taxonomicLevel <- taxmatch[match(x = db$taxonomicLevel, table = taxmatch$TaxID), "Taxon"]
  }
  
  # remove rows with missing data
  # which(
  # 	with(db, trophicMode == "NULL" & guild == "NULL" & growthForm == "NULL" & trait == "NULL" & notes == "NULL")
  # 	)
  
  ## Add database dump date as attributes to the result
  attr(db, "DownloadDate") <- date()
  
  return(db)
}