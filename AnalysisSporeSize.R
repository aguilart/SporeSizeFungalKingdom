                      ###################################################################################
                      ###### Mapping the distribution of spore size across the fungal kingdom      ######
                      ######                                                                       ######
                      ###################################################################################


rm(list=ls())

#packages
library(tidyverse)
library(VennDiagram)
source("MatchingSpore_FunctionData.R")
                      

                                                  #########################################
                                                  ### DATA ANALYSIS AND VISUALIZATION   ###
                                                  ###                                   ###
                                                  #########################################

                                                  ########################################
                                                  ### Number of unique sps in AllFungi ###
                                                  ########################################


data.frame(table(AllFungi[!duplicated(AllFungi$names_to_use),c("phylum")]))%>%
  ggplot()+
  aes(x=Var1,y=Freq)+
  geom_bar(stat = "identity")+
  #geom_text(aes(Freq))%>%
  #geom_text(aes(label=Freq),vjust=-0.3, size=5)+
  labs(y="Number of species",x="Taxonomic group")+
  theme(title = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.text =  element_text(size = 15))+
  scale_fill_discrete(name="Data source")


                                                  ########################################
                                                  ### Proportion of species per phylum
                                                  ### from which we have data          ###
                                                  ########################################


#For this I am using a waffle plot from the waffle package
#devtools::install_github("hrbrmstr/waffle")

#For some reason there are 899 entries that are duplicated in Fungal taxonomy dataframe
table(FungalTaxanomy_col$phylum[!duplicated(FungalTaxanomy_col$species)])

#Getting the most conservative estimate of the number of species from which we have
#spore data out of the Catalogue of Life recognized species
table(
  FungalTaxanomy_col$phylum[!duplicated(FungalTaxanomy_col$species)][#Dataframe without duplicates from COL
    unique(FungalTaxanomy_col$species)%in%unique(AllFungi$names_to_use)
    ]
)

#Since some species cleary do exist but are not in the COL (under any synonym) I decided to use a less conservative
#approach

data.frame(table(AllFungi[!duplicated(AllFungi$names_to_use),c("phylum")]))
  
Ascos<-c(`Spore` = 17656,`No_spore` = 66169)
Basidios<-c(`Spore` = 7530,`No_spore` = 40875)
Chytrids<-c(`Spore` = 2,`No_spore` = 1216)
Glomeros<-c(`Spore` = 274,`No_spore` = 10)
Zygos<-c(`Spore` = 295,`No_spore` = 1071)

library(waffle)
# library(extrafont)
# 
# fa <- tempfile(fileext = ".ttf")
# font_import()
# 
# devtools::install_github("rstudio/fontawesome")
# library(fontawesome)
# 
# fa <- tempfile(fileext = ".ttf")
# download.file("http://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/fonts/fontawesome-webfont.ttf?v=4.3.0",
#               destfile = fa, method = "curl")
# font_import(paths = dirname(fa), prompt = FALSE)
# 
# loadfonts(device = "win")
library(RColorBrewer)

waffle(
  Ascos / 100, rows = 20, size = 0.5,
  colors = c("blue","light blue"),
  title = "Ascomycota",
  #xlab = "1 square = 100 Species",
  pad = 3,
  legend_pos = "none"
  #use_glyph = "star"
)->A

waffle(
  Basidios / 100, rows = 12, size = 0.5,
  colors = c("blue","light blue"),
  title = "Basidiomycota",
  #xlab = "1 square = 100 Species",
  pad = 3,
  legend_pos = "none"
  #use_glyph = "star"
)->B

waffle(
  Chytrids , rows = 20, size = 0.5,
  colors = c("blue","light blue"),
  title = "Chytridiomycota",
  #xlab = "1 square = 1 Species", 
  pad = 3,
  legend_pos = "none"
  #use_glyph = "star"
)->C

waffle(
  Glomeros , rows = 7, size = 0.5,
  colors = c("blue","light blue"),
  title = "Glomeromycota",
  #xlab = "1 square = 1 Species",
  pad = 3,
  legend_pos = "none"
  #use_glyph = "star"
)->G

waffle(
  Zygos , rows = 20, size = 0.5,
  colors = c("blue","light blue"),
  title = "Zygomycota",
  #xlab = "1 square = 1 Species", 
  pad = 3,
  legend_pos = "none"
  #use_glyph = "star"
)->Z

#Ploting them. First the "higher fungi (A and B) and then the lower fungi (C,G,Z)
iron(A, B)
iron(C,G,Z)

#Knowing how many entries correspond to Mycobank and how many from other sources

data.frame(table(AllFungi[!duplicated(AllFungi$names_to_use),c("source_base_name")]))


#Having summary statistics on sizes and shape
AllFungi%>%
  filter(!is.na(phylum))%>%
  filter(!is.na(SporeName))%>%
  filter(!phylum=="Chytridiomycota")%>%#Filtering only because there are so few
  filter(!(phylum=="Ascomycota"&SporeName=="Basidiospores"))%>% #weird cases
  filter(!(phylum=="Ascomycota"&SporeName=="Teliospores"))%>% #weird  cases
  filter(!(phylum=="Basidiomycota"&SporeName=="Ascospores"))%>% #weird  cases
  filter(SporeName!="papulospore")%>%#Filtering only because there are so few
  filter(SporeName!="bulbil")%>%#Filtering only because there are so few
  group_by(phylum,names_to_use,SporeName,description__id)%>%
  mutate(SporeArea=spore_width*spore_length*(pi/4))%>% 
  summarise_at(c("spore_width","spore_length","SporeArea"),min)

AllFungi$SporeArea<-AllFungi$spore_width*AllFungi$spore_length*(pi/4)
AllFungi$Q_ratio<-AllFungi$length/AllFungi$spore_width

#Checking errors:

#Clean sporangiospores entries
#Typhula ishikariensis from Compendium is wrong. The basidiospores should be 6-8 x 3-4 um
#Cladophialophora minutissima is wrong (conidia entry). It has the format d+error x d+error
#Aphanoascella galapagosensis	is wrong. It has sub-structures
#Also check bigger sizes


                  ########################################
                  ### Spore Size ACCROSS PHYLUM AND    ###
                  ###           SPORE TYPE             ###
                  ########################################

#Spore size distribution across major taxonomic groups and spore types (sexual
#or asexual)

#why do flies lay eggs on ephemeral resources?
#why do fungi grow at different growth rates?
#reporductive scalin  increases from the tropics to the ploes in fish
#centre for geometric biology


#SIZE

AllFungi%>%
          filter(!is.na(phylum))%>%
          filter(!is.na(SporeName))%>%
          filter(phylum!="Chytridiomycota")%>%#Filtering only because there are so few
          filter(!(phylum=="Ascomycota"&SporeName=="Basidiospores"))%>% #weird cases
          filter(!(phylum=="Ascomycota"&SporeName=="Teliospores"))%>% #weird  cases
          filter(!(phylum=="Basidiomycota"&SporeName=="Ascospores"))%>% #weird  cases
          filter(SporeName!="papulospore")%>%#Filtering only because there are so few
          filter(SporeName!="bulbil")%>%#Filtering only because there are so few
          group_by(phylum,names_to_use,SporeName,description__id)%>%
          #mutate(SporeArea=spore_width*spore_length*(pi/4))%>% 
          summarise_at(c("SporeArea"),mean)%>%
  
          ggplot()+
          aes(SporeName,SporeArea,fill=phylum)+
          scale_fill_brewer(palette="Set1")+
          geom_jitter(size=0.5, width = 0.3,alpha=1)+
          geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75))+
          facet_grid(. ~ phylum, scales = "free")+
          scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
          labs(y=expression("Spore size as area ("*mu*"m²)"))+
          theme(title = element_text(size = 18),
                axis.title.x=element_blank(),
                axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
                axis.text.y = element_text(size = 20),
                strip.text.x = element_text(size = 20),
                legend.position = "none")#+
          #ggtitle(label = "Data from Aguilar_18,Comp. Soil Fungi, Bassler_15")


                  ########################################
                  ###           SPORE SHAPE            ###
                  ########################################

#SPORE SHAPE

#Across phyla and spore type (violin plots)

AllFungi%>%
  filter(!is.na(phylum))%>%
  filter(!is.na(SporeName))%>%
  filter(!phylum=="Chytridiomycota")%>%#Filtering only because there are so few
  filter(!(phylum=="Ascomycota"&SporeName=="Basidiospores"))%>% #weird cases
  filter(!(phylum=="Ascomycota"&SporeName=="Teliospores"))%>% #weird  cases
  filter(!(phylum=="Basidiomycota"&SporeName=="Ascospores"))%>% #weird  cases
  filter(SporeName!="papulospore")%>%#Filtering only because there are so few
  filter(SporeName!="bulbil")%>%#Filtering only because there are so few
  group_by(phylum,names_to_use,SporeName,description__id)%>%
  mutate(SporeArea=spore_width*spore_length*(pi/4))%>% 
  summarise_at(c("spore_width","spore_length","SporeArea"),mean)%>%
  
        ggplot()+
        aes(spore_length,spore_width,color=SporeName,size=0.3)+
        geom_point()+
        facet_grid(. ~ phylum, scales = "free")+
        scale_color_manual(values = rainbow(14))+
        scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
        scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
        labs(y=expression("Spore width ("*mu*"m)"),x=expression("Spore length ("*mu*"m)"))+
        theme(title = element_text(size = 18),
              #axis.title.x=element_blank(),
              axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
              axis.text.y = element_text(size = 20),
              strip.text.x = element_text(size = 20),
              legend.text =  element_text(size = 15))+
      ggtitle(label = "All phyla and spore types (log-log)")

#
#Scatterplot length:width and spore size (area)
p <- AllFungi%>%
  filter(!is.na(phylum))%>%
  filter(!is.na(SporeName))%>%
  filter(phylum!="Chytridiomycota")%>%#Filtering only because there are so few
  filter(!(phylum=="Ascomycota"&SporeName=="Basidiospores"))%>% #weird cases
  filter(!(phylum=="Ascomycota"&SporeName=="Teliospores"))%>% #weird  cases
  filter(!(phylum=="Basidiomycota"&SporeName=="Ascospores"))%>% #weird  cases
  filter(SporeName!="papulospore")%>%#Filtering only because there are so few
  filter(SporeName!="bulbil")%>%#Filtering only because there are so few
  group_by(phylum,names_to_use,SporeName)%>%
  mutate(SporeArea=spore_width*spore_length*(pi/4))%>% 
  mutate(length_width=spore_length/spore_width)%>%
  mutate(width_length=spore_width/spore_length)%>%
  summarise_at(c("spore_width","spore_length","SporeArea","length_width","width_length"),mean)%>%
  
  ggplot()+
  aes(SporeArea,length_width,color=SporeName,size=0.3)+
  #aes(SporeArea,width_length,color=SporeName,size=0.3)+
  geom_point(alpha=0.5, size=0.5)+
  #aes(SporeName,width_length,fill=phylum)+
  #geom_violin()+
  facet_grid(. ~ phylum, scales = "free")+
  scale_color_manual(values = rainbow(14))+
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(y=expression("length_to_width"),x=expression("Spore size as area ("*mu*"m²)"))+
  theme(title = element_text(size = 18),
        #axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.text =  element_text(size = 15))+
  ggtitle(label = "All phyla and spore types")
p

#contour plot (it is like a topographic plot) -- results are really weird...
p + geom_density_2d(aes(color = SporeName), bins=10)

#Spore shape of only sexual spores
AllFungi%>%
  filter(!is.na(phylum))%>%
  filter(!is.na(SporeName))%>%
  filter(phylum!="Chytridiomycota")%>%#Filtering only because there are so few
  filter(!(phylum=="Ascomycota"&SporeName=="Basidiospores"))%>% #weird cases
  filter(!(phylum=="Ascomycota"&SporeName=="Teliospores"))%>% #weird  cases
  filter(!(phylum=="Basidiomycota"&SporeName=="Ascospores"))%>% #weird  cases
  filter(SporeName!="papulospore")%>%#Filtering only because there are so few
  filter(SporeName!="bulbil")%>%#Filtering only because there are so few
  group_by(phylum,names_to_use,SporeName,description__id)%>%
  mutate(SporeArea=spore_width*spore_length*(pi/4))%>% 
  summarise_at(c("spore_width","spore_length","SporeArea"),mean)%>%
        
  filter(SporeName=="Basidiospores"|SporeName=="Ascospores"|SporeName=="Zygospores")%>%
        #filter(SporeName=="Basidiospores")%>%
        #mutate(Phylum_type=paste(Phylum,SporeName))%>%
        #filter(Phylum!="Glomeromycota")%>%
        ggplot()+
        aes(spore_length,spore_width,color=SporeName,shape=SporeName)+
        geom_point()+
  facet_grid(. ~ phylum, scales = "free")+
        scale_color_brewer(palette="Set1")+
        scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
        scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
        labs(y=expression("Spore width ("*mu*"m)"),x=expression("Spore length ("*mu*"m)"))+
        theme(title = element_text(size = 18),
              #axis.title.x=element_blank(),
              axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
              axis.text.y = element_text(size = 20),
              strip.text.x = element_text(size = 20),
              legend.text =  element_text(size = 15))+
      ggtitle(label = "Phyla with sexual spores (log-log)")

#Spore shape of asexual spores
AllFungi%>%
  filter(!is.na(phylum))%>%
  filter(!is.na(SporeName))%>%
  filter(phylum!="Chytridiomycota")%>%#Filtering only because there are so few
  filter(!(phylum=="Ascomycota"&SporeName=="Basidiospores"))%>% #weird cases
  filter(!(phylum=="Ascomycota"&SporeName=="Teliospores"))%>% #weird  cases
  filter(!(phylum=="Basidiomycota"&SporeName=="Ascospores"))%>% #weird  cases
  filter(SporeName!="papulospore")%>%#Filtering only because there are so few
  filter(SporeName!="bulbil")%>%#Filtering only because there are so few
  group_by(phylum,names_to_use,SporeName,description__id)%>%
  mutate(SporeArea=spore_width*spore_length*(pi/4))%>% 
  summarise_at(c("spore_width","spore_length","SporeArea"),mean)%>%

        filter(SporeName!="Basidiospores")%>%
        filter(SporeName!="Ascospores")%>%
        filter(SporeName!="Zygospores")%>%
        #filter(Phylum!="Glomeromycota")%>%
        ggplot()+
        aes(spore_length,spore_width,color=SporeName,shape=SporeName)+
        geom_point()+
        scale_color_brewer(palette="Set1")+
  facet_grid(. ~ phylum, scales = "free")+      
  #scale_color_brewer(palette="Paired")+
        scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
        scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
        labs(y=expression("Spore width ("*mu*"m)"),x=expression("Spore length ("*mu*"m)"))+
        theme(title = element_text(size = 18),
              #axis.title.x=element_blank(),
              axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
              axis.text.y = element_text(size = 20),
              strip.text.x = element_text(size = 20),
              legend.text =  element_text(size = 15))+
      ggtitle(label = "Phyla with asexual spores (log-log")




# #Standarized major axis series of analysis:
# # 1. Testing wehther host influces the slope of the relationship & wehther the relationship
# # is inversely proportinal (testing wehther slope = -1)
# 
# #I need to make this one for each spore type, across the taxonomy
# library(smatr)
# 
# # AllFungi_filtered%>%
# #   group_by(codigo)%>%
# #   summari
# 
# Spores_filtered<-
# AllFungi%>%
#   filter(!is.na(phylum))%>%
#   filter(!is.na(SporeName))%>%
#   filter(phylum!="Chytridiomycota")%>%#Filtering only because there are so few
#   filter(!(phylum=="Ascomycota"&SporeName=="Basidiospores"))%>% #weird cases
#   filter(!(phylum=="Ascomycota"&SporeName=="Teliospores"))%>% #weird  cases
#   filter(!(phylum=="Basidiomycota"&SporeName=="Ascospores"))%>% #weird  cases
#   filter(SporeName!="papulospore")%>%#Filtering only because there are so few
#   filter(SporeName!="bulbil")%>%#Filtering only because there are so few
#   group_by(phylum,lclass,order,family,genus,names_to_use,
#            SporeName,Specific_sporeName)%>%#In this way we have a value for a given species per spore type
#   mutate(SporeArea=spore_width*spore_length*(pi/4))%>% 
#   summarise_at(c("spore_width","spore_length","SporeArea"),mean)%>%
#   summarise_at()
# 
# AllFungi_filtered$codigo<-paste(AllFungi_filtered$names_to_use,
#                                 AllFungi_filtered$SporeName,
#                                 AllFungi_filtered$Specific_sporeName,
#                                 AllFungi_filtered$description__id,
#                                 sep = "_")
# 
# 
# scalings<-
# function(x){
#   sma(spore_width ~ spore_length,
#       data = x, log = "xy",slope.test = 1)
#   }
# 
# 
# lapply(
#   split(Spores_filtered,Spores_filtered$SporeName),
#   scalings)
# 
#   trial<-
#   sma(spore_width ~ spore_length,
#       data = AllFungi_filtered, log = "xy",slope.test = 1)
  
#####################################
#work done to create "phylogenetic tree" with Will on 09 Dec 2019  
  
#   if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
#   
#   BiocManager::install("ggtree")
#   
#   
# library(ggtree)
#   
#   tr <- rtree(30) 
#   p <- ggtree(tr)
#   d1 <- data.frame(id=tr$tip.label, 
#                    location=sample(c("GZ", "HK", "CZ"), 30, replace=TRUE)) 
#   
#   p1 <- p %<+% d1 + geom_tippoint(aes(color=location)) 
#   
#   d2 <- data.frame(id=tr$tip.label, val=rnorm(30, sd=3)) 
#   
#   p2 <- facet_plot(p1, panel="dot", data=d2, geom=geom_point, aes(x=val), color="firebrick") + theme_tree2()
#   
#   ##
#   d3 <- data.frame(id = rep(tr$tip.label, each=2),
#                    value = abs(rnorm(60, mean=100, sd=50)),
#                    category = rep(LETTERS[1:2], 30))
#   library(ggstance)
#   p3 <- facet_plot(p2, panel = 'Stacked Barplot', data = d3, 
#                    geom = geom_barh, 
#                    mapping = aes(x = value, fill = as.factor(category)), 
#                    stat='identity' ) 
  
genus_names<-read_csv("phylo/genus_names.csv")

length(
unique(genus_names$genus)  )
  
orders_phylo<-
AllFungi[which(
AllFungi$genus%in%genus_names$genus),
c("genus","order")
]

orders_phylo<-
  orders_phylo[-which(duplicated(paste(orders_phylo$genus,orders_phylo$order,sep = "_")
                                             )),];rownames(orders_phylo)<-NULL

write_csv(orders_phylo,"phylo/orders_phylo.csv")

#look which fungi are the lichens

#write.csv(FunGuildData,"FunGuildData.csv",row.names = F)




                ########################################
                ###      OVERLAP WITH FUNGUILD       ###
                ###   AND SPORE SIZE ACROSS GUILDS   ###
                ########################################


# temp<-AllFungi$Col_name
# temp[which(is.na(temp))]<-AllFungi$base_name[which(is.na(temp))]

venn.diagram(list(SporeData=AllFungi$base_name,
                  #SporeData=temp,
                  FungGuild_matches=FunGuildData$taxon
                  ),
                  height = 3480 ,
                  width =3480 ,
                  cat.pos = c(-23, 23),
                  cex=2,
                  cat.cex= 2,
                  lty = 'blank',
                  filename = "FungGuild&Spores_3.png",
                  fill=c("yellow","green"))

#########################################################################################################
################### Spore size across Guilds and trophic modes ##########################################
#########################################################################################################

Spore_functions %>% 
  filter(!is.na(Life_style)) %>%
  filter(!grepl("-",host))%>%#The issue is that we only have data for 2503 species
  filter(!grepl("Fungi",host))%>%
  filter(!SporeName%in%c("Azygospores","Teliospores"))%>%
  
  group_by(phylum,names_to_use,SporeType,SporeName,Specific_sporeName,Life_style)%>%
  summarise_at(c("SporeArea"),mean)%>%
  
  ggplot()+
  
  aes(Life_style,SporeArea,fill=Life_style)+
  #aes(Life_style,SporeArea,color=SporeName)+
  #geom_point(size=1)+
  
  # aes(host,SporeArea,fill=SporeName)+
  geom_jitter(size=1.5, width = 0.3,alpha=0.8)+
  geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75))+
  
  facet_grid(.~SporeName , scales = "free")+
  #facet_grid(. ~ Life_style, scales = "free")+
  scale_color_brewer(palette="Set1")+
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(y=expression("Spore size as area ("*mu*"m²)"))+
  scale_x_discrete(labels=c("AFree living" = "Free living", "Animal" = "Animal",
                            "Lichen" = "Lichen", "Plant" = "Plant"))+
  
  theme(title = element_text(size = 18),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.position = "none")
  #)

#For analysis

#It makes sense to make a priori contrasts. However I will perform those contrasts independently for each type of Spore where
#multiple host types occur

To_Analysis<-
  Spore_functions %>% 
  filter(!is.na(Life_style)) %>%
  filter(!grepl("-",host))%>%#The issue is that we only have data for 2503 species
  #filter(!grepl("Fungi",host))%>%
  #filter(!grepl(""))
  group_by(phylum,names_to_use,SporeType,SporeName,Specific_sporeName,Life_style)%>%
  summarise_at(c("SporeArea"),mean)


#Deviations of Life_style 
tapply(log10(To_Analysis$SporeArea),data$phylum,mean)


tapply(log10(To_Analysis$SporeArea),list(To_Analysis$phylum,To_Analysis$SporeType,To_Analysis$Life_style),mean)
#Here we assume that each species is an independent replicate. And Spore Area depends on the Life_style
#But Life_style is nested within Phylum and this within Spore type. This means the following:

#Phyla is not replicated: For examples there is only one Ascomycota, phylum cannot be replicated; However, all 
#species in the Ascomycota are more similar to each other than to the Basidiomycota. Then it does not make sense
#to determine whether spore size differe between sexual and asexual spores. Most of the asexual spores
#are in the ascomycota anyways and there will be a huge effect of different sizes in the zygos. 

#Option 1: Doing analysis as a hierachical one with error terms

summary(
  aov(log10(SporeArea)~Life_style+Error(phylum/SporeType),
      data =To_Analysis%>%
        filter(!grepl("Azygospores",SporeName))%>%
        filter(!grepl("Teliospores",SporeName))
      
  )
)
#####
temporal<-To_Analysis%>%
  filter(!grepl("Azygospores",SporeName))%>%
  filter(!grepl("Teliospores",SporeName))

temporal$SporeArea<-log10(temporal$SporeArea)

summary(
  aov(SporeArea~Life_style+Error(phylum/SporeType),
      data =temporal
      
  )
)

length(temporal$SporeArea-mean(temporal$SporeArea))
sum((tapply(temporal$SporeArea,temporal$phylum,mean)-mean(temporal$SporeArea))^2)#= 194.44
#The problem I am finding here is that the data is extremely unbalanced
#In the ideal world I would have the same amount of points per phylum, sexual spore and life style
#But the data is far from that. That is why it is a better idea to do the analysis for each phyla
#separated


#Option 2. Doing a priori contrast separated for each spore type

#Ascospores
Ascospores<-To_Analysis[which(To_Analysis$SporeName=="Ascospores"),]
Ascospores<-Ascospores[-which(Ascospores$Life_style=="Fungi"),]
Ascospores$Life_style<-as.factor(Ascospores$Life_style)
levels(Ascospores$Life_style)
contrasts(Ascospores$Life_style)<-cbind(
  c(3,-1,-1,-1),
  c(0,2,-1,-1),
  c(0,0,1,-1)
)

#Basidiospores
Basidiospores<-To_Analysis[which(To_Analysis$SporeName=="Basidiospores"),]
Basidiospores$Life_style<-as.factor(Basidiospores$Life_style)
levels(Basidiospores$Life_style)
contrasts(Basidiospores$Life_style)<-cbind(
  c(3,-1,-1,-1),
  c(0,2,-1,-1),
  c(0,0,1,-1)
)

#Chlamydospores
Chlamydospores<-To_Analysis[which(To_Analysis$SporeName=="Chlamydospores"),]
Chlamydospores$Life_style<-as.factor(Chlamydospores$Life_style)
levels(Chlamydospores$Life_style)
contrasts(Chlamydospores$Life_style)<-cbind(
  c(3,-1,-1,-1),
  c(0,2,-1,-1),
  c(0,0,1,-1)
)

#Conidia
Conidia<-To_Analysis[which(To_Analysis$SporeName=="Conidia"),]
Conidia<-Conidia[-which(Conidia$Life_style=="Fungi"),]
Conidia$Life_style<-as.factor(Conidia$Life_style)
levels(Conidia$Life_style)
contrasts(Conidia$Life_style)<-cbind(
  c(3,-1,-1,-1),
  c(0,2,-1,-1),
  c(0,0,1,-1)
)

#sporangiospores
Sporangiospores<-To_Analysis[which(To_Analysis$SporeName=="Sporangiospores"),]
Sporangiospores$Life_style<-as.factor(Sporangiospores$Life_style)
levels(Sporangiospores$Life_style)
contrasts(Sporangiospores$Life_style)<-cbind(
  c(2,-1,-1),
  c(0,1,-1)
)

#Zygospores
Zygospores<-To_Analysis[which(To_Analysis$SporeName=="Zygospores"),]
Zygospores$Life_style<-as.factor(Zygospores$Life_style)
levels(Zygospores$Life_style)
contrasts(Zygospores$Life_style)<-cbind(
  c(2,-1,-1),
  c(0,1,-1)
)

#Making a priori contrasts

#Ascospores
summary.lm(
  aov(log10(SporeArea) ~ Life_style,data = Ascospores)
)

summary(
  aov(log10(SporeArea) ~ Life_style,data = Ascospores)
)

#Basidiospores
summary.lm(
  aov(log10(SporeArea) ~ Life_style,data = Basidiospores)
)

summary(
  aov(log10(SporeArea) ~ Life_style,data = Basidiospores)
)

#Chlamydospores
summary.lm(
  aov(log10(SporeArea) ~ Life_style,data = Chlamydospores)
)

summary(
  aov(log10(SporeArea) ~ Life_style,data = Chlamydospores)
)

#Conidia
summary.lm(
  aov(log10(SporeArea) ~ Life_style,data = Conidia)
)

summary(
  aov(log10(SporeArea) ~ Life_style,data = Conidia)
)

#sporangiospores
summary.lm(
  aov(log10(SporeArea) ~ Life_style,data = Sporangiospores)
)

summary(
  aov(log10(SporeArea) ~ Life_style,data = Sporangiospores)
)

#Zygospores
summary.lm(
  aov(log10(SporeArea) ~ Life_style,data = Zygospores)
)

summary(
  aov(log10(SporeArea) ~ Life_style,data = Zygospores)
)




#Example from the R book: page 378 in the pdf in my laptop
levels(clipping)
contrasts(clipping)<-cbind(c(4,-1,-1,-1,-1),c(0,1,1,-1,-1),c(0,0,0,1,-1),c(0,1,-1,0,0))
contrasts(clipping)
model2<-aov(biomass ~ clipping)
summary.lm(model2)






###

summary.aov(
  lm(log10(SporeArea)~host*phylum*SporeName,data = 
       Spore_functions %>% 
       filter(!is.na(host)) %>%
       filter(!grepl("-",host))
     )
)

quick_analysis<-
lm(log10(SporeArea)~host*phylum*SporeName,data = 
     Spore_functions %>% 
     filter(!is.na(host)) %>%
     filter(!grepl("-",host)))

plot(quick_analysis)

#Spore shape
Spore_functions %>% 
  filter(!is.na(host)) %>%
  filter(!grepl("-",host))%>%
  group_by(phylum,names_to_use,SporeName,Specific_sporeName,description__id,host)%>%
  summarise_at(c("Q_ratio"),mean)%>%
  
  ggplot()+
  
  aes(host,Q_ratio,color=SporeName)+
  #geom_point(alpha=0.3)+
  
  # aes(host,Aspect_Ratio,fill=SporeName)+
  geom_jitter(size=1.5, width = 0.3,alpha=0.8)+
  # geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75))+
  
  facet_grid(. ~ SporeName, scales = "free")+
  scale_color_brewer(palette="Set1")+
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(y=expression("Q ratio (length:width)"))+
  theme(title = element_text(size = 18),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.position = "none")
  #)

#Selecting the fungi that we have as animal symbionts
animal_symbionts<-unique(Spore_functions$names_to_use[which(Spore_functions$host=="Animal")])

table(
FunGuildData$citationSource[FunGuildData$taxon%in%animal_symbionts])

#140 species are coming from the Iriyini database on medical mycology out of 159 species that I have reported as "animal" pathogens


#                     ########################################
#                     ###      OVERLAP WITH USDA           ###
#                     ###    SPORE SIZE HABITAT RANGE      ###
#                     ########################################
# 
# 
# #Spore size  and host range
# AllFungi[300:1523,]%>%
#             filter(SporeName!="bulbil"&
#                      SporeName!="oospore"&
#                      SporeName!="papulospore")%>%
#             ggplot()+
#             aes(x=log10(SporeArea),y=log10(host_range),col=SporeName)+
#             geom_point()
# 
# #Plot for sexual spores & analysis of the relationship
# AllFungi[300:1523,]%>%
#           filter(!is.na(host_range))%>%
#           filter(SporeName=="ascospores"|
#                   SporeName=="Basidiospores"|
#                   SporeName=="zygospores"
#                    )%>%
#           ggplot()+
#           aes(x=SporeArea,y=host_range,col=SporeName)+
#           geom_point()+
#            scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
#            scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
#           labs(y="host range (log species number)",x=expression("Spore size as log area ("*mu*"m²)"))+
#           theme(title = element_text(size = 25),
#             axis.text.x = element_text(size = 20,hjust = 1),
#                 axis.text.y = element_text(size = 20),
#                 strip.text.x = element_text(size = 20))+
#                 
#           scale_size(guide = F)+
#           #geom_abline(slope = 0.4600, intercept=0.3574,size=1.5,col="red")+
#           #geom_abline( slope=(0.4600-0.7740),intercept=(0.3574+0.6359), size=1.5,col="green")+
#           #geom_abline( slope=(0.4600-0.1119 ),intercept=(0.3574-0.5313 ), size=1.5,col="blue")+
#           geom_smooth(method = "lm",formula = y~x)+
#           facet_wrap(~ SporeName,scales = "free_x")+
#           theme(legend.position="none")