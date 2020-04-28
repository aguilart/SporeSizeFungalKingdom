                      ###################################################################################
                      ###### Mapping the distribution of spore size across the fungal kingdom      ######
                      ######                                                                       ######
                      ###################################################################################


rm(list=ls())

#packages
library(tidyverse)
library(VennDiagram)
source("MatchingSpore_FunctionData.R")
                      
                      
                      
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
        aes(y=spore_length,x=spore_width,color=SporeName,size=0.3)+
        geom_point()+
        facet_grid(. ~ phylum)+
        scale_color_manual(values = rainbow(14))+
        scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
        scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
        labs(y=expression("Spore length ("*mu*"m)"),x=expression("Spore width ("*mu*"m)"))+
        theme(title = element_text(size = 18),
              #axis.title.x=element_blank(),
              #axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
              axis.text.x = element_text(size = 20),
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



                ########################################
                ###      OVERLAP WITH FUNGUILD       ###
                ###   AND SPORE SIZE ACROSS GUILDS   ###
                ########################################


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

To_Analysis %>% 
  #filter(phylum=="Ascomycota") %>% 
  #filter(SporeName=="Conidia") %>% 
  #filter(!(SporeName=="Conidia" | SporeName=="Chlamydospores")) %>% 
  #filter(!(phylum=="Basidiomycota" & simpleFunct=="Ectomycorrhiza")) %>% 
  #filter(!(phylum=="Zygomycota" & simpleFunct=="Plant Pathogen")) %>% 
  ggplot()+
  aes(simpleFunct,SporeArea,fill=Life_style)+
  #aes(simpleFunct,SporeArea,fill=SporeName)+
  geom_jitter(size=1.5, width = 0.3,alpha=0.8)+
  geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75))+
  #facet_grid(SporeName~phylum, scales = "free")+
  #facet_grid(phylum~SporeName, scales = "free")+
  facet_grid(phylum~SporeName, scales = "free_y")+
  scale_color_brewer(palette="Set1")+
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(y=expression("Spore size as area ("*mu*"m²)"))+
  scale_x_discrete(labels=c("AFree living" = "Free living", "Human"="Human pathogen","Insect"="Insect pathogen",
                            "Plant Endophyte"="Endophyte","Lichen" = "Lichen*","Plant Pathogen"="Plant Pathogen",
                            "Plant Ectomycorrhizal"="Ectomycorrhiza","Plant AMF"="Arbuscular Mycorrhiza"))+
  my_theme2+
  coord_flip()




#Ascomycota and #Basidiomycota
p1<-
To_Analysis %>% 
  ungroup() %>% 
  filter(phylum=="Ascomycota" | phylum=="Basidiomycota") %>% 
  filter(!SporeName=="Teliospores") %>% 
  mutate(SporeName=gsub("Ascospores|Basidiospores","Sexual spores",SporeName)) %>% 
  #filter(SporeName=="Conidia") %>% 
  #filter(!(phylum=="Basidiomycota" & simpleFunct=="Human")) %>% 
  #filter(!(phylum=="Basidiomycota" & simpleFunct=="Ectomycorrhiza")) %>% 
  #filter(!(phylum=="Zygomycota" & simpleFunct=="Plant Pathogen")) %>% 
  ggplot()+
  aes(simpleFunct,SporeVolume,fill=Life_style)+
  geom_jitter(size=1.5, width = 0.3,alpha=0.8)+
  geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75))+
  #facet_grid(SporeName~phylum, scales = "free")+
  facet_grid(phylum~SporeName, scales = "free_y")+
  scale_color_brewer(palette="Set1")+
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(y=expression("Spore size as volume ("*mu*"m³)"))+
  scale_x_discrete(labels=c("AFree living" = "Free living", "Human"="Human pathogen","Insect"="Insect pathogen",
                            "Plant Endophyte"="Endophyte","Lichen" = "Lichen*","Plant Pathogen"="Plant Pathogen",
                            "Plant Ectomycorrhizal"="Ectomycorrhiza","Plant AMF"="Arbuscular Mycorrhiza"))+
  my_theme4+#theme(axis.text.x = element_blank(),axis.title.x=element_blank())+
  coord_flip()
  
  
#Basidiomycota or sexual spores (Included in the analysis)
p2<-
  To_Analysis %>% 
  filter(phylum=="Basidiomycota") %>%
  filter(!SporeName=="Teliospores") %>% 
  ggplot()+
  aes(simpleFunct,SporeVolume,fill=Life_style)+
  geom_jitter(size=1.5, width = 0.3,alpha=0.8)+
  geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75))+
  facet_grid(phylum~SporeName, scales = "free_y")+
  scale_color_brewer(palette="Set1")+
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(y=expression("Spore size as area ("*mu*"m²)"))+
  scale_x_discrete(labels=c("AFree living" = "Free living", "Human"="Human pathogen","Insect"="Insect pathogen",
                            "Plant Endophyte"="Endophyte","Lichen" = "Lichen*","Plant Pathogen"="Plant Pathogen",
                            "Plant Ectomycorrhizal"="Ectomycorrhiza","Plant AMF"="Arbuscular Mycorrhiza"))+
  my_theme3+
  coord_flip()

#Zygomycota
p3<-
  To_Analysis %>% 
  filter(!c(SporeName=="Azygospores" & phylum=="Zygomycota")) %>% 
  filter(phylum=="Zygomycota" | phylum=="Glomeromycota") %>% 
  mutate(phylum_="Basal clades") %>% 
  
  ggplot()+
  aes(simpleFunct,SporeArea,fill=Life_style)+
  geom_jitter(size=1.5, width = 0.3,alpha=0.8)+
  geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75))+
  facet_grid(phylum_~SporeName , scales = "free_y")+
  scale_color_brewer(palette="Set1")+
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(y=expression("Spore size as volume ("*mu*"m³)"))+
  scale_x_discrete(labels=c("AFree living" = "Free living", "Human"="Human pathogen","Insect"="Insect pathogen",
                            "Plant Endophyte"="Endophyte","Lichen" = "Lichen*","Plant Pathogen"="Plant Pathogen",
                            "Plant Ectomycorrhizal"="Ectomycorrhiza","Plant AMF"="Arbuscular Mycorrhiza"))+
  my_theme4+
  coord_flip()
#grid.arrange(p1,p2);p3


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
Ascospores<-Ascospores[-which(Ascospores$simpleFunct=="Plant Ectomycorrhizal"),]
Ascospores$simpleFunct<-as.factor(Ascospores$simpleFunct)
levels(Ascospores$simpleFunct)
contrasts(Ascospores$simpleFunct)<-cbind(
  c(5,-1,-1,-1,-1,-1),
  c(0,4,-1,-1,-1,-1),
  c(0,0,3,-1,-1,-1),
  c(0,0,0,2,-1,-1),
  c(0,0,0,0,1,-1)
)

#Conidia
Conidia<-To_Analysis[which(To_Analysis$SporeName=="Conidia"),]
Conidia<-Conidia[-which(Conidia$simpleFunct=="Plant Ectomycorrhizal"),]
Conidia$simpleFunct<-as.factor(Conidia$simpleFunct)
levels(Conidia$simpleFunct)
contrasts(Conidia$simpleFunct)<-cbind(
  c(5,-1,-1,-1,-1,-1),
  c(0,4,-1,-1,-1,-1),
  c(0,0,3,-1,-1,-1),
  c(0,0,0,2,-1,-1),
  c(0,0,0,0,1,-1)
)

#Basidiospores
Basidiospores<-To_Analysis[which(To_Analysis$SporeName=="Basidiospores"),]
Basidiospores<-Basidiospores[-which(Basidiospores$simpleFunct=="Human"),]
#Basidiospores<-Basidiospores[-which(Basidiospores$Life_style=="Insect"),]
Basidiospores$simpleFunct<-as.factor(Basidiospores$simpleFunct)
levels(Basidiospores$simpleFunct)
contrasts(Basidiospores$simpleFunct)<-cbind(
  c(4,-1,-1,-1,-1),
  c(0,3,-1,-1,-1),
  c(0,0,2,-1,-1),
  c(0,0,0,1,-1)
)

#Chlamydospores
Chlamydospores<-To_Analysis[which(To_Analysis$SporeName=="Chlamydospores"),]
Chlamydospores<-Chlamydospores[-which(Chlamydospores$simpleFunct=="Lichen"),]
Chlamydospores<-Chlamydospores[-which(Chlamydospores$simpleFunct=="Plant Ectomycorrhizal"),]
Chlamydospores$simpleFunct<-as.factor(Chlamydospores$simpleFunct)
levels(Chlamydospores$simpleFunct)
table(Chlamydospores$simpleFunct)
contrasts(Chlamydospores$simpleFunct)<-cbind(
  c(3,-1,-1,-1),
  c(0,2,-1,-1),
  c(0,0,1,-1)
)

#sporangiospores
Sporangiospores<-To_Analysis[which(To_Analysis$SporeName=="Sporangiospores"),]
Sporangiospores<-Sporangiospores[-which(Sporangiospores$simpleFunct=="AFree living"),]
Sporangiospores<-Sporangiospores[-which(Sporangiospores$simpleFunct=="Insect"),]
Sporangiospores$simpleFunct<-as.factor(Sporangiospores$simpleFunct)
levels(Sporangiospores$simpleFunct)
table(Sporangiospores$simpleFunct)
contrasts(Sporangiospores$simpleFunct)<-cbind(
  c(-1,1)
)

#Zygospores
Zygospores<-To_Analysis[which(To_Analysis$SporeName=="Zygospores"),]
Zygospores<-Zygospores[-which(Zygospores$simpleFunct=="AFree living"),]
Zygospores$simpleFunct<-as.factor(Zygospores$simpleFunct)
levels(Zygospores$simpleFunct)
table(Zygospores$simpleFunct)
contrasts(Zygospores$simpleFunct)<-cbind(
  c(-1,-1,2),
  c(-1,1,0)
)

#Making a priori contrasts

#Ascospores:
#Contrast1:Free living vs all symbionts
#Contrast2: Human pathogens vs all other symbionts
#Contrast3: Insect pathogens vs the more "plant-ish" symbionts
#Contrast4: Lichen vs Plant symbionts
#Contrast5: Endophytes vs Plant pathogens
summary.lm(
  aov(log10(SporeVolume) ~ simpleFunct,data = Ascospores)
)

summary(
  aov(log10(SporeVolume) ~ simpleFunct,data = Ascospores)
)


tapply(log10(Ascospores$SporeArea),
       Ascospores$simpleFunct,mean)

tapply(log10(Ascospores$SporeArea),
       Ascospores$Life_style,mean)

tapply(log10(Ascospores$SporeVolume),
       Ascospores$Life_style,mean)

#Conidia
#Contrast1:Free living vs all symbionts
#Contrast2: Human pathogens vs all other symbionts
#Contrast3: Insect pathogens vs the more "plant-ish" symbionts
#Contrast4: Lichen vs Plant symbionts
#Contrast5: Endophytes vs Plant pathogens
summary.lm(
  aov(log10(SporeVolume) ~ simpleFunct,data = Conidia)
)

summary(
  aov(log10(SporeVolume) ~ simpleFunct,data = Conidia)
)



#Basidiospores
table(Basidiospores$simpleFunct)
#Contrast1:Free living vs all symbionts
#Contrast2: Insect vs Plant-ish symbionts
#Contrast3: Lichen vs Plant symbionts
#Contrast4: Ectomycorhizal vs Plant pathogen
summary.lm(
  aov(log10(SporeVolume) ~ simpleFunct,data = Basidiospores)
)

summary(
  aov(log10(SporeVolume) ~ simpleFunct,data = Basidiospores)
)

with(Basidiospores,
tapply(log10(SporeArea),simpleFunct,mean)
)

with(Basidiospores,
     tapply(SporeArea,simpleFunct,mean)
)

with(Basidiospores,
     tapply(SporeVolume,simpleFunct,mean)
)

#Chlamydospores
levels(Chlamydospores$simpleFunct)
#Contrast1: Free living vs all symbionts
#Contrast2: Human vs Plant symbionts
#Contrast3: Plant endophyte and plant pathogens
summary.lm(
  aov(log10(SporeVolume) ~ simpleFunct,data = Chlamydospores)
)

summary(
  aov(log10(SporeVolume) ~ simpleFunct,data = Chlamydospores)
)


#sporangiospores
levels(Sporangiospores$simpleFunct)
#Contrast1: Human vs Plant pathogen
summary.lm(
  aov(log10(SporeVolume) ~ simpleFunct,data = Sporangiospores)
)

summary(
  aov(log10(SporeVolume) ~ simpleFunct,data = Sporangiospores)
)

#Zygospores
levels(Zygospores$simpleFunct)
#Contrast1:Plant pathogens vs Animal symbionts
#Contrast2:Human vs Insect

summary.lm(
  aov(log10(SporeVolume) ~ simpleFunct,data = Zygospores)
)

summary(
  aov(log10(SporeVolume) ~ simpleFunct,data = Zygospores)
)

#Based on the previous analysis it only makes sense to compare Ascospores and 
#Condia because they have the largest sample sizes and span all host types and 
#in the case of Conidia the 3 main phyla

################################################################################################################
################################################################################################################
############################################  SPORE SHAPE ######################################################
################################################################################################################
################################################################################################################
  
#Spore shape
  To_Analysis %>%
  filter(SporeName%in%c("Ascospores","Conidia"))%>%
  filter(!simpleFunct=="Plant Ectomycorrhizal") %>% 
  #filter(!SporeName%in%c("Azygospores","Teliospores"))%>%
  #filter(!SporeName%in%c("Teliospores"))%>%
  ggplot()+
  
  aes(simpleFunct,Q_ratio,fill=Life_style)+
  #aes(Life_style,SporeArea,color=SporeName)+
  #geom_point(size=1)+
  
  # aes(host,SporeArea,fill=SporeName)+
  geom_jitter(size=1.5, width = 0.3,alpha=0.8)+
  geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75))+
  
  facet_grid(.~SporeName , scales = "free")+
  #facet_grid(. ~ Life_style, scales = "free")+
  scale_color_brewer(palette="Set1")+
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(y=expression("Spore shape as Q ratio (lengt/width)"))+
  # scale_x_discrete(labels=c("AFree living" = "Free living", "Human"="Human pathogen","Insect"="Insect pathogen","Plant Endophyte"="Endophyte",
  #                         "Lichen" = "Lichen*","Plant Ectomycorrhizal"="Ectomycorrhiza","Plant Pathogen"="Plant Pathogen",
  #                         "Plant AMF"="Arbuscular Mycorrhiza"))+
  
  scale_x_discrete(labels=c("AFree living" = "Free living", "Human"="Human pathogen","Insect"="Insect pathogen","Plant Endophyte"="Endophyte",
                            "Lichen" = "Lichen*","Plant Pathogen"="Plant Pathogen"))+
  
  theme(title = element_text(size = 18),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.position = "none")


###
library(smatr)
#Ascospores
model_ascospore_shape<-sma(spore_length ~ spore_width*simpleFunct,multcomp = T,
                           data = Ascospores,log = "xy",slope.test = 1)

summary(model_ascospore_shape)
plot(model_ascospore_shape,pch = 19)

model_ascospore_shape<-sma(spore_length ~ spore_width+ simpleFunct, #multcomp = T,
                           data = Ascospores,log = "xy",elev.test =  0)

summary(model_ascospore_shape)

sma(spore_length ~ spore_width, #multcomp = T,#It is different from 0
    data = Ascospores[which(Ascospores$simpleFunct=="AFree living"),],log = "xy",elev.test =  0)
sma(spore_length ~ spore_width, #multcomp = T,
    data = Ascospores[which(Ascospores$simpleFunct=="Human"),],log = "xy",elev.test =  0)
sma(spore_length ~ spore_width, #multcomp = T,#It is different from 0
    data = Ascospores[which(Ascospores$simpleFunct=="Insect"),],log = "xy",elev.test =  0)
sma(spore_length ~ spore_width, #multcomp = T,#It is diff from 0
    data = Ascospores[which(Ascospores$simpleFunct=="Lichen"),],log = "xy",elev.test =  0)
sma(spore_length ~ spore_width, #multcomp = T,It is diff from 0
    data = Ascospores[which(Ascospores$simpleFunct=="Plant Endophyte"),],log = "xy",elev.test =  0)
sma(spore_length ~ spore_width, #multcomp = T,It is diff from 0
    data = Ascospores[which(Ascospores$simpleFunct=="Plant Pathogen"),],log = "xy",elev.test =  0)


#Conidia
model_conidia_shape<-sma(spore_length ~ spore_width*simpleFunct,
                         data = Conidia,log = "xy",slope.test = 1)

model_conidia_shape2<-sma(spore_length ~ spore_width,
                         data = Conidia[which(Conidia$simpleFunct=="Plant Pathogen"),],
                         log = "xy",elev.test = 0)

summary(model_conidia_shape)
plot(model_conidia_shape,pch = 19)


data(leaflife);leaf.low.soilp <- subset(leaflife, soilp == 'low')
sma(longev~lma+rain, log="xy", data=leaf.low.soilp,elev.test = 0)

For_conidia_parameters<-data.frame(
simpleFunct=levels(Conidia$simpleFunct),
slopes=c(1.33,1.18,0.69,0.81,1.05,1.14),
intercetps=c(0.18,0.14,0.61,0.63,0.31,0.61),
intercetps_2=c(0,0,0,0,0,0),
slopes_2=c(1,1,1,1,1,1),
Life_style=c("AFree living","Human","Insect","Lichen","Plant","Plant"))


For_ascospores_parameters<-data.frame(
  simpleFunct=levels(Conidia$simpleFunct),
  slopes=rep(1,6),
  intercetps=c(0.3061455,0,0.4379020,0.2910940,0.26287892,0.3896577),
  intercetps_2=rep(0,6),
  Life_style=c("AFree living","Human","Insect","Lichen","Plant","Plant")
)

#Basidiospores
model_basidiospore_shape<-sma(spore_length ~ spore_width*simpleFunct,#multcomp = T,
                           data = Basidiospores,log = "xy",slope.test = 1)

sma(spore_length ~ spore_width,#*simpleFunct,#multcomp = T,
    data = Basidiospores,log = "xy",elev.test = 0)

summary(model_basidiospore_shape)

For_basidiospores_parameters<-data.frame(
  simpleFunct=levels(Basidiospores$simpleFunct),
  slopes=c(0.9,1,1,1,1),
  intercetps=c(0.28,0.56,0.16,0.19,0.14),
  slopes_2=rep(1,5),
  intercetps_2=rep(0,5)

)


library(hexbin)

#Ascospores %>% 
#Conidia %>% 
Basidiospores %>% 
  #filter(simpleFunct=="Plant Pathogen") %>% 
ggplot()+
  aes(x=spore_width,
      y=spore_length,
      color=SporeVolume,
      #color=simpleFunct,
      #shape=simpleFunct,
      #fill=Life_style
      
      )+
  #geom_point(alpha=0.2)+
  geom_point(size=2)+
  scale_y_log10(breaks=c(10^0,10^1,10^2,10^3,10^4,10^5,10^6,10^7),
    labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_x_log10(breaks=c(10^0,10^1,10^2,10^3,10^4,10^5,10^6,10^7),
    labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  #geom_text(aes(label=good.names))+
  labs(x=expression("Basidiospores width ("*mu*"m)"),
       y=expression("Basidiospores length ("*mu*"m)"))+
  
  #For conidia
  #geom_abline(data=For_conidia_parameters,aes(slope=slopes,intercept=intercetps),lwd=1.5)+
  #geom_abline(data=For_conidia_parameters,aes(slope=slopes_2,intercept=intercetps_2),lwd=1.5,lty=2)+
  
  #For Ascospores
  #geom_abline(data=For_ascospores_parameters,aes(slope=slopes,intercept=intercetps),lwd=1.5)+
  #geom_abline(data=For_ascospores_parameters,aes(slope=slopes,intercept=intercetps_2),lwd=1.5,lty=2)+
  
  #For basidiospores
  geom_abline(data=For_basidiospores_parameters,aes(slope=slopes,intercept=intercetps),lwd=1.5)+
  geom_abline(data=For_basidiospores_parameters,aes(slope=slopes_2,intercept=intercetps_2),lwd=1.5,lty=2)+
  
  #geom_hex() +
  
  facet_wrap(.~simpleFunct , scales = "fixed")+
  #scale_color_manual(values = rainbow(14))+
  #geom_abline(slope=-1,intercept = 0.58,lty=2)+
  my_theme3+
  #scale_shape_manual(values=c(15,16,17,3,8))#+
 scale_color_continuous(type = "viridis",name=expression("Spore\nVolume ("*mu*"m³)"),trans="log10",
                        labels = scales::trans_format("log10", scales::math_format(10^.x)))#+
  #scale_color_viridis_d()
#scale_fill_continuous(type = "viridis")

#stat_function(fun=function(x)intercepts*x^slopes, geom="line",lty=2,color="black")+

#the entry for :Cosmospora flammea is wrong



########################################################################################################
#Counting how many guilds occur per phyla
Spore_functions%>%
  filter(!duplicated(names_to_use)) %>% 
  filter(!is.na(Life_style)) %>%
  filter(!grepl("-",host))%>%#The issue is that we only have data for 2503 species
  filter(!grepl("Fungi",host))%>%
  #filter(!SporeName%in%c("Azygospores","Teliospores"))%>%
  group_by(phylum,Guild_1)%>%
  count(phylum) %>% 
  ggplot()+
  aes(fill=Guild_1,x=phylum,y=n)+
  geom_bar(stat = "identity",position = "stack")+
  scale_color_brewer(palette="Set1")+
  #scale_fill_manual(values = prueba)+
  #geom_text(aes(Freq))%>%
  #geom_text(aes(label=Freq),vjust=-0.3, size=5)+
  labs(y="Number of species",x="Taxonomic group")+
  theme(title = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.text =  element_text(size = 15))+
  scale_fill_discrete(name="Functional groups")


#prueba<-c(rep("black",13),"blue","black","black","black")

#Number of guilds associated to plants for fungi producing Conidia
table(To_Analysis$trophicMode[which(To_Analysis$Life_style=="Plant"&To_Analysis$SporeName=="Conidia")])
table(To_Analysis$guild[which(To_Analysis$Life_style=="Plant"&To_Analysis$SporeName=="Ascospores")])

table(To_Analysis$guild[which(To_Analysis$Life_style=="Insect"&To_Analysis$SporeName=="Conidia")])
table(To_Analysis$guild[which(To_Analysis$Life_style=="Human"&To_Analysis$SporeName=="Conidia")])

temp2<-
  To_Analysis %>%
  filter(SporeName%in%c("Ascospores","Conidia"))%>%
  filter(!simpleFunct=="Plant Ectomycorrhizal") %>% 
  
  group_by(SporeName,simpleFunct) %>% 
  tally()
  
temp<-
  Spore_functions %>% 
  filter(!is.na(Life_style)) %>%
  filter(!grepl("-",host))%>%#The issue is that we only have data for 2503 species
  filter(!grepl("Fungi",host))%>%
  filter(!grepl("Animal",host))%>%
  filter(!SporeName%in%c("Azygospores","Teliospores"))%>%
  
  group_by(phylum,names_to_use,SporeType,SporeName,Specific_sporeName,Life_style)%>%
  summarise_at(c("SporeArea"),mean)%>%
  group_by(phylum,SporeName,Life_style) %>% 
  tally()



Conidia %>% 
  filter(simpleFunct=="Insect") %>% 
  ggplot()+
  aes(x=spore_width,
          y=spore_length,
          #color=SporeArea,
          color=order)+
  geom_point(size=2)+
  scale_y_log10(#breaks=c(10^0,10^1,10^2),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_x_log10(#breaks=c(10^0,10^1,10^2),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))
  




  