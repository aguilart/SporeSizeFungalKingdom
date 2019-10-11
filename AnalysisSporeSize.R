###################################################################################
###### Mapping the distribution of spore size across the fungal kingdom and  ######
###### (potentially) other fungi like organisms                              ######
###################################################################################

#packages
library(tidyverse)
library(VennDiagram)

rm(list=ls())

#Loading spore dataset:

AllFungi<-read.csv('output/Spores_All_Sources.csv',header = T, stringsAsFactors = F)



#It turns out that several species were not found in the catalogue of Life
head(AllFungi[which(is.na(AllFungi$Col_acc_names)),])

#So, for the moment my solution is to create a new column with the "names to use" based on 
#mix of col_acc_names and base name

AllFungi$names_to_use<-AllFungi$Col_acc_names
AllFungi[which(is.na(AllFungi$names_to_use)),27]<-AllFungi[which(is.na(AllFungi$names_to_use)),3]

unique(AllFungi$phylum_)
unique(AllFungi$phylum)
#There are left 335 cases where no phylum is assigned. In almost all of these cases no other 
#piece of information is left, except for 64 species which they are reported as Fungi.
#Aside from those 64, looking at the genera, it seems there are a lot of Oomycetes
length(which(is.na(AllFungi$phylum_)))
all(is.na(AllFungi[which(is.na(AllFungi$phylum_)),]$Col_acc_names))
length(AllFungi[which(is.na(AllFungi$phylum_)&AllFungi$kingdom=="Fungi"),1])

#################################################################################

                    ########################################
                    ### Number of unique sps in AllFungi ###
                    ########################################

#To leave out: Protozoa, Chromista, Deuteromycota; 
data.frame(table(AllFungi[!duplicated(AllFungi$names_to_use),18]))%>%
  filter(Var1!="Deuteromycota")%>%
  filter(Var1!="Choanozoa")%>%
  filter(Var1!="Fossil Ascomycota")%>%
  filter(Var1!="Oomycota")



data.frame(table(AllFungi[!duplicated(AllFungi$names_to_use),26]))%>%
            filter(Var1!="Deuteromycota")%>%
            filter(Var1!="Choanozoa")%>%
            filter(Var1!="Fossil Ascomycota")%>%
            filter(Var1!="Oomycota")%>%
            #filter(is.na(phylum_))%>%
            #data.frame(table(AllFungi$Phylum))%>%
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



devtools::install_github("hrbrmstr/waffle")



#Calculating how many fungi per phylum we have or per datasource
data.frame(table(AllFungi[!duplicated(AllFungi$Species_names),c(6,7)]))
data.frame(table(AllFungi[!duplicated(AllFungi$Species_names),6]))
data.frame(table(AllFungi[!duplicated(AllFungi$Species_names),7]))
length(AllFungi$Species_names[is.na(AllFungi$Phylum)])#only 7 sps have not assigned a Phylum


                  ########################################
                  ### Spore Size ACCROSS PHYLUM AND    ###
                  ###           SPORE TYPE             ###
                  ########################################

#Spore size distribution across major taxonomic groups and spore types (sexual
#or asexual)
AllFungi[-c(5240,5607,5658,7283),]%>%#Removing the entries for Bulgaria pura,Torula carbonaria,Gloeosporium succineum, Peziza pura. They reported as ascomycetes with teliospores, which is weird
          filter(!is.na(Phylum))%>%
          filter(!is.na(SporeName))%>%
          filter(Phylum!="Entomophthoromycotina")%>%
          filter(Phylum!="Oomycota")%>%
          filter(!duplicated(measure_orig)|is.na(measure_orig))%>%
          #filter(Phylum!="Glomeromycota")%>%
          ggplot()+
          aes(SporeName,SporeArea,fill=Phylum)+
          scale_fill_brewer(palette="Set1")+
          geom_jitter(size=0.5, width = 0.3,alpha=1)+
          geom_violin(alpha=0.8, draw_quantiles=c(0.25, 0.5, 0.75))+
          facet_grid(. ~ Phylum, scales = "free")+
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

#Spore size of all species
AllFungi%>%
        filter(!is.na(Phylum))%>%
        filter(!is.na(SporeName))%>%
        filter(Phylum!="Entomophthoromycotina")%>%
        filter(Phylum!="Oomycota")%>%
        filter(!duplicated(measure_orig)|is.na(measure_orig))%>%
        mutate(Phylum_type=paste(Phylum,SporeName))%>%
        #filter(Phylum!="Glomeromycota")%>%
        ggplot()+
        aes(spore_length,spore_width,color=Phylum_type,size=0.3)+
        geom_point()+
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

#Spore shape of only sexual spores
AllFungi%>%
        filter(!is.na(Phylum))%>%
        filter(!is.na(SporeName))%>%
        filter(Phylum!="Entomophthoromycotina")%>%
        filter(Phylum!="Oomycota")%>%
        filter(SporeName=="Basidiospores"|SporeName=="ascospores"|SporeName=="zygospores")%>%
        #filter(SporeName=="Basidiospores")%>%
        filter(!duplicated(measure_orig)|is.na(measure_orig))%>%
        #mutate(Phylum_type=paste(Phylum,SporeName))%>%
        #filter(Phylum!="Glomeromycota")%>%
        ggplot()+
        aes(spore_length,spore_width,color=SporeName,shape=SporeName)+
        geom_point()+
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
        filter(!is.na(Phylum))%>%
        filter(!is.na(SporeName))%>%
        filter(Phylum!="Entomophthoromycotina")%>%
        filter(Phylum!="Oomycota")%>%
        filter(SporeName!="Basidiospores")%>%
        filter(SporeName!="ascospores")%>%
        filter(SporeName!="zygospores")%>%
        filter(!duplicated(measure_orig)|is.na(measure_orig))%>%
        #mutate(Phylum_type=paste(Phylum,SporeName))%>%
        #filter(Phylum!="Glomeromycota")%>%
        ggplot()+
        aes(spore_length,spore_width,color=SporeName,shape=Phylum)+
        geom_point()+
        scale_color_brewer(palette="Set1")+
        #scale_color_brewer(palette="Paired")+
        scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
        scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
        #facet_grid(. ~ Phylum, scales = "free")+
        labs(y=expression("Spore width ("*mu*"m)"),x=expression("Spore length ("*mu*"m)"))+
        theme(title = element_text(size = 18),
              #axis.title.x=element_blank(),
              axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
              axis.text.y = element_text(size = 20),
              strip.text.x = element_text(size = 20),
              legend.text =  element_text(size = 15))+
      ggtitle(label = "Phyla with asexual spores (log-log")

AllFungi$Species_names[which(AllFungi$spore_length==max(AllFungi$spore_length,na.rm = T))]


                  ########################################
                  ###  MATCHING WITH TEDERSOO DATASET  ###
                  ########################################

#Tedersoo Dataset (this was sent to me by Jeff Powell sometime in 2018)
TedersooBlastMatches<-
  read.table("hits_species.blast.out.txt",header = T,stringsAsFactors = F);
  TedersooBlastMatches$species<-sub("_"," ",TedersooBlastMatches$species)
length(
  which(AllFungi$Species_names%in%TedersooBlastMatches$species))
length(unique(TedersooBlastMatches$species[
  (which(TedersooBlastMatches$species%in%AllFungi$Species_names))]))

venn.diagram(list(Tederersoo_Matches=TedersooBlastMatches$species,
                  SporeData=AllFungi$Species_names),
                  height = 3480 , 
                  width =3480 ,
                  cat.pos = c(-23, 23),
                  cex=2,
                  cat.cex= 2,
                  marging=0.2,
                  lty = 'blank',
                  filename = "Tedersoo&Spores.png",
                  fill=c("yellow","blue"))


                ########################################
                ###      OVERLAP WITH FUNGUILD       ###
                ###   AND SPORE SIZE ACROSS GUILDS   ###
                ########################################

# FunToFun_sporeInfo$species%in%FunGuildSpecies$names
# length(unique(FunGuildSpecies$names[
#   (which(FunGuildSpecies$names%in%AllFungi$Species_names))]))
# length(
#   which(!FranzKrah_sporeValues$Species_names%in%FunGuildSpecies$names))
# FranzKrah_sporeValues$Species_names[which(!FranzKrah_sporeValues$Species_names%in%FunGuildSpecies$names)]

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

#Spore size across Guilds and trophic modes
left_join(
  AllFungi[-c(5240,5607,5658,7283),],#Removing the entries for Bulgaria pura,Torula carbonaria,Gloeosporium succineum, Peziza pura. They reported as ascomycetes with teliospores, which is weird
          FunGuildData%>%
            #filter(taxonomicLevel=="Genus")%>%
            filter(taxonomicLevel=="Species")%>%
            #rename(Genus=taxon)%>%
            rename(Species_names=taxon)%>%
            select(#Genus,
                   Species_names,
                   trophicMode,guild,host,substrate,Function,Number_of_guilds,Guild_1)
            # filter(is.na(host))%>%
            # unique
          )%>%
          filter(Number_of_guilds==1)%>%
          filter(Phylum=="Ascomycota"|Phylum=="Basidiomycota")%>%
          filter(trophicMode!="Pathotroph-Saprotroph")%>%
          filter(trophicMode!="Pathotroph-Symbiotroph")%>%
          #filter(trophicMode!="Saprotroph")%>%
          ggplot()+
          aes(trophicMode,SporeArea,fill=Phylum)+
          geom_jitter(size=0.3, width = 0.3,alpha=1)+
          geom_boxplot(alpha=0.8)+#, draw_quantiles=c(0.25, 0.5, 0.75))+
          facet_grid(. ~ Phylum, scales = "free")+
          scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
          labs(y=expression("Spore size as area ("*mu*"m²)"))+
          theme(title = element_text(size = 18),
                axis.title.x=element_blank(),
                axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
                axis.text.y = element_text(size = 20),
                strip.text.x = element_text(size = 20),
                legend.position = "none")



                    ########################################
                    ###      OVERLAP WITH USDA           ###
                    ###    SPORE SIZE HABITAT RANGE      ###
                    ########################################


#Spore size  and host range
AllFungi[300:1523,]%>%
            filter(SporeName!="bulbil"&
                     SporeName!="oospore"&
                     SporeName!="papulospore")%>%
            ggplot()+
            aes(x=log10(SporeArea),y=log10(host_range),col=SporeName)+
            geom_point()

#Plot for sexual spores & analysis of the relationship
AllFungi[300:1523,]%>%
          filter(!is.na(host_range))%>%
          filter(SporeName=="ascospores"|
                  SporeName=="Basidiospores"|
                  SporeName=="zygospores"
                   )%>%
          ggplot()+
          aes(x=SporeArea,y=host_range,col=SporeName)+
          geom_point()+
           scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
           scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
          labs(y="host range (log species number)",x=expression("Spore size as log area ("*mu*"m²)"))+
          theme(title = element_text(size = 25),
            axis.text.x = element_text(size = 20,hjust = 1),
                axis.text.y = element_text(size = 20),
                strip.text.x = element_text(size = 20))+
                
          scale_size(guide = F)+
          #geom_abline(slope = 0.4600, intercept=0.3574,size=1.5,col="red")+
          #geom_abline( slope=(0.4600-0.7740),intercept=(0.3574+0.6359), size=1.5,col="green")+
          #geom_abline( slope=(0.4600-0.1119 ),intercept=(0.3574-0.5313 ), size=1.5,col="blue")+
          geom_smooth(method = "lm",formula = y~x)+
          facet_wrap(~ SporeName,scales = "free_x")+
          theme(legend.position="none")


datos1<-
AllFungi[300:1523,]%>%
  filter(!is.na(host_range))%>%
  filter(SporeName=="ascospores"|
            SporeName=="Basidiospores"|
            SporeName=="zygospores"
  )%>%
  mutate(SporeArea=log10(SporeArea),host_range=log10(host_range))
  # datos1$SporeArea[which(is.nan(datos1$SporeArea))]=NA
  # datos1$host_range[which(is.nan(datos1$host_range))]=NA
  # datos1$SporeArea[which(datos1$SporeArea==Inf)]=NA
  # datos1$host_range[which(datos1$host_range==Inf)]=NA
  #datos1$SporeArea[which(datos1$SporeArea==-Inf)]=NA
  datos1$host_range[which(datos1$host_range==-Inf)]=NA



modelForSexualSpores<-lm(host_range~SporeArea*SporeName,data = datos1)#;rm(datos1)
summary.lm(modelForSexualSpores)
summary.aov(modelForSexualSpores)

#Note: Just to have it clear, the parameter for the regression lines from this model
#correspond to the ones drawn by the function geom_smooth in the plot for sexual spores


#Plot for asexual spores
AllFungi[300:1523,]%>%
  filter(SporeName!="ascospores"&
           SporeName!="Basidiospores"&
           SporeName!="zygospores"&
         SporeName!="bulbil"&
           SporeName!="oospore"&
           SporeName!="papulospore")%>%
  ggplot()+
  aes(x=SporeArea,y=host_range,col=SporeName)+
  geom_point()+
  scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  labs(y="host range (species number)",x=expression("Spore size as log area ("*mu*"m²)"))+
  theme(title = element_text(size = 25),
        axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20))+
        
  scale_size(guide = F)+
  #geom_abline(slope = -0.9076 , intercept=3.4565,size=1.5,col="red")+
  geom_smooth(method = "lm",formula = y~x)+
  facet_wrap(~ SporeName)+
  theme(legend.position="none")


datos2<-AllFungi[300:1523,]%>%
  filter(SporeName!="ascospores"&
           SporeName!="Basidiospores"&
           SporeName!="zygospores"&
           SporeName!="bulbil"&
           SporeName!="oospore"&
           SporeName!="papulospore")%>%mutate(SporeArea=log10(SporeArea),host_range=log10(host_range))
        # datos2$SporeArea[which(is.nan(datos2$SporeArea))]=NA
        # datos2$host_range[which(is.nan(datos2$host_range))]=NA
        # datos2$SporeArea[which(datos2$SporeArea==Inf)]=NA
        # datos2$host_range[which(datos2$host_range==Inf)]=NA
        # datos2$SporeArea[which(datos2$SporeArea==-Inf)]=NA
        datos2$host_range[which(datos2$host_range==-Inf)]=NA


modelForAsexualSpores<-lm(host_range~SporeArea*SporeName,data = datos2);#rm(datos2)
summary.lm(modelForAsexualSpores)
summary.aov(modelForAsexualSpores)

                    ########################################
                    ###      OVERLAP WITH GBIF           ###
                    ###   SPORE SIZE LATITUDE-LONGITUD   ###
                    ########################################



left_join(AllFungi,
          GBIF_keys)%>%
  filter(is.na(GBIF_code))%>%
  count(Phylum)%>%
  ggplot()+
  aes(x=Phylum,y=n)+
  geom_bar(stat = "identity")

  m <- matrix(1:4, 2)
  m
  prop.table(m, 1)

#random stuff
names(step2)

rownames(OverLap_Usdr)

associations(c("Alternaria porri"),database = "both",
             clean = TRUE, syn_include = TRUE,
             spec_type = "fungus", process = TRUE)

associations(rownames(OverLap_Usdr),database = "both",
             clean = TRUE, syn_include = TRUE,
             spec_type = "fungus", process = TRUE)

data.frame(rownames(OverLap_Usdr))
#############################################################################

#Trying to use eggstractor

# Eggxtractor runs in Matlab. You only need to run the demo file,
# demo_EggShape.m, so long as the Eggxtractor folder is set as the
# working directory. Please read the comments in the code about
# modifying the threshold, and please cite our paper,
# Stoddard et al. 2017, when using the code.

#matlabR
run_matlab_code("demo_EggShape.m")

#R.matlab
readMat("demo_EggShape.m")

#Probando_MatLab_1Time!

#Probando_MatLab_1Time!


