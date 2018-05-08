#####################################################
###Analysis data from Compendium of soil fungi#######
## Part of the analysis of Tim bachelor thesis#######
#####################################################

##loading packages


#To know which fungus has the largest conidia

SporeData[grep("conidia$",SporeData$SporeName),][
          which(SporeData[grep("conidia$",SporeData$SporeName),]$SporeArea==
          max(SporeData[grep("conidia$",SporeData$SporeName),]$SporeArea,
              na.rm = TRUE)),];paste("this is the fungus with the largest conidia")


#Organizing the fungi from the biggest to the smallest, 
#showing the first 5 fungi with the biggest spores

head(SporeData[grep("conidia$",SporeData$SporeName),][
    order(SporeData[grep("conidia$",SporeData$SporeName),]$SporeArea,
          decreasing = TRUE),c(16,7:8,11:12,15)]);paste("this are the top five regarding conida size")

##Plotting some histograms for describing data variation

#histograms for colony size, measured as colony area
ggplot(data=ColonyData,
       aes(ColonyArea)) +
        geom_histogram(
            fill=I("blue"),
            col=I("black"), 
            alpha=I(.2))+
            labs(x="colony area cm^2",y="Number of species")+
            ggtitle("Colony area distribution soil fungi")

#Barplot to know how many fungi have been cultivated in different media
ggplot(data=(data.frame(table(ColonyData$Media))),aes(Var1,Freq))+
  geom_bar(stat="identity")+
  labs(x="media type",y="number of species")+
  ggtitle("Types of media used in the compendium")
       
#Determining how many species

length(unique(ColonyAndSpore$Name))

#histograms for conidia size, measured as conidia area in logarithmic scale
ggplot(data=SporeData[grep("conidia$",SporeData$SporeName),],# just for conidia type
       aes(log10(SporeArea))) +
  geom_histogram(
    fill=I("blue"),
    col=I("black"), 
    alpha=I(.2))+
  labs(x="log10 conidia area (um)",y="Number of species")+
  ggtitle("Conidia area distribution soil fungi")


length(SporeData[grep("asco",SporeData$SporeName),]$Species)

#This is just to create a temporal copy of the data. I used this for another project
write.csv(SporeData,"ConidiaFromCompendium.csv")

#Scatterplot to see the relationship between colony area and conidia area

#In normal scale
ggplot(data = ColonyAndSpore[grep("conidia$",SporeData$SporeName),])+
  geom_point(mapping = aes(x=ColonyArea,
                           y=SporeArea))+
  ggtitle("Relationship colony area and spore area")+
  labs(x="Colony area (cm^2)",y="Spore area (um^2)")

#In log-log scale
ggplot(data = ColonyAndSpore[grep("conidia$",SporeData$SporeName),])+
  geom_point(mapping = aes(x=log10(ColonyArea),
                           y=log10(SporeArea)))+
  ggtitle("Relationship colony area and spore area in log-log scale")+
  labs(x="log10 Colony area (cm^2)",y="log10 Spore area (um^2)")

#statistical analysis: relationship between colony size and spore size

modelSporeColony<-lm(log10(SporeArea)~log10(ColonyArea),data = ColonyAndSpore[grep("conidia$",SporeData$SporeName),])

plot(modelSporeColony)

shapiro.test(residuals(modelSporeColony))

install.packages(Hmsic)

library(Hmisc)

temp<-data.frame(
  SporeArea=ColonyAndSpore[grep("conidia$",SporeData$SporeName),]$SporeArea,
  ColonyArea=ColonyAndSpore[grep("conidia$",SporeData$SporeName),]$ColonyArea)


rcorr(as.matrix(temp),type = "spearman")

cor(temp,method = "spearman",use="complete.obs")
cor.test(temp$SporeArea,temp$ColonyArea,method = "spearman",na.action="na.exclude")



#################################################################
######Determining if spore size is conserved in the phylogeny####
#################################################################

#loading packages
library(picante)

#identifying and removing which fungi have NA in the conidia entry. For most cases
#this actually refers to the lack of conidia data from the book, but in few it is just
#that the data was not entried. Thus it needs to be updated!

ColonyAndSporeTemp<-ColonyAndSpore[!is.na(ColonyAndSpore$SporeArea),c(1,3,4)]
                    ColonyAndSporeTemp<-ColonyAndSporeTemp[
                    ColonyAndSporeTemp$SporeName=="conidia",]

#creating a list with the names of species that produce conidia
write.csv(ColonyAndSporeTemp[ColonyAndSporeTemp$SporeName=="conidia",]$Name,
          "FungalNames.csv")


#load the tree
tree<-read.tree("phyliptree_expanded.phy")

tree$tip.label

TreeLabel<-tree$tip.label
          TreeLabel<-data.frame(TreeNames=TreeLabel)
          TreeLabel$TreeNames<-as.character(TreeLabel$TreeNames)

#Making column where with names of the species exactly mathcing
# the format of  names of the tree

treeSynonyms<-read.csv("TreenamesSynonyims.csv",header = TRUE,
                       stringsAsFactors = FALSE)
          
ColonyAndSporeTemp$Name<-sub("\\s$","",ColonyAndSporeTemp$Name)

                
                

        
ColonyAndSporeTemp$TreeNames<-ColonyAndSporeTemp[,1]

ColonyAndSporeTemp<-left_join(ColonyAndSporeTemp,treeSynonyms)

ColonyAndSporeTemp[!is.na(ColonyAndSporeTemp$NewTreeName),4]<-
  ColonyAndSporeTemp[!is.na(ColonyAndSporeTemp$NewTreeName),5]

          ColonyAndSporeTemp$TreeNames<-sub("\\s","",ColonyAndSporeTemp$TreeNames)
          ColonyAndSporeTemp$TreeNames<-sub("\\s","",ColonyAndSporeTemp$TreeNames)
          ColonyAndSporeTemp$TreeNames<-sub("\\s","",ColonyAndSporeTemp$TreeNames)
          
          ColonyAndSporeTemp$TreeNames<-paste("'",ColonyAndSporeTemp$TreeNames,sep="")
          ColonyAndSporeTemp$TreeNames<-paste(ColonyAndSporeTemp$TreeNames,"'",sep="")
          ColonyAndSporeTemp[which(ColonyAndSporeTemp$TreeNames=="'Clonostachysroseaf.Rosea'"),
                             4]<-"'Clonostachysroseaf.rosea'"

          
          
          
          ColonyAndSporeTemp$TreeNames=="'Ochroconistshawytschae'"
          
          TraitsForTree$TreeNames
          
          
          ColonyAndSporeTemp$TreeNames%in%TreeLabel$TreeNames
          
#Creating a new table exclusively with the trait info from the tree I created
TraitsForTree<-left_join(TreeLabel,
                         ColonyAndSporeTemp[ColonyAndSporeTemp$SporeName=="conidia",c(3,4)])
length(TreeLabel$TreeNames)
length(ColonyAndSporeTemp$TreeNames)

# TreeLabel[-which(TreeLabel$TreeNames%in%ColonyAndSporeTemp$TreeNames),]
# 
# write.csv(TreeLabel[-which(TreeLabel$TreeNames%in%ColonyAndSporeTemp$TreeNames),],
#           "treenameproblems.csv")
# 
# write.csv(ColonyAndSporeTemp$TreeNames,"treenamesolutions.csv")

#Creating a vector that have the traits from species inlcuded in the tree with
#the rownames being the species names

ConidiaS<-TraitsForTree[,2];
          names(ConidiaS)<-TraitsForTree[,1]    


#Doing the phylogenetic independent contrasts and the estimating the p values of those
#contrasts
is.binary.tree(tree)



tree2<-multi2di(tree)
is.binary.tree(tree2)

plot(tree2,font = 1,cex=0.5,edge.width = 1)

tree2$tip.label

#Statistical analsis
pic(ConidiaS,tree2)

phylosignal(ConidiaS,tree2)

length(TraitsForTree$TreeNames)
length(TreeLabel$TreeNames)

?multi2di


####

x <- c(1:9, 8:1)
y <- c(1, 2*(5:3), 2, -1, 17, 9, 8, 2:9)
op <- par(mfcol = c(3, 1))
for(xpd in c(FALSE, TRUE, NA)) {
  plot(1:10, main = paste("xpd =", xpd))
  box("figure", col = "pink", lwd = 3)
  polygon(x, y, xpd = xpd, col = "orange", lty = 2, lwd = 2, border = "red")
}
par(op)

n <- 100
xx <- c(0:n, n:0)
yy <- c(c(0, cumsum(stats::rnorm(n))), rev(c(0, cumsum(stats::rnorm(n)))))
plot   (xx, yy, type = "n", xlab = "Time", ylab = "Distance")
polygon(xx, yy, col = "gray", border = "red")
title("Distance Between Brownian Motions")



####

#Just looking the shared species between Anika´s dataset for soil aggregation
# and my data

SoilAggr<-read.csv("AnikaSpeciesList_SoilAggr.csv",header = T,sep = ",",
                   stringsAsFactors = F);
          SoilAggr<-SoilAggr[1:8]

entries<-
  which(
SoilAggr[SoilAggr$phylum=="Ascomycota"|SoilAggr$phylum=="Basidiomycota",]$organism%in%
sub("\\s$","",ColonyAndSpore$Name))

SoilAggr[SoilAggr$phylum=="Ascomycota"|SoilAggr$phylum=="Basidiomycota",]$organism[-entries]
