library(tidyverse)
library(ape)
library(ggtree)

devtools::install_github("ropenscilabs/datastorr")
devtools::install_github("wcornwell/taxonlookup")

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
matched_data$log_spore_area<-log10(matched_data$spore_length*matched_data$spore_width * pi /4)
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
temp<-select(match2,id,log_spore_area,SporeName,length_divided_by_width)


#tree for panel 1
p <- ggtree(dropped.tree2)+ geom_tiplab(size = 2)+ theme_tree2()

tree3 <- groupClade(dropped.tree2, .node=3)

ggtree(dropped.tree2,layout="equal_angle",size=1)+ geom_tiplab(size = 3)+ theme_tree2()

ggtree(dropped.tree2,layout="equal_angle",size=2)+ geom_tiplab(size = 3)+ theme_tree2()
  #geom_cladelabel(node=17, label="Some random clade", color="red")


# spore size for panel 2
p1 <- facet_plot(
  p,
  panel = "Spore area (log)",
  data = temp,
  geom = geom_point,
  mapping = aes(x = log_spore_area,col=SporeName),
  alpha=0.1,size=2
  #colour = guide_legend(override.aes = list(alpha=1)),
  #outlier.size = 0.1
)+
  #scale_color_brewer(palette="Set1")
  scale_color_manual(values = rainbow(8))

#spore shape for panel 3
p2<-facet_plot(p1,
               panel = "Q ratio (length/width)",
               data = temp,
               geom = geom_point,
               mapping = aes(x = length_divided_by_width,col=SporeName),
               alpha=0.1, size=2)+
  theme(title = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20,angle = 45,hjust = 1),
        #axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.text =  element_text(size = 15))
               
               
p2
ggsave("phylo_spore_point.pdf", width = 8.5)

