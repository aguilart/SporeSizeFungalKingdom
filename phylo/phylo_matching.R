library(tidyverse)
library(ape)
library(ggtree)
library(taxonlookup)
library(ggstance)

tt<-read.nexus("phylo/Lutzoni_fungi_timetree.nex")
vcapply<-taxonlookup:::vcapply

split<-function(str){
  str_split <- strsplit(str, "[_ ]+")
  vcapply(str_split, "[[", 2L)
}

nnn<-tt$tip.label
fungal_genera<-split(nnn)[1:197]
write_csv(tibble(genus=fungal_genera),"phylo/genus_names.csv")


orders<-read_csv("phylo/orders_phylo.csv")
orders %>% group_by(order) %>%
  summarize(genus=genus[1]) -> only_one_genera_per_order

tt$tip.label<-split(nnn)


sdf<-read_csv("output/Spore_Database_Fungi.csv")
dropped.tree<-drop.tip(tt,tt$tip.label[which(!tt$tip.label %in% only_one_genera_per_order$genus)])
dup.tips<-names(which(table(dropped.tree$tip.label)>1))


tips<-dropped.tree$tip.label 
dropped.tree2<-drop.tip(dropped.tree,which(duplicated(tips)))
dropped.tree2$tip.label<-orders$order[match(dropped.tree2$tip.label,orders$genus)]

pdf("test.pdf",height=11)
plot(dropped.tree2)
dev.off()





matched_data<-filter(sdf,order%in%dropped.tree2$tip.label)
matched_data$log.length<-log10(matched_data$spore_length)
matched_data$log_spore_area<-log10(matched_data$spore_length*matched_data$spore_width * pi /4)
matched_data$width_divided_by_length<-matched_data$spore_width/matched_data$spore_length


dropped.tree2$tip.label %in% matched_data$order

matched_data$id <- matched_data$order
matched_data<-filter(matched_data,order!="Not assigned")
dropped.tree2<-drop.tip(dropped.tree2,"Not assigned")
match2<-filter(matched_data,!SporeName %in% c("bulbil","papulospore"))
temp<-select(match2,id,log_spore_area,SporeName,width_divided_by_length)



#tree2 <- groupClade(dropped.tree2) #c(83, 83))
p <- ggtree(dropped.tree2)+ geom_tiplab(size = 1.2)+ theme_tree2()

# p <- ggtree(phy.o.dt) + theme_tree2() + geom_tiplab(size = 1.2)
p1 <- facet_plot(
  p,
  panel = "log_spore_area",
  data = temp,
  geom = geom_point,
  mapping = aes(x = log_spore_area,col=SporeName),
  alpha=0.1
  #colour = guide_legend(override.aes = list(alpha=1)))
 # outlier.size = 0.1
)
p2<-facet_plot(p1,
               panel = "width_divided_by_length",
               data = temp,
               geom = geom_point,
               mapping = aes(x = width_divided_by_length,col=SporeName),
               alpha=0.1)
               
               
               p2
ggsave("phylo_box.pdf", width = 8.5)

tr <- rtree(10)
dd = data.frame(id=tr$tip.label, value=abs(rnorm(10)))
p <- ggtree(tr)
facet_plot(p, 'Trait', data = dd, geom=geom_point, mapping=aes(x=value))


