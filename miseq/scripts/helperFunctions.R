
# summarise trait data by species and spore type
summarise_trait <- function(){
  
  # load libraries
  library(tidyverse)
  library(Hmisc)
  
  # calculate median spore length values per species 
  trait.sum <- trait %>% 
    rename(Species = names_to_use) %>% 
    mutate(log10.sporeLength = log10(spore_length),
           log10.sporeWidth = log10(spore_width), 
           log10.sporeArea = log10(spore_width*spore_length*(pi/4)), 
           log10.sporeVolume = log10((spore_width^2)*spore_length*(pi/6)), 
           log10.sporeAspectRatio = log10(spore_length/spore_width)) %>% 
    group_by(Species, phylum, class, order, family, genus, Life_style, simpleFunct) %>% 
    summarise_if(is.numeric, median, na.rm=T) %>% 
    as.data.frame
  
  # number of spore types
  trait.sum.t <- trait %>% 
    rename(Species = names_to_use, SporeName = SporeType) %>% 
    group_by(Species, phylum) %>% 
    summarise(n.sporetypes = length(unique(SporeName))) %>% 
    as.data.frame
  trait.sum <- left_join(trait.sum, trait.sum.t); rm(trait.sum.t)
  
  # group by spore type, then create new columns for each type
  trait.sum.t <- trait %>%
    rename(Species = names_to_use, SporeName = SporeType) %>% 
    mutate(log10.sporeLength = log10(spore_length),
           log10.sporeWidth = log10(spore_width),
           log10.sporeArea = log10(spore_width*spore_length*(pi/4)),
           log10.sporeVolume = log10((spore_width^2)*spore_length*(pi/6)), 
           log10.sporeAspectRatio = log10(spore_length/spore_width)) %>%
    group_by(Species, phylum, SporeName) %>%
    summarise_if(is.numeric, median, na.rm=T) %>% 
    as.data.frame
  temp <- split(trait.sum.t, trait.sum.t$SporeName)
  temp <- temp[names(which(sapply(temp, nrow) > 10))]
  temp <- lapply(temp, function(x) {
    x %>% rename_if(is.numeric, .funs = ~paste0(., '_', unique(x$SporeName))) %>% 
      dplyr::select(-SporeName)
  })
  for(i in temp) trait.sum <- left_join(trait.sum, i)
  
  # return data
  return(trait.sum)
  
}
