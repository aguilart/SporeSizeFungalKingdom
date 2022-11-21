
# summarise trait data by species and spore type
summarise_trait <- function(){
  
  # load libraries
  library(tidyverse)

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

# update taxonomic information in miseq data with those from CoL (to match trait data)
update_tax <- function(){
  
  # those annotated with accepted names
  temp <- tax %>% 
    filter(status_base_name %in% c('accepted', 'accepted name')) %>% 
    group_by(names_to_use) %>% 
    slice(1) %>% 
    ungroup()
  geo1 <- geo %>% 
    filter(Species %in% temp$names_to_use) %>% 
    left_join(temp %>% 
                select(names_to_use, genus, order) %>% 
                rename(Species = names_to_use)) %>% 
    select(-Class:-Genus) %>% 
    rename(Genus = genus, 
           Order = order) %>% 
    filter(Order != 'Not assigned')  # due to missing taxonomic info on mycobank
  
  # those annotated with synonyms
  temp <- tax %>% 
    filter(!status_base_name %in% c('accepted', 'accepted name')) %>% 
    filter(base_name != names_to_use)
  # some base_names have multiple accepted names, next lines gets those that match in geo
  # x <- sapply(split(temp, temp$base_name), nrow); x <- names(x[x>1])
  # temp <- temp %>% filter(base_name %in% x)
  # cat(sort(unique(filter(geo, Species %in% temp$base_name)$Species)), file='output/ambiguous_synonyms.tsv', sep='\n')
  # remove these rows
  x <- sapply(split(temp, temp$base_name), nrow); x <- names(x[x==1])
  temp <- temp %>% filter(base_name %in% x)
  geo2 <- geo %>% filter(Species %in% temp$base_name) %>% 
    left_join(temp %>% 
                select(base_name, names_to_use, genus, order) %>% 
                rename(Species = base_name)) %>% 
    select(-Class:-Species) %>% 
    rename(Species = names_to_use, 
           Genus = genus, 
           Order = order) %>% 
    filter(Order != 'Not assigned')  # due to missing taxonomic info on mycobank
  
  # bind dataframes
  geo <- bind_rows(geo1, geo2)
  
  # return data
  return(geo)

}

# calculate species associations with environmental variables
get_niche <- function(){
  
  # prepare environmental data
  env %>% 
    rename(mat = bio01, 
           map = bio12, 
           seasonality_t = bio04, 
           seasonality_p = bio15) %>% 
    select(database_id, alt, mat, map, seasonality_t, seasonality_p, maxsrad, minvapr) %>% 
    # join with miseq data
    right_join(geo %>% select(database_id, Species, Primers.name)) %>% 
    # remove sites with no environmental data
    filter(complete.cases(.)) %>% 
    # calculate frequency of observations and summary statistics of environmental data for each species
    group_by(Species) %>% 
    mutate(n_samples = n()) %>% 
    summarise_at(vars(n_samples, alt:minvapr), list(mean=mean, sd=sd)) %>% 
    rename(n_samples_env = n_samples_mean) %>% 
    select(-n_samples_sd) -> out  # %>% 
    # # join with taxonomic, guild and trait data
    # left_join(trait %>% 
    #             rename(Species = names_to_use) %>% 
    #             mutate(log10.sporeVolume = log10((spore_width^2)*spore_length*(pi/6)), 
    #                    log10.sporeAspectRatio = log10(spore_length/spore_width)) %>% 
    #             group_by(Species, SporeType, phylum, class, order, Life_style, simpleFunct) %>% 
    #             summarise_if(is.numeric, median, na.rm=T) %>% 
    #             select(Species:simpleFunct, log10.sporeVolume, log10.sporeAspectRatio) %>%
    #             as.data.frame) %>% 
    # # remove species with multiple spore types, missing trait data
    # filter(!Species %in% Species[duplicated(Species)], !is.na(log10.sporeVolume)) -> out
  
  # return data
  return(out)
  
}
