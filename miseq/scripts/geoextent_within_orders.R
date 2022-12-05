### load libraries
library(tidyverse)
library(lme4)
library(spatstat)
library(geosphere)


### download data
# miseq data -- this download is required
if(!file.exists('miseq/rawdata')) dir.create('miseq/rawdata')
options(timeout = max(300, getOption("timeout")))
if(!file.exists('miseq/rawdata/df_allSamples.txt')) {
  download.file('https://cloudstor.aarnet.edu.au/plus/s/MKb7lgaK3cY8SPK/download',
                'miseq/rawdata/df_allSamples.txt')
}
# taxonomic data -- this download is required
if(!file.exists('output/taxonomy_used.RDS')) {
  download.file('https://cloudstor.aarnet.edu.au/plus/s/1qFDLib8XHuFRd1/download',
                'output/taxonomy_used.RDS')
}
# environmental data -- this download is required
if(!file.exists('miseq/rawdata/df_env_fine.csv')) {
  download.file('https://cloudstor.aarnet.edu.au/plus/s/AFpqSzi0GX4H7rA/download',
                'miseq/rawdata/df_env_fine.csv')
}
# range areas -- can either download or create using 'miseq/scripts/dataPrep_rangeSize_primers.R' (see below)
if(!file.exists('miseq/output')) dir.create('miseq/output')
if(!file.exists('miseq/output/workspace')) dir.create('miseq/output/workspace')
if(!file.exists('miseq/output/workspace/alphahull_terrestrial_primers.RData')) {
  download.file('https://cloudstor.aarnet.edu.au/plus/s/rfYvzEALwQmcZD2/download',
                'miseq/output/workspace/alphahull_terrestrial_primers.RData')
}


### read functions, prep spore data
source('MatchingSpore_FunctionData.R')
source('miseq/scripts/helperFunctions.R')


### load/prep data
geo <- read_tsv('miseq/rawdata/df_allSamples.txt')
env <- read_csv('miseq/rawdata/df_env_fine.csv')
trait <- To_Analysis
tax <- readRDS('output/taxonomy_used.RDS')



### manipulate and join dataframes
# calculate median spore length values per species 
trait.sum <- summarise_trait() %>% 
  dplyr::select(Species, phylum, class, order, Life_style, simpleFunct, n.sporetypes, 
                starts_with('log10.sporeVolume_'), 
                starts_with('log10.sporeAspectRatio_')) %>% 
  group_by(Species) %>% 
  slice(1) %>% 
  as.data.frame
# check species names in geo
# spp <- unique(geo$Species); spp <- spp[!spp %in% grep('sp.$', spp, value=TRUE)]
geo <- update_tax()
# join observations and traits
dat <- left_join(geo, trait.sum)


# subset of primer pairs to keep (first line used in analysis, uncomment second line to keep all)
keep.primers <- c('fITS7/ITS4', 'ITS1F/ITS2', 'ITS1F/ITS4', 'ITS3/ITS4')
# keep.primers <- unique(dat$Primers.name)

# calculating geographic extent by maximum geometric distance (in metres)
temp <- dat %>% 
  filter(phylum %in% c('Glomeromycota', 'Zygomycetous fungi', 'Basidiomycota', 'Ascomycota') & 
           # Sample.type %in% c('soil', 'topsoil') & 
           n.sporetypes == 1 & 
           # count/Sequences.extracted > 0.0001 &
           Primers.name %in% keep.primers & 
           simpleFunct %in% grep('Sapro|Plant', simpleFunct, value=TRUE))
geo.extent <- sapply(split(temp, interaction(temp$Species, temp$Primers.name, sep='__', drop=TRUE)), function(x){  # at least 3 samples
  x <- x %>% dplyr::select(longitude, latitude) %>% distinct
  if(nrow(x) <= 2) res <- NA
  # if(nrow(x) > 2) res <- median(distHaversine(x[, c('longitude', 'latitude')]))  # for median extent
  # if(nrow(x) > 2) res <- max(distHaversine(x[, c('longitude', 'latitude')]))  # for maximum extent
  if(nrow(x) > 2) res <- max(distVincentyEllipsoid(x[, c('longitude', 'latitude')]))  # for maximum extent
  # if(!is.na(res)) if(res <= 0.1) res <- NA
  return(res)
})

# create/load objects containing geographic area estimates
# source('miseq/scripts/dataPrep_rangeSize_primers.R')
load('miseq/output/workspace/alphahull_terrestrial_primers.RData')
hist(log10(geo.extent))
hist(log10(geo.area))

# prepare data.frames
geo.extent <- data.frame(Species=names(geo.extent), geo_extent=as.numeric(geo.extent)) %>% 
  separate(Species, c('Species', 'Primers.name'), sep='__')
geo.area <- data.frame(Species=names(geo.area), geo_area=as.numeric(geo.area)) %>% 
  separate(Species, c('Species', 'Primers.name'), sep='__') %>% 
  mutate(Species = gsub('_', ' ', Species), 
         Primers.name = gsub('_', '/', Primers.name))
geo.extent <- full_join(geo.extent, geo.area)
temp <- left_join(geo.extent, trait.sum) %>% 
  filter(phylum %in% c('Glomeromycota', 'Zygomycetous fungi', 'Basidiomycota', 'Ascomycota') & 
           n.sporetypes == 1 & 
           Primers.name %in% keep.primers & 
           simpleFunct %in% grep('Sapro|Plant', simpleFunct, value=TRUE)) %>% 
  pivot_longer(cols=starts_with('log10'), names_to='res', values_to='trait_value') %>% 
  filter(!is.na(trait_value)) %>% 
  mutate(trait = str_replace(res, '_.+$', ''),
         spore_type = str_replace(res, '^.+_', '')#,
         # phylum_sporeType = paste(phylum, spore_type, sep='_'),
         # order_sporeType = paste(order, spore_type, sep='_'),
         # phylum_funct = paste(phylum, simpleFunct, sep='_'),
         # funct_sporeType = paste(simpleFunct, spore_type, sep='_'),
         # phylum_funct_sporeType = paste(phylum, simpleFunct, spore_type, sep='_'),
         # group_size = ifelse(phylum_sporeType %in% c('Glomeromycota_Azygospores',
         #                                             'Zygomycota_Zygospores',
         #                                             'Zygomycota_Chlamydospores'),
         #                     '>100um', '<100um')
         ) %>%
  dplyr::select(-res) %>%
  spread(key='trait', value='trait_value') #%>% 
  # filter(spore_type != 'teliospores') %>%
  # filter(phylum_sporeType != 'Zygomycota_azygospores') %>%
  # mutate(phylum_sporeType.ord.sv = fct_reorder(phylum_sporeType, log10.sporeVolume, mean),
  #        phylum_sporeType.ord.sar = fct_reorder(phylum_sporeType, log10.sporeAspectRatio, mean),
  #        funct_sporeType.ord.sv = fct_reorder(funct_sporeType, log10.sporeVolume, mean),
  #        funct_sporeType.ord.sar = fct_reorder(funct_sporeType, log10.sporeAspectRatio, mean))


### accounting for host-association
temp <- temp %>%
  filter(!phylum %in% c(NA, 'Not assigned') &
           # !class %in% c(NA, 'Not assigned') &
           !order %in% c(NA, 'Not assigned')) %>% 
  mutate(order = gsub('^ ', '', order)) %>% 
  group_by(order, Primers.name) %>% 
  mutate(n_spp = n(), 
         log10.sporeVolume.mean = mean(log10.sporeVolume)) %>% 
  ungroup() %>% 
  filter(n_spp > 2) %>% 
  mutate(host.assoc = case_when(simpleFunct %in% grep('Sapro', simpleFunct, value=TRUE) ~ 'no', 
                                simpleFunct %in% grep('Plant', simpleFunct, value=TRUE) ~ 'yes'), 
         host.assoc_order = paste(host.assoc, order, sep='_'), 
         log10.sporeVolume.cent = log10.sporeVolume - mean(log10.sporeVolume), 
         geo_area_prop = geo_area / ((sqrt(148326000) * 1000)^2),
         geo_extent_prop = geo_extent / 20000000, 
         # phylum.grp = case_when(phylum %in% c('Glomeromycota', 'Zygomycota') ~ 'Glom_Zygo', 
         #                        TRUE ~ phylum),
         order.ord.sv = fct_reorder(order, log10.sporeVolume, mean),
         order.ord.sv = ordered(order.ord.sv, levels=levels(order.ord.sv))) %>% 
  filter(!is.na(host.assoc)) %>% 
  droplevels()

# temp %>% 
#   select(Species, spore_type, Primers.name, phylum:simpleFunct, host.assoc, log10.sporeVolume, log10.sporeAspectRatio, 
#          geo_extent, geo_extent_prop, geo_area, geo_area_prop) %>% 
#   rename(geo_extent_m = geo_extent, geo_area_m = geo_area) %>% 
#   write_csv('derivedData/df_species_byPrimerSet.csv')


##### analysis of extent and area within order and host association

### area (table S7)

grps <- temp %>% 
  filter(!is.na(log10.sporeVolume.cent), !is.na(geo_area_prop)) %>% 
  group_by(order, host.assoc) %>% 
  summarise(n_species = length(unique(Species)), 
            n_ext = sum(!is.na(geo_extent_prop)), 
            n_area = sum(!is.na(geo_area_prop))) %>% 
  filter(n_species >= 5, n_area >= 5) %>% 
  mutate(ord_host = paste(order, host.assoc, sep='_'))

temp1 <- temp %>% 
  group_by(order, host.assoc) %>% 
  mutate(ord_host = paste(order, host.assoc, sep='_')) %>% 
  ungroup() %>% 
  filter(ord_host %in% grps$ord_host, 
         !is.na(geo_area_prop))
temp1 <- split(temp1, temp1$ord_host)

l <- lapply(temp1, function(x){
  if(length(unique(x$Primers.name) > 1) & length(unique(x$Primers.name)) < nrow(x)){
    lmer(car::logit(geo_area_prop) ~ log10.sporeVolume.cent + (1|Primers.name), data=x)} else {
      lm(car::logit(geo_area_prop) ~ log10.sporeVolume.cent, data=x)} 
})
ll <- sapply(l, function(x){
  if(class(x) == 'lmerMod'){
    c(est = summary(x)[['coefficients']]['log10.sporeVolume.cent', 'Estimate'], 
      se = summary(x)[['coefficients']]['log10.sporeVolume.cent', 'Std. Error'], 
      p = car::Anova(x, test='F')[['Pr(>F)']])} else {
        c(est = summary(x)[['coefficients']]['log10.sporeVolume.cent', 'Estimate'], 
          se = summary(x)[['coefficients']]['log10.sporeVolume.cent', 'Std. Error'],
          p = summary(x)[['coefficients']]['log10.sporeVolume.cent', 'Pr(>|t|)'])}
})

out_a <- right_join(grps, t(ll) %>% as.data.frame %>% 
                    rownames_to_column('order') %>% 
                    separate(order, c('order', 'host.assoc')) %>% 
                    arrange(host.assoc)) %>% 
  select(-n_ext, -n_area, -ord_host) %>% 
  mutate(host.assoc = recode(host.assoc, no='Free-living', yes='Symbiotic'), 
         across(c(est, se), ~round(., 2)), 
         across(c(p), ~round(., 3))) %>% 
  arrange(host.assoc) %>% 
  relocate(host.assoc) %>% 
  rename(`Taxonomic order` = order, Lifestyle = host.assoc, `# species` = n_species, 
         Estimate = est, `Standard error` = se, `P-value` = p)
kableExtra::kable(out_a, 'simple')


### extent (table S8)

grps <- temp %>% 
  filter(!is.na(log10.sporeVolume.cent), !is.na(geo_extent_prop)) %>% 
  group_by(order, host.assoc) %>% 
  summarise(n_species = length(unique(Species)), 
            n_ext = sum(!is.na(geo_extent_prop)), 
            n_area = sum(!is.na(geo_area_prop))) %>% 
  filter(n_species >= 5, n_ext >= 5) %>% 
  mutate(ord_host = paste(order, host.assoc, sep='_'))

temp1 <- temp %>% 
  group_by(order, host.assoc) %>% 
  mutate(ord_host = paste(order, host.assoc, sep='_')) %>% 
  ungroup() %>% 
  filter(ord_host %in% grps$ord_host, 
         !is.na(geo_extent_prop))
temp1 <- split(temp1, temp1$ord_host)

l <- lapply(temp1, function(x){
  if(length(unique(x$Primers.name) > 1) & length(unique(x$Primers.name)) < nrow(x)){
    lmer(car::logit(geo_extent_prop) ~ log10.sporeVolume.cent + (1|Primers.name), data=x)} else {
      lm(car::logit(geo_extent_prop) ~ log10.sporeVolume.cent, data=x)} 
})
ll <- sapply(l, function(x){
  if(class(x) == 'lmerMod'){
    c(est = summary(x)[['coefficients']]['log10.sporeVolume.cent', 'Estimate'], 
      se = summary(x)[['coefficients']]['log10.sporeVolume.cent', 'Std. Error'], 
      p = car::Anova(x, test='F')[['Pr(>F)']])} else {
        c(est = summary(x)[['coefficients']]['log10.sporeVolume.cent', 'Estimate'], 
          se = summary(x)[['coefficients']]['log10.sporeVolume.cent', 'Std. Error'],
          p = summary(x)[['coefficients']]['log10.sporeVolume.cent', 'Pr(>|t|)'])}
})

out_e <- right_join(grps, t(ll) %>% as.data.frame %>% 
                    rownames_to_column('order') %>% 
                    separate(order, c('order', 'host.assoc')) %>% 
                    arrange(host.assoc)) %>% 
  select(-n_ext, -n_area, -ord_host) %>% 
  mutate(host.assoc = recode(host.assoc, no='Free-living', yes='Symbiotic'), 
         across(c(est, se), ~round(., 2)), 
         across(c(p), ~round(., 3))) %>% 
  arrange(host.assoc) %>% 
  relocate(host.assoc) %>% 
  rename(`Taxonomic order` = order, Lifestyle = host.assoc, `# species` = n_species, 
         Estimate = est, `Standard error` = se, `P-value` = p)
kableExtra::kable(out_e, 'simple')


