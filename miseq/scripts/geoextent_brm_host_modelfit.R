### load libraries
library(tidyverse)
library(scales)
library(spatstat)
library(geosphere)
library(colorspace)
library(patchwork)
library(brms)
library(ggridges)


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

# estimating environmental envelopes of species (for use)
nch <- get_niche()
# write_csv(nch, 'miseq/output/df_species_noPrimers.csv')

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


##### analysis of extent

ggplot(temp, aes(x=log10.sporeVolume.cent, y=car::logit(geo_extent_prop), colour=host.assoc)) + 
  geom_point(alpha=0.5, shape=16) + 
  # stat_smooth(method='lm', formula=y ~ poly(x, 2), alpha=0.5) +
  stat_smooth(method='lm', alpha=0.5) +
  facet_grid(cols=vars(Primers.name))

# check whether linear or nonlinear model is better
m1 <- lme4::lmer(car::logit(geo_extent_prop) ~ log10.sporeVolume.cent * host.assoc +
                   (1+log10.sporeVolume.cent|order) + (1|Primers.name), data=temp)
summary(m1)
car::Anova(m1)
car::qqPlot(resid(m1))
plot(m1)
m2 <- lme4::lmer(car::logit(geo_extent_prop) ~ poly(log10.sporeVolume.cent, 2) * host.assoc +
                   (1+log10.sporeVolume.cent|order) + (1|Primers.name), data=temp)
anova(m1, m2)
rm(m1, m2)

# fit model with random slope
fit1.ext <- brm(formula = car::logit(geo_extent_prop) ~ log10.sporeVolume.cent * host.assoc + 
              (1+log10.sporeVolume.cent|order) + (1|Primers.name),
            data=temp,
            family = gaussian(),
            prior = c(set_prior("normal(0,5)", class = "b"),
                      set_prior("student_t(10,0,1)", class = "sd"),
                      set_prior("lkj(2)", class = "cor")),
            warmup = 1000, iter = 2000, chains = 4,
            control = list(adapt_delta = 0.995))
fit1.ext.loo <- loo(fit1.ext)

# fit model without random slope
fit2.ext <- update(fit1.ext, formula. = ~ . - (1+log10.sporeVolume.cent|order) + (1|order))
fit2.ext.loo <- loo(fit2.ext)
loo_compare(fit1.ext.loo, fit2.ext.loo)

# fit model with random slope by order and host association
fit3.ext <- update(fit1.ext, formula. = ~ . - (1+log10.sporeVolume.cent|order) + (1+log10.sporeVolume.cent|host.assoc_order), newdata=temp)
fit3.ext.loo <- loo(fit3.ext)
fit4.ext <- update(fit3.ext, formula. = ~ . - (1+log10.sporeVolume.cent|host.assoc_order) + (1|host.assoc_order))
fit4.ext.loo <- loo(fit4.ext)
loo_compare(fit1.ext.loo, fit2.ext.loo, fit3.ext.loo, fit4.ext.loo)

# model summary
summary(fit1.ext, prob=0.95)

# population-level effects
plot(fit1.ext, pars = "^b_")

# extent greater for host.assoc == 'yes'
hypothesis(fit1.ext, "host.assocyes > 0", class = "b")

# slope greater for host.assoc == 'yes'
hypothesis(fit1.ext, "log10.sporeVolume.cent:host.assocyes + log10.sporeVolume.cent > log10.sporeVolume.cent", class = "b")

# negative slope for host.assoc == 'no'
hypothesis(fit1.ext, "log10.sporeVolume.cent < 0", class = "b")

# negative slope for host.assoc == 'yes'
hypothesis(fit1.ext, "log10.sporeVolume.cent:host.assocyes + log10.sporeVolume.cent < 0", class = "b")





##### analysis of area

ggplot(temp, aes(x=log10.sporeVolume.cent, y=car::logit(geo_area_prop), colour=host.assoc)) + 
  geom_point(alpha=0.5, shape=16) + 
  # stat_smooth(method='lm', formula=y ~ poly(x, 2), alpha=0.5) +
  stat_smooth(method='lm', alpha=0.5) +
  facet_grid(cols=vars(Primers.name))

# check whether linear or nonlinear model is better
m1 <- lme4::lmer(car::logit(geo_area_prop) ~ log10.sporeVolume.cent * host.assoc +
                   (1+log10.sporeVolume.cent|order) + (1|Primers.name), data=temp)
summary(m1)
car::Anova(m1)
car::qqPlot(resid(m1))
plot(m1)
m2 <- lme4::lmer(car::logit(geo_area_prop) ~ poly(log10.sporeVolume.cent, 2) * host.assoc +
                   (1+log10.sporeVolume.cent|order) + (1|Primers.name), data=temp)
anova(m1, m2)
rm(m1, m2)

# fit model with random slope
fit1.area <- brm(formula = car::logit(geo_area_prop) ~ log10.sporeVolume.cent * host.assoc + 
                  (1+log10.sporeVolume.cent|order) + (1|Primers.name),
                data=temp,
                family = gaussian(),
                prior = c(set_prior("normal(0,5)", class = "b"),
                          set_prior("student_t(10,0,1)", class = "sd"),
                          set_prior("lkj(2)", class = "cor")),
                warmup = 1000, iter = 2000, chains = 4,
                control = list(adapt_delta = 0.995))
fit1.area.loo <- loo(fit1.area)

# fit model without random slope
fit2.area <- update(fit1.area, formula. = ~ . - (1+log10.sporeVolume.cent|order) + (1|order))
fit2.area.loo <- loo(fit2.area)
loo_compare(fit1.area.loo, fit2.area.loo)

# fit model with random slope by order and host association
fit3.area <- update(fit1.area, formula. = ~ . - (1+log10.sporeVolume.cent|order) + (1+log10.sporeVolume.cent|host.assoc_order), newdata=temp)
fit3.area.loo <- loo(fit3.area)
fit4.area <- update(fit3.area, formula. = ~ . - (1+log10.sporeVolume.cent|host.assoc_order) + (1|host.assoc_order))
fit4.area.loo <- loo(fit4.area)
loo_compare(fit1.area.loo, fit2.area.loo, fit3.area.loo, fit4.area.loo)

# model summary
summary(fit1.area, prob=0.95)

# population-level effects
plot(fit1.area, pars = "^b_")

# area greater for host.assoc == 'yes'
hypothesis(fit1.area, "host.assocyes > 0", class = "b")

# slope greater for host.assoc == 'yes'
hypothesis(fit1.area, "log10.sporeVolume.cent:host.assocyes + log10.sporeVolume.cent > log10.sporeVolume.cent", class = "b")

# negative slope for host.assoc == 'no'
hypothesis(fit1.area, "log10.sporeVolume.cent < 0", class = "b")

# negative slope for host.assoc == 'yes'
hypothesis(fit1.area, "log10.sporeVolume.cent:host.assocyes + log10.sporeVolume.cent < 0", class = "b")




### save image
# save(list=c('temp', 'geo.extent', ls()[grep('^fit', ls())]), file='miseq/output/workspace/geoextent_brm_host.RData')
