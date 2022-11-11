### load libraries
library(tidyverse)
library(scales)
library(spatstat)
library(geosphere)
library(colorspace)
library(patchwork)
library(brms)
library(ggridges)


### read functions, prep spore data
source('miseq/scripts/helperFunctions.R')
source('MatchingSpore_FunctionData.R')


### download data
# miseq data -- this download is required
if(!file.exists('miseq/rawdata')) dir.create('miseq/rawdata')
options(timeout = max(300, getOption("timeout")))
if(!file.exists('miseq/rawdata/df_allSamples.txt')) {
  download.file('https://cloudstor.aarnet.edu.au/plus/s/MKb7lgaK3cY8SPK/download',
                'miseq/rawdata/df_allSamples.txt')
}
# environmental data -- this download is required
if(!file.exists('miseq/rawdata/df_env_fine.csv')) {
  download.file('https://cloudstor.aarnet.edu.au/plus/s/AFpqSzi0GX4H7rA/download',
                'miseq/rawdata/df_env_fine.csv')
}
# range areas -- can either download or create using 'miseq/scripts/dataPrep_rangeSize_primers.R' (see below)
if(!file.exists('miseq/output/workspace')) dir.create('miseq/output/workspace')
if(!file.exists('miseq/output/workspace/alphahull_terrestrial_primers.RData')) {
  download.file('https://cloudstor.aarnet.edu.au/plus/s/rfYvzEALwQmcZD2/download',
                'miseq/output/workspace/alphahull_terrestrial_primers.RData')
}


### load/prep data
geo <- read_tsv('miseq/rawdata/df_allSamples.txt')
env <- read_csv('miseq/rawdata/df_env_fine.csv')
trait <- To_Analysis


### manipulate and join dataframes
# calculate median spore length values per species 
trait.sum <- summarise_trait() %>% 
  dplyr::select(Species, phylum, class, order, Life_style, simpleFunct, n.sporetypes, 
                starts_with('log10.sporeVolume_'), 
                starts_with('log10.sporeAspectRatio_')) %>% 
  group_by(Species) %>% 
  slice(1) %>% 
  as.data.frame
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
summary(fit4.ext, prob=0.95)

# population-level effects
plot(fit4.ext, pars = "^b_")

# extent greater for host.assoc == 'yes'
hypothesis(fit4.ext, "host.assocyes > 0", class = "b")

# slope greater for host.assoc == 'yes'
hypothesis(fit4.ext, "log10.sporeVolume.cent:host.assocyes + log10.sporeVolume.cent > log10.sporeVolume.cent", class = "b")

# negative slope for host.assoc == 'no'
hypothesis(fit4.ext, "log10.sporeVolume.cent < 0", class = "b")

# negative slope for host.assoc == 'yes'
hypothesis(fit4.ext, "log10.sporeVolume.cent:host.assocyes + log10.sporeVolume.cent < 0", class = "b")





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
summary(fit4.area, prob=0.95)

# population-level effects
plot(fit4.area, pars = "^b_")

# area greater for host.assoc == 'yes'
hypothesis(fit4.area, "host.assocyes > 0", class = "b")

# slope greater for host.assoc == 'yes'
hypothesis(fit4.area, "log10.sporeVolume.cent:host.assocyes + log10.sporeVolume.cent > log10.sporeVolume.cent", class = "b")

# negative slope for host.assoc == 'no'
hypothesis(fit4.area, "log10.sporeVolume.cent < 0", class = "b")

# negative slope for host.assoc == 'yes'
hypothesis(fit4.area, "log10.sporeVolume.cent:host.assocyes + log10.sporeVolume.cent < 0", class = "b")




### save image

# save.image('output/workspace/geoextent_brm_host.RData')




### plotting output

load('output/workspace/geoextent_brm_host.RData')


### confidence bands for slope

# ext
slp.n <- c(attr(fit4.ext$fit, 'sim')$samples[[1]][['b_log10.sporeVolume.cent']][1001:2000], 
           attr(fit4.ext$fit, 'sim')$samples[[2]][['b_log10.sporeVolume.cent']][1001:2000], 
           attr(fit4.ext$fit, 'sim')$samples[[3]][['b_log10.sporeVolume.cent']][1001:2000], 
           attr(fit4.ext$fit, 'sim')$samples[[4]][['b_log10.sporeVolume.cent']][1001:2000])
conf.95.n <- slp.n > quantile(slp.n, prob=0.025) & slp.n < quantile(slp.n, prob=0.975)
conf.50.n <- slp.n > quantile(slp.n, prob=0.25) & slp.n < quantile(slp.n, prob=0.75)
slp.y <- c(attr(fit4.ext$fit, 'sim')$samples[[1]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000], 
           attr(fit4.ext$fit, 'sim')$samples[[2]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000], 
           attr(fit4.ext$fit, 'sim')$samples[[3]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000], 
           attr(fit4.ext$fit, 'sim')$samples[[4]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000])
conf.95.y <- slp.y > quantile(slp.y, prob=0.025) & slp.y < quantile(slp.y, prob=0.975)
conf.50.y <- slp.y > quantile(slp.y, prob=0.25) & slp.y < quantile(slp.y, prob=0.75)
slp.e <- bind_rows(data.frame(host.assoc='no', response='extent', slope=slp.n, conf.50=conf.50.n, conf.95=conf.95.n), 
                 data.frame(host.assoc='yes', response='extent', slope=slp.n+slp.y, conf.50=conf.50.y, conf.95=conf.95.y))
rm(slp.n, slp.y, conf.95.n, conf.50.n, conf.95.y, conf.50.y)


# area
slp.n <- c(attr(fit4.area$fit, 'sim')$samples[[1]][['b_log10.sporeVolume.cent']][1001:2000], 
                 attr(fit4.area$fit, 'sim')$samples[[2]][['b_log10.sporeVolume.cent']][1001:2000], 
                 attr(fit4.area$fit, 'sim')$samples[[3]][['b_log10.sporeVolume.cent']][1001:2000], 
                 attr(fit4.area$fit, 'sim')$samples[[4]][['b_log10.sporeVolume.cent']][1001:2000])
conf.95.n <- slp.n > quantile(slp.n, prob=0.025) & slp.n < quantile(slp.n, prob=0.975)
conf.50.n <- slp.n > quantile(slp.n, prob=0.25) & slp.n < quantile(slp.n, prob=0.75)
slp.y <- c(attr(fit4.area$fit, 'sim')$samples[[1]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000], 
                 attr(fit4.area$fit, 'sim')$samples[[2]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000], 
                 attr(fit4.area$fit, 'sim')$samples[[3]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000], 
                 attr(fit4.area$fit, 'sim')$samples[[4]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000])
conf.95.y <- slp.y > quantile(slp.y, prob=0.025) & slp.y < quantile(slp.y, prob=0.975)
conf.50.y <- slp.y > quantile(slp.y, prob=0.25) & slp.y < quantile(slp.y, prob=0.75)
slp.a <- bind_rows(data.frame(host.assoc='no', response='area', slope=slp.n, conf.50=conf.50.n, conf.95=conf.95.n), 
                   data.frame(host.assoc='yes', response='area', slope=slp.n+slp.y, conf.50=conf.50.y, conf.95=conf.95.y))
rm(slp.n, slp.y, conf.95.n, conf.50.n, conf.95.y, conf.50.y)

# join
slp <- bind_rows(slp.e, slp.a)
slp %>% group_by(host.assoc, response) %>% summarise(mean=mean(slope))


### plot
labs <- c(area = 'Range area', extent = 'Maximum distance')

# extent and area by volume
temp %>% 
  select(Species, order, log10.sporeVolume, host.assoc, geo_extent_prop, geo_area_prop) %>% 
  pivot_longer(cols=ends_with('prop'), names_to='response', values_to='value') %>% 
  ggplot(aes(x=log10.sporeVolume, y=car::logit(value), colour=host.assoc)) + 
  geom_point(alpha=0.5, shape=16) + 
  stat_smooth(method='lm', alpha=0.5) + 
  labs(x='Spore volume (um^3, log_10)', y='Geographic extent (relative to maximum distance/area, logit)') + 
  facet_grid(rows=vars(response), scales='free_y') + 
  scale_colour_manual(name='', values=c('black', 'orange'), 
                      labels=c('free-living', 'host-associated')) + 
  theme(legend.position='none', 
        strip.background = element_blank(),
        strip.text.y = element_blank()) -> p1

ggplot(slp, aes(x=slope, colour=host.assoc, fill=host.assoc)) + 
  geom_density(alpha=0.5) + 
  geom_vline(xintercept=0) + 
  labs(x='Slope estimate', y='Density') + 
  facet_grid(rows=vars(response), labeller=labeller(response=labs)) + 
  scale_colour_manual(name='', values=c('black', 'orange'), 
                      labels=c('free-living', 'host-associated')) + 
  scale_fill_manual(name='', values=c('black', 'orange'), 
                      labels=c('free-living', 'host-associated')) -> p2

p1 + p2


### group-level effects (with host.assoc)

# ext
grp.int <- grep('^r_host.assoc_order.+Intercept', names(attr(fit3.ext$fit, 'sim')$samples[[1]]))
grp.slp <- grep('^r_host.assoc_order.+log10.sporeVolume.cent', names(attr(fit3.ext$fit, 'sim')$samples[[1]]))

df.int <- bind_rows(as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
  pivot_longer(cols=starts_with('r_host.assoc_order'), names_to='order', values_to='response') %>% 
  group_by(order) %>% 
  summarise(int.mean = mean(response), 
            int.l50 = quantile(response, prob=0.25), 
            int.u50 = quantile(response, prob=0.75), 
            int.l95 = quantile(response, prob=0.025), 
            int.u95 = quantile(response, prob=0.975)) %>% 
  mutate(order = gsub('r_host.assoc_order.', '', order), 
         order = gsub('.Intercept.', '', order)) %>% 
  separate(col=order, into=c('host.assoc', 'order'), sep='_')

df.slp <- bind_rows(as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[1]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[2]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[3]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[4]][grp.slp])[1001:2000, ]) %>% 
  pivot_longer(cols=starts_with('r_host.assoc_order'), names_to='order', values_to='response') %>% 
  group_by(order) %>% 
  summarise(slp.mean = mean(response), 
            slp.l50 = quantile(response, prob=0.25), 
            slp.u50 = quantile(response, prob=0.75), 
            slp.l95 = quantile(response, prob=0.025), 
            slp.u95 = quantile(response, prob=0.975)) %>% 
  mutate(order = gsub('r_host.assoc_order.', '', order), 
         order = gsub('.log10.sporeVolume.cent.', '', order)) %>% 
  separate(col=order, into=c('host.assoc', 'order'), sep='_')

df.ext <- full_join(df.int, df.slp) %>% 
  mutate(response = 'distance')
rm(df.int, df.slp)
summary(df.ext)

# area
grp.int <- grep('^r_host.assoc_order.+Intercept', names(attr(fit3.area$fit, 'sim')$samples[[1]]))
grp.slp <- grep('^r_host.assoc_order.+log10.sporeVolume.cent', names(attr(fit3.area$fit, 'sim')$samples[[1]]))

df.int <- bind_rows(as.data.frame(attr(fit3.area$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
  pivot_longer(cols=starts_with('r_host.assoc_order'), names_to='order', values_to='response') %>% 
  group_by(order) %>% 
  summarise(int.mean = mean(response), 
            int.l50 = quantile(response, prob=0.25), 
            int.u50 = quantile(response, prob=0.75), 
            int.l95 = quantile(response, prob=0.025), 
            int.u95 = quantile(response, prob=0.975)) %>% 
  mutate(order = gsub('r_host.assoc_order.', '', order), 
         order = gsub('.Intercept.', '', order)) %>% 
  separate(col=order, into=c('host.assoc', 'order'), sep='_')

df.slp <- bind_rows(as.data.frame(attr(fit3.area$fit, 'sim')$samples[[1]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[2]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[3]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[4]][grp.slp])[1001:2000, ]) %>% 
  pivot_longer(cols=starts_with('r_host.assoc_order'), names_to='order', values_to='response') %>% 
  group_by(order) %>% 
  summarise(slp.mean = mean(response), 
            slp.l50 = quantile(response, prob=0.25), 
            slp.u50 = quantile(response, prob=0.75), 
            slp.l95 = quantile(response, prob=0.025), 
            slp.u95 = quantile(response, prob=0.975)) %>% 
  mutate(order = gsub('r_host.assoc_order.', '', order), 
         order = gsub('.log10.sporeVolume.cent.', '', order)) %>% 
  separate(col=order, into=c('host.assoc', 'order'), sep='_')

df.area <- full_join(df.int, df.slp) %>% 
  mutate(response = 'area')
rm(df.int, df.slp)
summary(df.area)

# join tables with spore size estimates
trt <- geo.extent %>% 
  filter(!is.na(order)) %>%
  group_by(Species) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(phylum, order) %>% 
  summarise(log10.sporeVolume.mean = mean(log10.sporeVolume), 
            log10.sporeVolume.sd = sd(log10.sporeVolume)) %>% 
  select(phylum, order, log10.sporeVolume.mean, log10.sporeVolume.sd)
df <- bind_rows(df.ext, df.area) %>% 
  left_join(trt)

# plot
ggplot(df, aes(x=int.mean, y=slp.mean, col=order)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = slp.l50, ymax = slp.u50), width=0) + 
  geom_errorbarh(aes(xmin = int.l50, xmax = int.u50)) + 
  stat_smooth(method='lm', aes(col=NULL), colour='black') + 
  facet_grid(rows=vars(response), cols=vars(host.assoc), scales='free') + 
  labs(x='Intercept\n<- reduced geographical extent on average --- greater geographical extent on average ->', 
       y='Slope\n<- extent decreases with spore volume --- extent increases with spore volume ->')

ggplot(df, aes(x=log10.sporeVolume.mean, y=slp.mean, col=order)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = slp.l50, ymax = slp.u50), width=0) + 
  geom_errorbarh(aes(xmin = log10.sporeVolume.mean - log10.sporeVolume.sd, 
                     xmax = log10.sporeVolume.mean + log10.sporeVolume.sd)) + 
  stat_smooth(method='lm', aes(col=NULL), colour='black') + 
  facet_grid(rows=vars(response), cols=vars(host.assoc), scales='free') + 
  labs(x='Mean spore volume (log10-transformed)', 
       y='Slope\n<- extent decreases with spore volume --- extent increases with spore volume ->')



### group-level effects (without host.assoc)

# ext
grp.int <- grep('^r_order.+Intercept', names(attr(fit1.ext$fit, 'sim')$samples[[1]]))
grp.slp <- grep('^r_order.+log10.sporeVolume.cent', names(attr(fit1.ext$fit, 'sim')$samples[[1]]))

df.int <- bind_rows(as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
  pivot_longer(cols=starts_with('r_order'), names_to='order', values_to='response') %>% 
  group_by(order) %>% 
  summarise(int.mean = mean(response), 
            int.l50 = quantile(response, prob=0.25), 
            int.u50 = quantile(response, prob=0.75), 
            int.l95 = quantile(response, prob=0.025), 
            int.u95 = quantile(response, prob=0.975)) %>% 
  mutate(order = gsub('r_order.', '', order), 
         order = gsub('.Intercept.', '', order))

df.slp <- bind_rows(as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[1]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[2]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[3]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[4]][grp.slp])[1001:2000, ]) %>% 
  pivot_longer(cols=starts_with('r_order'), names_to='order', values_to='response') %>% 
  group_by(order) %>% 
  summarise(slp.mean = mean(response), 
            slp.l50 = quantile(response, prob=0.25), 
            slp.u50 = quantile(response, prob=0.75), 
            slp.l95 = quantile(response, prob=0.025), 
            slp.u95 = quantile(response, prob=0.975)) %>% 
  mutate(order = gsub('r_order.', '', order), 
         order = gsub('.log10.sporeVolume.cent.', '', order))

df.ext <- full_join(df.int, df.slp) %>% 
  mutate(response = 'distance')
rm(df.int, df.slp)
summary(df.ext)

# area
grp.int <- grep('^r_order.+Intercept', names(attr(fit1.area$fit, 'sim')$samples[[1]]))
grp.slp <- grep('^r_order.+log10.sporeVolume.cent', names(attr(fit1.area$fit, 'sim')$samples[[1]]))

df.int <- bind_rows(as.data.frame(attr(fit1.area$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit1.area$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit1.area$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit1.area$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
  pivot_longer(cols=starts_with('r_order'), names_to='order', values_to='response') %>% 
  group_by(order) %>% 
  summarise(int.mean = mean(response), 
            int.l50 = quantile(response, prob=0.25), 
            int.u50 = quantile(response, prob=0.75), 
            int.l95 = quantile(response, prob=0.025), 
            int.u95 = quantile(response, prob=0.975)) %>% 
  mutate(order = gsub('r_order.', '', order), 
         order = gsub('.Intercept.', '', order))

df.slp <- bind_rows(as.data.frame(attr(fit1.area$fit, 'sim')$samples[[1]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit1.area$fit, 'sim')$samples[[2]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit1.area$fit, 'sim')$samples[[3]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit1.area$fit, 'sim')$samples[[4]][grp.slp])[1001:2000, ]) %>% 
  pivot_longer(cols=starts_with('r_order'), names_to='order', values_to='response') %>% 
  group_by(order) %>% 
  summarise(slp.mean = mean(response), 
            slp.l50 = quantile(response, prob=0.25), 
            slp.u50 = quantile(response, prob=0.75), 
            slp.l95 = quantile(response, prob=0.025), 
            slp.u95 = quantile(response, prob=0.975)) %>% 
  mutate(order = gsub('r_order.', '', order), 
         order = gsub('.log10.sporeVolume.cent.', '', order))

df.area <- full_join(df.int, df.slp) %>% 
  mutate(response = 'area')
rm(df.int, df.slp)
summary(df.area)

# join tables with spore size estimates
trt <- geo.extent %>% 
  filter(!is.na(order)) %>%
  group_by(Species) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(phylum, order) %>% 
  summarise(log10.sporeVolume.mean = mean(log10.sporeVolume), 
            log10.sporeVolume.sd = sd(log10.sporeVolume)) %>% 
  select(phylum, order, log10.sporeVolume.mean, log10.sporeVolume.sd)
df <- bind_rows(df.ext, df.area) %>% 
  left_join(trt)

# plot
ggplot(df, aes(x=int.mean, y=slp.mean, col=order)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = slp.l50, ymax = slp.u50), width=0) + 
  geom_errorbarh(aes(xmin = int.l50, xmax = int.u50)) + 
  stat_smooth(method='lm', aes(col=NULL), colour='black') + 
  facet_grid(rows=vars(response), scales='free') + 
  labs(x='Intercept\n<- reduced geographical extent on average --- greater geographical extent on average ->', 
       y='Slope\n<- extent decreases with spore volume --- extent increases with spore volume ->')

ggplot(df, aes(x=log10.sporeVolume.mean, y=slp.mean, col=order)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = slp.l50, ymax = slp.u50), width=0) + 
  geom_errorbarh(aes(xmin = log10.sporeVolume.mean - log10.sporeVolume.sd, 
                     xmax = log10.sporeVolume.mean + log10.sporeVolume.sd)) + 
  stat_smooth(method='lm', aes(col=NULL), colour='black') + 
  facet_grid(rows=vars(response), cols=vars(phylum), scales='free') + 
  labs(x='Mean spore volume (log10-transformed)', 
       y='Slope\n<- extent decreases with spore volume --- extent increases with spore volume ->')



### group-level effects (with host.assoc) -- densities

filter(temp, !is.na(geo_extent_prop)) %>% 
  group_by(order, host.assoc) %>% 
  summarise(n_spp = n()) %>% 
  pivot_wider(names_from=host.assoc, values_from=n_spp, values_fill=0) %>% 
  as.data.frame()


## group = order

# ext
grp.int <- grep('^r_host.assoc_order.+Intercept', names(attr(fit3.ext$fit, 'sim')$samples[[1]]))
grp.slp <- grep('^r_host.assoc_order.+log10.sporeVolume.cent', names(attr(fit3.ext$fit, 'sim')$samples[[1]]))

df.int <- bind_rows(as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
  pivot_longer(cols=starts_with('r_host.assoc_order.'), names_to='order', values_to='value') %>% 
  mutate(order = gsub('r_host.assoc_order.', '', order), 
         order = gsub('.Intercept.', '', order), 
         estimate = 'intercept') %>% 
  separate(col=order, into=c('host.assoc', 'order'), sep='_')

df.slp <- bind_rows(as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[1]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[2]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[3]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[4]][grp.slp])[1001:2000, ]) %>% 
  pivot_longer(cols=starts_with('r_host.assoc_order.'), names_to='order', values_to='value') %>% 
  mutate(order = gsub('r_host.assoc_order.', '', order), 
         order = gsub('.log10.sporeVolume.cent.', '', order), 
         estimate = 'slope') %>% 
  separate(col=order, into=c('host.assoc', 'order'), sep='_')

df.ext <- bind_rows(df.int, df.slp) %>% 
  mutate(response = 'distance')
rm(df.int, df.slp)
summary(df.ext)

# area
grp.int <- grep('^r_host.assoc_order.+Intercept', names(attr(fit3.area$fit, 'sim')$samples[[1]]))
grp.slp <- grep('^r_host.assoc_order.+log10.sporeVolume.cent', names(attr(fit3.area$fit, 'sim')$samples[[1]]))

df.int <- bind_rows(as.data.frame(attr(fit3.area$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
  pivot_longer(cols=starts_with('r_host.assoc_order.'), names_to='order', values_to='value') %>% 
  mutate(order = gsub('r_host.assoc_order.', '', order), 
         order = gsub('.Intercept.', '', order), 
         estimate = 'intercept') %>% 
  separate(col=order, into=c('host.assoc', 'order'), sep='_')

df.slp <- bind_rows(as.data.frame(attr(fit3.area$fit, 'sim')$samples[[1]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[2]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[3]][grp.slp])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[4]][grp.slp])[1001:2000, ]) %>% 
  pivot_longer(cols=starts_with('r_host.assoc_order.'), names_to='order', values_to='value') %>% 
  mutate(order = gsub('r_host.assoc_order.', '', order), 
         order = gsub('.log10.sporeVolume.cent.', '', order), 
         estimate = 'slope') %>% 
  separate(col=order, into=c('host.assoc', 'order'), sep='_')

df.area <- bind_rows(df.int, df.slp) %>% 
  mutate(response = 'area')
rm(df.int, df.slp)
summary(df.area)

# join tables
df <- bind_rows(df.ext, df.area)

# summarise
df %>% 
  filter(estimate == 'slope') %>% 
  group_by(host.assoc, order, response) %>% 
  summarise(l95 = quantile(value, probs=0.025), u95 = quantile(value, probs=0.975)) %>% 
  filter(u95 < 0 | l95 > 0)

# plot
ggplot(filter(df, estimate == 'intercept'), aes(x=value, y=order, fill=host.assoc)) + 
  geom_density_ridges(alpha=0.5) + 
  geom_vline(xintercept=0) + 
  xlim(c(-0.5, 0.5)) + 
  facet_grid(cols=vars(response))
ggplot(filter(df, estimate == 'slope'), aes(x=value, y=order, fill=host.assoc)) + 
  geom_density_ridges(alpha=0.5) + 
  geom_vline(xintercept=0) + 
  xlim(c(-0.15, 0.15)) + 
  facet_grid(cols=vars(response))


## group = primer type

# ext
grp.int <- grep('^r_Primers.name.+Intercept', names(attr(fit3.ext$fit, 'sim')$samples[[1]]))
df.ext <- bind_rows(as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
  pivot_longer(cols=starts_with('r_Primers.name.'), names_to='primers', values_to='value') %>% 
  mutate(primers = gsub('r_Primers.name', '', primers), 
         primers = gsub('.Intercept.', '', primers), 
         response = 'distance')
summary(df.ext)

# area
grp.int <- grep('^r_Primers.name.+Intercept', names(attr(fit3.area$fit, 'sim')$samples[[1]]))
df.area <- bind_rows(as.data.frame(attr(fit3.area$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
                    as.data.frame(attr(fit3.area$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
  pivot_longer(cols=starts_with('r_Primers.name.'), names_to='primers', values_to='value') %>% 
  mutate(primers = gsub('r_Primers.name', '', primers), 
         primers = gsub('.Intercept.', '', primers), 
         response = 'area')
summary(df.area)

# join tables
df <- bind_rows(df.ext, df.area)

# plot
ggplot(df, aes(x=value, y=primers, fill=area)) + 
  geom_density_ridges(alpha=0.5) + 
  geom_vline(xintercept=0) + 
  xlim(c(-2, 2))
