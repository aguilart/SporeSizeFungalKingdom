### load libraries
library(tidyverse)
library(scales)
library(spatstat)
library(geosphere)
library(colorspace)
library(patchwork)
library(brms)
library(ggridges)


### load output
# modelling results -- can either download or create using 'miseq/scripts/geoextent_brm_host_modelfit.R' (run before proceeding, if needed)
if(!file.exists('miseq')) dir.create('miseq')
if(!file.exists('miseq/output')) dir.create('miseq/output')
if(!file.exists('miseq/output/workspace')) dir.create('miseq/output/workspace')
if(!file.exists('miseq/output/workspace/geoextent_brm_host.RData')) {
  download.file('https://cloudstor.aarnet.edu.au/plus/s/c7kamHx0RNM6pCd/download',
                'miseq/output/workspace/geoextent_brm_host.RData')
}
load('miseq/output/workspace/geoextent_brm_host.RData')


### analysis of extent

# compare models -- not much difference, go with simplest model
loo_compare(fit1.ext.loo, fit2.ext.loo, fit3.ext.loo, fit4.ext.loo)
loo_compare(fit1.ext.loo, fit2.ext.loo)

# model summary
summary(fit2.ext, prob=0.95)

# population-level effects
plot(fit2.ext, pars = "^b_")

# extent greater for host.assoc == 'yes'
hypothesis(fit2.ext, "host.assocyes > 0", class = "b")

# slope greater for host.assoc == 'yes'
hypothesis(fit2.ext, "log10.sporeVolume.cent:host.assocyes + log10.sporeVolume.cent > log10.sporeVolume.cent", class = "b")

# negative slope for host.assoc == 'no'
hypothesis(fit2.ext, "log10.sporeVolume.cent < 0", class = "b")

# negative slope for host.assoc == 'yes'
hypothesis(fit2.ext, "log10.sporeVolume.cent:host.assocyes + log10.sporeVolume.cent < 0", class = "b")



### analysis of area

# compare models -- not much difference, go with simplest model
loo_compare(fit1.area.loo, fit2.area.loo, fit3.area.loo, fit4.area.loo)
loo_compare(fit1.area.loo, fit2.area.loo)

# model summary
summary(fit2.area, prob=0.95)

# population-level effects
plot(fit2.area, pars = "^b_")

# area greater for host.assoc == 'yes'
hypothesis(fit2.area, "host.assocyes > 0", class = "b")

# slope greater for host.assoc == 'yes'
hypothesis(fit2.area, "log10.sporeVolume.cent:host.assocyes + log10.sporeVolume.cent > log10.sporeVolume.cent", class = "b")

# negative slope for host.assoc == 'no'
hypothesis(fit2.area, "log10.sporeVolume.cent < 0", class = "b")

# negative slope for host.assoc == 'yes'
hypothesis(fit2.area, "log10.sporeVolume.cent:host.assocyes + log10.sporeVolume.cent < 0", class = "b")


### plotting output

### confidence bands for slope

# ext
slp.n <- c(attr(fit2.ext$fit, 'sim')$samples[[1]][['b_log10.sporeVolume.cent']][1001:2000], 
           attr(fit2.ext$fit, 'sim')$samples[[2]][['b_log10.sporeVolume.cent']][1001:2000], 
           attr(fit2.ext$fit, 'sim')$samples[[3]][['b_log10.sporeVolume.cent']][1001:2000], 
           attr(fit2.ext$fit, 'sim')$samples[[4]][['b_log10.sporeVolume.cent']][1001:2000])
conf.95.n <- slp.n > quantile(slp.n, prob=0.025) & slp.n < quantile(slp.n, prob=0.975)
conf.50.n <- slp.n > quantile(slp.n, prob=0.25) & slp.n < quantile(slp.n, prob=0.75)
slp.y <- c(attr(fit2.ext$fit, 'sim')$samples[[1]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000], 
           attr(fit2.ext$fit, 'sim')$samples[[2]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000], 
           attr(fit2.ext$fit, 'sim')$samples[[3]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000], 
           attr(fit2.ext$fit, 'sim')$samples[[4]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000])
conf.95.y <- slp.y > quantile(slp.y, prob=0.025) & slp.y < quantile(slp.y, prob=0.975)
conf.50.y <- slp.y > quantile(slp.y, prob=0.25) & slp.y < quantile(slp.y, prob=0.75)
slp.e <- bind_rows(data.frame(host.assoc='no', response='extent', slope=slp.n, conf.50=conf.50.n, conf.95=conf.95.n), 
                 data.frame(host.assoc='yes', response='extent', slope=slp.n+slp.y, conf.50=conf.50.y, conf.95=conf.95.y))
rm(slp.n, slp.y, conf.95.n, conf.50.n, conf.95.y, conf.50.y)


# area
slp.n <- c(attr(fit2.area$fit, 'sim')$samples[[1]][['b_log10.sporeVolume.cent']][1001:2000], 
                 attr(fit2.area$fit, 'sim')$samples[[2]][['b_log10.sporeVolume.cent']][1001:2000], 
                 attr(fit2.area$fit, 'sim')$samples[[3]][['b_log10.sporeVolume.cent']][1001:2000], 
                 attr(fit2.area$fit, 'sim')$samples[[4]][['b_log10.sporeVolume.cent']][1001:2000])
conf.95.n <- slp.n > quantile(slp.n, prob=0.025) & slp.n < quantile(slp.n, prob=0.975)
conf.50.n <- slp.n > quantile(slp.n, prob=0.25) & slp.n < quantile(slp.n, prob=0.75)
slp.y <- c(attr(fit2.area$fit, 'sim')$samples[[1]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000], 
                 attr(fit2.area$fit, 'sim')$samples[[2]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000], 
                 attr(fit2.area$fit, 'sim')$samples[[3]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000], 
                 attr(fit2.area$fit, 'sim')$samples[[4]][['b_log10.sporeVolume.cent:host.assocyes']][1001:2000])
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








# 
# 
# 
# 
# ### group-level effects (with host.assoc)
# 
# # ext
# grp.int <- grep('^r_host.assoc_order.+Intercept', names(attr(fit3.ext$fit, 'sim')$samples[[1]]))
# grp.slp <- grep('^r_host.assoc_order.+log10.sporeVolume.cent', names(attr(fit3.ext$fit, 'sim')$samples[[1]]))
# 
# df.int <- bind_rows(as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
#   pivot_longer(cols=starts_with('r_host.assoc_order'), names_to='order', values_to='response') %>% 
#   group_by(order) %>% 
#   summarise(int.mean = mean(response), 
#             int.l50 = quantile(response, prob=0.25), 
#             int.u50 = quantile(response, prob=0.75), 
#             int.l95 = quantile(response, prob=0.025), 
#             int.u95 = quantile(response, prob=0.975)) %>% 
#   mutate(order = gsub('r_host.assoc_order.', '', order), 
#          order = gsub('.Intercept.', '', order)) %>% 
#   separate(col=order, into=c('host.assoc', 'order'), sep='_')
# 
# df.slp <- bind_rows(as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[1]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[2]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[3]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[4]][grp.slp])[1001:2000, ]) %>% 
#   pivot_longer(cols=starts_with('r_host.assoc_order'), names_to='order', values_to='response') %>% 
#   group_by(order) %>% 
#   summarise(slp.mean = mean(response), 
#             slp.l50 = quantile(response, prob=0.25), 
#             slp.u50 = quantile(response, prob=0.75), 
#             slp.l95 = quantile(response, prob=0.025), 
#             slp.u95 = quantile(response, prob=0.975)) %>% 
#   mutate(order = gsub('r_host.assoc_order.', '', order), 
#          order = gsub('.log10.sporeVolume.cent.', '', order)) %>% 
#   separate(col=order, into=c('host.assoc', 'order'), sep='_')
# 
# df.ext <- full_join(df.int, df.slp) %>% 
#   mutate(response = 'distance')
# rm(df.int, df.slp)
# summary(df.ext)
# 
# # area
# grp.int <- grep('^r_host.assoc_order.+Intercept', names(attr(fit3.area$fit, 'sim')$samples[[1]]))
# grp.slp <- grep('^r_host.assoc_order.+log10.sporeVolume.cent', names(attr(fit3.area$fit, 'sim')$samples[[1]]))
# 
# df.int <- bind_rows(as.data.frame(attr(fit3.area$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
#   pivot_longer(cols=starts_with('r_host.assoc_order'), names_to='order', values_to='response') %>% 
#   group_by(order) %>% 
#   summarise(int.mean = mean(response), 
#             int.l50 = quantile(response, prob=0.25), 
#             int.u50 = quantile(response, prob=0.75), 
#             int.l95 = quantile(response, prob=0.025), 
#             int.u95 = quantile(response, prob=0.975)) %>% 
#   mutate(order = gsub('r_host.assoc_order.', '', order), 
#          order = gsub('.Intercept.', '', order)) %>% 
#   separate(col=order, into=c('host.assoc', 'order'), sep='_')
# 
# df.slp <- bind_rows(as.data.frame(attr(fit3.area$fit, 'sim')$samples[[1]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[2]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[3]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[4]][grp.slp])[1001:2000, ]) %>% 
#   pivot_longer(cols=starts_with('r_host.assoc_order'), names_to='order', values_to='response') %>% 
#   group_by(order) %>% 
#   summarise(slp.mean = mean(response), 
#             slp.l50 = quantile(response, prob=0.25), 
#             slp.u50 = quantile(response, prob=0.75), 
#             slp.l95 = quantile(response, prob=0.025), 
#             slp.u95 = quantile(response, prob=0.975)) %>% 
#   mutate(order = gsub('r_host.assoc_order.', '', order), 
#          order = gsub('.log10.sporeVolume.cent.', '', order)) %>% 
#   separate(col=order, into=c('host.assoc', 'order'), sep='_')
# 
# df.area <- full_join(df.int, df.slp) %>% 
#   mutate(response = 'area')
# rm(df.int, df.slp)
# summary(df.area)
# 
# # join tables with spore size estimates
# trt <- geo.extent %>% 
#   filter(!is.na(order)) %>%
#   group_by(Species) %>% 
#   slice(1) %>% 
#   ungroup() %>% 
#   group_by(order) %>% 
#   summarise(log10.sporeVolume.mean = mean(log10.sporeVolume), 
#             log10.sporeVolume.sd = sd(log10.sporeVolume)) %>% 
#   select(phylum, order, log10.sporeVolume.mean, log10.sporeVolume.sd)
# df <- bind_rows(df.ext, df.area) %>% 
#   left_join(trt)
# 
# # plot
# ggplot(df, aes(x=int.mean, y=slp.mean, col=order)) + 
#   geom_point() + 
#   geom_errorbar(aes(ymin = slp.l50, ymax = slp.u50), width=0) + 
#   geom_errorbarh(aes(xmin = int.l50, xmax = int.u50)) + 
#   stat_smooth(method='lm', aes(col=NULL), colour='black') + 
#   facet_grid(rows=vars(response), cols=vars(host.assoc), scales='free') + 
#   labs(x='Intercept\n<- reduced geographical extent on average --- greater geographical extent on average ->', 
#        y='Slope\n<- extent decreases with spore volume --- extent increases with spore volume ->')
# 
# ggplot(df, aes(x=log10.sporeVolume.mean, y=slp.mean, col=order)) + 
#   geom_point() + 
#   geom_errorbar(aes(ymin = slp.l50, ymax = slp.u50), width=0) + 
#   geom_errorbarh(aes(xmin = log10.sporeVolume.mean - log10.sporeVolume.sd, 
#                      xmax = log10.sporeVolume.mean + log10.sporeVolume.sd)) + 
#   stat_smooth(method='lm', aes(col=NULL), colour='black') + 
#   facet_grid(rows=vars(response), cols=vars(host.assoc), scales='free') + 
#   labs(x='Mean spore volume (log10-transformed)', 
#        y='Slope\n<- extent decreases with spore volume --- extent increases with spore volume ->')
# 
# 
# 
# ### group-level effects (without host.assoc)
# 
# # ext
# grp.int <- grep('^r_order.+Intercept', names(attr(fit1.ext$fit, 'sim')$samples[[1]]))
# grp.slp <- grep('^r_order.+log10.sporeVolume.cent', names(attr(fit1.ext$fit, 'sim')$samples[[1]]))
# 
# df.int <- bind_rows(as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
#   pivot_longer(cols=starts_with('r_order'), names_to='order', values_to='response') %>% 
#   group_by(order) %>% 
#   summarise(int.mean = mean(response), 
#             int.l50 = quantile(response, prob=0.25), 
#             int.u50 = quantile(response, prob=0.75), 
#             int.l95 = quantile(response, prob=0.025), 
#             int.u95 = quantile(response, prob=0.975)) %>% 
#   mutate(order = gsub('r_order.', '', order), 
#          order = gsub('.Intercept.', '', order))
# 
# df.slp <- bind_rows(as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[1]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[2]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[3]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit1.ext$fit, 'sim')$samples[[4]][grp.slp])[1001:2000, ]) %>% 
#   pivot_longer(cols=starts_with('r_order'), names_to='order', values_to='response') %>% 
#   group_by(order) %>% 
#   summarise(slp.mean = mean(response), 
#             slp.l50 = quantile(response, prob=0.25), 
#             slp.u50 = quantile(response, prob=0.75), 
#             slp.l95 = quantile(response, prob=0.025), 
#             slp.u95 = quantile(response, prob=0.975)) %>% 
#   mutate(order = gsub('r_order.', '', order), 
#          order = gsub('.log10.sporeVolume.cent.', '', order))
# 
# df.ext <- full_join(df.int, df.slp) %>% 
#   mutate(response = 'distance')
# rm(df.int, df.slp)
# summary(df.ext)
# 
# # area
# grp.int <- grep('^r_order.+Intercept', names(attr(fit1.area$fit, 'sim')$samples[[1]]))
# grp.slp <- grep('^r_order.+log10.sporeVolume.cent', names(attr(fit1.area$fit, 'sim')$samples[[1]]))
# 
# df.int <- bind_rows(as.data.frame(attr(fit1.area$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit1.area$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit1.area$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit1.area$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
#   pivot_longer(cols=starts_with('r_order'), names_to='order', values_to='response') %>% 
#   group_by(order) %>% 
#   summarise(int.mean = mean(response), 
#             int.l50 = quantile(response, prob=0.25), 
#             int.u50 = quantile(response, prob=0.75), 
#             int.l95 = quantile(response, prob=0.025), 
#             int.u95 = quantile(response, prob=0.975)) %>% 
#   mutate(order = gsub('r_order.', '', order), 
#          order = gsub('.Intercept.', '', order))
# 
# df.slp <- bind_rows(as.data.frame(attr(fit1.area$fit, 'sim')$samples[[1]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit1.area$fit, 'sim')$samples[[2]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit1.area$fit, 'sim')$samples[[3]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit1.area$fit, 'sim')$samples[[4]][grp.slp])[1001:2000, ]) %>% 
#   pivot_longer(cols=starts_with('r_order'), names_to='order', values_to='response') %>% 
#   group_by(order) %>% 
#   summarise(slp.mean = mean(response), 
#             slp.l50 = quantile(response, prob=0.25), 
#             slp.u50 = quantile(response, prob=0.75), 
#             slp.l95 = quantile(response, prob=0.025), 
#             slp.u95 = quantile(response, prob=0.975)) %>% 
#   mutate(order = gsub('r_order.', '', order), 
#          order = gsub('.log10.sporeVolume.cent.', '', order))
# 
# df.area <- full_join(df.int, df.slp) %>% 
#   mutate(response = 'area')
# rm(df.int, df.slp)
# summary(df.area)
# 
# # join tables with spore size estimates
# trt <- geo.extent %>% 
#   filter(!is.na(order)) %>%
#   group_by(Species) %>% 
#   slice(1) %>% 
#   ungroup() %>% 
#   group_by(phylum, order) %>% 
#   summarise(log10.sporeVolume.mean = mean(log10.sporeVolume), 
#             log10.sporeVolume.sd = sd(log10.sporeVolume)) %>% 
#   select(phylum, order, log10.sporeVolume.mean, log10.sporeVolume.sd)
# df <- bind_rows(df.ext, df.area) %>% 
#   left_join(trt)
# 
# # plot
# ggplot(df, aes(x=int.mean, y=slp.mean, col=order)) + 
#   geom_point() + 
#   geom_errorbar(aes(ymin = slp.l50, ymax = slp.u50), width=0) + 
#   geom_errorbarh(aes(xmin = int.l50, xmax = int.u50)) + 
#   stat_smooth(method='lm', aes(col=NULL), colour='black') + 
#   facet_grid(rows=vars(response), scales='free') + 
#   labs(x='Intercept\n<- reduced geographical extent on average --- greater geographical extent on average ->', 
#        y='Slope\n<- extent decreases with spore volume --- extent increases with spore volume ->')
# 
# ggplot(df, aes(x=log10.sporeVolume.mean, y=slp.mean, col=order)) + 
#   geom_point() + 
#   geom_errorbar(aes(ymin = slp.l50, ymax = slp.u50), width=0) + 
#   geom_errorbarh(aes(xmin = log10.sporeVolume.mean - log10.sporeVolume.sd, 
#                      xmax = log10.sporeVolume.mean + log10.sporeVolume.sd)) + 
#   stat_smooth(method='lm', aes(col=NULL), colour='black') + 
#   facet_grid(rows=vars(response), cols=vars(phylum), scales='free') + 
#   labs(x='Mean spore volume (log10-transformed)', 
#        y='Slope\n<- extent decreases with spore volume --- extent increases with spore volume ->')
# 
# 
# 
# ### group-level effects (with host.assoc) -- densities
# 
# filter(temp, !is.na(geo_extent_prop)) %>% 
#   group_by(order, host.assoc) %>% 
#   summarise(n_spp = n()) %>% 
#   pivot_wider(names_from=host.assoc, values_from=n_spp, values_fill=0) %>% 
#   as.data.frame()
# 
# 
# ## group = order
# 
# # ext
# grp.int <- grep('^r_host.assoc_order.+Intercept', names(attr(fit3.ext$fit, 'sim')$samples[[1]]))
# grp.slp <- grep('^r_host.assoc_order.+log10.sporeVolume.cent', names(attr(fit3.ext$fit, 'sim')$samples[[1]]))
# 
# df.int <- bind_rows(as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
#   pivot_longer(cols=starts_with('r_host.assoc_order.'), names_to='order', values_to='value') %>% 
#   mutate(order = gsub('r_host.assoc_order.', '', order), 
#          order = gsub('.Intercept.', '', order), 
#          estimate = 'intercept') %>% 
#   separate(col=order, into=c('host.assoc', 'order'), sep='_')
# 
# df.slp <- bind_rows(as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[1]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[2]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[3]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[4]][grp.slp])[1001:2000, ]) %>% 
#   pivot_longer(cols=starts_with('r_host.assoc_order.'), names_to='order', values_to='value') %>% 
#   mutate(order = gsub('r_host.assoc_order.', '', order), 
#          order = gsub('.log10.sporeVolume.cent.', '', order), 
#          estimate = 'slope') %>% 
#   separate(col=order, into=c('host.assoc', 'order'), sep='_')
# 
# df.ext <- bind_rows(df.int, df.slp) %>% 
#   mutate(response = 'distance')
# rm(df.int, df.slp)
# summary(df.ext)
# 
# # area
# grp.int <- grep('^r_host.assoc_order.+Intercept', names(attr(fit3.area$fit, 'sim')$samples[[1]]))
# grp.slp <- grep('^r_host.assoc_order.+log10.sporeVolume.cent', names(attr(fit3.area$fit, 'sim')$samples[[1]]))
# 
# df.int <- bind_rows(as.data.frame(attr(fit3.area$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
#   pivot_longer(cols=starts_with('r_host.assoc_order.'), names_to='order', values_to='value') %>% 
#   mutate(order = gsub('r_host.assoc_order.', '', order), 
#          order = gsub('.Intercept.', '', order), 
#          estimate = 'intercept') %>% 
#   separate(col=order, into=c('host.assoc', 'order'), sep='_')
# 
# df.slp <- bind_rows(as.data.frame(attr(fit3.area$fit, 'sim')$samples[[1]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[2]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[3]][grp.slp])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[4]][grp.slp])[1001:2000, ]) %>% 
#   pivot_longer(cols=starts_with('r_host.assoc_order.'), names_to='order', values_to='value') %>% 
#   mutate(order = gsub('r_host.assoc_order.', '', order), 
#          order = gsub('.log10.sporeVolume.cent.', '', order), 
#          estimate = 'slope') %>% 
#   separate(col=order, into=c('host.assoc', 'order'), sep='_')
# 
# df.area <- bind_rows(df.int, df.slp) %>% 
#   mutate(response = 'area')
# rm(df.int, df.slp)
# summary(df.area)
# 
# # join tables
# df <- bind_rows(df.ext, df.area)
# 
# # summarise
# df %>% 
#   filter(estimate == 'slope') %>% 
#   group_by(host.assoc, order, response) %>% 
#   summarise(l95 = quantile(value, probs=0.025), u95 = quantile(value, probs=0.975)) %>% 
#   filter(u95 < 0 | l95 > 0)
# 
# # plot
# ggplot(filter(df, estimate == 'intercept'), aes(x=value, y=order, fill=host.assoc)) + 
#   geom_density_ridges(alpha=0.5) + 
#   geom_vline(xintercept=0) + 
#   xlim(c(-0.5, 0.5)) + 
#   facet_grid(cols=vars(response))
# ggplot(filter(df, estimate == 'slope'), aes(x=value, y=order, fill=host.assoc)) + 
#   geom_density_ridges(alpha=0.5) + 
#   geom_vline(xintercept=0) + 
#   xlim(c(-0.15, 0.15)) + 
#   facet_grid(cols=vars(response))
# 
# 
# ## group = primer type
# 
# # ext
# grp.int <- grep('^r_Primers.name.+Intercept', names(attr(fit3.ext$fit, 'sim')$samples[[1]]))
# df.ext <- bind_rows(as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.ext$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
#   pivot_longer(cols=starts_with('r_Primers.name.'), names_to='primers', values_to='value') %>% 
#   mutate(primers = gsub('r_Primers.name', '', primers), 
#          primers = gsub('.Intercept.', '', primers), 
#          response = 'distance')
# summary(df.ext)
# 
# # area
# grp.int <- grep('^r_Primers.name.+Intercept', names(attr(fit3.area$fit, 'sim')$samples[[1]]))
# df.area <- bind_rows(as.data.frame(attr(fit3.area$fit, 'sim')$samples[[1]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[2]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[3]][grp.int])[1001:2000, ], 
#                     as.data.frame(attr(fit3.area$fit, 'sim')$samples[[4]][grp.int])[1001:2000, ]) %>% 
#   pivot_longer(cols=starts_with('r_Primers.name.'), names_to='primers', values_to='value') %>% 
#   mutate(primers = gsub('r_Primers.name', '', primers), 
#          primers = gsub('.Intercept.', '', primers), 
#          response = 'area')
# summary(df.area)
# 
# # join tables
# df <- bind_rows(df.ext, df.area)
# 
# # plot
# ggplot(df, aes(x=value, y=primers, fill=area)) + 
#   geom_density_ridges(alpha=0.5) + 
#   geom_vline(xintercept=0) + 
#   xlim(c(-2, 2))
