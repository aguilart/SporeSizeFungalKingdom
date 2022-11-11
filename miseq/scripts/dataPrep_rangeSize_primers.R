# load libraries
library(tidyverse)
library(rangeBuilder)
library(rgeos)

dir.create('miseq/output/workspace/rangeEsts')

# temp is created in 'miseq/scripts/geoextent_brm_host.R'
temp1 <- split(temp, interaction(temp$Species, temp$Primers.name, sep='__', drop=TRUE))
names(temp1) <- gsub('/', '_', names(temp1), fixed=TRUE)
for(i in 1:length(temp1)){
  x <- temp1[[i]]
  y <- names(temp1)[i]
  res <- x %>% dplyr::select(longitude, latitude) %>% distinct
  if(nrow(x) <= 2) res <- NA
  if(nrow(x) > 2) res <- tryCatch(rangeBuilder::getDynamicAlphaHull(x, coordHeaders=c('longitude','latitude'), 
                                                                    clipToCoast = 'terrestrial', initialAlpha = 3), 
                                  error = function(x) y) # for alpha hull
  save(res, file=paste('output/workspace/rangeEsts/alphahull_', y, '.RData', sep=''))
}
# > which(names(temp1) == 'Cystoderma_jasonis__ITS1F_ITS2')
# [1] 164
# > temp1[1:165] <- NULL
# > head(names(temp1))

filenames <- list.files('output/workspace/rangeEsts', full.names=TRUE)
geo.alpha <- lapply(filenames, function(x){
  load(x)
  return(res)})
names(geo.alpha) <- gsub('.RData$', '', gsub('^.+alphahull_', '', filenames))
geo.area <- sapply(geo.alpha[sapply(geo.alpha, function(x)class(x[[1]]) == 'SpatialPolygons')], function(x)gArea(spTransform(x[[1]], CRS('+proj=moll +ellps=WGS84'))))
# geo.area <- sapply(geo.alpha[sapply(geo.alpha, function(x)class(x[[1]]) == 'SpatialPolygons')], function(x)gArea(spTransform(x[[1]], CRS('+proj=aea +ellps=WGS84'))))
save(geo.alpha, geo.area, file='output/workspace/alphahull_terrestrial_primers.RData')

