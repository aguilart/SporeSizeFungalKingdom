# load libraries
library(rangeBuilder)
library(maptools)
library(rgeos)

if(!file.exists('miseq/output/workspace/rangeEsts')) dir.create('miseq/output/workspace/rangeEsts')

# temp is created in 'miseq/scripts/geoextent_brm_host_modelfit.R'
temp1 <- split(temp, interaction(temp$Species, temp$Primers.name, sep='__', drop=TRUE))
names(temp1) <- gsub('/', '_', names(temp1), fixed=TRUE)
# one element in temp1 is problematic, possibly because only three samples all with same latitude -> delete
temp1[which(names(temp1) == 'Cystoderma jasonis__ITS1F_ITS2')] <- NULL
# elements that result in segfaults - > delete
temp1[which(names(temp1) == 'Ilyonectria mors-panacis__fITS7_ITS4')] <- NULL
temp1[which(names(temp1) == 'Paramyrothecium roridum__fITS7_ITS4')] <- NULL
temp1[which(names(temp1) == 'Rhexocercosporidium panacis__fITS7_ITS4')] <- NULL
temp1[which(names(temp1) == 'Rhizophagus intraradices__fITS7_ITS4')] <- NULL
temp1[which(names(temp1) == 'Cortinarius croceus__ITS1F_ITS4')] <- NULL
temp1[which(names(temp1) == 'Cortinarius fulvescens__ITS1F_ITS4')] <- NULL
temp1[which(names(temp1) == 'Epicoccum nigrum__ITS1F_ITS4')] <- NULL
temp1[which(names(temp1) == 'Keissleriella poagena__ITS1F_ITS4')] <- NULL
temp1[which(names(temp1) == 'Penicillium glabrum__ITS1F_ITS4')] <- NULL
# next two lines avoid rerunning on completed elements
done <- gsub('.RData$', '', gsub('^alphahull_', '', list.files('miseq/output/workspace/rangeEsts')))
temp1[done] <- NULL
names(temp1)[1]  # if previous run ended in segfault, gives problem element
# calculate alpha hull
for(i in 1:length(temp1)){
  x <- temp1[[i]]
  y <- names(temp1)[i]
  res <- x %>% dplyr::select(longitude, latitude) %>% distinct
  if(nrow(x) <= 2) res <- NA
  if(nrow(x) > 2) res <- tryCatch(getDynamicAlphaHull(x, coordHeaders=c('longitude','latitude'), 
                                                      clipToCoast='terrestrial', initialAlpha=3), 
                                  error=function(x) y) # for alpha hull
  save(res, file=paste('miseq/output/workspace/rangeEsts/alphahull_', y, '.RData', sep=''))
}

# summarise output
filenames <- list.files('miseq/output/workspace/rangeEsts', full.names=TRUE)
geo.alpha <- lapply(filenames, function(x){
  load(x)
  return(res)})
names(geo.alpha) <- gsub('.RData$', '', gsub('^.+alphahull_', '', filenames))
geo.area <- sapply(geo.alpha[sapply(geo.alpha, function(x)class(x[[1]]) == 'SpatialPolygons')], function(x)gArea(spTransform(x[[1]], CRS('+proj=moll +ellps=WGS84'))))
# geo.area <- sapply(geo.alpha[sapply(geo.alpha, function(x)class(x[[1]]) == 'SpatialPolygons')], function(x)gArea(spTransform(x[[1]], CRS('+proj=aea +ellps=WGS84'))))
save(geo.alpha, geo.area, file='miseq/output/workspace/alphahull_terrestrial_primers.RData')

