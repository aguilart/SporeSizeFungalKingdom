library(plyr)
library(readxl)
library(xlsx)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

## Franz Krah
## Using Regex to extract spore values from MycoBank descriptions
## 2018 - 11 - 21

get_spore_measure <-
  function(txt,
           start.regex = "spores\\b|spore\\b|\\bSpore|Conidia\\b|uredosporis\\b",
           end.regex = "µm",
           extract.regex) {
    
  require(stringr)
  
  ## some character normalizations; mainly changing diverse comma seperations to comma
  txt <- gsub("\u00B7", ",", txt)
  txt <- gsub("\\.", ",", txt)
  txt <- gsub("\u2013", "-", txt) ## en dash to minus
  txt <- gsub("\u2014", "-", txt) ## en dash to minus
  txt <- gsub("–", "-", txt) ## en dash to minus
  txt <- gsub("\\s?-\\s?", "-", txt) ## en dash to minus
  txt <- gsub("\\sto\\s", "-", txt)
  txt <- gsub("×", "x", txt)
  txt <- gsub("±", "+-", txt)
  
  
  ## get start and end of potential spore text
  starts <- txt %>% strsplit(split = " ") %>% unlist(recursive = FALSE) %>%
    grep(pattern = start.regex)
  
  # "Ascospore\\b|Ascospores\\b|Basidiospore\\b|Basidiospores\\b|Conidia\\b|uredosporis\\b|spores\\b"
  
  stops <- txt %>% strsplit(split = " ") %>% unlist(recursive = FALSE) %>%
    grep(pattern = end.regex)
  
  
  l.start <- length(starts)
  l.stops <- length(stops)
  
  if(l.start == 0 | l.stops == 0){
    res <- "No spore measurements found"
    return(res)
  }else{
    
    det_range <- function(starts, stops){
      g <- expand.grid(starts, stops)
      g <- g[(g[,2] - g[,1])>0,]
      g <- cbind(g, diff = g[,2] - g[,1])
      m <- tapply(g$diff, g[,1], min)
      g <- g[g$Var1 %in% names(m) & g$diff %in% m, ]
      g <- g[,1:2]
      g <- as.data.frame(g)
      names(g) <- c("start", "stop")
      g
    }
    
    ranges <- det_range(starts, stops)
    
    if(all((ranges[,2] - ranges[,1])<0)){
      res <- "No spore measurements found"
      return(res)
    }
    
    ## subset text area to spore information
    spore.val.somewhere <- list()
    txt.split <- txt %>% strsplit(split = " ") %>% unlist(recursive = FALSE)
    for (j in 1:nrow(ranges)) {
      spore.val.somewhere[[j]] <-
        paste(txt.split[ranges$start[[j]]:ranges$stop[[j]]], collapse = " ")
    }
    
    
    ## indicate mean, av by \\
    if(length(grep("\\bav|\\bmean", spore.val.somewhere))>0)
      spore.val.somewhere <- gsub("\\bav|\\bmean", "\\\\", spore.val.somewhere)
    
    ## clean spore texts from "spore size, spore volume", etc.
    wds <- lapply(spore.val.somewhere, word, start = 1, end = 5)
    if(length(grep("Illustrations|illustrations|volume|Basidia\\b", spore.val.somewhere))>0)
      spore.val.somewhere <- spore.val.somewhere[-grep("Illustrations|illustrations|volume|Basidia\\b", wds)]
    
    if(length(spore.val.somewhere)==0){
      return("No spore measurements found")
    }
    
    ## use regex to extract spore values
    spore.val <- list()
    for(i in 1:length(spore.val.somewhere)){
      
      is.my <- grep("µm", spore.val.somewhere[[i]])
      is.x <- grep("x",  spore.val.somewhere[[i]])
      
      if(length(is.my)>0 & length(is.x)==0){
        spore <-
          str_extract_all(spore.val.somewhere[[i]], 
                          pattern = ".[^a-wy-zA-WY-Z]+\\s?µm")
      }else{
        spore <-
          str_extract_all(spore.val.somewhere[[i]], 
                          pattern = extract.regex)
      }
      
      spore <- gsub("[a-wy-z]|µ", "", spore)
      spore <- trimws(spore)
      spore <- gsub("\\s?,\\s", "", spore)
      nam <- word(spore.val.somewhere[[i]], 1, 1)
      names(spore) <- nam
      spore.val[[i]] <- spore
    }
    spore.val <- unlist(spore.val, recursive = FALSE)
    names(spore.val.somewhere) <- names(spore.val)
    
    ## exclude very long results 
    del <- unlist(lapply(spore.val, length))
    spore.val <- spore.val[del<=20]
    spore.val.somewhere <- spore.val.somewhere[del<=20]
    
    spore.val.somewhere <- spore.val.somewhere[!duplicated(spore.val)]
    spore.val <- spore.val[!duplicated(spore.val)]
    
    if(length(grep("\\(0\\)", spore.val))>0){
      spore.val.somewhere <- spore.val.somewhere[-grep("\\(0\\)", spore.val)]
      spore.val <- spore.val[-grep("\\(0\\)", spore.val)]
    }
    
    ## delete Basidia results
    
    res <- list(spore.val = spore.val, txt = spore.val.somewhere)
  }
  return(res)
}



## Extract spores ---------------------------------------
d <- readRDS("Downloads/mycobank_descriptions.RDS")

## Test the function

# s <- sample(nrow(d), 1)
# get_spore_measure(d$description_description_[214],
#                   start.regex = "spores\\b|spore\\b|\\bSpore|Conidia\\b|uredosporis\\b",
#                   end.regex = "µm",
#                   extract.regex = ".[^a-wy-zA-WY-Z]+\\s?x\\s?.[^a-wy-zA-WY-Z]+")





## Run function on all discriptions

spore.dat <- d
sp <- lapply(spore.dat$description_description_, get_spore_measure, 
             start.regex = "spores\\b|spore\\b|\\bSpore|Conidia\\b|uredosporis\\b",
             end.regex = "µm",
             extract.regex = ".[^a-wy-zA-WY-Z]+\\s?x\\s?.[^a-wy-zA-WY-Z]+")
names(sp) <- paste(spore.dat$base_name, spore.dat$base__id, sep ="_")
sp <- sp[-grep("No spore measurements found", sp)]
sp.sp <- lapply(sp, function(x) x[[1]])


## clean names
nam <- unlist(lapply(sp.sp, names), use.names = F)
nam <- cbind(nam, str_extract(nam, ".+spore|.+sporis|spore|Spore|Conidia"))
nam[,2] <- gsub("'|\\(", "", nam[,2])
nam[,2] <- sapply(nam[,2], simpleCap)
sp.sp <- lapply(sp.sp, function(x) {
  names(x) <- nam[match(names(x), nam[,1]),2]
  x
})

## restructure data as table
sp.sp2 <- list()
for(i in 1:length(sp.sp)){
  x <- rbind.fill(lapply(sp.sp[[i]], function(y) { as.data.frame(t(y), stringsAsFactors=FALSE) }))
  x <- cbind(spore_type = names(sp.sp[[i]]), x)
  sp.sp2[[i]] <- x
}
names(sp.sp2) <- names(sp.sp)

## combine all spore results in one table
sp.df <- rbind.fill(lapply(sp.sp2, function(y) { as.data.frame(y) }))
sp.df <- cbind(spec = rep(names(sp.sp2), lapply(sp.sp2, nrow)), sp.df)
sp.df <- sp.df[,-length(sp.df)]
names(sp.df)[3] <- "measure_orig"
head(sp.df, 30)

sp.df$measure_red <- gsub('"', "", sp.df$measure_orig)
sp.df$measure_red <- gsub("\\(.*?\\)", "", sp.df$measure_red)
sp.df$measure_red <- trimws(sp.df$measure_red)
sp.df$measure_red <- gsub("\\s", "", sp.df$measure_red)
sp.df$measure_red <- gsub("[a-wy-zA-WY-Z]", "", sp.df$measure_red)

## new algorythm to seperate spore values into categories

nr.brackets <- 
  unlist(lapply(lapply(sp.df$measure_red, str_extract_all, pattern = "\\(|\\)"), function(x) length(x[[1]])))

means_given <- 
  unlist(lapply(lapply(sp.df$measure_red, str_extract_all, pattern = "\\\\"), function(x) length(x[[1]])))
means_given <- (means_given>0)*1
nr.brackets[means_given==1] <- "mean"

globose <- 
  unlist(lapply(lapply(sp.df$measure_red, str_extract_all, pattern = "x"), function(x) length(x[[1]])))
globose <- (!globose)*1
nr.brackets[globose==1] <- "globose"

sp.df <- split(sp.df, factor(nr.brackets))

## for those without brackets
############################################################
x0 <- rbind.fill(lapply(strsplit(sp.df$`0`$measure_red, "x"), 
                        function(y) { as.data.frame(t(y), stringsAsFactors=FALSE) }))
x0[x0 == ""] <- NA
x0$V2[is.na(x0[,2])] <- x0$V1[is.na(x0[,2])]
x0$V1[is.na(x0[,1])] <- x0$V2[is.na(x0[,1])]

x0 <- apply(x0, 2, trimws)
x0 <- apply(x0, 2, gsub, pattern = "^-+|-$", replacement = "")

x0 <- apply(x0, 2, function(x){
  rbind.fill(lapply(strsplit(x, "-"), 
                    function(y) { as.data.frame(t(y), stringsAsFactors=FALSE) }))
})

## fill single values everywhere
x0 <- lapply(x0, function(x){
  x[x == ""] <- NA
  x[,2][is.na(x[,2])] <- x[,1][is.na(x[,2])]
  x[,1][is.na(x[,1])] <- x[,2][is.na(x[,1])]
  return(x)
})

x0 <- do.call(cbind, x0)
x0 <- x0[, colSums(!apply(x0, 2, is.na))/nrow(x0)>0.9]
names(x0) <- c("length_min", "length_max", "width_min", "width_max")
x0 <- apply(gsub(",", "\\.", apply(x0, 2, trimws)), 2, as.numeric)
x0 <- cbind(spec = sp.df$`0`, x0)
head(x0, 100)


## save to excell file
write.xlsx2(x0, file =  paste("Desktop/mycobank_spore_extract_", Sys.Date(), ".xls", sep =""))
