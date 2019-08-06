library(tidyverse)
library(stringr)

get_text<-function(txt,start.regex,end.regex){
  
    ## some character normalizations; mainly changing diverse comma seperations to comma
    txt <- gsub("\u2013", "-", txt) ## en dash to minus
    txt <- gsub("\u2014", "-", txt) ## en dash to minus
    txt <- gsub("–", "-", txt) ## en dash to minus
    txt <- gsub("\\s?-\\s?", "-", txt) ## en dash to minus
    txt <- gsub("×", "x", txt)
    txt <- gsub("±", "+-", txt)
    txt <- gsub('\n', ' ', txt)
    txt <- gsub('[a-zA-Z]µm[a-zA-Z]', 'um', txt)
    txt <- gsub('\\-[lI]', '-1', txt)
    txt <- gsub('\\-[lI]', '-1', txt)
    
    #get the start of the text
    starts <- txt %>%
      strsplit(split = " ") %>%
      unlist(recursive = FALSE) %>%
      grep(pattern = start.regex)
    
    ##get the end of potential spore text:
    stops <- txt %>%
      strsplit(split = " ") %>%
      unlist(recursive = FALSE) %>%
      grep(pattern = end.regex)
    
    #Select text ranges in between the words referring to spore and µm following the
    #pattern: "spore--------µm"
    rangos<-function(starts,stops){
      names(starts)<-starts
      names(stops)<-stops
      todos<-data.frame(outer(
        stops,starts,"-"))#,function(a){which(a==min(a[a>0]))})
      names(todos)<-names(starts)
      todos<-todos[!sapply(todos, function (a){all(a<=0)})]#Changed, originally it was: all(a<0)
      r<-data.frame(principio=names(sapply(todos,function(a){which(a==min(a[a>0]))})),
                    fin=stops[sapply(todos,function(a){which(a==min(a[a>0]))})],
                    stringsAsFactors = F)
      r}
    
    spore.end<-
      if(all(outer(starts,stops,">"))){spore.end<-NA}else{
        if(all(c(length(stops)!=0,length(starts)!=0))){
          
          spore.end<-rangos(starts,stops)}}#else{first.spore.end[[i]]<-NA}
    
    spore.val.somewhereA <- list()
    txt.splitA <- txt %>%
      strsplit(split = " ") %>% 
      unlist(recursive = FALSE)
    
    if(length(nrow(spore.end))!=0){
      for (j in 1:nrow(spore.end)) {
        spore.val.somewhereA[[j]] <-
          paste(
            txt.splitA[spore.end$principio[[j]]:spore.end$fin[[j]]], collapse = " ")
        
              }
    }else{spore.val.somewhereA<-"Result not found"}

    #Remove hymenophore entries: this applies only when extracting conidiophores
    # wds <- lapply(spore.val.somewhereA, word, start = 1, end = 1)
    # if(length(grep("w*ymenophore\\b", spore.val.somewhereA))>0)
    #   spore.val.somewhereA <- spore.val.somewhereA[-grep("w*ymenophore\\b", wds)]

    #spore.val.somewhereA<-unlist(spore.val.somewhereA, recursive = FALSE)
    Datos<-spore.val.somewhereA
    return(Datos)}
################################################################################################    
    
get_dimensions2<-
  function(txt,extract.regex){
    #To get the values within the text
    spore.val<-list()
    if(length(txt)>0)
      for(i in 1:length(txt)){
        sporeA<-str_extract_all(txt[[i]],
                                pattern = extract.regex)
        
        sporeA <- gsub("[a-wy-z]|µ", "", sporeA)
        sporeA <- trimws(sporeA)
        sporeA <- gsub("\\s?,\\s", "", sporeA)
        nam <- word(txt[[i]], 1, 1)
        names(sporeA) <- nam
        
        spore.val[[i]] <- sporeA}
    
    spore.val <- unlist(spore.val, recursive = FALSE)
    
    Datos<-list(values=spore.val)
    return(Datos)}
