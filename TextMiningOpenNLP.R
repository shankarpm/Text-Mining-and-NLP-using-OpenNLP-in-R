library(rJava)
library(RCurl)
library(XML) 
library(NLP)
library(openNLP)
library("openNLPmodels.en")
library(RWeka)
library(magrittr)
library(stringi)
#devtools::install_github("DataWookie/feedeR") 

# install.packages(c("NLP", "openNLP", "RWeka", "qdap"))
# install.packages("openNLPmodels.en",
#                  repos = "http://datacube.wu.ac.at/",
#                  type = "source")

 
generateUrl(Sys.Date() - 100,'http://www.reuters.com/finance/stocks/company-news/AAPL.O?date=')
generateUrl <- function(startdate, url)
{
  while(startdate != Sys.Date() )
  { 
    #print(paste(url,as.String(format(startdate, "%m%d%Y")),sep=""))
    print(paste(as.String(format(startdate, "%Y-%m-%d")),sep=""))
    startdate <- startdate + 1
  }
}
 
#url <- 'http://www.reuters.com/finance/stocks/company-news/AAPL.O?date=02012017'
 
getReutersContents <- function(url)
{
  doc <- getURL(url)
  html <- htmlParse(doc)
  news <-  xpathSApply(html,'//div[@id = "companyNews"]/div/div/div/p',xmlValue)
  news
}
 
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

convert_text_to_sentences <- function(text, lang = "en") {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  
  # Convert text to class String from package NLP
  text <- as.String(text)
  
  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  
  # Extract sentences
  sentences <- text[sentence.boundaries]
  
  # return sentences
  return(sentences)
}

FindMatchEntity <- function(array,entity)
{
  #array <- colnames(pestel.dic)
  #entity <- 'Apple1'
  for(name in array)
  {
  #  name <- array[1]
  #  any(grepl(name, c(entity)))
    if(any(grepl(name, c(entity))))
    {
     return(name)
    }
  }
  return('')
}
FindVerbsAndPersons <- function()
{
  df.entities <<- NULL
  todayDate <- as.String(Sys.Date())
  people.words <- pestel.dic[,'Actor_filter']
  people.words <- people.words[stri_length(people.words)> 1] # remove empty strings
  
  verb.words <- pestel.dic[,'Verb_filter']
  verb.words <- verb.words[stri_length(verb.words)> 1] # remove empty strings
  
  for(entity in url.Entity$Entity)
  {
  #  entity <-  as.character(url.Entity$Entity[1])
    # print(entity)
    url <- as.character(url.Entity[url.Entity$Entity == entity,c('Url')])
     
    todayDate <- as.character(url.Entity[url.Entity$Entity == entity,c('Date')])
   
    entity <-  FindMatchEntity(colnames(pestel.dic),entity) 
    
    entity.words <- ''
    if(nchar(entity) > 0) 
    {
     entity.words <- pestel.dic[,entity]
     entity.words <- entity.words[stri_length(entity.words)> 1] 
    }
    #url.contents  <- getURL(url)      #url.doc     <- xmlParse(url.contents)
    titles  <- getReutersContents(url)#  xpathSApply(url.doc,'//item/title',xmlValue)  
     #  class(entity.words) 
      if(nchar(as.character(entity.words))[1] > 0)
      {
        match.sentenses <- lapply(entity.words,function(match) { titles[grep(match,titles,ignore.case = TRUE)] }) 
        match.sentenses <- match.sentenses[lapply(match.sentenses,length)>0]
        match.sentenses <- unique(unlist(match.sentenses))
      }
      else
      {
        match.sentenses<- unique(unlist(titles))
      }
    for(sentense in match.sentenses)
    {
    #  sentense <- as.String(match.sentenses[2])
      sentense <- as.String(sentense)
     # print(sentense)
      #sentense <- as.String("i am the best Paul Ryan likes Trump Bush and Obama")
      word_ann <- Maxent_Word_Token_Annotator()
      sent_ann <- Maxent_Sent_Token_Annotator()
      person_ann <- Maxent_Entity_Annotator(kind = "person")
      sentence_annotations <- annotate(sentense, list(sent_ann, word_ann,person_ann))
      sentence.annotate.textDoc <- AnnotatedPlainTextDocument(sentense, sentence_annotations)
      
      person <- entities(sentence.annotate.textDoc, kind = "person") 
      sent.array <-  as.character(unlist(strsplit(sentense, " ")))
    
      match.people.words <-  intersect( sent.array, as.character(people.words))
       
      match.verb.words <-  intersect( tolower(sent.array), tolower(as.character(verb.words)))
      
      person.list <- paste("",as.character( c(person,match.people.words)),"",collapse=", ",sep="")
      #person.list <- paste(person, match.people.words, sep=sepChar,collapse = '')
       
     #person.list
      pos_tag_annotator <-Maxent_POS_Tag_Annotator()
      gc()
      annon.list <-annotate(sentense, pos_tag_annotator, sentence_annotations)
      annon.list <-subset(annon.list, type == "word")
      tags <-sapply(annon.list$features, '[[', "POS")
      taglist <- sprintf("%s/%s", sentense[annon.list], tags)
      taglist <-  gsub("/", ";", taglist, fixed=TRUE) # replace slash with semi
      #separate words and tags
      vblist <- unlist(lapply(taglist,function(s) { substr(s, 1, unlist(stri_locate_all(pattern = ';V', s, fixed = TRUE))[1]-1) } ))
      vblist <- vblist[!is.na(vblist)]
     
      vblist <- paste("",as.character( c(vblist,match.verb.words)),"",collapse=", ",sep="")
       
      if( nchar(person.list) > 0 || nchar(vblist) > 0 )
      {
        df.match.sentenses <- data.frame(cbind(Sentense = sentense),
                                         cbind(Entity = entity), cbind(Persons = person.list),
                                         cbind(Verbs = vblist),cbind(Date = todayDate))
        df.entities <<- rbind(df.entities,df.match.sentenses)
      }
    } 
  }
  
  colnames(df.entities) <<- c('Sentence','Entity', 'Persons','Verbs','Date')
  #head(df.entities)
} 

setwd("C:\\Users\\Shankar\\Documents\\R Docs\\Projects\\TextMiningOpenNLP")
dir.Name <- getwd()  
 
pestel.dic <- read.csv(paste(dir.Name,"\\PESTEL-dic.csv",sep=''))
url.Entity <- read.csv(paste(dir.Name,"\\FeedUrlEntity.csv",sep=''))
df.entities <- NULL
FindVerbsAndPersons()

#head(pestel.dic) 
#df.entities
write.csv(df.entities, paste(dir.Name,"\\",'PestelOutput.csv',sep=''),row.names = FALSE) 
  #http://stud ylib.net/doc/7157291/pos-tagging-in-r

 
# 
# entities(bio_doc, kind = "person")
# entities(bio_doc, kind = "location")
# entities(bio_doc, kind = "organization")
# 
 
