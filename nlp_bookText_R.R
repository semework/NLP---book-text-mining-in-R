library(NLP)
library(openNLP)
library(tm)
library(RWeka)
library(magrittr)
library(RColorBrewer)
library(SnowballC)
library(wordcloud)

## #####    ###  #######    ###  #######    ###  #######    ###  ####### # 
#                                                                        #
#   This code imports text docs (here using a published novel),          # 
#   text-mines and wordcloud plots the top 'maxWords' most frequent      #
#   words (names, locations, etc.)                                       # 
#                                                                        # 
#   Mulugeta Semework May 30, 2017                                       #
#                                                                        #
#   Thank you: RStudio @ https://rpubs.com/lmullen/nlp-chapter           #
#                                                                        #
## #####    ###  #######    ###  #######    ###  #######    ###  ####### #  

maxFontSize = 300
maxWords = 100
# setpath ------------------------------
nlpPath = "...."
setwd(nlpPath)

# import/read text file ------------------------------
con <- file("LoveInAnotherCountry.txt", "r", blocking = FALSE)
bookTXT <- readLines("LoveInAnotherCountry.txt")
alltx = bookTXT
bookTXT <- paste(bookTXT, collapse = " ")
                                                                                                                          
bookTXT <- as.String(bookTXT)

# anotate ------------------------------
word_ann <- Maxent_Word_TknAntr()
sent_ann <- Maxent_Sent_TknAntr()
bookTXT_annotations <- annotate(bookTXT, list(sent_ann, word_ann))
bookTXT_doc <- AnnotatedPlainTextDocument(bookTXT, bookTXT_annotations)


person_ann <- Maxent_EnAntr(kind = "person")
location_ann <- Maxent_EnAntr(kind = "location")
organization_ann <- Maxent_EnAntr(kind = "organization")
pipelineAll <- list(word_ann)

pipeline <- list(sent_ann, word_ann, person_ann, location_ann,organization_ann)

bookTXT_annotations <- annotate(bookTXT, pipeline)
bookTXT_doc <- AnnotatedPlainTextDocument(bookTXT, bookTXT_annotations)

# extract entity ------------------------------
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

allTxt <- entities(bookTXT_doc)
Orgs  <- entities(bookTXT_doc, kind = "organization")
persn <- entities(bookTXT_doc, kind = "person")
locs <- entities(bookTXT_doc, kind = "location")

# -------------- PLOTS ------------------------------------------------------------
# title ------------------------------------------------------------
figTitle <-   " most frequent wordsin the novel: \n'Love in Another Country' "
figTitle <- paste("The top ", maxWords, figTitle)

graphics.off()
# set.seed(1002) 
png(file = "bookNLP_R.png", bg = "transparent")

par(mfrow = c(2, 2),  oma = c(1, 1, 1, 0), mar = c(3, 3, 0, 0), 
    mgp = c(1, 1, 0), xpd = NA,bg="black")  

# all text ----------------------------------------------------------

docs <- Corpus(VectorSource(alltx))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,res=maxFontSize,
          max.words= maxWords, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2") )
mtext("All words", line = 1,side = 1,
      cex.main = 1,   font.main= 4, col.main= "blue",col="white",
      cex.sub = 0.75, font.sub = 3, col.sub = "red")


# people names------------------------------------------------------------

docs <- Corpus(VectorSource(persn))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,res=maxFontSize,
          max.words= maxWords, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


mtext("Names", line = 1,side = 1,
      cex.main = 1,   font.main= 4, col.main= "blue",col="white",
      cex.sub = 0.75, font.sub = 3, col.sub = "red")

# geo-locations ------------------------------------------------------------

docs <- Corpus(VectorSource(locs))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


wordcloud(words = d$word, freq = d$freq, min.freq = 1,res=maxFontSize,
          max.words= maxWords, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
mtext("Locations", line = 1,side = 1,
      cex.main = 1,   font.main= 4, col.main= "blue",col="white",
      cex.sub = 0.75, font.sub = 3, col.sub = "red")

# organizations  -----------------------------------------------------
docs <- Corpus(VectorSource(Orgs))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud(words = d$word, freq = d$freq, min.freq = 1, res=maxFontSize,
          max.words= maxWords, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
mtext("Organizations", line = 1,side = 1,
      cex.main = 1,   font.main= 4, col.main= "blue",col="white",
      cex.sub = 0.75, font.sub = 3, col.sub = "red")

# main title  -----------------------------------------------------
title(figTitle,font = 2,cex = 2, line = -2, 
      cex.main = 1.5,   font.main= 2, col.main= "white",
      cex.sub = 0.75, font.sub = 3, col.sub = "red",outer=TRUE)

dev.off()