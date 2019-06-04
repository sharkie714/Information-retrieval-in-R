library(tidytext)
library(dplyr)
library(tm)
library(SnowballC)

df <- read.table("plotsummaries.txt", header = FALSE, sep="\t")
df_title <- data.frame(doc_id = df[,1], text = df[,2])
corpus <- VCorpus(DataframeSource(df_title))
inspect(corpus)

indexer <- df_title %>%
mutate(text=gsub("(http|https).+$|\\n|&amp|[[:punct:]]","",text),
rI=as.numeric(row.names(.))) %>%
select(text,rI)

docList <- as.list(df_title$text)
N.docs <- length(docList)

QS <- function(queryTerm) {
    
    # Record starting time to measure your search engine performance
    start.time <- Sys.time()
    
    # store docs in Corpus class which is a fundamental data structure in text mining
    my.docs <- VectorSource(c(docList, queryTerm))
    
    
    # Transform/standaridze docs to get ready for analysis
    my.corpus <- VCorpus(my.docs) %>%
    tm_map(stemDocument) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords,stopwords("en")) %>%
    tm_map(stripWhitespace)
    
    # Store docs into a term document matrix where rows=terms and cols=docs
    # Normalize term counts by applying TDiDF weightings
    term.doc.matrix.stm <- TermDocumentMatrix(my.corpus,
    control=list(
    weighting=function(x) weightSMART(x,spec="ltc"),
    wordLengths=c(1,Inf)))
    
    
    
    # Transform term document matrix into a dataframe
    term.doc.matrix <- tidy(term.doc.matrix.stm) %>%
    group_by(document) %>%
    mutate(vtrLen=sqrt(sum(count^2))) %>%
    mutate(count=count/vtrLen) %>%
    ungroup() %>%
    select(term:count)
    dM <- term.doc.matrix %>%
    mutate(document=as.numeric(document)) %>%
    filter(document<N.docs+1)
    qM <- term.doc.matrix %>%
    mutate(document=as.numeric(document)) %>%
    filter(document>=N.docs+1)
    
    
    
    
    
    # Calcualte top ten results by cosine similarity
    sRes <- dM %>%
    inner_join(qM,by=c("term"="term"),
    suffix=c(".doc",".query")) %>%
    mutate(termScore=round(count.doc*count.query,4)) %>%
    group_by(document.query,document.doc) %>%
    summarise(Score=sum(termScore)) %>%
    filter(row_number(desc(Score))<=10) %>%
    arrange(desc(Score)) %>%
    left_join(indexer,by=c("document.doc"="rI")) %>%
    ungroup() %>%
    rename(Result=text) %>%
    select(Result,Score) %>%
    data.frame()
    
    
    # Record when it stops and take the difference
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time,4)
    print(paste("Used",time.taken,"seconds"))
    
    return(sRes)
    
}
#here we have to give the data.
#usually in R we cannot give the data in middle. we should give the values initially as command line argument.
#when q is given for query. It exits.
QS("funny movie")
