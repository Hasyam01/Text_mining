library("NLP")
library("tm")
library("RColorBrewer")
library("wordcloud")
library("SnowballC")

######Chargement du texte######
filepath="C:/Users/Samsung UltraBook i5/Downloads/text_mining.txt"
text= readLines(filepath)
text
docs= Corpus(VectorSource(text))
inspect(docs)
docs

#####Prétraitement du texte :######
####Remplacer les caractères spéciaux non utiles. Par exemple : remplacer “/”, “@” et “|”avec un espace#####
toSpace <- content_transformer(function (x ,pattern) gsub(pattern," ", x))
toSpace
docs <- tm_map(docs, toSpace, "/")
inspect(docs)
docs <- tm_map(docs, toSpace, "@")
inspect(docs)
docs <- tm_map(docs, toSpace, "\\|")
inspect(docs)
#####Convertir le texte en minuscule######
docs <- tm_map(docs, content_transformer(tolower)) 
inspect(docs)
####Supprimer les nombres#####
docs <- tm_map(docs, removeNumbers)
inspect(docs)
####Supprimer les mots vides anglais#####
docs <- tm_map(docs, removeWords, stopwords("english"))
inspect(docs)
###Supprimer des stop-words####
docs <- tm_map(docs, removeWords, c("[Ll]e", "[Uu]n" ,"[Ll]a"))
inspect(docs)
#####Supprimer des stop-words à partir d’une liste personnalisée####
mystopwords <- c(stopwords("french"), "les", "tout", "avoir", "être", "de")
mystopwords
docs <- tm_map(docs, removeWords, mystopwords)
docs
inspect(docs)
#####Supprimer les ponctuations #####
docs <- tm_map(docs, removePunctuation) 
inspect(docs)
#####Supprimer les espaces vides supplémentaires####
docs <- tm_map(docs, stripWhitespace)
inspect(docs)

######Construction de la matrice des mots par documents#######
dtm <- TermDocumentMatrix(docs)
dtm
m <- as.matrix(dtm) 
m
v <- sort(rowSums(m),decreasing=TRUE)
v
d <- data.frame(word = names(v),freq=v)
d
head(d, 10)
head(d,20)
head(d,30)

#######Exploration de la fréquence des mots ######
#####Afficher les mots dont la fréquence d’apparition dans le texte est supérieurs à 4#####
findFreqTerms(dtm, lowfreq = 4)
####Afficher la liste des mots qui sont associés au mot « fouille » avec un taux de corrélation supérieurs à 0.35####
findAssocs(dtm, terms = "fouille", corlimit = 0.3)
######Représenter par un graphe les fréquences des 10 mots les plus fréquents###
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col ="lightblue", main 
        ="Mots les plus fréquents", ylab = "Fréquence par mot")

#### Génération du nuage des mots######
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
######Stemming######
docs <- tm_map(docs, stemDocument)
inspect(docs)
