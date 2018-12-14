###############################Data_Mining_Assignment ##################
############################### Thomas Varghese ##################


#Initial data gathering
install.packages('devtools')
library(devtools)
install.packages('httr')
library(httr)
install.packages('jsonlite')
library(jsonlite)
#Structured data
data3<-GetAnswers("rohan123test","Rohan@123",c("5b76ae91c474540e2c6b1f6c", "5b99595c760d68155cf7594b","5b995952760d68155cf75874","5b995945760d68155cf7579f","5b99593b760d68155cf756ca","5b99592f760d68155cf755f7"), "2018-06-01", "2018-08-31", 2000, TRUE)
#Unstructured data
data4<-GetAnswers("rohan123test","Rohan@123",c("5b7be56502dedf1cbca94aa3"), "2018-06-01", "2018-08-31", 2000, TRUE)
#Renaming column names of the structured data
names(data3)<- c("NPS","Requirements","Ambience","Courteousness","Quality","Speed")
View(data3)


####################################################################
#Q1) To find out which attributes have significant changes

str(data3)
table(data3$NPS)
install.packages("ggplot2")
library(ggplot2)
ggplot(data3, aes(factor(NPS))) + geom_bar()
ggplot(data3, aes(factor(Requirements))) + geom_bar()
ggplot(data3, aes(factor(Courteousness))) + geom_bar()
ggplot(data3, aes(factor(Ambience))) + geom_bar()
ggplot(data3, aes(factor(Quality))) + geom_bar()
ggplot(data3, aes(factor(Speed))) + geom_bar()

#Even though all the variaables are of integer class in nature, the attributes have a following categorical
#Characteristic in the data. The reason being the data is a feedback data ehich is traditionally categorical in nature.Hence, to plot the lines using scatter or line graph is not easy.
#hence using Bar graph data gives us better perspective. As per the data and the graphs, the quality attribute has been showing 
#signifiant changes.

###################################################################
#Q2)What were the contributing factors towards the change?

#The reason for the significant change is probably the increase in the quality measure of the corporation 
#The attributes of the data complements each other. Hence all the attributes help in contributing the success factors
#of the change


###################################################################
#Q3)What impact do these attributes have on the NPS

#NPS(Net Promoter score is evaluated by taking all the attributes into consideration, totaling it, taking 
#percentage and determing NPS Criteria by subtraction method
#Hence, every attribute has a significant contribution in determing NPS
#However, in here, Courteousness has a marginally higher correlation with the NPS which primarily means that
#Courteousness has a better contribution rate

cor(data3$NPS,data3$Requirements)
cor(data3$NPS,data3$Ambience)
cor(data3$NPS,data3$Quality)
cor(data3$NPS,data3$Courteousness)
cor(data3$NPS,data3$Speed)

linearMod <- lm(NPS ~ Courteousness, data=data3)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

#Therefore putting courteousness as the predicted variable in the linear model  determines a good but 
#not a better value in terms of relationship between NPS and Quality(R-Squared having a 
#marginally better value(>0.70)) 

###################################################################
#Q4) to find frequent phrases(keywords) used by customers

#install packages for data mining unstructured data
install.packages("readr")
library(readr)
install.packages("tm")
library(tm)
install.packages("RWeka")
library(RWeka)
install.packages("SnowballC")
library(SnowballC)

#Create a corpus
Comdocs<-VectorSource(data4)
View(Comdocs)
Comdocs$names<-c(names(data4))
Comdocs1<-Corpus(Comdocs)
View(Comdocs1)
View(data4)
comextr<-data.frame(text=sapply(Comdocs1, as.character),stringsAsFactors = FALSE)
comextr

#Eliminate variables that won't be necessarily useful using regular expression, lower case,  remove punctuation,
#stopwords and whitespace

remspchars<-function(x) gsub("[^a-zA-Z0-9]"," ",x)
comdocs2<-tm_map(Comdocs1,remspchars)

comdocs2<-tm_map(comdocs2,content_transformer(tolower))

comdocs2<-tm_map(comdocs2,content_transformer(removePunctuation))
comdocs2<-tm_map(comdocs2,removeWords,stopwords('en'))
stopwords()
comdocs2<-tm_map(comdocs2,stripWhitespace)
class(comdocs2)

comextr<-data.frame(text=sapply(comdocs2, as.character), stringsAsFactors = FALSE)
View(comextr)

#comextr important

#Make another corpus for the new edited data
df<-data.frame(doc_id=1:2000, text=comextr$text,stringsAsFactors = FALSE)
tm<-VCorpus(DataframeSource(df))
View(tm)

#use ngram tokenizer to identify keywords. COnsists of unigrams(one word) or bigrams(two word)
tokenizer<-function(x) NGramTokenizer(x,Weka_control(min=1, max=2))
head(NGramTokenizer(comextr, Weka_control(min=1,max=2)))

#Make a term frequency matrix to determine the tokenized keywords
dtm<-TermDocumentMatrix(tm,control = list(tokenize=tokenizer))

inspect(dtm)#This command would help determine the no of frequent words

bigramDF<-as.data.frame(t(as.matrix(dtm)))
keywords<-colnames(bigramDF)
kw<-as.data.frame(keywords)

#filter keywords to find the frequent phrases
dtm_filtered<-dtm[c('comment','experience'),]
inspect(dtm_filtered)

m<-as.matrix(dtm)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word=names(v), freq=v)
View(d)

library(wordcloud)
wordcloud(d$word,d$freq, random.order = FALSE,rot.per = 0.3,scale = c(4,.5),max.words = 1245,colors = brewer.pal(8,"Dark2"))
#As shown in the wordcloud image, the data haveeing frequent keywords specified is customer with experience, 
#good and support coming behind as per the frequency.



#########################################################################
