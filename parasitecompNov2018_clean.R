#ejd 2018

#for parasitology retreat

################################################
#Analysing data that you download 500 at a time#
################################################

#First up set your R up by loading the required packages and setting the working directory. Change the path in setwd to where you have saved your files.

#If you failed to follow Eddy's instructions and did not install the packages already you will need to install them also.

#unhash this if you can't follow instructions
#install.packages('bibliometrix') 
#install.packages('tidyverse') 
#install.packages("wordcloud")
#install.packages('tm')
#install.packages('RColorBrewer')

#load packages
library(bibliometrix)
library(tidyverse)
library(wordcloud)
library(tm)
library(RColorBrewer)
#change to your working directory
setwd("~/Documents/CoAuthorMS/parasitebibsearch/parasitesonly/")

#We are going to use the bibliometrix package to load our many files from the downloads in but first we are just going to modify a function from that package to streamline the read in of files (dont worry about what is going on here)

readFilesmod<-function (...) 
{
  arguments <- as.list(...) 
  k = length(arguments)
  D = list()
  enc = "UTF-8"
  for (i in 1:k) {
    D[[i]] = suppressWarnings(readLines(arguments[[i]], encoding = enc))
  }
  D = unlist(D)
  return(D)
}


#Now we are going to read our data in using the function from above. This will generate a huge character file which is horible to look at. We will use a function from the bibliometrix package to turn this into a table.

file_list<-list.files(pattern='*.bib',full.names=T)
citations<-readFilesmod(dput(as.character(file_list))) 
citations_df <- convert2df(citations, dbsource = "isi", format = "bibtex")

#Now we have a dataframe-much easier!
#You can view this to see the various feilds with the authors etc.
View(citations_df)

#We can also subset this dataset here: 
#If we only wanted to look at records from 2000-2005 (year is under PY in the dataframe-publication year).
citations_df_2000_2005<-citations_df %>% filter(between(PY, 2000, 2005))

#We are going to use the function biblioanalysis to turn our dataframe into a object of various tables. None of these statistics are particularily hard to generate or special-it just does it in one hit which is nice! This function builds an object with various dataframes stored in it (23 dataframes). To access a dataframe you can call it directly. Remember you can view the help file for a function at anytime in Rstudio e.g. ?biblioAnalysis

#This transformation drops some information so we will go back to the orginal table every now and then-depending on what we want to do. 
citations_ana <- biblioAnalysis(citations_df, sep = ";")
#you can call them directly with:
head(citations_ana$Years)
#Most of them are just straight forward transformation, e.g.
head(citations_ana$TotalCitation)
#is just this column from the orginal dataframe
head(citations_df$TC)

#The handy thing about turning it into a bibliometrix object is that use can use the summary function on the bibliometrix object. You can set the number of entries to return by changing the k = X

citations_ana.sum <- summary(object = citations_ana, k = 100, pause = FALSE)

#We may want to save the top 10 countries that have published for this dataset

citations_ana.sum$MostProdCountries

#bar chart of top 10 countries
df_count<-data.frame(Country=as.character(citations_ana.sum$MostProdCountries$`Country  `),Article_count=as.integer(citations_ana.sum$MostProdCountries$Articles)) %>% slice(.,1:10)

View(df_count)

ggplot(df_count, aes(Country, Article_count)) +
  geom_bar(stat = "identity",fill=brewer.pal(10, "Spectral")) +
  coord_flip() +
  theme_bw() 

#with everyone else category
vec<-as.data.frame(citations_ana$Countries,stringsAsFactors = F) %>% filter(!Tab %in% trimws(as.character(df_count$Country),which = c("both", "left", "right"))) %>% select(.,Freq) %>% sum()
vec2<-data.frame(Country='OTHER',Article_count=as.integer(vec))
df_count<-rbind(df_count,vec2)

ggplot(df_count, aes(Country, Article_count)) +
  geom_bar(stat = "identity",fill=brewer.pal(11, "Spectral")) +
  coord_flip() +
  theme_bw() 

#write it out as a table
write.table(citations_ana.sum$MostProdCountries,'TopProducingCountriesForAllozymeParasiteSearch',row.names=F,quote=F,sep='\t')

#We are interested in how many papers are produced per year - we can see that in the summary file. We can also calculate the length of time it took for X number of publications

citations_ana.sum$AnnualProduction
#to see when XX % of papers were published
table<-citations_ana.sum$AnnualProduction %>% mutate(cumsum=cumsum(Articles),cumper=cumsum(Articles)/sum(Articles)*100)
write.table(table,'ProductionPerYearForAllozymeParasiteSearch',row.names=F,quote=F,sep='\t')

#basic line graph
ggplot(citations_ana.sum$AnnualProduction, aes(`Year   `,Articles, group=1)) +
  geom_line(aes(`Year   `,Articles))

ggplot(citations_ana.sum$AnnualProduction, aes(`Year   `,Articles, group=1)) +
  geom_point(aes(citations_ana.sum$AnnualProduction$`Year   `,citations_ana.sum$AnnualProduction$Articles), size = 3,colour='red') +
  geom_line(aes(`Year   `,Articles)) +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#create some splines to smooth the curve
spline_int <- as.data.frame(spline(citations_ana.sum$AnnualProductio$`Year   `, citations_ana.sum$AnnualProduction$Articles))

#just make it look a bit prettier
ggplot(citations_ana.sum$AnnualProduction) + 
  geom_point(aes(citations_ana.sum$AnnualProduction$`Year   `,citations_ana.sum$AnnualProduction$Articles), size = 3) +
  geom_line(data = spline_int, aes(x,y)) +
  geom_area(data = spline_int, aes(x,y,fill='red'),alpha=0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset") +
  scale_fill_manual(labels = "Parasites", values = alpha("red",.6))

#We also want to do some word clouds. There is two ways to use word clouds. The first way is that we treat every word in the keyword list individually. There is two types of keywords. The first is the Author Keywords, the second is the 'Keywords-Plus' which is generated by WoS. Im going to use the Author Keywords but commented in is the lines for the 'Keywords-Plus'.

#For author keywords:
citations_ana.sum$MostRelKeywords$`Author Keywords (DE)     `

#For WoS keywords:
citations_ana.sum$MostRelKeywords$`Keywords-Plus (ID)    `

#Here we will build a new dataframe of our keywords and filter out the stuff we dont want before creating a corpus. Which is a sort of list used by text mining packages in R. Im not totally sure what is special about it - but we need it!
  
#I have done this example of the allozyme data and have tried roughly to filter out method specific terms. If you are in the allozyme group you will need to remove this and re-do this properly as I did this very quickly and may have over or under filtered!!

#careful with this table the biblio function has built a table which has two columns named 'Articles'
head(citations_ana.sum$MostRelKeywords)
colnames(citations_ana.sum$MostRelKeywords)

#get rid of punctuation and create data frame
forwordcloud<-as.data.frame(cbind(as.character(trimws(citations_ana.sum$MostRelKeywords$`Author Keywords (DE)     `, which = c("both", "left", "right"))),citations_ana.sum$MostRelKeywords[2]),stringsAsFactors=FALSE)
colnames(forwordcloud)<-c('keyword','count_papers')

#if we want to plot 'Keywords-Plus (ID)'
#forwordcloud<-as.data.frame(cbind(as.character(trimws(citations_ana.sum$MostRelKeywords$`Keywords-Plus (ID)    `, which = c("both", "left", "right"))),citations_ana.sum$MostRelKeywords[4]),stringsAsFactors=FALSE)

#dataframe:
head(forwordcloud)

#we want to drop the keywords we searched for from our dataframe
forwordcloud<- forwordcloud %>% filter(!grepl('allozyme|electrophoresis|isoenzyme|isozyme|rapd|carbonic anhydrase|aflp|creatine kinase|protein kinase|alkaline phosphatase|cytochrome P450|glutathione S-transferase|alcohol dehydrogenase|lactate dehydrogenase|catalase|aldehyde dehydrogenase|hexokinase|peroxidase|5 alpha-reductase',keyword,ignore.case = TRUE))

#create corpus
forwordcloud.Corpus<-Corpus(VectorSource(forwordcloud[rep(row.names(forwordcloud), forwordcloud$count_papers), 1]))

#can use the function inspect to display the information on the corpus
#inspect(forwordcloud.Corpus)

#we dont have any special characters but we could remove funky characters if we have them
#these will throw a warning but dont worry they dont mean anything here-its not dropping documents its because I used a vector source for the corpus and for whatever reason that generates a warning
#forwordcloud.Corpus<- tm_map(forwordcloud.Corpus, removePunctuation)
#forwordcloud.Corpus <- tm_map(forwordcloud.Corpus, removeNumbers)
#forwordcloud.Corpus <- tm_map(forwordcloud.Corpus, stripWhitespace)
#forwordcloud.Corpus <- tm_map(forwordcloud.Corpus, remove_stopwords) #with package 'tau' 
#forwordcloud.Corpus <- tm_map(forwordcloud.Corpus,content_transformer(tolower))
#qdap package offers other cleaning functions if we need them

#create wordclouds
wordcloud(forwordcloud.Corpus,scale=c(2.0,.6),max.words=30)

#make it pretty-look up brewer.pal for colour pallets https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf

wordcloud(forwordcloud.Corpus,colors=brewer.pal(8, "Dark2"),max.words=30,scale=c(2.0,.6))

#You can see that 'genetic' and 'genetics' come up. You can try to use the stemming function to look for the root of the word but I found this a bit ugly and ended up doing it by hand.

#steming function
#forwordcloud.Corpus <- tm_map(forwordcloud.Corpus,content_transformer(tolower))
#forwordcloud.doc<- tm_map(forwordcloud.Corpus, stemDocument, "english")
#wordcloud(forwordcloud.doc,colors=brewer.pal(8, "Dark2"))

#code to reduce redundancy by hand
forwordcloud<-forwordcloud %>%  mutate(fixkeyword=sub("GENETICS", "GENETIC", keyword)) 

forwordcloud.Corpus<-Corpus(VectorSource(forwordcloud[rep(row.names(forwordcloud), forwordcloud$count_papers), 3]))

wordcloud(forwordcloud.Corpus,colors=brewer.pal(8, "Dark2"),max.words=30,scale=c(2.2,.6))

#you can change the percentage that are rotated with the rot.per call
wordcloud(forwordcloud.Corpus,colors=brewer.pal(8, "Dark2"),max.words=30,rot.per=0,scale=c(1.8,.8))

#The second way to make a word cloud is to consider the whole phrase a word. I think this makes more sense but it may not be as nice to look at.

wordcloud(tolower(forwordcloud$keyword),as.numeric(forwordcloud$count_papers), colors="black",max.words=30,scale=c(2.0,.6))

#colours and font
wordcloud(tolower(forwordcloud$keyword),as.numeric(forwordcloud$count_papers), colors=brewer.pal(8, "Set1"),max.words=30,scale=c(2.0,.6))
wordcloud(tolower(forwordcloud$keyword),as.numeric(forwordcloud$count_papers), colors=brewer.pal(8, "Dark2"),vfont=c("script","bold"),max.words=30,rot.per=0,scale=c(2.0,.6))
wordcloud(tolower(forwordcloud$keyword),as.numeric(forwordcloud$count_papers), colors=brewer.pal(8, "Dark2"),family = "mono",font = 2,max.words=30,scale=c(2.0,.6))


#################################################################################
#Okay we have our parasite plots but really what we want is to compare these to the other search terms
#################################################################################

#This block will be if you are comparing parasite searches that you downloaded 500 at a time to the 'publication per regions' and 'publication per year' files that you downloaded from the WoS website for the general search terms (via the analyse function in WoS).

#Bring in the file that you brought down from WoS and set it up with our orginal file from above:
  
d1v2<-read.table('../broadersearchers/AllozymePerYearBroadSearch.txt', sep="\t",header=T,row.names=NULL)
head(d1v2)
colnames(d1v2) <- c("Year", "ArticlesGeneral","PercentArticles")
d1v2<-d1v2 %>%  arrange(.,Year) %>%  mutate(PercentPerYearGeneral=cumsum(ArticlesGeneral)/sum(ArticlesGeneral)*100) %>% select(.,-PercentArticles)
d1v2$Year <- as.character(d1v2$Year)

#have to merge them with earlier dataset
df1<-citations_ana.sum$AnnualProduction 
colnames(df1)
#this is a good example of how not to name column names-the biblio package adds a bunch of trailing white space which is super frustrating to work around
colnames(df1) <- c("Year", "ArticlesParasite")
df1<-df1 %>%  arrange(.,Year) %>%  mutate(PercentPerYearParasites=cumsum(ArticlesParasite)/sum(ArticlesParasite)*100)
df1$Year <- as.character(df1$Year)

dmerged<-full_join(d1v2,df1,by='Year',all=TRUE) 
head(dmerged)

#NA should be 0
dmerged[is.na(dmerged)] <- 0 
dmerged$Year<-as.integer(dmerged$Year)

#lets drop 2019 because its a bit of a dumb point
dmerged %>% select(.,ArticlesGeneral,ArticlesParasite,Year) %>% filter(.,Year!=2019) %>% tidyr::gather("id", "value", 1:2) %>% ggplot(aes(Year, value)) + 
  geom_point(aes(colour = factor(id)),size = 1) +
  geom_line(aes(colour = factor(id))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset",color = "Article Type\n") +
  scale_y_continuous(trans='sqrt')

#for splines
dmerged<-dmerged %>% filter(.,Year!=2019)
spline_int <- as.data.frame(spline(dmerged$Year, dmerged$ArticlesParasite))
spline_int2 <- as.data.frame(spline(dmerged$Year, dmerged$ArticlesGeneral))
spline_int$y[spline_int$y < 0] <- 0

ggplot(dmerged) + 
  geom_point(aes(dmerged$Year,dmerged$ArticlesGeneral), col='red',size = 1) +
  geom_point(aes(dmerged$Year,dmerged$ArticlesParasite), col='blue',size = 1) +
  geom_line(data = spline_int2, aes(x,y)) +
  geom_area(data = spline_int2, aes(x,y,fill='blue')) +
  geom_line(data = spline_int, aes(x,y)) +
  geom_area(data = spline_int, aes(x,y,fill='red')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset") +
  scale_fill_manual(labels = c("Everyone", "Parasites"), values = alpha(c("red", "blue"),.6)) +
  scale_y_continuous(trans='sqrt')


#And you can do the same sort of bar graphs for the regions from the file you download from WoS countries/regions tab 

df_count<-read.table('../broadersearchers/AllozymePerCountryBroadSearch.txt',header=T,row.names=NULL,sep='\t')
head(df_count)
#take top 10
df_count %>% arrange(.,desc(records)) %>% slice(.,1:10) %>% ggplot(., aes(Countries_Regions, records)) +
  geom_bar(stat = "identity",fill=brewer.pal(10, "Spectral")) +
  coord_flip() +
  theme_bw() 

#with everyone else category
vec<-df_count %>% arrange(.,desc(records)) %>% slice(.,11:nrow(.)) %>% select(.,records) %>% sum()
vec2<-data.frame(Countries_Regions='OTHER',records=as.integer(vec))
df_count<-df_count %>% arrange(.,desc(records)) %>% slice(.,1:10) %>% select(.,-percent_of_38597) %>% rbind(.,vec2)
ggplot(df_count, aes(Countries_Regions, records)) +
  geom_bar(stat = "identity",fill=brewer.pal(11, "Spectral")) +
  coord_flip() +
  theme_bw() 

#########################################################
#Comparing parasites to general searches via API download
#########################################################

#This block will be if you are comparing parasite searches that you downloaded 500 at a time to the publication per regions and publication per year files that you downloaded using the API.

#Bring in the file that you brought down using the API and set it up with our orginal file from above:

df_api<-read.table('../broadersearchers/AllozymeFromAPI.txt',header=F,row.names=NULL,sep='|',quote="",comment.char="",stringsAsFactors = F)

colnames(df_api) <- c("WoS_id", "Title","Year","Author","Journal","Keywords","Article_type")

head(df_api)
df_api_year<-count(df_api,Year) %>% arrange(.,Year) %>% mutate(PercentPerYearGeneral=cumsum(n)/sum(n)*100)
colnames(df_api_year) <- c("Year","ArticlesGeneral","PercentPerYearGeneral")
df_api_year
write.table(df_api_year,'../broadersearchers/ProductionPerYearForAllozymeGeneralSearch_api',row.names=F,quote=F,sep='\t')


df_api_year$Year <- as.character(df_api_year$Year)

#have to merge them with earlier dataset
df1<-citations_ana.sum$AnnualProduction 
colnames(df1) <- c("Year", "ArticlesParasite")
df1<-df1 %>%  arrange(.,Year) %>%  mutate(PercentPerYearParasites=cumsum(ArticlesParasite)/sum(ArticlesParasite)*100)
df1$Year <- as.character(df1$Year)

dmerged<-full_join(df_api_year,df1,by='Year',all=TRUE) 
head(dmerged)

#NA should be 0
dmerged[is.na(dmerged)] <- 0 
dmerged$Year<-as.integer(dmerged$Year)

#lets drop 2019 because its a bit of a dumb point
dmerged %>% select(.,ArticlesGeneral,ArticlesParasite,Year) %>% filter(.,Year!=2019) %>% tidyr::gather("id", "value", 1:2) %>% ggplot(aes(Year, value)) + 
  geom_point(aes(colour = factor(id)),size = 1) +
  geom_line(aes(colour = factor(id))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset",color = "Article Type\n") +
  scale_y_continuous(trans='sqrt')

#for splines
dmerged<-dmerged %>% filter(.,Year!=2019)
spline_int <- as.data.frame(spline(dmerged$Year, dmerged$ArticlesParasite))
spline_int2 <- as.data.frame(spline(dmerged$Year, dmerged$ArticlesGeneral))
spline_int$y[spline_int$y < 0] <- 0

ggplot(dmerged) + 
  geom_point(aes(dmerged$Year,dmerged$ArticlesGeneral), col='red',size = 1) +
  geom_point(aes(dmerged$Year,dmerged$ArticlesParasite), col='blue',size = 1) +
  geom_line(data = spline_int2, aes(x,y)) +
  geom_area(data = spline_int2, aes(x,y,fill='blue')) +
  geom_line(data = spline_int, aes(x,y)) +
  geom_area(data = spline_int, aes(x,y,fill='red')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset") +
  scale_fill_manual(labels = c("Everyone", "Parasites"), values = alpha(c("red", "blue"),.6)) +
  scale_y_continuous(trans='sqrt')

#We can do a word cloud using the keywords from the API download
head(df_api$Keywords)
#treating each word individually, just spliting up mutlple key words into individual rows and removing search term words as before
df_api_Keywords <- df_api %>% select(.,Keywords) %>% separate_rows(.,Keywords,sep=",") %>% na.omit() %>% filter(!grepl('allozyme|electrophoresis|isoenzyme|isozyme|rapd|carbonic anhydrase|aflp|creatine kinase|protein kinase|alkaline phosphatase|cytochrome P450|glutathione S-transferase|alcohol dehydrogenase|lactate dehydrogenase|catalase|aldehyde dehydrogenase|hexokinase|peroxidase|5 alpha-reductase',Keywords,ignore.case = TRUE))

#needs a bit more cleaning for punctuation etc ignore warnings here
forwordcloud.Corpus<-Corpus(VectorSource(df_api_Keywords))
forwordcloud.Corpus<- tm_map(forwordcloud.Corpus, removePunctuation)
forwordcloud.Corpus <- tm_map(forwordcloud.Corpus, removeNumbers)
forwordcloud.Corpus <- tm_map(forwordcloud.Corpus, stripWhitespace)
forwordcloud.Corpus <- tm_map(forwordcloud.Corpus,content_transformer(tolower))

#inspect(forwordcloud.Corpus)

#limit word output
wordcloud(forwordcloud.Corpus,max.words = 50,scale=c(2.2,.6))
wordcloud(forwordcloud.Corpus,max.words = 50,colors=brewer.pal(8, "Dark2"),scale=c(2.2,.6))
#may need some more cleaning of terms

#Again we can also consider each term as a single phrase as well

df_api_Keywords_count<-df_api_Keywords %>% count(Keywords) %>% arrange(.,desc(n)) %>% slice(.,1:100) %>% filter(!grepl(1,Keywords))

wordcloud(tolower(df_api_Keywords_count$Keywords),as.numeric(df_api_Keywords_count$n), colors="black",max.words=30,scale=c(2.1,.6))

#again we have some double ups
df_api_Keywords_count<-df_api_Keywords_count %>%  mutate(fixkeyword=sub("Genetic diversity", "genetic diversity", Keywords)) %>% mutate(fixkeyword=sub("Antioxidant enzymes", "antioxidant enzymes", fixkeyword)) %>% group_by(.,fixkeyword) %>% summarise(n = sum(n)) %>% arrange(.,desc(n)) 

#colours and font
wordcloud(tolower(df_api_Keywords_count$fixkeyword),as.numeric(df_api_Keywords_count$n), colors=brewer.pal(8, "Set1"),max.words=30,scale=c(1.9,.6))
wordcloud(tolower(df_api_Keywords_count$fixkeyword),as.numeric(df_api_Keywords_count$n), colors=brewer.pal(8, "Dark2"),vfont=c("script","bold"),max.words=30,rot.per=0,scale=c(1.9,.6))
wordcloud(tolower(df_api_Keywords_count$fixkeyword),as.numeric(df_api_Keywords_count$n), colors=brewer.pal(8, "Dark2"),family = "mono",font = 2,max.words=30,scale=c(1.7,.6))

###########################################
#Comparing your different parasite searches
###########################################

#In your searches you will end up with multiple sets of downloads for parasites sets from WoS (using the 500 at a time approach). Make sure you move these into seperate folders for each search term so they dont get mixed up!
  
#  To bring in a second set of .bib files follow what we did above again.
file_list2<-list.files(path='../nonmedicalparasites/',pattern='*.bib',full.names=T)
citations_nonmed<-readFilesmod(dput(as.character(file_list2))) 
citations_nonmed_df <- convert2df(citations_nonmed, dbsource = "isi", format = "bibtex")
citations_nomed_ana <- biblioAnalysis(citations_nonmed_df, sep = ";")
citations_nomed_ana.sum <- summary(object = citations_nomed_ana, k = 100, pause = FALSE)

#bar chart of top 10 countries
df_count_nomed<-data.frame(Country=as.character(citations_nomed_ana.sum$MostProdCountries$`Country  `),Article_count=as.integer(citations_nomed_ana.sum$MostProdCountries$Articles)) %>% slice(.,1:10)

ggplot(df_count_nomed, aes(Country, Article_count)) +
  geom_bar(stat = "identity",fill=brewer.pal(10, "Spectral")) +
  coord_flip() +
  theme_bw() 

#with everyone else category
vec<-as.data.frame(citations_nomed_ana$Countries,stringsAsFactors = F) %>% filter(!Tab %in% trimws(as.character(df_count_nomed$Country),which = c("both", "left", "right"))) %>% select(.,Freq) %>% sum()
vec2<-data.frame(Country='OTHER',Article_count=as.integer(vec))
df_count_nomed<-rbind(df_count_nomed,vec2)

ggplot(df_count_nomed, aes(Country, Article_count)) +
  geom_bar(stat = "identity",fill=brewer.pal(11, "Spectral")) +
  coord_flip() +
  theme_bw() 

#write it out as a table
write.table(citations_nomed_ana.sum$MostProdCountries,'../nonmedicalparasites/TopProducingCountriesForAllozymeNonMediacalParasiteSearch',row.names=F,quote=F,sep='\t')

#to see when XX % of papers were published
table<-citations_nomed_ana.sum$AnnualProduction %>% mutate(cumsum=cumsum(Articles),cumper=cumsum(Articles)/sum(Articles)*100)
table
write.table(table,'../nonmedicalparasites/ProductionPerYearForNonMedicalParasites',row.names=F,quote=F,sep='\t')


ggplot(citations_nomed_ana.sum$AnnualProduction, aes(`Year   `,Articles, group=1)) +
  geom_point( size = 3,colour='red') +
  geom_line() +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#create some splines to smooth the curve
spline_int <- as.data.frame(spline(citations_nomed_ana.sum$AnnualProductio$`Year   `, citations_nomed_ana.sum$AnnualProduction$Articles))

ggplot(citations_nomed_ana.sum$AnnualProduction) + 
  geom_point(aes(citations_nomed_ana.sum$AnnualProduction$`Year   `,citations_nomed_ana.sum$AnnualProduction$Articles), size = 1) +
  geom_line(data = spline_int, aes(x,y)) +
  geom_area(data = spline_int, aes(x,y,fill='red'),alpha=0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset") +
  scale_fill_manual(labels = "Parasites", values = alpha("red",.6))

#words cloud
forwordcloud_nomed<-as.data.frame(cbind(as.character(trimws(citations_nomed_ana.sum$MostRelKeywords$`Author Keywords (DE)     `, which = c("both", "left", "right"))),citations_nomed_ana.sum$MostRelKeywords[2]),stringsAsFactors=FALSE)
colnames(forwordcloud_nomed)<-c('keyword','count_papers')

forwordcloud_nomed<- forwordcloud_nomed %>% filter(!grepl('allozyme|electrophoresis|isoenzyme|isozyme|rapd|carbonic anhydrase|aflp|creatine kinase|protein kinase|alkaline phosphatase|cytochrome P450|glutathione S-transferase|alcohol dehydrogenase|lactate dehydrogenase|catalase|aldehyde dehydrogenase|hexokinase|peroxidase|5 alpha-reductase',keyword,ignore.case = TRUE))

#create corpus
forwordcloud_nomed<-forwordcloud_nomed %>%  mutate(fixkeyword=sub("GENETICS", "GENETIC", keyword)) 

forwordcloud_nomed.Corpus<-Corpus(VectorSource(forwordcloud_nomed[rep(row.names(forwordcloud_nomed), forwordcloud_nomed$count_papers), 3]))

wordcloud(forwordcloud_nomed.Corpus,colors=brewer.pal(8, "Dark2"),max.words=30,scale=c(2.2,.6))

#whole phrases
wordcloud(tolower(forwordcloud_nomed$keyword),as.numeric(forwordcloud_nomed$count_papers), colors=brewer.pal(8, "Set1"),max.words=30,scale=c(1.3,.6))
wordcloud(tolower(forwordcloud_nomed$keyword),as.numeric(forwordcloud_nomed$count_papers), colors=brewer.pal(8, "Dark2"),vfont=c("script","bold"),max.words=30,rot.per=0,scale=c(1.5,.6))
wordcloud(tolower(forwordcloud_nomed$keyword),as.numeric(forwordcloud_nomed$count_papers), colors=brewer.pal(8, "Dark2"),family = "mono",font = 2,max.words=30,scale=c(1.3,.6))


#Combining three into a plot by years

#Here Im just joining it on the to dmerged file that we used earlier. It already has the parasite + broadscale search we just need to add non-medical parasites.

df_nonmed<-citations_nomed_ana.sum$AnnualProduction 
colnames(df_nonmed) <- c("Year", "ArticlesParasite_nonmed")
df_nonmed<-df_nonmed %>%  arrange(.,Year) %>%  mutate(PercentPerYearParasites_nonmed=cumsum(ArticlesParasite_nonmed)/sum(ArticlesParasite_nonmed)*100)
df_nonmed$Year <- as.character(df_nonmed$Year)

head(dmerged)
dmerged$Year <- as.character(dmerged$Year)
dmerged3<-full_join(dmerged,df_nonmed,by='Year',all=TRUE) 
dmerged3[is.na(dmerged3)] <- 0 
dmerged3$Year<-as.integer(dmerged3$Year)

#lets drop 2019 because its a bit of a dumb point
dmerged3 %>% select(.,ArticlesGeneral,ArticlesParasite,ArticlesParasite_nonmed,Year) %>% filter(.,Year!=2019) %>% tidyr::gather("id", "value", 1:3) %>% ggplot(aes(Year, value)) + 
  geom_point(aes(colour = factor(id)),size = 1) +
  geom_line(aes(colour = factor(id))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset",color = "Article Type\n") +
  scale_y_continuous(trans='sqrt')

#for splines
dmerged3<-dmerged3 %>% filter(.,Year!=2019)
spline_int <- as.data.frame(spline(dmerged3$Year, dmerged3$ArticlesParasite))
spline_int2 <- as.data.frame(spline(dmerged3$Year, dmerged3$ArticlesGeneral))
spline_int3 <- as.data.frame(spline(dmerged3$Year, dmerged3$ArticlesParasite_nonmed))
spline_int$y[spline_int$y < 0] <- 0
spline_int3$y[spline_int3$y < 0] <- 0

ggplot(dmerged3) + 
  geom_point(aes(dmerged3$Year,dmerged3$ArticlesGeneral), col='red',size = 1) +
  geom_point(aes(dmerged3$Year,dmerged3$ArticlesParasite), col='blue',size = 1) +
  geom_point(aes(dmerged3$Year,dmerged3$ArticlesParasite_nonmed), col='green',size = 1) +
  geom_line(data = spline_int2, aes(x,y)) +
  geom_area(data = spline_int2, aes(x,y,fill='blue')) +
  geom_line(data = spline_int, aes(x,y)) +
  geom_area(data = spline_int, aes(x,y,fill='red')) +
  geom_line(data = spline_int3, aes(x,y)) +
  geom_area(data = spline_int3, aes(x,y,fill='green')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset") +
  scale_fill_manual(labels = c("Everyone", "Parasites not medical","Parasites"), values = alpha(c("red", "green","blue"),.6)) +
  scale_y_continuous(trans='sqrt')

###################################
#Just some ideas for playing around
###################################

#You should have a play around with the data and see what you can see. I have just given you some broad ideas-explore the data in your own way....
#The files from the 500 at a time download have a lot of other metadata that you could explore

#depending on how the data comes down we can look at other things as well
head(citations_df$TC)
dmerged.violin<-data.frame(citationcount=citations_df$TC,type='Parasite Search')
dmerged.violin<-rbind(dmerged.violin,data.frame(citationcount=citations_nonmed_df$TC,type='Parasite no medical'))

ggplot(dmerged.violin,aes(type,citationcount))  +
  geom_violin(aes(fill = factor(type))) +
  scale_y_continuous(trans='sqrt')+
  labs(title="Citation count per article",x='Search Group', y=expression(sqrt(italic('Citation Count'))), fill="Subset") +
  theme_bw()+
  scale_fill_manual(values = alpha(c("red", "blue"),.6)) 

#could look at citation over the years
para_citationperyear<-citations_df %>% select(.,PY,TC) %>% group_by(PY) %>% tally(TC)
nomed_citationperyear<-citations_nonmed_df %>% select(.,PY,TC) %>% group_by(PY) %>% tally(TC)
colnames(para_citationperyear) <- c("Year", "CitationParasites")
colnames(nomed_citationperyear) <- c("Year", "CitationParasitesNoMedical")
dmerged.citationPY<-full_join(para_citationperyear,nomed_citationperyear,by='Year')
dmerged.citationPY[is.na(dmerged.citationPY)] <- 0 
head(dmerged.citationPY)

dmerged.citationPY %>% filter(.,Year!=2019) %>% tidyr::gather("id", "value", 2:3) %>%  ggplot(aes(Year, value)) + 
  geom_point(aes(colour = factor(id)),size = 1) +
  geom_line(aes(colour = factor(id))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Allozymes",x='Year', y='Citation Count', fill="Subset",color = "Article Type\n") +
  scale_y_continuous(trans='sqrt')





