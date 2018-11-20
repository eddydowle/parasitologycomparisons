#EJD 
#13/11/2018

#make R markdown

#To get the files there are three options:

#option one: full download from the web
#from the website
#put in search
#hit add to marked list on first seach return page
#put in 1 to the total number of searches returned to save them all
#select marked list on the top left (should now have the number of records saved)
#step 1 select the span of records to download 1:500, 501:1000 etc etc (500 at a time)
#step 2 on output options choose 'select all' for information brought down
#step 3 choose save to other file formats and then select bibTex
#this is the most datarich option, it will bring down keywords, abstracts, citation information

#option two: partial download usng the api 
#the api is working and playing ball!? 
#this will bring down paper titles, keywords, author afiliations and some other metadata but not citation information
#you can get citations per year from online if its less than 10000 (see below)

#option three: the api is 50 shades of useless and you're not going to download them 500 at a time
#put your search in online
#click the analyse tab
#select publication years
#scroll to the bottom of the page (bottom right)
#select all data rows (up to 200,000-this works because we are looking at dates not individual records) 
#hit download
#this will download as analyze.txt
#you are going to move it to the location you want
#you are going to open it in a text editor (i.e. BBEdit, TextEdit, notepad etc)
#you are going to change spaces to underscores and remove the random lines at the end of the file that are not data
#you can download toher similar tables to what we will produce
#by selecting the Countries/Regions tab you can download a table with that data
#you can do a citation report and get citation per year, if its less than 10000
#you cannot get keywords from this (as far as I can tell anyhow)

#we are going to use a few pacakges which you will first have to install and then load
#you will first need to install the package 'bibliometrix'
install.packages('bibliometrix') 
install.packages('tidyverse') #installs ggplot/dplyr allows pipes etc
install.packages("wordcloud")
install.packages('tm')
install.packages('RColorBrewer')


#load package
library(bibliometrix)
library(tidyverse)
library(wordcloud)
library(tm)
library(RColorBrewer)
#library(slam)

##########OPTION ONE##############


#set the working directory to where you have saved your downloaded .bib files
setwd("~/Documents/CoAuthorMS/parasitebibsearch/parasitesonly/")

#there is this clunky thing in the bibliometrix code so we are just going to modify their input function slightly to make it easier to read in our files
readFilesmod<-function (...) 
{
  arguments <- as.list(...) #as.list(...) so it will take a vector easier
  k = length(arguments)
  D = list()
  enc = "UTF-8"
  for (i in 1:k) {
    D[[i]] = suppressWarnings(readLines(arguments[[i]], encoding = enc))
  }
  D = unlist(D)
  return(D)
}

#just going to read our files in:
file_list<-list.files(pattern='*.bib',full.names=T)
file_list
citations<-readFilesmod(dput(as.character(file_list))) #generates a really large character you can view it just reads in the data as one set

#okay our files are read in now we can change the format (currently its a huge character vector) using a function out of the bibliometrix package
#will take a while to run depending on citation number
citations_df <- convert2df(citations, dbsource = "isi", format = "bibtex")

#now we have a dataframe much easier!
#you can view this to see the various feilds with the authors etc

#if you want to subset by a time frame this is the place to do that
#so if we only wanted to look at records from 2000-2005 (yeer is under PY in the dataframe-publication year I guess)
citations_df_2000_2005<-citations_df %>% filter(between(PY, 2000, 2005))
citations_df %>% arrange(., PY) %>% slice(., round(nrow(citations_df)*0.1, digits = 0)) %>% select(.,PY)


#we are going to use the function biblioanalysis to turn out dataframe into a object of various data statistics
#none of these statistics are particularily hard to generate or special-it just does it in one hit which is nice
?biblioAnalysis

#this object drops some information so we may need to go back to the spreadsheet if we want to use the abstract or other data
citations_ana <- biblioAnalysis(citations_df, sep = ";")

#this is an object with various dataframes stored in it (23 dataframes)
#to access a dataframe you can call it directly like:
citations_ana$Years

#They arnt that special for instance:
citations_ana$TotalCitation
#is just this column from the orginal dataframe
citations_df$TC

#the handy thing about turning it into a bibliometrix object is that use can use the summary function 
#summary function the bibliometrix object
?summary
citations_ana.sum <- summary(object = citations_ana, k = 20, pause = FALSE)
#this gives you an overview of the data the K value specifys the number of entries to return 
citations_ana.sum 

#so for instance:
citations_ana.sum$AnnualProduction
#is the same as this on the orginal dataframe
as.data.frame(table(citations_df$PY))

#maybe we want to save the top 10 countries that have published for this dataset
citations_ana.sum <- summary(object = citations_ana, k = 10, pause = FALSE)

citations_ana.sum$MostProdCountries

#bar chart
df_count<-data.frame(Country=as.character(citations_ana.sum$MostProdCountries$`Country  `),Article_count=as.integer(citations_ana.sum$MostProdCountries$Articles))
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


write.table(citations_ana.sum$MostProdCountries,'TopProducingCountriesForAllozymeGeneralSearch',row.names=F,quote=F,sep='\t')

#if we want to plot part of the data we can 
?plot.bibliometrix
plot(x = citations_ana) #the bibliometrix plot overrides the base R plot function 

#We are interested in how many papers are produced per year so we can see that in the summary file
sum(citations_ana.sum$AnnualProduction$Articles)
citations_ana.sum$AnnualProduction %>% mutate(cumsum=cumsum(Articles),cumper=cumsum(Articles)/sum(Articles)*100)

#we also want things to be nice and flexible so Im switching to ggplot2 here for plotting
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
#add the splines in
ggplot(citations_ana.sum$AnnualProduction) + 
  geom_point(aes(citations_ana.sum$AnnualProduction$`Year   `,citations_ana.sum$AnnualProduction$Articles), size = 3) +
  geom_line(data = spline_int, aes(x,y))

#just make it look a bit prettier
ggplot(citations_ana.sum$AnnualProduction) + 
  geom_point(aes(citations_ana.sum$AnnualProduction$`Year   `,citations_ana.sum$AnnualProduction$Articles), size = 1) +
  geom_line(data = spline_int, aes(x,y)) +
  geom_area(data = spline_int, aes(x,y,fill='red'),alpha=0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset") +
  scale_fill_manual(labels = "Everyone", values = alpha("red",.6))


#you can obviously play around with different plots for various types of data etc

#word cloud
?wordcloud

#change the summary to return more hits (before it was just 10 here it is 50)
citations_ana.sum <- summary(object = citations_ana, k = 50, pause = FALSE)

#what table/columns do we need:
#you will notice that this table is not well made it has two columns names 'Articles'
citations_ana.sum$MostRelKeywords
citations_ana.sum$MostRelKeywords$`Author Keywords (DE)     `
citations_ana.sum$MostRelKeywords$`Keywords-Plus (ID)    `
colnames(citations_ana.sum$MostRelKeywords)
#Articles pulls the first one:
citations_ana.sum$MostRelKeywords$Articles

#so if we want to plot 'Author Keywords (DE)' we can do this:
#get rid of punctuation and create data frame
forwordcloud<-as.data.frame(cbind(as.character(trimws(citations_ana.sum$MostRelKeywords$`Author Keywords (DE)     `, which = c("both", "left", "right"))),as.integer(trimws(citations_ana.sum$MostRelKeywords$Articles, which = c("both", "left", "right")))),stringsAsFactors=FALSE)
colnames(forwordcloud)<-c('keyword','count_papers')

#if we want to plot 'Keywords-Plus (ID)'
#forwordcloud<-as.data.frame(cbind(as.character(trimws(citations_ana.sum$MostRelKeywords$`Keywords-Plus (ID)    `, which = c("both", "left", "right"))),citations_ana.sum$MostRelKeywords[4]),stringsAsFactors=FALSE)

#have dataframe now
#orginal
head(citations_ana.sum$MostRelKeywords)
#new
head(forwordcloud)


#we want to drop the keywords we searched for from our dataframe
forwordcloud<-forwordcloud %>% filter(.,keyword!='ALLOZYMES',keyword!='ALLOZYME',keyword!='ALLOZYME ELECTROPHORESIS',keyword!='ALLOZYME VARIATION')

#need to create a corpus which is a sort of list used by text mining packages in R
#Im not totally sure what is special about it - but we need it!
?Corpus
forwordcloud.Corpus<-Corpus(VectorSource(forwordcloud[rep(row.names(forwordcloud), forwordcloud$count_papers), 1]))

#can use the function inspect to display the information on the corpus
inspect(forwordcloud.Corpus)

#we dont have any special characters but we could remove funky characters using:
#these will throw a warning but dont worry they dont mean anything here-its not dropping documents its because I used a vector source for the corpus and for whatever reason that generates a warning
#?tm_map
#forwordcloud.Corpus<- tm_map(forwordcloud.Corpus, removePunctuation)
#forwordcloud.Corpus <- tm_map(forwordcloud.Corpus, removeNumbers)
#forwordcloud.Corpus <- tm_map(forwordcloud.Corpus, stripWhitespace)
#forwordcloud.Corpus <- tm_map(forwordcloud.Corpus, remove_stopwords) #with package 'tau' 
#you can drop it to lower case with:
#forwordcloud.Corpus <- tm_map(forwordcloud.Corpus,content_transformer(tolower))
#inspect(forwordcloud.Corpus)
#qdap package offers other cleaning functions if we need them


#create wordclouds
?wordcloud
wordcloud(forwordcloud.Corpus)
#make it pretty
#look up brewer.pal for colour pallets https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
wordcloud(forwordcloud.Corpus,colors=brewer.pal(8, "Dark2"))
#you can see that genetic and genetics come up
#you can use the stemming function to look for the root of the word but it is kind of ugly
forwordcloud.Corpus <- tm_map(forwordcloud.Corpus,content_transformer(tolower))
forwordcloud.doc<- tm_map(forwordcloud.Corpus, stemDocument, "english")
wordcloud(forwordcloud.doc,colors=brewer.pal(8, "Dark2"))

#code to reduce redundancy by hand
forwordcloud<-forwordcloud %>%  mutate(fixkeyword=sub("GENETICS", "GENETIC", keyword)) 
forwordcloud.Corpus<-Corpus(VectorSource(forwordcloud[rep(row.names(forwordcloud), forwordcloud$count_papers), 3]))
wordcloud(forwordcloud.Corpus,colors=brewer.pal(8, "Dark2"))
?summary

#the code above treated each word as indepenent 
#to treat them as phrases rather than words
#Im not sure what the best way to treat a phase is for example 'gene flow'
#phrases are easier because we can use the counts directly from the summary file
#tolower because I think they look nicer lowercase
wordcloud(tolower(forwordcloud$keyword),as.numeric(forwordcloud$count_papers), colors="black")
#reset the scale so they fit
wordcloud(tolower(forwordcloud$keyword),as.numeric(forwordcloud$count_papers), colors="black",scale=c(3,.5))

#colours and font
wordcloud(tolower(forwordcloud$keyword),as.numeric(forwordcloud$count_papers), colors=brewer.pal(8, "Set1"),scale=c(3,.5))
wordcloud(tolower(forwordcloud$keyword),as.numeric(forwordcloud$count_papers), colors=brewer.pal(8, "Dark2"),vfont=c("script","bold"),scale=c(3,.5))
wordcloud(tolower(forwordcloud$keyword),as.numeric(forwordcloud$count_papers), colors=brewer.pal(8, "Dark2"),family = "mono",font = 2,scale=c(2.5,.5))


################################################################
############combing option one with option one##################
################################################################

#working with a second lot of .bib files downloaded 500 at a time from the website for the 2nd search

setwd("~/Documents/CoAuthorMS/parasitebibsearch/parasitesonly/")

file_list<-list.files(pattern='*.bib')
file_list

#same as before
para.citations<-readFilesmod(dput(as.character(file_list))) 
para.citations <- convert2df(para.citations, dbsource = "isi", format = "bibtex")
para.citations_ana <- biblioAnalysis(para.citations, sep = ";")
para.citations_ana.sum <- summary(object = para.citations_ana, k = 10, pause = FALSE)
write.table(para.citations_ana.sum$MostProdCountries,'TopProducingCountriesForAllozymeParasiteSearch',row.names=F,quote=F,sep='\t')

#bar chart
para.df_count<-data.frame(Country=as.character(para.citations_ana.sum$MostProdCountries$`Country  `),Article_count=as.integer(para.citations_ana.sum$MostProdCountries$Articles))
ggplot(para.df_count, aes(Country, Article_count)) +
  geom_bar(stat = "identity",fill=brewer.pal(10, "Spectral")) +
  coord_flip() +
  theme_bw() 

#with everyone else category
vec<-as.data.frame(para.citations_ana$Countries,stringsAsFactors = F) %>% filter(!Tab %in% trimws(as.character(para.df_count$Country),which = c("both", "left", "right"))) %>% select(.,Freq) %>% sum()
vec2<-data.frame(Country='OTHER',Article_count=as.integer(vec))
para.df_count<-rbind(para.df_count,vec2)

ggplot(para.df_count, aes(Country, Article_count)) +
  geom_bar(stat = "identity",fill=brewer.pal(11, "Spectral")) +
  coord_flip() +
  theme_bw() 


para.citations_ana.sum 


#plotting
ggplot(para.citations_ana.sum$AnnualProduction, aes(`Year   `,Articles, group=1)) +
  geom_point(aes(para.citations_ana.sum$AnnualProduction$`Year   `,para.citations_ana.sum$AnnualProduction$Articles), size = 3,colour='blue') +
  geom_line(aes(`Year   `,Articles)) +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#create new splines
spline_int2 <- as.data.frame(spline(para.citations_ana.sum$AnnualProductio$`Year   `, para.citations_ana.sum$AnnualProduction$Articles))
ggplot(para.citations_ana.sum$AnnualProduction) + 
  geom_point(aes(para.citations_ana.sum$AnnualProduction$`Year   `,para.citations_ana.sum$AnnualProduction$Articles), size = 1) +
  geom_line(data = spline_int2, aes(x,y)) +
  geom_area(data = spline_int2, aes(x,y,fill='blue'),alpha=0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset") +
  scale_fill_manual(labels = "Parasites", values = alpha("blue",.6))

#to make this easier to view Im going to merge the parasites and everyone dataframe together
##merge them into the same graph
d1<-citations_ana.sum$AnnualProduction
d2<-para.citations_ana.sum$AnnualProduction
head(d1)
head(d2)
colnames(d1) <- c("Year", "ArticlesGeneral")
colnames(d2) <- c("Year", "ArticlesParasites")

#just good practise to merge on a character than a factor (R should do this automatically but dont put your trust in this sort of thing)
class(d1$Year)
class(d1$ArticlesGeneral)
d1$Year <- as.character(d1$Year)
d2$Year <- as.character(d2$Year)

#have to merge them together 
dmerged<-full_join(d1,d2,by='Year',all=TRUE) 

#NA should be 0
dmerged[is.na(dmerged)] <- 0 
dmerged

#EDDY fix it into one x and y

ggplot(dmerged) + 
  geom_point(aes(dmerged$Year,dmerged$ArticlesGeneral), col='red',size = 3) +
  geom_point(aes(dmerged$Year,dmerged$ArticlesParasite), col='blue',size = 3) +
  geom_line(aes(dmerged$Year,dmerged$ArticlesGeneral)) +
  geom_line(aes(dmerged$Year,dmerged$ArticlesParasite)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset") 

#overlay the figures with splines
spline_int <- as.data.frame(spline(dmerged$Year, dmerged$ArticlesParasite))
spline_int2 <- as.data.frame(spline(dmerged$Year, dmerged$ArticlesGeneral))

#because of the way the splines work they dont come out as exact values so we need to move the dates back to a integer
dmerged$Year<-as.integer(dmerged$Year)

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
  scale_fill_manual(labels = c("Everyone", "Parasites"), values = alpha(c("red", "blue"),.6))

#because robert didnt like my orginal figure
#creating the spline for the parasites created a few very slightly negative values-Im just forcing these to zero here so they wont cause issues in the data transformation
spline_int$y[spline_int$y < 0] <- 0
ggplot(dmerged) + 
  geom_point(aes(dmerged$Year,dmerged$ArticlesGeneral), col='red',size = 1) +
  geom_point(aes(dmerged$Year,dmerged$ArticlesParasites), col='blue',size = 1) +
  geom_line(data = spline_int2, aes(x,y)) +
  geom_area(data = spline_int2, aes(x,y,fill='blue')) +
  geom_line(data = spline_int, aes(x,y)) +
  geom_area(data = spline_int, aes(x,y,fill='red')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Allozymes",x='Year', y=expression(sqrt(italic('Article Number'))), fill="Subset") +
  scale_fill_manual(labels = c("General Search", "Parasites"), values = alpha(c("red", "blue"),.6)) +
  scale_y_continuous(trans='sqrt')

#logging doesnt work well in this example as the counts are so low for parasitoloty that it forces a lot of negative values but it may be useful for other comparisons
#ggplot(dmerged) + 
#  geom_point(aes(dmerged$Year,dmerged$ArticlesGeneral ), col='red',size = 1) +
#  geom_point(aes(dmerged$Year,dmerged$ArticlesParasites), col='blue',size = 1) +
#  geom_line(data = spline_int2, aes(x,y)) +
#  geom_area(data = spline_int2, aes(x,y,fill='blue')) +
#  geom_line(data = spline_int, aes(x,y)) +
#  geom_area(data = spline_int, aes(x,y,fill='red')) +
#  theme_bw() +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(title="Allozymes",x='Year', y=expression(log[10](italic('Article Number'))), fill="Subset") +
#  scale_fill_manual(labels = c("Everyone", "Parasites"), values = alpha(c("red", "blue"),.6)) +
#  scale_y_log10(breaks=c(0,1,10,100,200,300,400,500),limits = c(1,500))

#word clouds
para.citations_ana.sum <- summary(object = para.citations_ana, k = 50, pause = FALSE)
paraforwordcloud<-as.data.frame(cbind(as.character(trimws(para.citations_ana.sum$MostRelKeywords$`Author Keywords (DE)     `, which = c("both", "left", "right"))),as.integer(trimws(para.citations_ana.sum$MostRelKeywords$Articles, which = c("both", "left", "right")))),stringsAsFactors=FALSE)
colnames(paraforwordcloud)<-c('keyword','count_papers')

paraforwordcloud<-paraforwordcloud %>% filter(.,keyword!='ALLOZYMES',keyword!='ALLOZYME',keyword!='ALLOZYME ELECTROPHORESIS',keyword!='ALLOZYME MARKERS',keyword!='ALLOZYME VARIATION')
paraforwordcloud<-paraforwordcloud %>%  mutate(fixkeyword=sub("GENETICS", "GENETIC", keyword)) 

paraforwordcloud.Corpus<-Corpus(VectorSource(paraforwordcloud[rep(row.names(paraforwordcloud), paraforwordcloud$count_papers), 3]))
inspect(paraforwordcloud.Corpus)
wordcloud(paraforwordcloud.Corpus,colors=brewer.pal(8, "Dark2"))

#to treat them as phrases rather than words
wordcloud(tolower(paraforwordcloud$keyword),as.numeric(paraforwordcloud$count_papers), colors="black",scale=c(2.5,.5))
wordcloud(tolower(forwordcloud$keyword),as.numeric(forwordcloud$count_papers), colors=brewer.pal(8, "Dark2"),scale=c(2.5,.5))

#depending on how the data comes down we can look at other things as well
citations_df$TC
para.citations$TC

dmerged.violin<-data.frame(citationcount=para.citations$TC,type='Parasite Search')
dmerged.violin<-rbind(dmerged.violin,data.frame(citationcount=citations_df$TC,type='General Search'))

ggplot(dmerged.violin,aes(type,citationcount))  +
  geom_violin(aes(fill = factor(type))) +
  scale_y_continuous(trans='sqrt')+
  labs(title="Citation count per article",x='Search Group', y=expression(sqrt(italic('Citaiton Count'))), fill="Subset") +
  theme_bw()+
  scale_fill_manual(values = alpha(c("red", "blue"),.6)) 
  
#could look at citation over the years
para.citationperyear<-para.citations %>% select(.,PY,TC) %>% group_by(PY) %>% tally(TC)
citationperyear<-citations_df %>% select(.,PY,TC) %>% group_by(PY) %>% tally(TC)
colnames(para.citationperyear) <- c("Year", "CitationParasites")
colnames(citationperyear) <- c("Year", "CitationGeneral")
dmerged.citationPY<-full_join(citationperyear,para.citationperyear,by='Year')
dmerged.citationPY[is.na(dmerged.citationPY)] <- 0 

ggplot(dmerged.citationPY) + 
  geom_point(aes(dmerged.citationPY$Year,dmerged.citationPY$CitationGeneral), col='red',size = 3) +
  geom_point(aes(dmerged.citationPY$Year,dmerged.citationPY$CitationParasites), col='blue',size = 3) +
  geom_line(aes(dmerged.citationPY$Year,dmerged.citationPY$CitationGeneral)) +
  geom_line(aes(dmerged.citationPY$Year,dmerged.citationPY$CitationParasites)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Citations Per Year",x='Year', y=expression(sqrt(italic('Citation Number'))), fill="Subset") +
  scale_fill_manual(labels = c("Everyone", "Parasites"), values = alpha(c("red", "blue"),.6)) +
  scale_y_continuous(trans='sqrt')

################################################################
#############combining option one with option two###############
################################################################

#################################################################
#############combining option one with option three##############
#################################################################

#working with one lot of files brought down 500 at a time from website and comparing to a second search were we are going to bring the raw numbers in from online
#using publication years file
#the file downloaded here is basically the same file as d1 above

#read in file
d1v2<-read.table('../analyze.txt', sep="\t",header=T,row.names=NULL)
head(d1v2)
colnames(d1v2) <- c("Year", "ArticlesGeneral","PercentPerYearEveryone")


d1v2$Year <- as.character(d1v2$Year)

#have to merge them together 
dmerged<-full_join(d1v2,d2,by='Year',all=TRUE) 

#NA should be 0
dmerged[is.na(dmerged)] <- 0 
dmerged
#overlay the figures
spline_int <- as.data.frame(spline(dmerged$Year, dmerged$ArticlesParasites))
spline_int2 <- as.data.frame(spline(dmerged$Year, dmerged$ArticlesGeneral))

#because of the way the splines work they dont come out as exact values so we need to move the dates back to a integer
dmerged$Year<-as.integer(dmerged$Year)

ggplot(dmerged) + 
  geom_point(aes(dmerged$Year,dmerged$ArticlesGeneral), col='red',size = 1) +
  geom_point(aes(dmerged$Year,dmerged$ArticlesParasites), col='blue',size = 1) +
  geom_line(data = spline_int2, aes(x,y)) +
  geom_area(data = spline_int2, aes(x,y,fill='blue')) +
  geom_line(data = spline_int, aes(x,y)) +
  geom_area(data = spline_int, aes(x,y,fill='red')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Allozymes",x='Year', y='Article Number', fill="Subset") +
  scale_fill_manual(labels = c("Everyone", "Parasites"), values = alpha(c("red", "blue"),.6))

#same graph as before

#I was thinking you could do a sankeyNetwork on the parasite stuff to break it up into medical and non medical or specific parasite groups





