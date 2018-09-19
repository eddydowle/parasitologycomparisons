#ejd 2018
#can we pull from wos in a sane manner from R

#two packages:
#wosr looks to enable us to pull directly
#install.packages('wosr')
library(wosr)

#bibliometrix this is using data searched online and then brought into R but lets see if we can get both working. Some in built figure functions. 
#Has a shiny interface biblioshiny()
#install.packages('bibliometrix')
library(bibliometrix)

#test data wosr
#cant get this working yet because we need a API username/password from WoS
#auth(username = Sys.getenv(NULL),
#     password = Sys.getenv(NULL))

#pull_wos('host parasite', editions = c("SCI", "SSCI", "AHCI", "ISTP", "ISSHP", "BSCI",
#                             "BHCI", "IC", "CCR", "ESCI"), sid = auth(Sys.getenv(''),
#                                                                      Sys.getenv('')))


#library bibliometrix

D <- readFiles("~/Downloads/savedrecs.bib")
M <- convert2df(D, dbsource = "isi", format = "bibtex")
results <- biblioAnalysis(M, sep = ";")
results
S <- summary(object = results, k = 10, pause = FALSE)
S #NZ made the list at #8 -germany and portugal did not.
plot(x = results, k = 10, pause = FALSE)

#main one we are interested in:
S$AnnualProduction

plot(x = S$AnnualProduction, type= 'l')
S$AnnualProduction$`Year   `


library(ggplot2)
ggplot(S$AnnualProduction, aes(`Year   `,Articles, group=1)) +
  geom_line(aes(`Year   `,Articles))


spline_int <- as.data.frame(spline(S$AnnualProductio$`Year   `, S$AnnualProduction$Articles))
ggplot(S$AnnualProduction) + 
  geom_point(aes(S$AnnualProduction$`Year   `,S$AnnualProduction$Articles), size = 3) +
  geom_line(data = spline_int, aes(x,y))

ggplot(S$AnnualProduction) + 
  geom_point(aes(S$AnnualProduction$`Year   `,S$AnnualProduction$Articles), size = 1) +
  geom_line(data = spline_int, aes(x,y)) +
  geom_area(data = spline_int, aes(x,y,fill='blue'),alpha=0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#all other records
#can only export 500 at a time from WoS this is a shitty system (will try to get API access) nobody has time for that shit.


library(stringr)

#######create table of counts#######
setwd("~/Downloads/")

file_list<-list.files(pattern='*).bib')
file_list

#D2<-readFiles(file_list)  #fuck sake

test<-readFiles('savedrecs(1).bib','savedrecs(2).bib','savedrecs(3).bib','savedrecs(4).bib','savedrecs(5).bib','savedrecs(6).bib','savedrecs(7).bib','savedrecs(8).bib','savedrecs(9).bib','savedrecs(10).bib','savedrecs(11).bib','savedrecs(12).bib','savedrecs(13).bib','savedrecs(14).bib','savedrecs(15).bib','savedrecs(16).bib','savedrecs(17).bib')

M2 <- convert2df(test, dbsource = "isi", format = "bibtex")


results2 <- biblioAnalysis(M2, sep = ";")
results2
S2 <- summary(object = results2, k = 10, pause = FALSE)
S2 #

spline_int2 <- as.data.frame(spline(S2$AnnualProductio$`Year   `, S2$AnnualProduction$Articles))


ggplot(S2$AnnualProduction) + 
  geom_point(aes(S2$AnnualProduction$`Year   `,S2$AnnualProduction$Articles), size = 1) +
  geom_line(data = spline_int2, aes(x,y)) +
  geom_area(data = spline_int2, aes(x,y,fill='red'),alpha=0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#merge them into the same graph
d1<-S2$AnnualProduction
d2<-S$AnnualProduction
head(d1)
head(d2)
colnames(d1) <- c("Year", "ArticlesGeneral")
colnames(d2) <- c("Year", "ArticlesParasites")
#slowed my computer down so much I actually wrote out and restarted R to speed shit back up again
library(ggplot2)

write.table(d1,'test',row.names=F,quote=F,sep="\t")
write.table(d2,'test2',row.names=F,quote=F,sep="\t")
setwd("~/Downloads/")
d1<-read.table('test',sep="\t",header=T,row.names=NULL)
d2<-read.table('test2',sep="\t",header=T,row.names=NULL)

dmerged<-merge(d1,d2,by='Year',all=TRUE)
dmerged[is.na(dmerged)] <- 0 #NA should be 0
spline_int <- as.data.frame(spline(dmerged$Year, dmerged$ArticlesParasites))

spline_int2 <- as.data.frame(spline(dmerged$Year, dmerged$ArticlesGeneral))


ggplot(dmerged) + 
  geom_point(aes(dmerged$Year,dmerged$ArticlesGeneral), col='red',size = 1) +
  geom_point(aes(dmerged$Year,dmerged$ArticlesParasites), col='blue',size = 1) +
  geom_line(data = spline_int2, aes(x,y)) +
  geom_area(data = spline_int2, aes(x,y,fill='blue')) +
  geom_line(data = spline_int, aes(x,y)) +
  geom_area(data = spline_int, aes(x,y,fill='red')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Year', y='Article Number', fill="Subset") +
  scale_fill_manual(labels = c("Everyone", "Parasites"), values = alpha(c("red", "blue"),.6))



