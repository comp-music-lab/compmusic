#set working directory
setwd("/Users/psav050/Documents/GitHub/compmusic")

#install (if needed) and load required packages
install.packages("pacman")
pacman::p_load(ggplot2, ggfortify, plotly,readr)
library(vegan)
library(tidyr)

#tutorial analyses:
#read distance matrices

#Savage's perceived similarities
savage<-as.matrix(read.csv(file="Savage9SongDissimilarityMatrix.csv",row.names=1))
savage.dist<-as.dist(savage)

#Ozaki's perceived similarities
ozaki<-as.matrix(read.csv(file="Ozaki9SongDissimilarityMatrix.csv",row.names=1))
ozaki.dist<-as.dist(ozaki)

#MDS Savage
fit <- cmdscale(savage.dist,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Musical similarities (Savage)", type="n")
text(x, y, labels = row.names(savage), cex=.7)

#MDS Ozaki
fit <- cmdscale(ozaki.dist,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Musical similarities (Ozaki)", type="n")
text(x, y, labels = row.names(ozaki), cex=.7)

#Distance matrix correlation
plot(ozaki.dist,savage.dist,
     xlab="Musical dissimilarity between pairs (Ozaki)", ylab="Musical dissimilarity between pairs (Savage)",
     xlim=c(0, 1), ylim=c(0, 1))
mantel(ozaki.dist,savage.dist)
#Mantel statistic r: 0.4236 
#Significance: 0.039 

#Global comparison with Global Jukebox Cantometric codings

#load GJB data
d<-read_csv(file='https://raw.githubusercontent.com/theglobaljukebox/cantometrics/refs/heads/main/raw/data.csv') #read raw Qualtrics export file directly from GitHub
d[d == 0] <- NA #replace 0 codings with NA to indicate missing data

#convert binaries to 1-13 Likert
d[,4:40]<-log2(d[,4:40])

#normalize 1-13 Likert to 0-1
d[,4:40]<-(((d[,4:40])-1)/12)

#Make figure of low/med/high counts for key variables for Ch. 2 (Fig. 2.11)
df1 <- data.frame(Feature="3) Voices", 
                  "1low"=0, 
                  "2med"=table(d$line_1)[1],
                  "3high"=sum(table(d$line_1)[2:length(table(d$line_1))]))
df2 <- data.frame(Feature="7) Instruments", 
                  "1low"=table(d$line_3)[1], 
                  "2med"=table(d$line_3)[2],
                  "3high"=sum(table(d$line_3)[3:length(table(d$line_3))]))                  
df3 <- data.frame(Feature="1) Rhythmic regularity", 
                  "1low"=table(d$line_26)[1], 
                  "2med"=sum(table(d$line_26)[2:3]),
                  "3high"=sum(table(d$line_26)[4:length(table(d$line_26))])) 
df4 <- data.frame(Feature="8) Speed", 
                  "1low"=sum(table(d$line_24)[1:3]), 
                  "2med"=sum(table(d$line_24)[4:10]), 
                  "3high"=sum(table(d$line_24)[11:length(table(d$line_24))])) 
df5 <- data.frame(Feature="4) Phrase length", 
                  "1low"=sum(table(d$line_17)[1:3]), 
                  "2med"=sum(table(d$line_17)[4:7]), 
                  "3high"=sum(table(d$line_17)[8:length(table(d$line_17))]))
df6 <- data.frame(Feature="2) Pitch stability", 
                  "1low"=sum(table(d$line_28)[1]), 
                  "2med"=sum(table(d$line_28)[2]), 
                  "3high"=sum(table(d$line_28)[3:length(table(d$line_28))])) 
df7 <- data.frame(Feature="6) Pitch height", 
                  "1low"=sum(table(d$line_32)[1:3]), 
                  "2med"=sum(table(d$line_32)[4:7]), 
                  "3high"=sum(table(d$line_32)[8:length(table(d$line_32))])) 
df8 <- data.frame(Feature="5) Timbral brightness", 
                  "1low"=sum(table(d$line_34)[8:length(table(d$line_34))]), 
                  "2med"=sum(table(d$line_34)[4:7]), 
                  "3high"=sum(table(d$line_34)[1:3])) 
df<-rbind(df1,df2,df3,df4,df5,df6,df7,df8)

long <- df %>% 
  pivot_longer(
    cols = `X1low`:`X3high`, 
    names_to = "level",
    values_to = "Songs"
  )

p<-ggplot(long, aes(fill=level, y=Songs, x=Feature)) + 
  geom_bar(position="stack", stat="identity")
p+scale_fill_manual(values=c("indianred2", "white", "lightskyblue"))



#Ch3 Visualising Ellis's (1885) scale data (Fig. 3.1)

d<-read_csv(file='https://raw.githubusercontent.com/jomimc/DaMuSc/refs/heads/main/Data/measured_scales.csv') #read raw Musical Scale Database data directly from GitHub
d<-subset(d,Reference=="A. J. Ellis. On the Musical Scales of Various Nations. Journal of the Society of arts, 1885") #reduce to only Ellis's data
d <- separate(d, col = Intervals, into = c("one", "two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen"), sep = ";")
write.csv(d,"d.csv") #export (to convert strings to numeric)
d<-read.csv("d.csv") #reimport (as numeric)

dotchart(d$one, pch = 21, labels = d$Name, bg = "black",
         pt.cex = 1, xlim = range(0, 1250))
points(d$one+d$two, 1:nrow(d), col = "black", pch = 19, cex = 1)
points(d$one+d$two+d$three, 1:nrow(d), col = "black", pch = 19, cex = 1)
points(d$one+d$two+d$three+d$four, 1:nrow(d), col = "black", pch = 19, cex = 1)
points(d$one+d$two+d$three+d$four+d$five, 1:nrow(d), col = "black", pch = 19, cex = 1)
points(d$one+d$two+d$three+d$four+d$five+d$six, 1:nrow(d), col = "black", pch = 19, cex = 1)
points(d$one+d$two+d$three+d$four+d$five+d$six+d$seven, 1:nrow(d), col = "black", pch = 19, cex = 1)
points(d$one+d$two+d$three+d$four+d$five+d$six+d$seven+d$eight, 1:nrow(d), col = "black", pch = 19, cex = 1)
points(d$one+d$two+d$three+d$four+d$five+d$six+d$seven+d$eight+d$nine, 1:nrow(d), col = "black", pch = 19, cex = 1)