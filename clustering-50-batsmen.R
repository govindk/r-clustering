library(NbClust)

df <- read.csv("D:\\Projects\\GIDS\\data\\stats-guru.csv")
head(df)
summary(df)

#check for bad apples
sapply(tf,function(x)any(is.na(x)))
# remove most of unwanted columns
tf = subset( df, select = -c(ï.._num  ,X_widgetName ,X_source,X_resultNumber,span_number,span,player_link._title,player_link._text,player_link,player_link._source,X_pageUrl ) )
head(tf)

# how many clusters are good
set.seed(1234)
tf.dist <- scale(tf[-1])  
nc <- NbClust(tf.dist, min.nc=2, max.nc=15, method="kmeans")
wssplot(tf.dist)
summary(nc)
nc$Best.nc


# let us try distance based hierarchical clustering
tf.hc.complete  <- hclust(dist(tf), method = "complete")
# yesterday giving a name of column did not work - better to qualify
plclust(tf.hc.complete, label=tf$player)

# sachin is in different league, Ponting/Kallis/Dravid are together
# Bradman, hutton, jayasuria, gayle are together (was expecting Gary sobers, Richards too)
# Richards, Hayden, Sehwag though are together. 
# Expected Laxman to be with Dravid, Kallis but he is with Inzy and Javed
# that other reliable player M yousuf is with G. Greenidge and C Loyd - great company
# allthough views can change on kind of algoritm one chooses say for example avg but I like this better


# helper function to get an idea about right # of clusters
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

# let us try something else and see
library(mclust)
tf.mclus <- Mclust(tf)
plot(tf.mclus, data=tf, what="BIC")
# find out relation between features for clustering
plot(tf.mclus,  what="classification")
