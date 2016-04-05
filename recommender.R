
#not really needed here, just for improving speed by allowing parallel threads on multicore systems
library(foreach)
library(doMC)
registerDoMC(cores=4)

#read sourcefiles
#be sure to load them in your working directory first
titleFilmDF<-read.csv("uitem.csv",header=TRUE, sep=";", na.strings = "")
userDF<-read.csv("udata.csv",header=TRUE, sep=";", na.strings = "")


#set number of suggestions to view
no_films<-15
#user number
#userid<-6

#1
clusterFilms<-function(titleFilmDF){
  set.seed(123)
  i<-1
  #get rid of movie ids and titles
  titleFilmDF<-titleFilmDF[,c(-1,-2)]
  repeat {
    set.seed(123)
    #build two kmeans models starting with 2 and 3 clusters and repeat until dss<0.25
    i <- i + 1
    movieCluster<-kmeans(titleFilmDF,i)
    movieCluster2<-kmeans(titleFilmDF,i+1)
    #We want to look at the the sum of the squared distance between each member of a cluster and its cluster centroid
    #(SSE: sum of squared error). 
    #As the number of clusters increases, the SSE should decrease because clusters are, by definition, smaller.
    #Here it stops building clusters when SSE decreases by less than 25%
    #tot.withinss = sum(withinss of every cluster)
    dss<-((movieCluster$tot.withinss-movieCluster2$tot.withinss)/movieCluster$tot.withinss)
    # exit if dss < 0.25 
    if (dss < 0.25) break
  }
 return(movieCluster)
}

########### If you run this elbow plot, you can see that three clusters are also pretty decent.
# mydata <- titleFilmDF[,c(-1,-2)]
# wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
# for (i in 2:15) wss[i] <- sum(kmeans(mydata,
#                                      centers=i)$withinss)
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")
###########


#2
getUserInfo<-function(dat,id){
  #Select all rows from user_DF that have the userid==user_id and keep the columns itemid & rating
  a<-subset(dat, userid==id,select=c(itemid, rating))
  # allocate 0 to the cluster column
  cluster<-0
  activeUser <- data.frame( a[order(a$itemid),] ,cluster)
  return(activeUser)
}

#3
setUserFilmCluster<-function(movieCluster, activeUser){
  # set up temporary dataframe to match cluster assignments to movie ids
  df1<- data.frame(cbind(titleFilmDF$movid, clusterNum = movieCluster$cluster))
  names(df1)<-c("movie_id", "cluster")
  #This matches the cluster number to the activeUser movie id
  activeUser$cluster<-df1[match(activeUser$itemid, df1$movie_id),2]
  return(activeUser)
}

#4
getMeanClusterRating<-function(movieCluster, activeUser){
  #aggregate() function is used along with the cluster memberships to determine variable means for each cluster in the original metric
  like<-aggregate(activeUser$rating, by=list(cluster=activeUser$cluster), mean)
  #A bit different approach here: If the max mean rating is below three it gives out the dummy value zero
  if(max(like$x)<3){
    like<-as.vector(0)
  #Else it gives out the cluster number of the max mean value
  } else{
    like<-as.vector(t(max(subset(like, x>=3, select=cluster))))
  }
  return(like)
}

#5
getGoodFilms<-function(like, movieCluster, titleFilmDF){
  # Again a temporary dataframe is created to get a list of all movies and their associated clusters
  df1<- data.frame(cbind(titleFilmDF$movid, clusterNum = movieCluster$cluster))
  names(df1)<-c("movie_id", "cluster")
  #if like has the value zero it selects randomly 100 movies
  if(like==0){
    recommend<-titleFilmDf[sample.int(n = dim(titleFilmDF)[1], size = 100), 1]
  }
  #else it selects all movies from the winning max mean cluster
  else{
    recommend<-as.vector(t(subset(df1, cluster==like, select=movie_id)))
  }
  return(recommend)
}

#6
getRecommendedFilms<-function(titleFilmDF, userDF, userid){
  # according to plan we call all functions in order of task-logic
  movieCluster<-clusterFilms(titleFilmDF)
  activeUser<-getUserInfo(userDF, userid)
  activeUser<-setUserFilmCluster(movieCluster, activeUser)
  like<-getMeanClusterRating(movieCluster, activeUser)
  recommend<-getGoodFilms(like, movieCluster, titleFilmDF)
  # only select not yet watched movies
  recommend<-recommend[-activeUser$itemid]
  # add movietitle
  movtitle<-titleFilmDF[match(recommend,titleFilmDF$movid),2]
  recommend<-data.frame(recommend,movtitle)
  return(recommend)
}

#7
suggestFilms<-function(titleFilmDF, userDF, userid, no_films){
  #get suggestions
  suggestions = getRecommendedFilms(titleFilmDF, userDF, userid)
  #select stated number of selections
  suggestions = suggestions[1:no_films,]
  #implementing some German here
  writeLines("Diese Filme koennten Ihnen auch gefallen:")
  #print suggestions without column headers or row indices
  write.table(suggestions[2], row.names = FALSE, col.names = FALSE)
}


###suggestFilms(titleFilmDF, userDF, 6, 15)
