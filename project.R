#In this assignment i used (Content based filtering) in this analyzing an item a user
#interacted with , and giving recommendations that are similar in content to that item.

rm(list=ls())

setwd("F:/project")
getwd()


movie=read.csv("movie.csv",header=T)
user=read.csv("user.csv",header=T)

##content based filtering approch

#To obtain the movie feature matrix ,the separator(|) is available in the movie dataset
#had to be split . I installed data.table package in which tstrsplit() function that is
#used to split features.

genres = as.data.frame(movie$genres, stringsAsFactors=FALSE)
library(data.table)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres2) <- c(1:10)

#In this line of code i create a matrix with columns representing every unique genre and 
#it indicate whether a genre was present or notin each movie.

genre_list <- c("Action" , "Adventure" , "Comedy" ,"Crime" , "Drama" , "Film-Noir" , "Horror" , "Mystery" , "Thriller" , "Western")

genre_matrix <- matrix(0,9743,10) # create a empty matrix
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

#convert into dataframe
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
}

# first convert the ratings into a binary format to keep things simple.
#ratings of 4 and 5 are mapped to 1,
# representing likes, and ratings of 3 and below are mapped to -1, representing dislikes.

ratings <- as.data.frame(user[-1,], stringsAsFactors=FALSE)


binaryratings = ratings
for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else{
    binaryratings[i,3] <- -1
  }
}

#Then i have to create user profile matrix . Then i install reshape2 package in which 
# dcast function is available this function is used to transform the data from a long format
#to wide format and this also create NA values because not every user rated every movie
# I substituted the NA values with 0.

library(reshape2)


binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
binaryratings2 = binaryratings2[,-1] ##remove movieIds col. Rows are movieIds, cols are userIds

#In this i removed themovie dataset that have never been rated from the genere matrix.

movieIds <- length(unique(movie$movieId)) 
ratingmovieIds <- length(unique(user$movieId)) 
movies2 <- movie[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(movies2) <- NULL
#Remove rows that are not rated from genre_matrix2
genre_matrix3 <- genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(genre_matrix3) <- NULL


#Then I create the simple user profile matrix.In which i calculated the dot product of the 
#gnere matrix and the rating matrix and obtain the user profiles. This user profiles shows the
# aggregated inclination of each user towards movie genere.
result = matrix(0,10,610)
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c]))
  }
}

#############
#The values were again simplified into a binary matrix - positive values were mapped to 1
#to represent likes, negative values were mapped to 0 to represent dislikes.
for (i in 1:nrow(result)){
  if (result[i] < 0){
    result[i] <- 0
  }
  else {
    result[i] <- 1
  }
}

# Now we have user profile then i used this approach (Assume that users like similar items,
#and retrieve movies that are closest in similarity to a user's profile, which represents
#a user's preference for an item's feature.) I used  Jaccard Distance to measure the 
#similarity between user profile and the movie genere matrix .

result2 <- result[1,] #First user's profile
sim_mat <- rbind.data.frame(result2, genre_matrix3)
sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer

#i installed proxy library in which dist() function is used  it calculates the distance between
#rows from a single matrix. Then i combined the genere matrix with the user profile matrix
#and retrive the minimum distance for each user.
library(proxy)
sim_results <- dist(sim_mat, method = "Jaccard")
sim_results <- as.data.frame(as.matrix(sim_results[1:9742]))
rows <- which(sim_results == min(sim_results))



####Collaborative Filtering Approach##########

# Then i used collaborative filtering approachgroups users according to prior usage behavior 
# then we need a ratings matrix to build a recommender model

library(reshape2)
#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds


####Creation of recommender model#####
##The User-based Collaborative Filtering recommender model was created with recommenderlab
#with the below parameters and the ratings matrix.
#Method: UBCF
#Similarity Calculation Method: Cosine Similarity

library(recommenderlab)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

#Normalize the data
ratingmat_norm <- normalize(ratingmat)

#Create Recommender Model. "UBCF" stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat_norm, method = "UBCF", param=list(method="Cosine",nn=30))
recom <- predict(recommender_model, ratingmat[1], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list <- as(recom, "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in c(1:10)){
  recom_result[i] <- movie[as.integer(recom_list[[1]][i]),2]
}


#The recommenderlab package also provides an easy way to evaluate your model.
#The predicted item ratings of the user will be derived from the 5 nearest neighbors in its neighborhood.

evaluation_scheme <- evaluationScheme(ratingmat, method="cross-validation", k=5, given=3, goodRating=5) #k=5 meaning a 5-fold cross validation. given=3 meaning a Given-3 protocol
evaluation_results <- evaluate(evaluation_scheme, method="UBCF", n=c(1,3,5,10,15,20))
eval_results <- getConfusionMatrix(evaluation_results)[[1]]

