getwd()
setwd("/Users/Erik's PC/Desktop/DataMining")
full_data<-read.csv("movie_metadatav3.csv",sep=",",header=TRUE)

#******** Clean Date: Remove movies not from U.S and w/ missing gross

#Data Contains 31 variables
summary(full_data)
table(full_data$country)
3807/5047
#75% of Movies are from the U.S 

#Delete rows with movies from all countries other than USA
cleaned_data = full_data[which(full_data$country %in% "USA"),]

#count rows with missing gross
sum(is.na(cleaned_data$gross))
#572 rows have missing gross

#get rid of movies with no gross
cleaned_data = cleaned_data[!(is.na(cleaned_data$gross) | cleaned_data$gross==""), ]

# Get relevant variables
# For now left off facebook related varibles since they are correlated with title year. Also left off language since 99% are English
data2 = cleaned_data[, c("duration","genres", "content_rating", "director_facebook_likes", "actor_1_facebook_likes", "cast_total_facebook_likes", "actor_2_facebook_likes", "actor_3_facebook_likes", "title_year",
                     "imdb_score", "budget2016", "gross2016", "movie_facebook_likes") ]
#get rid of rows w/ missing/NA values 
data2 = deletemissing(data2)

#Count # rows for each feature that is empty. 
#check 0 for budget & gross
sum(data2$gross2016 == 0)
#53 rows have 0 gross
sum(data2$budget2016 == 0)
#206 rows have 0 budget
#remove rows with 0 gross or budget
data2 = data2[!(data2$gross2016 == 0 | data2$budget2016 == 0), ]
#Convet catagorical features into columns
#Genre
  data2$is_action<-grepl("Action",data2$genre)
  #Adventure
  data2$is_adventure<-grepl("Adventure",data2$genre)
  #Animation
  data2$is_animation<-grepl("Animation",data2$genre)
  #Biography
  data2$is_biography<-grepl("Biography",data2$genre)
  #Comedy
  data2$is_comedy<-grepl("Comedy",data2$genre)
  #Crime
  data2$is_crime<-grepl("Crime",data2$genre)
  #Documentary
  data2$is_documentary<-grepl("Documentary",data2$genre)
  #Drama
  data2$is_drama<-grepl("Drama",data2$genre)
  #Family
  data2$is_family<-grepl("Family",data2$genre)
  #Fantasy
  data2$is_fantasy<-grepl("Fantasy",data2$genre)
  #History
  data2$is_history<-grepl("History",data2$genre)
  #Horror
  data2$is_horror<-grepl("Horror",data2$genre)
  #Music/#Musical
  data2$is_music<-grepl("Music",data2$genre)
  #Mystery
  data2$is_mystery<-grepl("Mystery",data2$genre)
  #Romance
  data2$is_romance<-grepl("Romance",data2$genre)
  #Sci-Fi
  data2$is_scifi<-grepl("Sci-Fi",data2$genre)
  #Sport
  data2$is_sport<-grepl("Sport",data2$genre)
  #Thriller
  data2$is_thriller<-grepl("Thriller",data2$genre)
  #War
  data2$is_war<-grepl("War",data2$genre)
  #Western
  data2$is_western<-grepl("Western",data2$genre)
#Rating
  data2$is_G<-data2$content_rating=="G"
  data2$is_PG<-data2$content_rating=="PG"
  data2$is_PG_13<-data2$content_rating=="PG-13"
  data2$is_R<-data2$content_rating=="R"
#Get rid of Genres and Content Rating Column
  data2$genres <- NULL
  data2$content_rating <- NULL
#Currenty data2 has 3235 rows and 31 Columns
#Convert gross and budget to numeric
  data2$gross2016<-as.numeric(data2$gross2016)
  data2$budget2016<-as.numeric(data2$budget2016)
  