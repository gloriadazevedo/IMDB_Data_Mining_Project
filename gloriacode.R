#ORIE 4740 Project 
#Data Cleaning Script

getwd()
setwd("/Users/Erik's PC/Desktop/DataMining")
#Import data 
full_data<-read.csv("movie_metadatav3.csv",sep=",",header=TRUE)

#look at the metrics for all the data points
summary(full_data)

#Want to create new variables that say whether or not the movie has a specific genre
#breakdown the current genre category into binary predictors
#Action
full_data$is_action<-grepl("Action",full_data$genre)
#Adventure
full_data$is_adventure<-grepl("Adventure",full_data$genre)
#Animation
full_data$is_animation<-grepl("Animation",full_data$genre)
#Biography
full_data$is_biography<-grepl("Biography",full_data$genre)
#Comedy
full_data$is_comedy<-grepl("Comedy",full_data$genre)
#Crime
full_data$is_crime<-grepl("Crime",full_data$genre)
#Documentary
full_data$is_documentary<-grepl("Documentary",full_data$genre)
#Drama
full_data$is_drama<-grepl("Drama",full_data$genre)
#Family
full_data$is_family<-grepl("Family",full_data$genre)
#Fantasy
full_data$is_fantasy<-grepl("Fantasy",full_data$genre)
#Film-Noir
full_data$is_filmnoir<-grepl("Film-Noir",full_data$genre)
#Game-Show
full_data$is_gameshow<-grepl("Game-Show",full_data$genre)
#History
full_data$is_history<-grepl("History",full_data$genre)
#Horror
full_data$is_horror<-grepl("Horror",full_data$genre)
#Music/#Musical
full_data$is_music<-grepl("Music",full_data$genre)
#Mystery
full_data$is_mystery<-grepl("Mystery",full_data$genre)
#News
full_data$is_news<-grepl("News",full_data$genre)
#Reality-TV
full_data$is_realitytv<-grepl("Reality-TV",full_data$genre)
#Romance
full_data$is_romance<-grepl("Romance",full_data$genre)
#Sci-Fi
full_data$is_scifi<-grepl("Sci-Fi",full_data$genre)
#Short
full_data$is_short<-grepl("Short",full_data$genre)
#Sport
full_data$is_sport<-grepl("Sport",full_data$genre)
#Thriller
full_data$is_thriller<-grepl("Thriller",full_data$genre)
#War
full_data$is_war<-grepl("War",full_data$genre)
#Western
full_data$is_western<-grepl("Western",full_data$genre)


#only including genres that have a decent number of non-zero movies
all_pred<-c("color","num_critic_for_reviews","duration","director_facebook_likes" , "actor_3_facebook_likes","actor_1_facebook_likes",
            "num_voted_users","cast_total_facebook_likes","facenumber_in_poster",
            "num_user_for_reviews","content_rating","budget","title_year","actor_2_facebook_likes","imdb_score","aspect_ratio","movie_facebook_likes","budget2016","gross2016","is_biography","is_comedy","is_crime","is_documentary","is_drama","is_family","is_fantasy","is_history","is_horror","is_music","is_mystery","is_romance","is_scifi","is_sport","is_thriller","is_war","is_western")

#Get the subset of the data with only these columns
sub_data<-subset(full_data,gross2016!="#N/A"&country=="USA"&language=="English",select=all_pred)

#convert gross and budget to float
#note that many values are 0 so the summary statistics seem low
sub_data$gross2016<-as.numeric(as.character(sub_data$gross2016))
sub_data$budget2016<-as.numeric(as.character(sub_data$budget2016))

new_sub<-sub_data[complete.cases(sub_data),]

#Need to create indicator variables for the content ratings
new_sub$is_G<-new_sub$content_rating=="G"
new_sub$is_PG<-new_sub$content_rating=="PG"
new_sub$is_PG_13<-new_sub$content_rating=="PG-13"
new_sub$is_R<-new_sub$content_rating=="R"

#Need to create indicator variables for color
new_sub$is_color<-new_sub$color=="Color"
#new_sub$not_color<-new_sub$color!="Color"