getwd()
setwd("/Users/Erik's PC/Desktop/DataMining")
full_data<-read.csv("movie_metadata.csv",sep=",",header=TRUE)

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
# For now left off facebook related varibles since they are correlated with title year
data2 = cleaned_data[, c("duration","genres", "language", "content_rating", "title_year",
                     "imdb_score", "budget2016", "gross2016") ]
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