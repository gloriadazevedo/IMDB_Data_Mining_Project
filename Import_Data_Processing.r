#ORIE 4740 Project 
#Data Cleaning Script

#Import data (relevant only to Gloria)
	full_data<-read.csv("C:/Users/glori/Documents/GitHub/IMDB_Data_Mining_Project/movie_metadata.csv",sep=",",header=TRUE)

#look at the metrics for all the data points
	summary(full_data)

#Naive method, fit all the variables to the data 
#Try to predict gross2016, even though we know it's in different units
#can't complete the run on Gloria's laptop
	#all_predictors<-lm(gross2016~.,data=full_data)


#Trying a smaller subset of predictors
#All variables are significant which seems to indicate that we can 
#add more variables; also adjusted R-squared is low
	model_1<-lm(gross2016~budget2016+movie_facebook_likes+imdb_score+cast_total_facebook_likes,data=full_data)
	summary(model_1)


#add country to see if that effect is present
#note that none of the countries are significant
	model_2<-lm(gross2016~country+budget2016+movie_facebook_likes+imdb_score+cast_total_facebook_likes,data=full_data)
	summary(model_2)
	

#Take out country but add in language and rating
#Korean language is significant
#major content ratings (G, PG, PG-13) are significant
	model_3<-lm(gross2016~language+content_rating+budget2016+movie_facebook_likes+imdb_score+cast_total_facebook_likes,data=full_data)
	summary(model_3)
	
#What are the general breakdowns of language, country, and rating?
#Note that there are more R rated movies than PG-13 but the R-rating content
#was not significant in the model.  That may imply that the gross2016 amount
#for those movies are more spread out.	
	table(full_data$content_rating)
	       # Approved         G        GP 
      # 303        55       112         6 
        # M     NC-17 Not Rated    Passed 
        # 5         7       116         9 
       # PG     PG-13         R     TV-14 
      # 701      1461      2118        30 
     # TV-G     TV-MA     TV-PG      TV-Y 
       # 10        20        13         1 
    # TV-Y7   Unrated         X 
        # 1        62        13 
		
#breakdown for language
#overwhelming amount for English movies
#possible bias since IMDB is a US-based website
#most reviews and movies available are in English.
	table(full_data$language)
	           # Aboriginal     Arabic    Aramaic 
        # 12          2          5          1 
   # Bosnian  Cantonese    Chinese      Czech 
         # 1         11          3          1 
    # Danish       Dari      Dutch   Dzongkha 
         # 5          2          4          1 
   # English   Filipino     French     German 
      # 4704          1         73         19 
     # Greek     Hebrew      Hindi  Hungarian 
         # 1          5         28          1 
 # Icelandic Indonesian    Italian   Japanese 
         # 2          2         11         18 
   # Kannada     Kazakh     Korean   Mandarin 
         # 1          1          8         26 
      # Maya  Mongolian       None  Norwegian 
         # 1          1          2          4 
   # Panjabi    Persian     Polish Portuguese 
         # 1          4          4          8 
  # Romanian    Russian  Slovenian    Spanish 
         # 2         11          1         40 
   # Swahili    Swedish      Tamil     Telugu 
         # 1          5          1          1 
      # Thai       Urdu Vietnamese       Zulu 
         # 3          1          1          2 
		 
#breakdown of country
#most movies are from the US, UK, France, Canada
#again almost all are English speaking countries
#surprisingly the number of movies from France
#outnumber the number of movies in French so there
#may also be some English movies made in France
	table(full_data$country)
	                              # Afghanistan 
                   # 5                    1 
           # Argentina                Aruba 
                   # 4                    1 
           # Australia              Bahamas 
                  # 55                    1 
             # Belgium               Brazil 
                   # 4                    8 
            # Bulgaria             Cambodia 
                   # 1                    1 
            # Cameroon               Canada 
                   # 1                  126 
               # Chile                China 
                   # 1                   30 
            # Colombia       Czech Republic 
                   # 1                    3 
             # Denmark   Dominican Republic 
                  # 11                    1 
               # Egypt              Finland 
                   # 1                    1 
              # France              Georgia 
                 # 154                    1 
             # Germany               Greece 
                  # 97                    2 
           # Hong Kong              Hungary 
                  # 17                    2 
             # Iceland                India 
                   # 3                   34 
           # Indonesia                 Iran 
                   # 1                    4 
             # Ireland               Israel 
                  # 12                    4 
               # Italy                Japan 
                  # 23                   23 
               # Kenya           Kyrgyzstan 
                   # 1                    1 
               # Libya               Mexico 
                   # 1                   17 
         # Netherlands             New Line 
                   # 5                    1 
         # New Zealand              Nigeria 
                  # 15                    1 
              # Norway        Official site 
                   # 8                    1 
            # Pakistan               Panama 
                   # 1                    1 
                # Peru          Philippines 
                   # 1                    1 
              # Poland              Romania 
                   # 5                    4 
              # Russia             Slovakia 
                  # 11                    1 
            # Slovenia         South Africa 
                   # 1                    8 
         # South Korea         Soviet Union 
                  # 14                    1 
               # Spain               Sweden 
                  # 33                    6 
         # Switzerland               Taiwan 
                   # 3                    2 
            # Thailand               Turkey 
                   # 5                    1 
                  # UK United Arab Emirates 
                 # 448                    1 
                 # USA         West Germany 
                # 3807                    3 

				
#Let's look at actor/director effects--dont' use the direct names for now since
#that would be a whole lot of categorical variables
#adjusted R-squared is still low and all predictors are signficant
	model_4<-lm(gross2016~budget2016+movie_facebook_likes+imdb_score+cast_total_facebook_likes+director_facebook_likes+actor_3_facebook_likes+actor_1_facebook_likes+actor_2_facebook_likes,data=full_data)
	summary(model_4)
	
	# Call:
	# lm(formula = gross2016 ~ budget2016 + movie_facebook_likes + imdb_score + 
		# cast_total_facebook_likes + director_facebook_likes + actor_3_facebook_likes + 
		# actor_1_facebook_likes + actor_2_facebook_likes, data = full_data)

	# Residuals:
		   # Min         1Q     Median         3Q        Max 
	# -308377397  -32865132  -15164146   15016343  661421773 

	# Coefficients:
								# Estimate Std. Error t value Pr(>|t|)    
	# (Intercept)               -2.682e+07  6.281e+06  -4.270 2.00e-05 ***
	# budget2016                     2.224e-02  4.363e-03   5.098 3.59e-07 ***
	# movie_facebook_likes       8.278e+02  4.960e+01  16.690  < 2e-16 ***
	# imdb_score                 8.277e+06  9.724e+05   8.512  < 2e-16 ***
	# cast_total_facebook_likes  1.349e+04  8.956e+02  15.059  < 2e-16 ***
	# director_facebook_likes    1.025e+03  3.306e+02   3.099  0.00195 ** 
	# actor_3_facebook_likes    -1.414e+04  1.499e+03  -9.431  < 2e-16 ***
	# actor_1_facebook_likes    -1.338e+04  8.993e+02 -14.875  < 2e-16 ***
	# actor_2_facebook_likes    -1.262e+04  9.513e+02 -13.262  < 2e-16 ***
	# ---
	# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

	# Residual standard error: 60380000 on 3872 degrees of freedom
	  # (1162 observations deleted due to missingness)
	# Multiple R-squared:  0.2543,    Adjusted R-squared:  0.2527 
	# F-statistic:   165 on 8 and 3872 DF,  p-value: < 2.2e-16

	
#total number of rows
	dim(full_data)
	#5043 total movies
	
#Subset the data so that we can work in the same units
	keep_country<-c("USA","Canada","UK","France","Germany")
	keep_language<-c("English")
	keep_content_rating<-c("G","PG","PG-13","R")
	us_english_data<-full_data[which(full_data$country %in% keep_country),]
	us_english_data<-us_english_data[which(us_english_data$language %in% keep_language),]
	us_english_data<-us_english_data[which(us_english_data$content_rating %in% keep_content_rating),]
	summary(us_english_data)

#also need to get rid of the NAs from some of the remaining columns
	#num_critic_for_reviews
	us_english_data<-us_english_data[which(!is.na(us_english_data$num_critic_for_reviews)),]
	#duration
	us_english_data<-us_english_data[which(!is.na(us_english_data$duration)),]
	#actor_3_facebook_likes
	us_english_data<-us_english_data[which(!is.na(us_english_data$actor_3_facebook_likes)),]
	#gross2016
	us_english_data<-us_english_data[which(!is.na(us_english_data$gross2016)),]
	#facenumber_in_poster
	us_english_data<-us_english_data[which(!is.na(us_english_data$facenumber_in_poster)),]
	#budget2016
	us_english_data<-us_english_data[which(!is.na(us_english_data$budget2016)),]
	#aspect_ratio
	us_english_data<-us_english_data[which(!is.na(us_english_data$aspect_ratio)),]
	
	
#How many rows in the subset?
	dim(us_english_data)
	#3420 rows
	#about 68% of the original data
	#most accurate subset of the data
	
#Want to create new variables that say whether or not the movie has a specific genre
#breakdown the current genre category into binary predictors
	#Action
	us_english_data$is_action<-grepl("Action",us_english_data$genre)
	#Adventure
	us_english_data$is_adventure<-grepl("Adventure",us_english_data$genre)
	#Animation
	us_english_data$is_animation<-grepl("Animation",us_english_data$genre)
	#Biography
	us_english_data$is_biography<-grepl("Biography",us_english_data$genre)
	#Comedy
	us_english_data$is_comedy<-grepl("Comedy",us_english_data$genre)
	#Crime
	us_english_data$is_crime<-grepl("Crime",us_english_data$genre)
	#Documentary
	us_english_data$is_documentary<-grepl("Documentary",us_english_data$genre)
	#Drama
	us_english_data$is_drama<-grepl("Drama",us_english_data$genre)
	#Family
	us_english_data$is_family<-grepl("Family",us_english_data$genre)
	#Fantasy
	us_english_data$is_fantasy<-grepl("Fantasy",us_english_data$genre)
	#Film-Noir
	us_english_data$is_filmnoir<-grepl("Film-Noir",us_english_data$genre)
	#Game-Show
	us_english_data$is_gameshow<-grepl("Game-Show",us_english_data$genre)
	#History
	us_english_data$is_history<-grepl("History",us_english_data$genre)
	#Horror
	us_english_data$is_horror<-grepl("Horror",us_english_data$genre)
	#Music/#Musical
	us_english_data$is_music<-grepl("Music",us_english_data$genre)
	#Mystery
	us_english_data$is_mystery<-grepl("Mystery",us_english_data$genre)
	#News
	us_english_data$is_news<-grepl("News",us_english_data$genre)
	#Reality-TV
	us_english_data$is_realitytv<-grepl("Reality-TV",us_english_data$genre)
	#Romance
	us_english_data$is_romance<-grepl("Romance",us_english_data$genre)
	#Sci-Fi
	us_english_data$is_scifi<-grepl("Sci-Fi",us_english_data$genre)
	#Short
	us_english_data$is_short<-grepl("Short",us_english_data$genre)
	#Sport
	us_english_data$is_sport<-grepl("Sport",us_english_data$genre)
	#Thriller
	us_english_data$is_thriller<-grepl("Thriller",us_english_data$genre)
	#War
	us_english_data$is_war<-grepl("War",us_english_data$genre)
	#Western
	us_english_data$is_western<-grepl("Western",us_english_data$genre)
