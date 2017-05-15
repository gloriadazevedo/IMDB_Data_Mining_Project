#ORIE 4740 Project 
#Data Cleaning Script

#Import data (relevant only to Gloria)
	full_data<-read.csv("C:/Users/glori/Documents/GitHub/IMDB_Data_Mining_Project/movie_metadata_csv.csv",sep=",",header=TRUE)

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
sub_data$gross2016<-as.numeric(sub_data$gross2016)
sub_data$budget2016<-as.numeric(sub_data$budget2016)

model_1<-lm(gross2016~.-gross2016,data=sub_data)
summary(model_1)

#LOOCV for the above model
library(class)
library(boot)
new_sub<-sub_data[complete.cases(sub_data),]
dim(new_sub)
#[1] 3292   36

glm.fit<-glm(gross2016~.-gross2016,data=subset(new_sub,content_rating=="G"|content_rating=="PG"|content_rating=="PG-13"|content_rating=="R"))
cv_error<-cv.glm(subset(new_sub,content_rating=="G"|content_rating=="PG"|content_rating=="PG-13"|content_rating=="R"), glm.fit)
 cv_error$delta[1]
#[1] 1504668

#Need to create indicator variables for the content ratings
new_sub$is_G<-new_sub$content_rating=="G"
new_sub$is_PG<-new_sub$content_rating=="PG"
new_sub$is_PG_13<-new_sub$content_rating=="PG-13"
new_sub$is_R<-new_sub$content_rating=="R"

#Need to create indicator variables for color
new_sub$is_color<-new_sub$color=="Color"
#new_sub$not_color<-new_sub$color!="Color"

#implementing best subset
library(bestglm)
set.seed(1)
X<-new_sub
X$gross2016<-NULL
X$content_rating<-NULL
X$color<-NULL
X$budget<-NULL
y<-new_sub$gross2016
Xy<-cbind(X,y)
best_subset<-bestglm(Xy,IC="BIC",family=gaussian)
# BIC
# BICq equivalent for q in (0.385941678163465, 0.642763442897387)
# Best Model:
                         # Estimate  Std. Error   t value     Pr(>|t|)
# (Intercept)             325.23846 124.1807724  2.619073 8.857460e-03
# num_critic_for_reviews    1.04177   0.1794844  5.804236 7.080998e-09
# is_comedyTRUE           136.23307  44.9269349  3.032325 2.445740e-03
# is_GTRUE                922.72811 167.7611759  5.500248 4.082939e-08
# is_PGTRUE              1018.22383 110.0050704  9.256154 3.717907e-20
# is_PG_13TRUE           1119.49442 103.3816023 10.828759 7.116474e-27
# is_RTRUE                986.12541 100.7725607  9.785654 2.610533e-22
# is_colorTRUE            340.12541 113.2589236  3.003078 2.692792e-03

#10-fold cross validation for best subset
best_subset_cv<-bestglm(Xy, IC="CV", family=gaussian,CVArgs=list(Method="HTF", K=10, REP=1))
best_subset_cv$delta[1]

#forward backward subset selection
library(MASS)
model_step<-glm.fit
n<-length(X[,1])
best_forward_backward<-step(model_step,direction="both",k=log(n))
summary(best_forward_backward)

#Need to find the 90th percentile
percentile_90<-quantile(y,.90)






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
	full_data<-full_data[which(full_data$country %in% keep_country),]
	full_data<-full_data[which(full_data$language %in% keep_language),]
	full_data<-full_data[which(full_data$content_rating %in% keep_content_rating),]
	summary(full_data)

#also need to get rid of the NAs from some of the remaining columns
	#num_critic_for_reviews
	full_data<-full_data[which(!is.na(full_data$num_critic_for_reviews)),]
	#duration
	full_data<-full_data[which(!is.na(full_data$duration)),]
	#actor_3_facebook_likes
	full_data<-full_data[which(!is.na(full_data$actor_3_facebook_likes)),]
	#gross2016
	full_data<-full_data[which(!is.na(full_data$gross2016)),]
	#facenumber_in_poster
	full_data<-full_data[which(!is.na(full_data$facenumber_in_poster)),]
	#budget2016
	full_data<-full_data[which(!is.na(full_data$budget2016)),]
	#aspect_ratio
	full_data<-full_data[which(!is.na(full_data$aspect_ratio)),]
	

	

