#ORIE 4740 Project 
#Data Cleaning Script

#Import data (relevant only to Gloria)
	full_data<-read.csv("C:/Users/glori/Documents/GitHub/IMDB_Data_Mining_Project/movie_metadata.csv",sep=",",header=TRUE)

#look at the metrics for all the data points
	summary(full_data)

#Naive method, fit all the variables to the data 
#Try to predict gross, even though we know it's in different units
#can't complete the run on Gloria's laptop
	#all_predictors<-lm(gross~.,data=full_data)


#Trying a smaller subset of predictors
#All variables are significant which seems to indicate that we can 
#add more variables; also adjusted R-squared is low
	model_1<-lm(gross~budget+movie_facebook_likes+imdb_score+cast_total_facebook_likes,data=full_data)
	summary(model_1)
	# Call:
	# lm(formula = gross ~ budget + movie_facebook_likes + imdb_score + 
		# cast_total_facebook_likes, data = full_data)

	# Residuals:
		   # Min         1Q     Median         3Q        Max 
	# -352512837  -35104797  -15319825   15733749  674483079 

	# Coefficients:
								# Estimate Std. Error t value Pr(>|t|)    
	# (Intercept)               -1.169e+07  6.411e+06  -1.823   0.0684 .  
	# budget                     2.426e-02  4.552e-03   5.329 1.04e-07 ***
	# movie_facebook_likes       1.008e+03  5.030e+01  20.045  < 2e-16 ***
	# imdb_score                 7.059e+06  9.973e+05   7.078 1.73e-12 ***
	# cast_total_facebook_likes  6.043e+02  5.466e+01  11.057  < 2e-16 ***
	# ---
	# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

	# Residual standard error: 63030000 on 3886 degrees of freedom
	  # (1152 observations deleted due to missingness)
	# Multiple R-squared:  0.1855,    Adjusted R-squared:  0.1847 
	# F-statistic: 221.3 on 4 and 3886 DF,  p-value: < 2.2e-16


#add country to see if that effect is present
#note that none of the countries are significant
	model_2<-lm(gross~country+budget+movie_facebook_likes+imdb_score+cast_total_facebook_likes,data=full_data)
	summary(model_2)
	# Call:
	# lm(formula = gross ~ country + budget + movie_facebook_likes + 
		# imdb_score + cast_total_facebook_likes, data = full_data)

	# Residuals:
		   # Min         1Q     Median         3Q        Max 
	# -358347162  -34905539  -13078866   15589111  662950949 

	# Coefficients:
								# Estimate Std. Error t value Pr(>|t|)    
	# (Intercept)               -6.712e+07  6.214e+07  -1.080    0.280    
	# countryArgentina          -7.003e+06  7.124e+07  -0.098    0.922    
	# countryAruba               3.056e+07  8.729e+07   0.350    0.726    
	# countryAustralia           2.939e+07  6.246e+07   0.471    0.638    
	# countryBelgium            -1.098e+07  7.557e+07  -0.145    0.884    
	# countryBrazil             -9.851e+06  6.758e+07  -0.146    0.884    
	# countryCanada              2.762e+07  6.219e+07   0.444    0.657    
	# countryChile               1.363e+07  8.725e+07   0.156    0.876    
	# countryChina               1.044e+07  6.372e+07   0.164    0.870    
	# countryColombia            4.221e+06  8.725e+07   0.048    0.961    
	# countryCzech Republic     -8.961e+06  7.124e+07  -0.126    0.900    
	# countryDenmark            -1.826e+07  6.504e+07  -0.281    0.779    
	# countryFinland             1.010e+06  8.725e+07   0.012    0.991    
	# countryFrance              1.154e+07  6.199e+07   0.186    0.852    
	# countryGeorgia             1.444e+07  8.727e+07   0.165    0.869    
	# countryGermany             2.641e+07  6.208e+07   0.425    0.671    
	# countryGreece              5.498e+06  8.725e+07   0.063    0.950    
	# countryHong Kong           1.064e+07  6.403e+07   0.166    0.868    
	# countryHungary            -2.891e+07  7.582e+07  -0.381    0.703    
	# countryIceland             2.686e+06  7.556e+07   0.036    0.972    
	# countryIndia              -2.415e+06  6.387e+07  -0.038    0.970    
	# countryIndonesia          -4.978e+07  8.728e+07  -0.570    0.569    
	# countryIran               -1.636e+07  6.898e+07  -0.237    0.813    
	# countryIreland            -7.254e+06  6.596e+07  -0.110    0.912    
	# countryIsrael             -1.318e+06  7.124e+07  -0.019    0.985    
	# countryItaly              -9.540e+06  6.444e+07  -0.148    0.882    
	# countryJapan               1.268e+07  6.352e+07   0.200    0.842    
	# countryMexico              5.903e+06  6.444e+07   0.092    0.927    
	# countryNetherlands        -1.822e+06  7.124e+07  -0.026    0.980    
	# countryNew Line            2.846e+07  8.730e+07   0.326    0.744    
	# countryNew Zealand         1.002e+08  6.444e+07   1.555    0.120    
	# countryNorway             -1.967e+07  6.898e+07  -0.285    0.776    
	# countryOfficial site       2.656e+07  8.726e+07   0.304    0.761    
	# countryPeru                6.624e+07  8.728e+07   0.759    0.448    
	# countryPhilippines         9.029e+06  8.726e+07   0.103    0.918    
	# countryPoland              1.956e+07  8.728e+07   0.224    0.823    
	# countryRomania             9.378e+06  7.557e+07   0.124    0.901    
	# countryRussia             -4.568e+06  7.125e+07  -0.064    0.949    
	# countrySouth Africa        1.828e+07  7.125e+07   0.257    0.798    
	# countrySouth Korea        -6.995e+07  6.566e+07  -1.065    0.287    
	# countrySpain              -4.901e+06  6.309e+07  -0.078    0.938    
	# countrySweden             -4.215e+06  8.725e+07  -0.048    0.961    
	# countryTaiwan              6.468e+07  7.556e+07   0.856    0.392    
	# countryThailand            2.610e+06  6.899e+07   0.038    0.970    
	# countryUK                  2.381e+07  6.180e+07   0.385    0.700    
	# countryUSA                 4.864e+07  6.172e+07   0.788    0.431    
	# countryWest Germany       -1.044e+07  8.725e+07  -0.120    0.905    
	# budget                     3.484e-02  4.924e-03   7.075 1.77e-12 ***
	# movie_facebook_likes       9.832e+02  4.944e+01  19.886  < 2e-16 ***
	# imdb_score                 9.220e+06  1.000e+06   9.216  < 2e-16 ***
	# cast_total_facebook_likes  5.161e+02  5.395e+01   9.567  < 2e-16 ***
	# ---
	# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

	# Residual standard error: 61690000 on 3840 degrees of freedom
	  # (1152 observations deleted due to missingness)
	# Multiple R-squared:  0.2289,    Adjusted R-squared:  0.2188 
	# F-statistic:  22.8 on 50 and 3840 DF,  p-value: < 2.2e-16

#Take out country but add in language and rating
#Korean language is significant
#major content ratings (G, PG, PG-13) are significant
	model_3<-lm(gross~language+content_rating+budget+movie_facebook_likes+imdb_score+cast_total_facebook_likes,data=full_data)
	summary(model_3)
	# Call:
	# lm(formula = gross ~ language + content_rating + budget + movie_facebook_likes + 
		# imdb_score + cast_total_facebook_likes, data = full_data)

	# Residuals:
		   # Min         1Q     Median         3Q        Max 
	# -323637935  -31565342  -10280485   18253020  649148891 

	# Coefficients:
								# Estimate Std. Error t value Pr(>|t|)    
	# (Intercept)               -9.515e+07  3.500e+07  -2.718  0.00659 ** 
	# languageAboriginal        -4.082e+06  5.383e+07  -0.076  0.93955    
	# languageArabic            -5.036e+07  6.805e+07  -0.740  0.45931    
	# languageAramaic           -1.437e+07  6.803e+07  -0.211  0.83272    
	# languageBosnian            3.282e+07  6.805e+07   0.482  0.62963    
	# languageCantonese         -7.100e+06  3.991e+07  -0.178  0.85880    
	# languageCzech             -8.788e+06  6.804e+07  -0.129  0.89724    
	# languageDanish            -2.662e+07  4.828e+07  -0.551  0.58146    
	# languageDari              -3.328e+07  5.387e+07  -0.618  0.53667    
	# languageDutch             -6.498e+06  4.818e+07  -0.135  0.89272    
	# languageDzongkha           1.741e+06  6.823e+07   0.026  0.97965    
	# languageEnglish            2.379e+07  3.413e+07   0.697  0.48577    
	# languageFilipino           4.716e+06  6.803e+07   0.069  0.94474    
	# languageFrench            -1.500e+07  3.544e+07  -0.423  0.67209    
	# languageGerman            -1.275e+07  3.778e+07  -0.338  0.73565    
	# languageHebrew            -4.327e+05  4.825e+07  -0.009  0.99285    
	# languageHindi             -7.870e+06  3.879e+07  -0.203  0.83924    
	# languageHungarian         -9.621e+07  6.926e+07  -1.389  0.16490    
	# languageIcelandic          7.789e+06  6.822e+07   0.114  0.90911    
	# languageIndonesian        -3.475e+07  5.405e+07  -0.643  0.52025    
	# languageItalian           -1.551e+07  4.091e+07  -0.379  0.70454    
	# languageJapanese          -5.127e+07  3.821e+07  -1.342  0.17978    
	# languageKazakh             1.006e+07  6.803e+07   0.148  0.88245    
	# languageKorean            -1.423e+08  4.637e+07  -3.068  0.00217 ** 
	# languageMandarin          -1.184e+07  3.738e+07  -0.317  0.75154    
	# languageMaya               2.590e+07  6.804e+07   0.381  0.70348    
	# languageMongolian          6.155e+05  6.804e+07   0.009  0.99278    
	# languageNone              -7.493e+07  6.809e+07  -1.101  0.27118    
	# languageNorwegian         -2.661e+07  4.513e+07  -0.590  0.55543    
	# languagePersian           -4.963e+07  4.829e+07  -1.028  0.30414    
	# languagePortuguese        -1.303e+07  4.318e+07  -0.302  0.76290    
	# languageRomanian          -8.710e+06  6.868e+07  -0.127  0.89909    
	# languageRussian            7.190e+06  6.803e+07   0.106  0.91583    
	# languageSpanish           -4.565e+06  3.598e+07  -0.127  0.89905    
	# languageSwedish           -9.308e+05  6.824e+07  -0.014  0.98912    
	# languageTelugu            -2.323e+07  6.826e+07  -0.340  0.73362    
	# languageThai              -3.288e+06  4.818e+07  -0.068  0.94560    
	# languageVietnamese        -4.814e+06  6.804e+07  -0.071  0.94360    
	# languageZulu              -1.350e+06  6.804e+07  -0.020  0.98417    
	# content_ratingApproved     2.256e+07  1.696e+07   1.330  0.18352    
	# content_ratingG            6.760e+07  1.091e+07   6.198 6.32e-10 ***
	# content_ratingGP           3.080e+07  5.953e+07   0.517  0.60491    
	# content_ratingM            3.981e+07  4.259e+07   0.935  0.34996    
	# content_ratingNC-17       -1.057e+07  2.566e+07  -0.412  0.68044    
	# content_ratingNot Rated   -6.406e+06  1.291e+07  -0.496  0.61968    
	# content_ratingPassed      -1.167e+07  3.516e+07  -0.332  0.73993    
	# content_ratingPG           5.663e+07  9.335e+06   6.066 1.43e-09 ***
	# content_ratingPG-13        4.277e+07  9.176e+06   4.661 3.25e-06 ***
	# content_ratingR            8.134e+06  9.104e+06   0.893  0.37171    
	# content_ratingUnrated      1.883e+05  1.503e+07   0.013  0.99001    
	# content_ratingX            5.493e+06  2.080e+07   0.264  0.79171    
	# budget                     3.773e-02  5.194e-03   7.264 4.53e-13 ***
	# movie_facebook_likes       9.135e+02  4.762e+01  19.183  < 2e-16 ***
	# imdb_score                 1.247e+07  9.737e+05  12.806  < 2e-16 ***
	# cast_total_facebook_likes  4.863e+02  5.172e+01   9.402  < 2e-16 ***
	# ---
	# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

	# Residual standard error: 58840000 on 3836 degrees of freedom
	  # (1152 observations deleted due to missingness)
	# Multiple R-squared:  0.2992,    Adjusted R-squared:  0.2893 
	# F-statistic: 30.33 on 54 and 3836 DF,  p-value: < 2.2e-16

#What are the general breakdowns of language, country, and rating?
#Note that there are more R rated movies than PG-13 but the R-rating content
#was not significant in the model.  That may imply that the gross amount
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
