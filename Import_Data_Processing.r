#ORIE 4740 Project 
#Data Cleaning Script

#Import data (relevant only to Gloria)
full_data<-read.csv("C:/Users/glori/Documents/GitHub/IMDB_Data_Mining_Project/movie_metadata.csv",sep=",",header=TRUE)

#look at the metrics for all the data points
summary(full_data)

#Naive method, fit all the variables to the data 
#Try to predict gross, even though we know it's in different units
#all_predictors<-lm(gross~.,data=full_data)

#let's look at the results
#takes awhile to run since there are so many categorical variables
#summary(all_predictors)

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

