#----------------------------------Packages---------------------------------------------
library(tidyverse)
#install.packages("DescTools")
library(DescTools)
#install.packages("energy")
library(energy)
#install.packages("car")
library(car)
#install.packages("randomForest")
library(randomForest)
#----------------------------------Loading the data-------------------------------------

data_ <- read_csv("dataset.csv")

data_ <- select(data_,-1) # removing the index column

#----------------------------------Handling missing value-------------------------------

sum(is.na(data_$track_id) * 1) # to see if there is any missing value in track_id column

missing_values <- data_[rowSums(is.na(data_) * 1)>0,]

nrow(missing_values) # shows the number of the rows

view(missing_values)

data_MVRemoved <- data_[!data_$track_id %in% missing_values$track_id,] # removing rows that their track_id is in the missing_values data frame

Mode(data_MVRemoved$artists)
Mode(data_MVRemoved$album_name)
Mode(data_MVRemoved$track_name)

Beatles_songs <- data_MVRemoved[data_MVRemoved$artists == "The Beatles",]

RRR_Records <- data_MVRemoved[data_MVRemoved$track_name == "Run Rudolph Run",]

unique(Beatles_songs$track_genre)

unique(RRR_Records$track_genre)

data_clean <- data_MVRemoved # now we make a copy of the clean data before we proceed

#----------------------------------splitting the Data---------------------------------

set.seed(1026)
data_clean <- data_clean[sample(1:nrow(data_clean)),] # shuffling the data (make sure to include set.seed()  as well while running the code !)

train_data <- data_clean[1: (0.8 * nrow(data_clean)),]


test_data <- data_clean[(nrow(train_data)+ 1 ): nrow(data_clean), ]

nrow(test_data)/nrow(train_data) # testing to see if the data was splt correctly
(nrow(test_data) + nrow(train_data)) == nrow(data_clean)

#----------------------------------EDA---------------------------------------

GP_EDA <- train_data[,c("track_name","popularity","track_genre")] # Genre popularity data set for EDA

GP_EDA_PS <- GP_EDA %>%                            # summation of the popularity to identify most popular and least popular
  group_by(track_genre) %>%
  summarize(total_popularity = sum(popularity, na.rm = TRUE))

view(GP_EDA_PS)
GP_EDA_PS <- arrange(GP_EDA_PS,desc(total_popularity))
GP_EDA_PS$track_genre[1:4] # top 4
tail(GP_EDA_PS$track_genre,2) #bottom 2

GP_EDA_copy <- GP_EDA # making copy to change the data

#dividing them to top 4 categories and last 2 , everything else

GP_EDA_copy$track_genre <-                       
  case_when(
    GP_EDA$track_genre %in% c("chill","k-pop", "sad" , "pop-film") ~ "top4",
    GP_EDA$track_genre %in% c("iranian", "romance") ~ "bottom2",
    TRUE ~ "other"
  )

# visualization of the song popularity distribution by genre

ggplot(GP_EDA_copy, aes(x = track_name, y = popularity, color = track_genre)) + 
  geom_point(  size = 1) + 
  scale_color_manual(values = c("top4" = rgb(0,0,1), "bottom2" = rgb(1,0,0), "other" = rgb(0.82,0.82,0.82,0.25) ) ) + 
  labs(x="Songs", y="Popularity",
       title="Song Popularity Distribution by Genre",
       caption="Spotify DataSet") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

#extracting data for chill genres in a different dataset for train and test

train_data_chill <- train_data[train_data$track_genre=="chill" ,]

test_data_chill <- test_data[test_data$track_genre == "chill",]

# correlation analysis  ////////////////////////////////////////////////////////

#Chill track genre  ************************************************************

#popularity:
# normal distribution test
shapiro.test(train_data_chill$popularity)
#plot
qqnorm(train_data_chill$popularity)

#duration_ms:
#scatter-plot:

ggplot(train_data_chill, aes(x = duration_ms, y = popularity)) + 
  geom_point() +
  labs(x="duration", y="Popularity",
       title="song popularities based on their duration",
       caption="Spotify DataSet")

#Distance Correlation test:

dcor.test(train_data_chill$duration_ms, train_data_chill$popularity, R = 10000)


#explicit
#visualization
ggplot(train_data_chill, aes(x = track_name, y = popularity, color = explicit)) + 
  geom_point(aes( size = 1)) +
  labs(x="tracks", y="Popularity",
       title="song popularities",
       caption="Spotify DataSet") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) + 
  scale_color_manual(
    values = c( "TRUE" = rgb(1, 0, 0.247, 0.70) 
                , "FALSE" = rgb(0,0.875,1,0.70))
  )

#visualization for checking the distribution 

ExplicitQQtest0 <- train_data_chill[train_data_chill$explicit == FALSE ,]
ExplicitQQtest1 <- train_data_chill[train_data_chill$explicit == TRUE ,]

qqnorm(ExplicitQQtest1$popularity)
qqnorm(ExplicitQQtest0$popularity)

# Mann-Whitney U test


wilcox.test(popularity~explicit, data = train_data_chill)

#danceability
#scatter-plot
ggplot(train_data_chill, aes(x = danceability, y = popularity)) + 
  geom_point() +
  labs(x="danceability", y="Popularity",
       title="song popularities based on their danceability",
       caption="Spotify DataSet")


#Distance Correlation test:

dcor.test(train_data_chill$danceability, train_data_chill$popularity, R = 10000)

#energy
#scatter-plot
ggplot(train_data_chill, aes(x = energy, y = popularity)) + 
  geom_point() +
  labs(x="energy", y="Popularity",
       title="song popularities based on their energy",
       caption="Spotify DataSet")


#Distance Correlation test:

dcor.test(train_data_chill$energy, train_data_chill$popularity, R = 10000)

#key
#Fligner-Killeen Test of Homogeneity of Variances
fligner.test(popularity ~ key, data = train_data_chill)


# Kruskal-Wallis Test that is an non-parametric test
kruskal.test(popularity ~ key, data = train_data_chill)

#loudness
#scatter-plot
ggplot(train_data_chill, aes(x = loudness, y = popularity)) + 
  geom_point() +
  labs(x="loudness", y="Popularity",
       title="song popularities based on their loudness",
       caption="Spotify DataSet")


#Distance Correlation test:

dcor.test(train_data_chill$loudness, train_data_chill$popularity, R = 10000)

#mode
#Q-Q plot

modeQQtest0 <- train_data_chill[train_data_chill$mode == 0 ,]
modeQQtest1 <- train_data_chill[train_data_chill$mode == 1 ,]

qqnorm(modeQQtest0$popularity)
qqnorm(modeQQtest1$popularity)

# Mann-Whitney U test
wilcox.test(popularity~mode, data = train_data_chill)

#speechiness
#scatter-plot
ggplot(train_data_chill, aes(x = speechiness, y = popularity)) + 
  geom_point() +
  labs(x="speechiness", y="Popularity",
       title="song popularities based on their speechiness",
       caption="Spotify DataSet")


#Distance Correlation test:

dcor.test(train_data_chill$speechiness, train_data_chill$popularity, R = 10000)

#acousticness
#scatter-plot
ggplot(train_data_chill, aes(x = acousticness, y = popularity)) + 
  geom_point() +
  labs(x="acousticness", y="Popularity",
       title="song popularities based on their acousticness",
       caption="Spotify DataSet")


#Distance Correlation test:

dcor.test(train_data_chill$acousticness, train_data_chill$popularity, R = 10000)

#instrumentalness
#scatter-plot
ggplot(train_data_chill, aes(x = instrumentalness, y = popularity)) + 
  geom_point() +
  labs(x="instrumentalness", y="Popularity",
       title="song popularities based on their instrumentalness",
       caption="Spotify DataSet")


#Distance Correlation test:

dcor.test(train_data_chill$instrumentalness, train_data_chill$popularity, R = 10000)

#liveness
#scatter-plot
ggplot(train_data_chill, aes(x = liveness, y = popularity)) + 
  geom_point() +
  labs(x="liveness", y="Popularity",
       title="song popularities based on their liveness",
       caption="Spotify DataSet")


#Distance Correlation test:

dcor.test(train_data_chill$liveness, train_data_chill$popularity, R = 10000)

#valence
#scatter-plot
ggplot(train_data_chill, aes(x = valence, y = popularity)) + 
  geom_point() +
  labs(x="valence", y="Popularity",
       title="song popularities based on their valence",
       caption="Spotify DataSet")


#Distance Correlation test:

dcor.test(train_data_chill$valence, train_data_chill$popularity, R = 10000)


#tempo
#scatter-plot
ggplot(train_data_chill, aes(x = tempo, y = popularity)) + 
  geom_point() +
  labs(x="tempo", y="Popularity",
       title="song popularities based on their tempo",
       caption="Spotify DataSet")


#Distance Correlation test:

dcor.test(train_data_chill$tempo, train_data_chill$popularity, R = 10000)


#time_signature
#Fligner-Killeen Test of Homogeneity of Variances
fligner.test(popularity ~ time_signature, data = train_data_chill)


# Kruskal-Wallis Test that is an non-parametric test
kruskal.test(x = train_data_chill$popularity, g = train_data_chill$time_signature, simulate.p.value = TRUE, B = 10000)


#----------------------------------Heat map--------------------------------------
HM_data <- data.frame(
  Variable = c("duration_ms","danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveliness", "valanve", "tempo"),
  Correlation = c(0.081, 0.122, 0.091, 0.119, 0.115, 0.090, 0.093, 0.086, 0.099, 0.063 )
)

ggplot(HM_data, aes(x = Variable, y = 1, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Correlation), color = "black" , size = 14) +  
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", midpoint = 0,
    limit = c(-0.2, 0.2), name = "Correlation"
  ) +
  labs(title = "Correlation with Popularity", x = "Variable", y = "") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 20)
  )
#----------------------------------logistic regression------------------------------------------------------------

train_data_chill_LR <- train_data_chill
test_data_chill_LR <- test_data_chill

percentile_70 <- quantile(train_data_chill_LR$popularity, probs = 0.7)
percentile_70_test <- quantile(test_data_chill_LR$popularity, probs = 0.7)

train_data_chill_LR$popularity <- ifelse(train_data_chill_LR$popularity > percentile_70, 1 , 0)
test_data_chill_LR$popularity <- ifelse(test_data_chill_LR$popularity > percentile_70_test, 1 , 0)

#explicit

log_model_E <- glm(train_data_chill_LR$popularity ~ train_data_chill_LR$explicit, data = train_data_chill, family = binomial)

predicted_E <- predict(log_model_E, type = "response")

log_odds_E <- log(predicted_E / (1 - predicted_E))

plot(train_data_chill_LR$explicit, log_odds_E, main = "Log-Odds vs Independent Variable",
     xlab = "explicit", ylab = "Log-Odds", pch = 19, col = "blue")

lines(lowess(train_data_chill_LR$explicit, log_odds_E), col = "red", lwd = 2)

#danceability

log_model_D <- glm(train_data_chill_LR$popularity ~ train_data_chill_LR$danceability, data = train_data_chill, family = binomial)

predicted_D <- predict(log_model_D, type = "response")

log_odds_D <- log(predicted_D / (1 - predicted_D))

plot(train_data_chill_LR$danceability, log_odds_D, main = "Log-Odds vs Independent Variable",
     xlab = "danceability", ylab = "Log-Odds", pch = 19, col = "blue")

lines(lowess(train_data_chill_LR$danceability, log_odds_D), col = "red", lwd = 2)

#key

log_model_K <- glm(train_data_chill_LR$popularity ~ train_data_chill_LR$key, data = train_data_chill, family = binomial)

predicted_K <- predict(log_model_K, type = "response")

log_odds_K <- log(predicted_K / (1 - predicted_K))

plot(train_data_chill_LR$key, log_odds_K, main = "Log-Odds vs Independent Variable",
     xlab = "key", ylab = "Log-Odds", pch = 19, col = "blue")

lines(lowess(train_data_chill_LR$key, log_odds_K), col = "red", lwd = 2)

#loudness

log_model_L <- glm(train_data_chill_LR$popularity ~ train_data_chill_LR$loudness, data = train_data_chill, family = binomial)

predicted_L <- predict(log_model_L, type = "response")

log_odds_L <- log(predicted_L / (1 - predicted_L))

plot(train_data_chill_LR$loudness, log_odds_L, main = "Log-Odds vs Independent Variable",
     xlab = "loudness", ylab = "Log-Odds", pch = 19, col = "blue")

lines(lowess(train_data_chill_LR$loudness, log_odds_L), col = "red", lwd = 2)


#speechiness

log_model_S <- glm(train_data_chill_LR$popularity ~ train_data_chill_LR$speechiness, data = train_data_chill, family = binomial)

predicted_S <- predict(log_model_S, type = "response")

log_odds_S <- log(predicted_S / (1 - predicted_S))

plot(train_data_chill_LR$speechiness, log_odds_S, main = "Log-Odds vs Independent Variable",
     xlab = "speechiness", ylab = "Log-Odds", pch = 19, col = "blue")

lines(lowess(train_data_chill_LR$speechiness, log_odds_S), col = "red", lwd = 2)

#time_signature

log_model_TS <- glm(train_data_chill_LR$popularity ~ train_data_chill_LR$time_signature, data = train_data_chill, family = binomial)

predicted_TS <- predict(log_model_TS, type = "response")

log_odds_TS <- log(predicted_TS / (1 - predicted_TS))

plot(train_data_chill_LR$time_signature, log_odds_TS, main = "Log-Odds vs Independent Variable",
     xlab = "time_signature", ylab = "Log-Odds", pch = 19, col = "blue")

lines(lowess(train_data_chill_LR$time_signature, log_odds_TS), col = "red", lwd = 2)

#Logistic Regression model

LR_model <- glm(popularity ~ loudness +
                  explicit +
                  key +
                  danceability +
                  speechiness +
                  time_signature, 
                data = train_data_chill_LR, 
                family = binomial)

summary(LR_model)

LR_model_Enhanced <- glm(popularity ~ explicit +
                           time_signature +
                           danceability, 
                         data = train_data_chill_LR, 
                         family = binomial)

summary(LR_model_Enhanced)

#colinearity check
vif(LR_model)
vif(LR_model_Enhanced)

#logistic regression model accuracy 


predicted <- predict(LR_model,newdata = test_data_chill_LR, type = "response")

predicted_result <- ifelse(predicted > 0.5 , 1, 0)

predicted_error <- mean(
  predicted_result != test_data_chill_LR$popularity) 

print(paste('Accuracy',1-predicted_error))


#logistic regression model after enhancing


predicted_enhanced <- predict(LR_model_Enhanced,newdata = test_data_chill_LR, type = "response")

predicted_result_enhanced <- ifelse(predicted_enhanced > 0.5 , 1, 0)

predicted_error_Enhanced <- mean(
  predicted_result_enhanced != test_data_chill_LR$popularity) 

print(paste('Accuracy',1-predicted_error_Enhanced))


#----------------------------------Random Forest--------------------------------
#Rnadom Forest Model


RF_model <- randomForest( popularity ~ explicit +
                            danceability +
                            key + 
                            loudness + 
                            speechiness + 
                            time_signature , data = train_data_chill , ntree = 100)

RF_predict <- predict(RF_model , newdata = test_data_chill )

summary(RF_predict)

#evaluation


rmse <- sqrt(mean((RF_predict - test_data_chill$popularity)^2))
print(rmse)

rss <- sum((RF_predict - test_data_chill$popularity)^2)
tss <- sum((test_data_chill$popularity - mean(test_data_chill$popularity))^2)
r_squared <- 1 - (rss / tss)
print(r_squared)

