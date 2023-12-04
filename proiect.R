install.packages("openintro")
install.packages("tidyverse")

library(MASS)

library(openintro)
library(tidyverse)


data_game <- read_csv('C:/Users/Vlad/OneDrive/Рабочий стол/Scoala/Anul 3/Analiza Datelor/Lab1/Video_Games.csv')
glimpse(data_game)

popular_game <- data_game %>%
  filter(Global_Sales >= 20)

ggplot(data_game, aes(x = Rating, fill = Rating)) +
  geom_bar()

numar_total_century <- table(data_game$Century_release)

numar_total_century

ggplot(data_game, aes(x = Genre, y = Global_Sales, fill = Genre)) +
  geom_boxplot() +
  labs(title = "Distribuția Vânzărilor Globale pe Genuri de Jocuri",
       x = "Gen",
       y = "Vânzări Globale")

ggplot(data_game, aes(x = Genre, y = User_Count)) +
  geom_boxplot() +
  labs(title = "Distribuția Cantității Aproximative de Jucători pe Genuri de Jocuri",
       x = "Gen",
       y = "Cantitate Aproximativă de Jucători")

ggplot(data_game, aes(x = Platform, y = User_Count)) +
  geom_boxplot() +
  labs(title = "Distribuția Cantității Aproximative de Jucători pe Platforme de Jocuri",
       x = "Platformă",
       y = "Cantitate Aproximativă de Jucători")

popular_games <- data_game %>% filter(Global_Sales >= 20)

ggplot(popular_games, aes(x = Rating, fill = Rating)) +
  geom_bar() +
  labs(title = "Numărul de Jocuri Populare în Funcție de Rating",
       x = "Rating",
       y = "Număr de Jocuri")


data_game<- subset(data, !is.na(User_Count))

glimpse(data_game)

library(dplyr)
library(ggplot2)

# modeling packages
library(caret)
library(rsample)
library(vip)


data_game$Platform <- as.factor(data_game$Platform)
data_game$Genre <- as.factor(data_game$Genre)
data_game$Year_of_Release <- as.double(data_game$Year_of_Release)
data_game$Publisher <- as.factor(data_game$Publisher)
data_game$Developer <- as.factor(data_game$Developer)
data_game$Rating <- as.factor(data_game$Rating)
data_game$User_Score <- as.factor(data_game$User_Score)
data_game <- na.omit(data_game)
data_game$Name <- NULL
data_game$Publisher <- NULL
data_game$Developer <- NULL

data_game <- data_game[data_game$User_Count > 10, ]
data_game <- subset(data_game, rowSums(data_game[numeric_columns] < 0.01) == 0)

data_game$User_Count <- log(data_game$User_Count)
data_game$User_Count_org <- exp(data_game$User_Count)

set.seed(123)
split_gamedataset <- initial_split(data_game, prop = 0.7, strata = 'User_Count')
dim(data_game)
datagame_train <- training(split_gamedataset)
summary(datagame_train)

glimpse(data_game)

hist(data_game$User_Count)


#model 1
set.seed(123)
(datagame_model1 <- train(
  form = User_Count ~ Platform,
  data = datagame_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
))

#model 2
set.seed(123)
(datagame_model2 <- train(
  form = User_Count ~ Platform + Genre,
  data = datagame_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
))

summary(datagame_model2)
sigma(datagame_model2)

reziduri <- residuals(datagame_model2)
reziduri

#model 3
set.seed(123)
datagame_model3 <- train(
  User_Count ~ .,
  data = datagame_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10),
)

datagame_model3 

data_predict <- data.frame(Platform = c('PS4', 'PS4'),
                           Genre = c('Role-Playing', 'Sports'))

prediction <- predict(datagame_model2, data_predict)

prediction

summary(resamples(list(
  model1 = datagame_model1,
  model2 = datagame_model2,
  model3 = datagame_model3)))



data_game_plot <- ggplot(datagame_train, aes(User_Count, Global_Sales)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = FALSE) +
  scale_y_continuous('User_Count') + 
  ylab('Global_Sales') +
  ggtitle('Numarul de jucatori si Vanzarile Globale') 

data_game_plot

data_game_plot_platformUserCount <- ggplot(datagame_train, aes(User_Count_org, Platform)) +
  geom_point(size = 1, alpha = 1) +
  geom_smooth(se = FALSE) +
  scale_x_continuous('User_Count') +
  ylab('Platform') +
  ggtitle('Numarul de jucatori pe Platforme')

data_game_plot_platformUserCount


ggplot(data_game, aes(x = Genre, y = User_Count_org, fill = Genre)) +
  geom_bar(stat = "summary", fun = "mean") +
  facet_wrap(~ Platform, scales = "free_y", ncol = 2) +
  labs(title = "Numărul mediu de jucători în funcție de Gen și Platformă",
       x = "Gen",
       y = "Număr mediu de jucători",
       fill = "Gen")



library(gridExtra)

data_game_plot2 <- ggplot(datagame_train, aes(User_Count, Global_Sales)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method = 'lm', se = F) +
  scale_y_log10('Global Sales', labels = scales::dollar,
                breaks = seq(0, 400000, by = 100000)) +
  xlab('User Count')
gridExtra::grid.arrange(data_game_plot, data_game_plot2, nrow = 1)

# Condition 2: Constant variance among residuals
df_datagame <- broom::augment(datagame_model1$finalModel, data = datagame_train)
glimpse(df_datagame)
data_game_plot3 <- ggplot(df_datagame, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab('Predicted values') +
  ylab('Residuals') +
  ggtitle('Model 1', subtitle = 'User_Count ~ Global_Sales')
data_game_plot3

df2_datagame <- broom::augment(datagame_model3$finalModel, data = datagame_train)
glimpse(df2_datagame)
data_game_plot4 <- ggplot(df2_datagame, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab('Predicted values') +
  ylab('Residuals') +
  ggtitle('Model 3', subtitle = 'User_Count ~ ,')
data_game_plot4

#3
df_datagame <- mutate(df_datagame, id = row_number())
glimpse(df_datagame)

df2_datagame <- mutate(df2_datagame, id = row_number())
glimpse(df2_datagame)

data_game_plot5 <- ggplot(df_datagame, aes(id, .std.resid)) + 
  geom_point(size = 1, alpha = .4) + 
  xlab('Row ID') +
  ylab('Residuals') +
  ggtitle('Model 1', subtitle = 'Correlated residuals.')
data_game_plot5

data_game_plot6 <- ggplot(df2_datagame, aes(id, .std.resid)) + 
  geom_point(size = 1, alpha = .4) + 
  xlab('Row ID') +
  ylab('Residuals') +
  ggtitle('Model 3', subtitle = 'Uncorrelated residuals.')
data_game_plot6

#4
vip(datagame_model3, num_features = 10)

# Calculul numărului mediu de jucători pentru fiecare combinație de gen și platformă
numar_jucatori_mediu <- aggregate(data_game$User_Count_org, by = list(Genre = data_game$Genre, Platform = data_game$Platform), FUN = mean)

# Afișarea rezultatelor în consolă
print(numar_jucatori_mediu)

