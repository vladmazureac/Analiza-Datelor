install.packages("openintro")
install.packages("tidyverse")

library(MASS)
library(openintro)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(rsample)
library(vip)


data_game <- read_csv('C:/Users/Vlad/OneDrive/Рабочий стол/Scoala/Anul 3/Analiza Datelor/Lab1/Video_Games.csv')
glimpse(data_game)

data_game <- na.omit(data_game)
data_game$Platform <- as.factor(data_game$Platform)
data_game$Genre <- as.factor(data_game$Genre)
data_game$User_Score <- as.factor(data_game$User_Score)
data_game$Name <- NULL
data_game$Publisher <- NULL
data_game$Developer <- NULL
data_game$Year_of_Release <- NULL
data_game$Rating <- NULL


numeric_columns <- names(data_game)[sapply(data_game, is.numeric)]
data_game <- data_game[data_game$User_Count > 10, ]
data_game <- subset(data_game, rowSums(data_game[numeric_columns] < 0.01) == 0)

ggplot(data_game, aes(x = Genre, fill = Genre)) +
  geom_bar()

frecventa_genuri <- table(data_game$Genre)
print(frecventa_genuri)

ggplot(data_game, aes(x = Platform, fill = Platform)) +
  geom_bar()

frecventa_platform <- table(data_game$Platform)
print(frecventa_platform)

data_game$User_Count <- log(data_game$User_Count)
data_game$User_Count_org <- exp(data_game$User_Count)

ggplot(data_game, aes(x = Platform, y = User_Count_org, fill = Platform)) +
  geom_boxplot() +
  labs(title = "Distribuția Numărului de jucători pe Platforme",
       x = "Gen",
       y = "Numărul de Jucători")

ggplot(data_game, aes(x = Genre, y = User_Count_org, fill = Genre)) +
  geom_boxplot() +
  labs(title = "Distribuția Cantității de Jucători pe Genuri",
       x = "Gen",
       y = "Cantitate Aproximativă de Jucători")


set.seed(123)
split_gamedataset <- initial_split(data_game, prop = 0.7, strata = 'User_Count')
dim(data_game)
datagame_train <- training(split_gamedataset)
summary(datagame_train)

glimpse(data_game)

# Creează histograma
hist(data_game, main="Distribuția de frecvență a User Count", xlab="User Count", ylab="Frecvență", col="lightblue", border="black")


#model 2
set.seed(123)
(datagame_model2 <- train(
  form = User_Count ~ Platform + Genre,
  data = datagame_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
))

summary(datagame_model2)

# Setul de date initial
data_predict <- data.frame(
  Platform = c('PSP'),
  Genre = c("Action", "Adventure", "Fighting", "Misc", "Platform", "Puzzle", "Racing", 
            "Role-Playing", "Shooter", "Simulation", "Sports", "Strategy")
)


prediction <- predict(datagame_model2, data_predict)

exp(prediction)

# Generarea tuturor combinatiilor
all_combinations <- expand.grid(data_predict$Platform, data_predict$Genre)

# Atribuirea de nume coloanelor
colnames(all_combinations) <- c("Platform", "Genre")

data_game_plot <- ggplot(datagame_train, aes(User_Count, Global_Sales)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(se = FALSE) +
  labs(x = 'Numărul de Jucători', y = 'Vânzări Globale', title = 'Numărul de Jucători și Vânzările Globale') +
  ggtitle('Numărul de Jucători și Vânzările Globale')

data_game_plot

data_game_plot_platformUserCount <- ggplot(datagame_train, aes(User_Count_org, Platform)) +
  geom_point(size = 1, alpha = 1) +
  geom_smooth(se = FALSE) +
  scale_x_continuous('User_Count') +
  ylab('Platform') +
  ggtitle('Numarul de jucatori pe Platforme')

data_game_plot_platformUserCount


ggplot(data_game, aes(x = Genre, y = User_Count, fill = Genre)) +
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

df2_datagame <- broom::augment(datagame_model2$finalModel, data = datagame_train)
glimpse(df2_datagame)
data_game_plot4 <- ggplot(df2_datagame, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab('Predicted values') +
  ylab('Residuals') +
  ggtitle('Model 3', subtitle = 'User_Count ~ Platform + Genre')
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

