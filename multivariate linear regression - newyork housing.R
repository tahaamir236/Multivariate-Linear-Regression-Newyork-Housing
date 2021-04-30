install.packages("pacman")
library(pacman)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr, readr, mlbench, corrplot, Amelia, reshape2,
               caTools, car, knitr, dplyr, caret, ggstatsplot, Hmisc, sjPlot, sjmisc, sjlabelled) 

data <- import("housing-prices.txt")
data <-  subset(data)
data <- data[complete.cases(data), ]

summary(data)

summary(data$Price)
boxplot(data$Price, main = "Boxplot of Price",
        ylab = "Price",
        col = "darkgreen"
)

df_numeric <-  data.frame(subset(data, select = -c(Bathrooms,Bedrooms, Fireplaces)))
df_categorical <-  data.frame(subset(data, select = c(Price,Bathrooms,Bedrooms, Fireplaces)))

p <- ggplot(data, aes(Price)) + 
  geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
  geom_density(fill = "steelblue" , alpha = 0.5) + 
  theme(panel.background = element_rect(fill = '#ffffff')) + 
  ggtitle("Density Plot (Price)")

ggplotly(p)


hist.data.frame(df_numeric)


ggplot(data = df_categorical, aes(x=as.character(Bathrooms), y=Price)) +
  geom_boxplot(fill="steelblue") +
  labs(title="Number of Bathrooms vs Price", x="Bathrooms", y="Price")

ggplot(data = df_categorical, aes(x=as.character(Bedrooms), y=Price)) +
  geom_boxplot(fill="darkred") +
  labs(title="Number of Bedrooms vs Price", x="Bedrooms", y="Price")

ggplot(data = df_categorical, aes(x=as.character(Fireplaces), y=Price)) +
  geom_boxplot(fill="darkorange") +
  labs(title="Number of Fireplaces vs Price", x="Fireplaces", y="Price")

data %>%
  select(c(Living.Area, Bathrooms, Bedrooms, Fireplaces, Lot.Size, Age, Price,)) %>%
  melt(id.vars = "Price") %>%
  ggplot(aes(x = value, y = Price, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "House Price") +
  theme_minimal()

data.cor = cor(data)
corrplot(data.cor,  method = "number")

#preproc1 <- preProcess(data, method=c("range"))
#data <- predict(preproc1, data)

#data <- scale(dat)

set.seed(123)


split <- sample.split(data,SplitRatio =0.75)


train <- subset(data,split==TRUE)
test <- subset(data,split==FALSE)

model <- lm(Price ~ Living.Area + Bathrooms, data = train)
summary(model)
ggplot(data,aes(y=Price,x=Living.Area, Bathrooms))+geom_point()+geom_smooth(method="lm")
tab_model(model)
sstable <- Anova(model, type = 3)
sstable

residuals <- residuals(model)
residuals <- as.data.frame(residuals)
ggplot(residuals,aes(residuals)) +  geom_histogram(fill='blue',alpha=0.5)

plot(model)

test$predicted.Price <- predict(model,test)

pl1 <-test %>% 
  ggplot(aes(Price,predicted.Price)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of Price') +
  ylab('Predicted value of Price')+
  theme_bw()

ggplotly(pl1)

error <- test$Price-test$predicted.Price
rmse <- sqrt(mean(error)^2)

rmse
