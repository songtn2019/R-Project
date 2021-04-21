#Project: Stock Return Prediction

setwd("C:/Users/songt/Documents/Project")

# Load R packages
library(tidyverse)

# Load Data--------------------------------------------------------------------------------------------------------
stock=read_csv("Data_ML_Tian.csv")
str(stock)
summary(stock)

stock <- stock %>%
  mutate(Year = floor(Date/10000))%>%
  filter(Year>=1995)

#EDA---------------------------------------------------------------------------------------------------------------
#Summary Statistics, winsorize variables first

# Winsorization at 1st and 99th percentile
winsorization <- function(x){
  percentiles <- quantile(x, probs=c(0.01,0.99), na.rm=TRUE)
  pLOWER <- percentiles["1%"]
  pUPPER <- percentiles["99%"]
  x.w <- ifelse(x <= pLOWER, pLOWER, x)
  x.w <- ifelse(x >= pUPPER, pUPPER, x.w)
  return(x.w)
}
stock_win <- stock %>%
  mutate_at(c("MC","BM","NSI", "IA","SREV","MOM","ROE","DTV","IV","SEASON"), winsorization)
  
stock_sum <- stock_win %>%
  select(RET, MC, BM, NSI, IA, SREV, MOM, ROE, DTV, IV, SEASON) %>%
  summarize_each(funs(min = min, 
                      q25 = quantile(., 0.25), 
                      median = median, 
                      q75 = quantile(., 0.75), 
                      max = max,
                      mean = mean, 
                      sd = sd)) 

# reshape it using tidyr functions
stock_sum_tidy <- stock_sum %>% gather(stat, val) %>%
  separate(stat, into = c("Variable", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(Variable, min, q25, median, q75, max, mean, sd)  %>% # reorder columns
  mutate_at(vars(mean, sd), funs(round(., 2)))


# Transform predictors to [-1,1]

rankvar <- function(x) rank(x)

stock_rank <- stock %>%
  group_by(Date) %>%
  mutate_at(c("MC","BM","NSI", "IA","SREV","MOM","ROE","DTV","IV","SEASON"), rankvar)

transfvar <- function(x) (x-500.5)/499.5

stock_final <- stock_rank %>%
  mutate_at(c("MC","BM","NSI", "IA","SREV","MOM","ROE","DTV","IV","SEASON"), transfvar)

summary(stock_final)

# Correlation Matrix

corr_matrix <- as.data.frame(distinct(stock_final, Date))

for (i in 1:10){
r <- by(stock_final, stock_final$Date, FUN = function(X) cor(X$RET, X[,i+3]))
r <- data.frame(corr = as.vector(r))
corr_matrix <- cbind(corr_matrix, r)
}

colnames(corr_matrix) <- c("Date", "MC","BM","NSI", "IA","SREV","MOM","ROE","DTV","IV","SEASON")

corr_matrix_plot <- corr_matrix %>%
    mutate(Year = floor(Date/10000))%>%
    group_by(Year) %>%
    summarize_at(c("MC","BM","NSI", "IA","SREV","MOM","ROE","DTV","IV","SEASON"), mean)

corr_matrix_plot1 <- as.matrix(corr_matrix_plot[,2:11])
row.names(corr_matrix_plot1) <- seq(1995,2019,1)

#heatmap
library(NMF)
aheatmap(corr_matrix_plot1, Rowv=NA, Colv=NA, color = colorRampPalette(c("navy", "white", "firebrick3"))(50))
         

#Model Estimation--------------------------------------------------------------------------------------------------

# 1. Multiple Linear Regression
betaMatrix <- matrix(0, nrow = 15, ncol = 11)
listMSE_train <- matrix(0, nrow=15, ncol=1)
listMSE_test <- matrix(0, nrow=15, ncol=1)
prediction_lm <-matrix(0, nrow=12000, ncol=15)

for(i in 1:15) {
  
  # select data
  stock_train <- stock_final[stock_final$Year %in% c((i+1994): (i+2003)),]
  stock_test <- stock_final[stock_final$Year == (i+2004),]
  
  # train model
  model <- lm(RET~MC + BM + NSI +IA + SREV + MOM + ROE + DTV +IV + SEASON, data=stock_train)
  betaMatrix[i,] <- model$coefficients
  mysummary <- summary(model)
  listMSE_train[i] <- mysummary$sigma^2
  
  # predict 
  predict_test <-predict(model, newdata=stock_test)
  prediction_lm[,i] <- predict_test
  
  # out-of-sample MSE
  listMSE_test[i]=mean((stock_test$RET-predict_test)^2)
}

#portfolio level
return_lm <- data.frame(predict=double(),
                     ret = double(),
                     month=integer(),
                     year=integer()
                     )

for (i in 1:15) {
pl <- as.data.frame(prediction_lm[,i])
colnames(pl)<-c("predict")
test_actual <- stock_final[stock_final$Year == (i+2004),]$RET
month <- rep(c(seq(1,12)), each = 1000)
year <- i+2004
pl_ret <-cbind(pl, test_actual, month, year)
return_lm <- rbind(return_lm, pl_ret)
}

return_lm <- return_lm %>%
  group_by(year, month) %>%
  mutate_at(vars(predict), list(rank =rankvar))%>%
  mutate(rank_group=ceiling(rank/100))

ret_lm_mean <-return_lm %>%
  group_by(year, month, rank_group) %>%
  summarize(ret_mean=mean(test_actual))%>%
  spread(rank_group, ret_mean, sep = "")%>%
  mutate(hml=rank_group10-rank_group1)

ret_lm_mean_year <- ret_lm_mean %>%
  group_by(year) %>%
  summarize(mean = mean(hml))

SR_lm <- mean(ret_lm_mean$hml)/sd(ret_lm_mean$hml)  #0.02384626


# 2. Forecast Combination using Univariate Regreesions 

betaMatrix_u <- matrix(0, nrow = 150, ncol = 10)
listMSE_train_u <- matrix(0, nrow=15, ncol=1)
listMSE_test_u <- matrix(0, nrow=15, ncol=1)
prediction_lm_u <-matrix(0, nrow=12000, ncol=15)

for(i in 1:15) {
  
  # select data
  stock_train <- stock_final[stock_final$Year %in% c((i+1994): (i+2003)),]
  stock_test <- stock_final[stock_final$Year == (i+2004),]
  
  # train model
  model_MC <- lm(RET ~ MC, data=stock_train)
  betaMatrix_u[(10*(i-1)+1),] <- model_MC$coefficients
  predict_MC <- predict(model_MC, stock_train)
  predict_MC_test <- predict(model_MC, newdata=stock_test)
  
  model_BM <- lm(RET ~ BM, data=stock_train)
  betaMatrix_u[(10*(i-1)+2),] <- model_BM$coefficients
  predict_BM <- predict(model_BM, stock_train)
  predict_BM_test <- predict(model_BM, newdata=stock_test)
  
  model_NSI <- lm(RET ~ NSI , data=stock_train)
  betaMatrix_u[(10*(i-1)+3),] <- model_NSI$coefficients
  predict_NSI <- predict(model_NSI, stock_train)
  predict_NSI_test <- predict(model_NSI, newdata=stock_test) 
  
  model_IA <- lm(RET ~ IA , data=stock_train)
  betaMatrix_u[(10*(i-1)+4),] <- model_IA$coefficients
  predict_IA <- predict(model_IA, stock_train) 
  predict_IA_test <- predict(model_IA, newdata=stock_test)
  
  model_SREV <- lm(RET ~ SREV , data=stock_train)
  betaMatrix_u[(10*(i-1)+5),] <- model_SREV$coefficients
  predict_SREV <- predict(model_SREV, stock_train) 
  predict_SREV_test <- predict(model_SREV, newdata=stock_test)
  
  model_MOM <- lm(RET ~ MOM , data=stock_train)
  betaMatrix_u[(10*(i-1)+6),] <- model_MOM$coefficients
  predict_MOM <- predict(model_MOM, stock_train) 
  predict_MOM_test <- predict(model_MOM, newdata=stock_test)
  
  model_ROE <- lm(RET ~ ROE , data=stock_train)
  betaMatrix_u[(10*(i-1)+7),] <- model_ROE$coefficients
  predict_ROE <- predict(model_ROE, stock_train) 
  predict_ROE_test <- predict(model_ROE, newdata=stock_test)
  
  model_DTV <- lm(RET ~ DTV , data=stock_train)
  betaMatrix_u[(10*(i-1)+8),] <- model_DTV$coefficients
  predict_DTV <- predict(model_DTV, stock_train) 
  predict_DTV_test <- predict(model_DTV, newdata=stock_test)
  
  model_IV <- lm(RET ~ IV , data=stock_train)
  betaMatrix_u[(10*(i-1)+9),] <- model_IV$coefficients
  predict_IV <- predict(model_IV, stock_train) 
  predict_IV_test <- predict(model_IV, newdata=stock_test)
  
  model_SEASON <- lm(RET ~ SEASON , data=stock_train)
  betaMatrix_u[(10*(i-1)+10),] <- model_SEASON$coefficients
  predict_SEASON <- predict(model_SEASON, stock_train) 
  predict_SEASON_test <- predict(model_SEASON, newdata=stock_test)
  
  predict_train <- cbind(predict_MC, predict_BM, predict_NSI, predict_IA, predict_SREV,
                         predict_MOM, predict_ROE, predict_DTV, predict_IV, predict_SEASON)
  predict_train_mean <- apply(predict_train, 1, mean)
  listMSE_train_u[i] <- mean((stock_train$RET-predict_train_mean)^2)

  # out-of-sample  
  predict_test <- cbind(predict_MC_test, predict_BM_test, predict_NSI_test, predict_IA_test, predict_SREV_test,
                         predict_MOM_test, predict_ROE_test, predict_DTV_test, predict_IV_test, predict_SEASON_test)
  predict_test_mean  <- apply(predict_test, 1, mean)
  prediction_lm_u[,i] <- predict_test_mean
  listMSE_test_u[i]=mean((stock_test$RET-predict_test_mean)^2)
}


#portfolio level
return_lm_u <- data.frame(predict=double(),
                        ret = double(),
                        month=integer(),
                        year=integer()
)

for (i in 1:15) {
  plu <- as.data.frame(prediction_lm_u[,i])
  colnames(plu)<-c("predict")
  test_actual <- stock_final[stock_final$Year == (i+2004),]$RET
  month <- rep(c(seq(1,12)), each = 1000)
  year <- i+2004
  plu_ret <-cbind(plu, test_actual, month, year)
  return_lm_u <- rbind(return_lm_u, plu_ret)
}

return_lm_u <- return_lm_u %>%
  group_by(year, month) %>%
  mutate_at(vars(predict), list(rank =rankvar))%>%
  mutate(rank_group=ceiling(rank/100))

ret_lm_u_mean <-return_lm_u %>%
  group_by(year, month, rank_group) %>%
  summarize(ret_mean=mean(test_actual))%>%
  spread(rank_group, ret_mean, sep = "")%>%
  mutate(hml=rank_group10-rank_group1)

ret_lm_u_mean_year <- ret_lm_u_mean %>%
  group_by(year) %>%
  summarize(mean = mean(hml))

SR_lm_u <- mean(ret_lm_u_mean$hml)/sd(ret_lm_u_mean$hml)  #0.02957562



# 3. Random Forest

library(h2o)

# create feature names
y <- "RET"
x <- setdiff(names(stock_final), c("PERMNO","Date", "RET", "Rank_MC", "Year"))

listMSE_rf_train <- matrix(0, nrow=15, ncol=1)
listMSE_rf_OOB <- matrix(0, nrow=15, ncol=1)
listMSE_rf_test <- matrix(0, nrow=15, ncol=1)
Imp_rf <- data.frame(variable=character(), 
                  relative_importance=double(), 
                  scaled_importance=double(), 
                  percentage=double())
prediction_rf <-matrix(0, nrow=12000, ncol=15)

# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = 300, 
  mtries      = c(3, 4, 5),
  max_depth   = c(1, 2, 3, 4, 5, 6)
)

h2o.init(max_mem_size = "24g")

system.time(
  for(i in 1:15) {
    
    # select data
    stock_train <- stock_final[stock_final$Year %in% c((i+1994): (i+2003)),]
    stock_test <- stock_final[stock_final$Year == (i+2004),]
    
    # turn training set into h2o object
    train.h2o <- as.h2o(stock_train)
    
    # build grid search 
    grid <- h2o.grid(
      algorithm = "randomForest",
      grid_id = paste0("rf_grid",i), 
      x = x, 
      y = y, 
      training_frame = train.h2o,
      hyper_params = hyper_grid.h2o,
      search_criteria = list(strategy = "Cartesian")
    )
    
    # collect the results and sort by our model performance metric of choice
    grid_perf <- h2o.getGrid(
      grid_id = paste0("rf_grid",i), 
      sort_by = "mse", 
      decreasing = FALSE
    )
    
    best_model_id <- grid_perf@model_ids[[1]]
    best_model <- h2o.getModel(best_model_id)
    
    pred_rf_train <- predict(best_model, train.h2o)
    listMSE_rf_train[i]=mean((stock_train$RET-pred_rf_train)^2)
    listMSE_rf_OOB[i]=h2o.mse(best_model)
    
    stock_test.h2o <- as.h2o(stock_test)
    pred_rf_test <- predict(best_model, stock_test.h2o)
    prediction_rf[, i] <- as.vector(unlist(pred_rf_test))
    listMSE_rf_test[i]=mean((stock_test$RET-pred_rf_test)^2)
    
    png(paste0("rf_Impplot",i,".png"))
    h2o.varimp_plot(best_model)
    dev.off()
    
    Imp1<-h2o.varimp(best_model)
    Imp_rf <-rbind(Imp_rf, Imp1)
    
  }
)

#h2o.shutdown()

#portfolio return
return_rf <- data.frame(predict=double(),
                         ret = double(),
                         month=integer(),
                         year=integer()
)

for (i in 1:15) {
  prf <- as.data.frame(prediction_rf[,i])
  colnames(prf)<-c("predict")
  test_actual <- stock_final[stock_final$Year == (i+2004),]$RET
  month <- rep(c(seq(1,12)), each = 1000)
  year <- i+2004
  prf_ret <-cbind(prf, test_actual, month, year)
  return_rf <- rbind(return_rf, prf_ret)
}

return_rf <- return_rf %>%
  group_by(year, month) %>%
  mutate_at(vars(predict), list(rank =rankvar))%>%
  mutate(rank_group=ceiling(rank/100))

ret_rf_mean <-return_rf %>%
  group_by(year, month, rank_group) %>%
  summarize(ret_mean=mean(test_actual))%>%
  spread(rank_group, ret_mean, sep = "")%>%
  mutate(hml=rank_group10-rank_group1)

ret_rf_mean_year <- ret_rf_mean %>%
  group_by(year) %>%
  summarize(mean = mean(hml, na.rm=TRUE))

SR_rf <- mean(ret_rf_mean$hml, na.rm=TRUE)/sd(ret_rf_mean$hml, na.rm=TRUE) #0.06334078



# 4. Gradient Boosting

# create feature names
y <- "RET"
x <- setdiff(names(stock_final), c("PERMNO","Date", "RET", "Rank_MC", "Year"))

listMSE_gbm_train <- matrix(0, nrow=15, ncol=1)
listMSE_gbm_test <- matrix(0, nrow=15, ncol=1)
Imp_gbm <- data.frame(variable=character(), 
                  relative_importance=double(), 
                  scaled_importance=double(), 
                  percentage=double())
prediction_gbm <-matrix(0, nrow=12000, ncol=15)


# create hyperparameter grid
hyper_grid <- list(
  max_depth = c(1, 2, 3, 5),
  learn_rate = c(0.01, 0.05, 0.1)
)

h2o.init(max_mem_size = "24g")

system.time(
for(i in 1:15) {
  
  # select data
  stock_train <- stock_final[stock_final$Year %in% c((i+1994): (i+2003)),]
  stock_test <- stock_final[stock_final$Year == (i+2004),]
  
  # turn training set into h2o object
  train.h2o <- as.h2o(stock_train)
  
  # create training & validation sets
  split <- h2o.splitFrame(train.h2o, ratios = 0.75)
  train <- split[[1]]
  valid <- split[[2]]

  # perform grid search 
  grid <- h2o.grid(
    algorithm = "gbm",
    grid_id = paste0("gbm_grid",i),
    x = x, 
    y = y, 
    training_frame = train,
    validation_frame = valid,
    hyper_params = hyper_grid,
    ntrees = 5000,
    stopping_rounds = 10,
    stopping_tolerance = 0,
    seed = 236
  )

  # collect the results and sort by our model performance metric of choice
  grid_perf <- h2o.getGrid(
    grid_id = paste0("gbm_grid",i), 
    sort_by = "mse", 
    decreasing = FALSE
)

  best_model_id <- grid_perf@model_ids[[1]]
  best_model <- h2o.getModel(best_model_id)
  
  pred_gbm_train <- predict(best_model, train.h2o)
  listMSE_gbm_train[i]=mean((stock_train$RET-pred_gbm_train)^2)
  
  stock_test.h2o <- as.h2o(stock_test)
  pred_gbm_test <- predict(best_model, stock_test.h2o)
  prediction_gbm[, i] <- as.vector(unlist(pred_gbm_test))
  listMSE_gbm_test[i]=mean((stock_test$RET-pred_gbm_test)^2)
  
  png(paste0("gbm_Impplot",i,".png"))
  h2o.varimp_plot(best_model)
  dev.off()
  
  Imp2<-h2o.varimp(best_model)
  Imp_gbm <-rbind(Imp_gbm, Imp2)
  
}
)

#portfolio level
return_gbm <- data.frame(predict=double(),
                     ret = double(),
                     month=integer(),
                     year=integer()
)

for (i in 1:15) {
  pgbm <- as.data.frame(prediction_gbm[,i])
  colnames(pgbm)<-c("predict")
  test_actual <- stock_final[stock_final$Year == (i+2004),]$RET
  month <- rep(c(seq(1,12)), each = 1000)
  year <- i+2004
  pgbm_ret <-cbind(pgbm, test_actual, month, year)
  return_gbm <- rbind(return_gbm, pgbm_ret)
}

return_gbm <- return_gbm %>%
  group_by(year, month) %>%
  mutate_at(vars(predict), list(rank =rankvar))%>%
  mutate(rank_group=ceiling(rank/100))

ret_gbm_mean <-return_gbm %>%
  group_by(year, month, rank_group) %>%
  summarize(ret_mean=mean(test_actual))%>%
  spread(rank_group, ret_mean, sep = "")%>%
  mutate(hml=rank_group10-rank_group1)

ret_gbm_mean_year <- ret_gbm_mean %>%
  group_by(year) %>%
  summarize(mean = mean(hml, na.rm=TRUE))

SR_gbm <-mean(ret_gbm_mean$hml, na.rm=TRUE)/sd(ret_gbm_mean$hml, na.rm=TRUE) #0.1286673


# summarize results -----------------------------------------------------------------------------------------------
#stock-level
MSE_train <- as.data.frame(cbind(listMSE_train, listMSE_train_u, listMSE_rf_train, listMSE_gbm_train))
colnames(MSE_train) <- c("MLR", "FC", "RF","Boosting")

MSE_test <- as.data.frame(cbind(listMSE_test, listMSE_test_u, listMSE_rf_test, listMSE_gbm_test))
colnames(MSE_test) <- c("MLR", "FC", "RF","Boosting")

Year <- seq(2005,2019,1)
MSE_test_plot <- cbind(MSE_test,Year) 
MSE_test_plot <- MSE_test_plot %>% 
  gather(method, MSE, MLR, FoC, RF, Boosting) %>% 
  mutate(RMSE=MSE^(1/2)) 

MSE_test_plot %>% 
  ggplot(aes(x = Year, y = RMSE, color = method)) +
  geom_line(size = 1) + 
  scale_x_continuous(breaks = seq(2005, 2019, 1)) + 
  theme_minimal()+  
  scale_color_brewer(palette = "Set2")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x= "Year", y= "RMSE")  


#portfolio-level

HML <- as.data.frame(cbind(ret_lm_mean_year, ret_lm_u_mean_year[,2], ret_rf_mean_year[,2], ret_gbm_mean_year[,2]))
colnames(HML) <- c("Year", "MLR", "FC", "RF","Boosting")

HML_plot <- HML %>% 
  gather(method, HML, MLR, FC, RF, Boosting) 


HML_plot %>% 
  ggplot(aes(x = Year, y = HML, color = method)) +
  geom_line(size = 1) + 
  scale_x_continuous(breaks = seq(2005, 2019, 1)) + 
  theme_minimal()+  
  scale_color_brewer(palette = "Set2")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x= "Year", y= "10 - 1 Spread")  

#variable importance
beta <- as.data.frame(betaMatrix)
colnames(beta)<-c("Intercept", "MC","BM","NSI", "IA","SREV","MOM","ROE","DTV","IV","SEASON")
beta_mean <- apply(beta,2,mean)
beta_var <- apply(beta,2,var)
beta_t <-as.data.frame(beta_mean/(beta_var^(1/2)))


Imp_rf_mean <-Imp_rf %>%
  group_by(variable) %>%
  summarize(imp_per=mean(percentage))

Imp_gbm_mean <-Imp_gbm %>%
  group_by(variable) %>%
  summarize(imp_per=mean(percentage))

library(gridExtra)  # Combine plots
library(grid)       # Combine plots

g1<-Imp_rf_mean %>%
  ggplot(aes(x=reorder(variable, imp_per), y=imp_per)) + 
  geom_bar(stat = "identity",  width=0.6, fill = "steelblue") +
  geom_text(aes(label=round(imp_per,digits=3), y=imp_per+0.011), , size=3.25)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x= "Stock Characteristics", y = "") + 
  ggtitle("Random Forests") + 
  theme(plot.title = element_text(hjust = 0.5, size=11), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  coord_flip()

g2<-Imp_gbm_mean %>%
  ggplot(aes(x=reorder(variable, imp_per), y=imp_per)) + 
  geom_bar(stat = "identity",  width=0.6, fill = "steelblue") +
  geom_text(aes(label=round(imp_per,digits=3), y=imp_per+0.013), , size=3.25)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x= "Stock Characteristics", y = "") + 
  ggtitle("Boosting") + 
  theme(plot.title = element_text(hjust = 0.5, size=11), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  coord_flip()

grid.arrange(arrangeGrob(g1 + theme(legend.position="none"),
                         g2 + theme(legend.position="none"),
                         nrow=1))









