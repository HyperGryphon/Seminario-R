rm(list = ls())
graphics.off()

pacman::p_load(caret,GGally,magrittr,pacman,parallel,randomForest,rattle,rio,tictoc,tidyverse,ggfortify)

#setwd("data/seminario R/")
data = readxl::read_excel("data/osl_parnaiba_table.xlsx", sheet = "by_aliquot") %>%
  print()

data$BOSLF = as.numeric(data$BOSLF)
data$BOSLM = as.numeric(data$BOSLM)
data$BOSLS = as.numeric(data$BOSLS)

colnames(data)[9] = "IRSL"

data$BOSLF[is.na(data$BOSLF)] = 33.333
data$BOSLM[is.na(data$BOSLM)] = 33.333
data$BOSLS[is.na(data$BOSLS)] = 33.333

osl.c = 1:length(data$BOSL1s)
for (i in 1:length(data$BOSL1s)) {
  if (data$BOSL1s[i]>9) {
    osl.c[i] = "High"
  } else if (data$BOSL1s[i]<4) {
    osl.c[i] = "Low"
  } else {
    osl.c[i] = "Medium"}
}

data %<>%
  #mutate(osl.t = ifelse(osl>=9,"High","Medium"), osl.t = as_factor(osl.t)) %>%
  mutate(osl.c = as.factor(osl.c)) %>%
  print()

# Random forest of decision trees on training data ####
#set.seed(123)
train = data %>% slice_sample(n=216)
test = anti_join(data, train)

control = trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "random",
                       allowParallel = T)

#tic("Random forest")
fit.rf = train(Unit ~ BOSL1s + BOSLF + BOSLM + BOSLS + IRSL + TL110oC + TL110pos + TL325pos,
               data = train, method = "rf", trControl = control,
               metric = "Accuracy", tuneLength = 15, ntree = 300,
               na.action = "na.omit")
#toc()
fit.rf
fit.rf %>% plot()
fit.rf$finalModel
fit.rf$finalModel %>% plot()

osl.t = predict(fit.rf, newdata = train)
table(actualclass = train$Unit, predictedclass = osl.t) %>% 
  confusionMatrix()

#Random forest on test data
osl.p = predict(fit.rf, newdata = test)
table(actualclass = test$Unit, predictedclass = osl.p) %>% 
  confusionMatrix()

# Summary of results ####
test %>% pull(osl.c) %>% summary()
