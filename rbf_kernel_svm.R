makefeature <- function(x){
  is.num <- sapply(x, class) == "integer"
  x[, is.num] <- lapply(x[, is.num], scale)
  x
  }
set.seed(123)
bank <- read.csv("data/bank-full.csv", sep=";")
head(bank, 3)
sapply(bank, class)
bank.processed <- makefeature(bank)
N <- nrow(bank)
inds.tr <- sample(seq(N), as.integer(0.7 * N))
bank.train <- bank.processed[inds.tr, ]
bank.test <- bank.processed[-inds.tr, ]

#RBFカーネルのSVMによる予測
library(kernlab)
fit.svm <- ksvm(y ~ ., data = bank.train)
fit.svm

pred <- predict(fit.svm, bank.test)
(conf.mat <- table(pred, bank.test$y))
(prec <- conf.mat["yes", "yes"]/sum(conf.mat["yes",]))
(rec <- conf.mat["yes", "yes"]/sum(conf.mat[,"yes"]))
(f.value <- 2 * prec * rec/(prec + rec))
(acc <- sum(diag(conf.mat))/sum(conf.mat))

#ランダムフォレストによる予測
library(randomForest)
set.seed(123)
fit.rf <- randomForest(y ~., data = bank.train)
fit.rf

pred <- predict(fit.rf, bank.test)
(conf.mat <- table(pred, bank.test$y))
(prec <- conf.mat["yes", "yes"]/sum(conf.mat["yes",]))
(rec <- conf.mat["yes", "yes"]/sum(conf.mat[,"yes"]))
(f.value <- 2 * prec * rec/(prec + rec))
(acc <- sum(diag(conf.mat))/sum(conf.mat))