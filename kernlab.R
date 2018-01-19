library(RCurl)
library(kernlab)

#訓練量データの読み込み
url1 <- "https://raw.githubusercontent.com/Salinger/testset1/master/learning.csv"
learning_df <- read.csv(text = getURL(url1, ssl.verifypeer = FALSE, .encoding ="UTF-8"))
head(learning_df)

plot(x = learning_df$b, y = learning_df$a, xlim = c(-5, 13), ylim = c(70, 109), pch = ifelse(learning_df$continued == 1, 1, 17))

#テストデータの読み込み
url2 <- "https://raw.githubusercontent.com/Salinger/testset1/master/try.csv"
test_df <- read.csv(text = getURL(url2, ssl.verifypeer = FALSE, .encoding ="UTF-8"))
head(test_df)

#SVMによる学習
classifier <- ksvm(continued ~., data = learning_df, type = "C-svc")# , kernel = "radial")
plot(classifier, data = learning_df)

#予測
predict(classifier, test_df)

#データの読み込み
data("freeny")
freeny.y
lm_result <- lm(freeny.y ~ time(freeny.y))
summary(lm_result)

plot(freeny.y)
abline(lm_result, lty = 2)

#ランダムフォレストによる回帰
library("randomForest")
data(freeny)
df <- data.frame("time" = time(freeny.y), "freeny.y" = freeny.y)

rf_result <- randomForest(freeny.y ~ ., df, mtry = 1, ntree = 500, type = "regression")
print(rf_result)
plot(rf_result)

df["predict"] <- ts(predict(rf_result, df["time"]), start = c(1962, 2), frequency = 4)
plot(df["freeny.y"], ylim = c(8.8, 9.8), xlim = c(1962, 1972))
par(new=T)
plot(df["predict"], lty = 2, ylim = c(8.8, 9.8), xlim = c(1962, 1972))