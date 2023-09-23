#Machine learning
library(tidyverse)
library(caret)
library(dslabs)
data(heights)

y <- heights$sex
x <- heights$height

####INTRO####
#Times cuantas veces quiero cortar la muestra, p que proportion de la base de datos quiro usar como training data
set.seed(2007)#?
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))
# compute accuracy
mean(y_hat == test_set$sex)

#Algorithms
heights %>% group_by(sex) %>% dplyr::summarize(mean(height), sd(height))
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

#Exos
mnist<-read_mnist()
i <- 5
image(1:28, 1:28, matrix(mnist$test$images[i,], nrow=28)[ , 28:1],
      col = gray(seq(0, 1, 0.05)), xlab = "", ylab="")
mnist$test$labels[i]
str(mnist)

#Confution matrix because prevalence (balance of your data) matters
# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")

confusionMatrix(data = y_hat, reference = test_set$sex)

#High sensitivity means y equals 1 implies y hat equals 1.
# High specificity means y equals 0 implies y hat equals 0.

#Balance accuracy = average of specificity and sensitivity
# maximize F-score = harmonic average => Rate of the quality of the algorithm
#beta  represent how much more important sensitivity is compared to specificity
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))#F_meas beta is by default 1
})
max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

plot(F_1~cutoff)


#Guessing male randomly would be better than with cutoff but this is because unbalance data
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <-
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>%
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x,
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall This when prevalence is importante
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index),
                  replace = TRUE, prob=c(p, 1-p)) %>%
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE,
                  prob=c(p, 1-p)) %>%
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

#Machine learning with lineal regression when we have continous variables
library(HistData)
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

m <- mean(train_set$son)
# squared loss
mean((m - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
summary(fit)
fit$coef
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

#Predict
y_hat <- predict(fit, test_set)

y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

# read help files
?predict.lm
?predict.glm

# Regression for categorical data
library(dslabs)
data("heights")
y <- heights$height

set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind = "Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#What is the propability of a 66inch tall to be female
train_set %>%
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female"))
# The probability is 0.24

heights %>%
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]


# A better way to do it is using a losgistic regression

heights %>%
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])
range(p_hat)

# fit logistic regression model
glm_fit <- train_set %>%
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]

# Study case determinated if a digit is a 2 or a 7
mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]

mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black")

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)

# Algorithms  -------------------------------------------------------------

# But there is still better ways to do machine learning
## Smoothing is a very powerful technique used all across data analysis. It is designed to detect trends in the
    # presence of noisy data in cases in which the shape of the trend is unknown.
# The concepts behind smoothing techniques are extremely useful in machine learning because conditional
    # expectations/probabilities can be thought of as trends of unknown shapes that we need to estimate in the
    # presence of uncertainty.

# bin smoothers We center (compute the average) a group of point that happend in the same week interval
data("polls_2008")
qplot(day, margin, data = polls_2008)

span <- 7
fit <- with(polls_2008,ksmooth(day, margin, x.points = day, kernel="box", bandwidth =span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")

# kernel eich point receved a weight (The points near the edges recived little weight wich give a smoother plot)
span <- 7
fit <- with(polls_2008, ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")


# We are gona improve the kenel smoothing with Local Weighted Regression (loess)
    # Here we gona use a a larger windows than just a week (3 week) we now fit a line to each window a nos just a point
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree = 1, span = span, data = polls_2008)

# Does code don't work
polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day,margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(color = "red",
              span = 0.3,
              method.args = list(degree = 1)) # degree = 1 is promt to less noise default is 2



# Matrix ------------------------------------------------------------------

library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)

x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]


# build a matrix

vector = 1:15
matrix = matrix(vector, 5, 3) # fill by column

matrix_t = matrix(vector, 3, 5, byrow = T) # fill by row

identical(t(mat), mat_t) # t(mat) transposing de matrix

# show the matrix as an image
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)

# flip the image back
image(1:28, 1:28, grid[, 28:1])

# Row and Column Summaries and Apply
library(matrixStats) # rowSds colSds

# We can also use apply function to use more fuction to each row
# apply(matrix, dimension, function)
avgs = apply(x, 1, mean) # row mean
sds = apply(x, 2, sd) # columns sd




#  1 Remove the data that isn't usefull for the prediction
    # we can remove rows and colums from the matrix with ,drop = FALSE function
#  2 We can also binarize the data and we dont loose much information in the numbers images
    # We can turn matrices into vectors with as.vector()
#  3 Standardize the rows or colums with vectorization (we subtract a vector from a matrix)
    # We can scale each row of a matrix using this line of code:
(x - rowMeans(x)) / rowSds(x)
    # To scale each column of a matrix, we use this code:
t(t(X) - colMeans(X)) # we need first to transpose the matrix or use function sweep
# Sweep takes each entry of a vector and subtracts it from the corresponding row or column:
X_mean_0 <- sweep(x, 2, colMeans(x)) # it's a colums because of the 2 it substract the columnMean() substract is the default
X_mean_0 <- sweep(x, 2, colSds(x), FUN = "/") # we what to divide every column by the SD

# Matrix multiplication: %*%
# The cross product: crossprod(x)
# The inverse of a function in this case function crossprod: solve(crossprod(x))
# The QR decomposition: qr(x)



# Section 4: Distance, Knn, Cross-validation, and Generative Model --------
# Many machine learning techniques rely on being able to define distance between observations
# Create data for examples
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
set.seed(1995)
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]
y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]
#compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

#compute distance between each row
d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]

#visualize these distances
image(as.matrix(d))

#order the distance by labels
image(as.matrix(d)[order(y), order(y)])

#compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))


#Knn machine learning algorithm

x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x,y)
Code
#logistic regression
library(caret)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#fit knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train)
#or
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)
#result
knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#Over-training You can see that the accuracy computed on the training side is quite higher.
# It's 0.882 compared to what we get on the test set, which is only 0.815.
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#fit knn with k=1 perfect acuracy on training set with k=1 he predict himself
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]

#fit knn with k=401 There its the opposite ist oversmoothing
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]

#pick the k in knn
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]

  tibble(train = train_error, test = test_error)
})
})

#pick the k that maximizes accuracy using the estimates built on the test data
#The probleme is that we usea the testing data to find the best K witch can't be done
ks[which.max(accuracy$test)]
max(accuracy$test)


#Cross validation find the best K using only the training set (k-fold Cross Validation)
# test set from data are 20-30% of the training data
# We pick k number of non overlapping validation set to test the error BUT
# we can also pick a higher k random sample of random size without worrying of overlapping (This is the bootstrap approch)

# Bootstrap : The bootstrap permits us to approximate a Monte Carlo simulation without access to the entire distribution.
    # The general idea is relatively simple. We act as if the sample is the entire population and sample with replacement data sets of the same size.
    # Then we compute the summary statistic, in this case, the median, on what is called the bootstrap sample.
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
m

set.seed(1995)
#use set.seed(1995, sample.kind="Rounding") instead if using R 3.6 or later
N <- 250
X <- sample(income, N) # Extract randomly N numbers form income
M<- median(X)
M

library(gridExtra)
B <- 10^4
M <- replicate(B, { # do the same thing B number of times
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(M)
sd(M)

# This is how we construct bootstrap samples in an approximate distribution.
# If we know the distribution is normal, we can use a bootstrap to estimate the mean, the standard error, and then form a confidence interval that way.
B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) +
  geom_abline()

quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))

# This is what we optain with the central limit theorem (this only works for mean not for median)
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

mean(M) + 1.96 * sd(M) * c(-1,1)

mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)


# Exo bootstrap
library(dslabs)
library(caret)
library(dplyr)
data(mnist_27)
glimpse(mnist_27)
set.seed(1995) # if R 3.6 or later, set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)
# exo 1
sum(indexes[[10]] == 3)
# exo 2
x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)
# exo 3
y <- rnorm(100, 0, 1)
qnorm(0.75)
quantile(y, 0.75)
B <- 10^4


set.seed(1)
B <- 10000
q_75 <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

mean(q_75)
sd(q_75)

# Exo 4 Monte Carlo simulation
set.seed(1)    # set.seed(1, sample.kind="Rounding") if R 3.6 or later
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)


# Naive Bayes ----

library("caret")
library(dslabs)
library(dplyr)
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# Estimating averages and standard deviations
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params

# Estimating the prevalence
pi <- train_set %>% summarize(pi=mean(sex=="Female")) %>% pull(pi)
pi

# Getting an actual rule
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))


# The Naive Bayes approach includes a parameter to account for differences in prevalence
# The Naive Bayes approach gives us a direct way to correct the imbalance between sensitivity and specificity by simply forcing  ??^  to be whatever value we want it to be in order to better balance specificity and sensitivity.
# Computing sensitivity
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Computing specificity
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Changing the cutoff of the decision rule
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

# Draw plot
qplot(x, p_hat_bayes_unbiased, geom = "line") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_vline(xintercept = 67, lty = 2)


# qda and lda ----

# Quadratic discriminate analysis, or QDA,
# is a version of Naive Bayes in which we assume
# that the conditional probabilities for the predictors are multivariate normal. ex sex and heights

# Load data
data("mnist_27")

# Estimate parameters from the data
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))

# Contour plots
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm", lwd = 1.5)

# Fit model
library(caret)
train_qda <- train(y ~., method = "qda", data = mnist_27$train)
# Obtain predictors and accuracy
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

# Draw separate plots for 2s and 7s
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm") +
  facet_wrap(~y)
# LDA
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
params <- params %>% mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r = mean(r))
train_lda <- train(y ~., method = "lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]


# So in summary, generating models can be very powerful but only
# when we're able to successfully approximate the joint distribution
# of predictor's condition on each class.



# Classification with more than two classes -------------------------------


# LDA and QDA are not meant to be used with many predictors  p  because the number of parameters needed to be
    # estimated becomes too large.
# Curse of dimensionality: For kernel methods such as kNN or local regression, when they have multiple predictors
    # used,  the span/neighborhood/window made to include a given percentage of the data become large.
    # With larger neighborhoods, our methods lose flexibility.
    # The dimension here refers to the fact that when we have  p  predictors,
    # the distance between two observations is computed in  p -dimensional space.

# Classification and Regression Trees (CART)

# A tree is basically a flow chart of yes or no questions. The general idea of the methods we are describing is to define an algorithm that uses data to create these trees with predictions at the ends, referred to as nodes. When the outcome is continuous, we call the decision tree method a regression tree.
# Regression and decision trees operate by predicting an outcome variable  Y  by partitioning the predictors.

# Load data
library(tidyverse)
library(dslabs)
data("olive")
olive %>% as_tibble()
table(olive$region)
olive <- select(olive, -area) # We remove it because we don't use it as a predictor

# Predict region using KNN
library(caret)
fit <- train(region ~ .,  method = "knn",
             tuneGrid = data.frame(k = seq(1, 150, 2)),
             data = olive)
ggplot(fit)

# Plot distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())

# plot values for eicosenoic and linoleic
p <- olive %>%
  ggplot(aes(eicosenoic, linoleic, color = region)) +
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) +
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)








# Guide to decision tree
# 1)  We partition the space into j non-overlapping regions, R1, R2, all the way up to Rj. For every observation that follows within a region, let's say, region R1, we predict the Y hat with the average of all the training observations in that region.
      # Who to partition de data => Regression trees create partitions recursively. => We pick the combination that minimizes the residual sum of squares
      # Who many partition => To avoid this overtraining, the algorithm sets a minimum for how much the residual sum of squares must improve for another partition to be added. = Complexity Parameter, or CP.

# load data for regression tree
data("polls_2008")
qplot(day, margin, data = polls_2008)

library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)


# visualize the splits
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
polls_2008 %>%
  mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")


# change parameters
    # minsplit set de min of spliting allow 20 is default
    # minbucket set the min of observation that a partition can have the default is round(minsplit/3)
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% # Visualize
  mutate(y_hat = predict(fit)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")
# prune the tree => we create a big tree then we simplify it by removing partition that don't met the criterion
pruned_fit <- prune(fit, cp = 0.01)

    # But how do you pic CP => by using cross-validation
# use cross validation to choose cp
library(caret)
train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = polls_2008)
ggplot(train_rpart)

# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
polls_2008 %>% # We can plot this because we only have one parameter
  mutate(y_hat = predict(train_rpart)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")


# Random Forests

# Random forests are a very popular machine learning approach that addresses the shortcomings of decision trees. The goal is to improve prediction performance and reduce instability by averaging multiple decision trees for each training set created (a forest of trees constructed with randomness).
# The general idea of random forests is to generate many predictors, each using regression or classification trees, and then forming a final prediction based on the average prediction of all these trees. To assure that the individual trees are not the same, we use the bootstrap to induce randomness.

library(randomForest)
# applying random forest to the 2008 polls data.
fit <- randomForest(margin~., data = polls_2008, nodesize = 50, maxnodes = 25)
plot(fit)

# Visualize final result
polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>%
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

# applying random forest to two or seven example.
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# use cross validation to choose parameter in other to optimise it (we don't what a result to precise we need to smooth by:limit the size of each node by requiring the number of points per node to be larger.
train_rf_2 <- train(y ~ .,
                    method = "Rborist",# Another random forest algorithm (is faster)
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# A second feature of random forest that we have not yet described is that we can use a random selection of features to use for the splits. Specifically, when building each tree at each recursive partition, we only consider a randomly selected subset of predictors to check for the best split.And every tree has a different random selection of features.

# This reduces correlation between trees in the forests, which in turn improves prediction accuracy.
# The argument for this tuning parameter in the random forest function is mtry.

# A disadvantage of random forests is that we lose interpretability.
# An approach that helps with interpretability is to examine variable importance. To define variable importance we count how often a predictor is used in the individual trees. The caret package includes the function varImp that extracts variable importance from any model in which the calculation is implemented.





# Caret And How to use all the algorithms implemented
library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train) #
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

# Tuning Parameters with Caret
# When an algorithm includes a tuning parameter, train automatically uses cross-validation to decide among a few default values.
getModelInfo("knn")
modelLookup("knn")


train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE) # We can se that default parameter is 5, 7, 9 and the best is 9

train_knn <- train(y ~ ., method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2))) # tuneGrid allow use to use more parameters (between 9 and 71)
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune # This code show us directly wich is the best parameter
train_knn$finalModel # This run the best performing model if you apply the function predict it also choose the best model
# The trControl parameter and trainControl() function can be used to change the way cross-validation is performed.
# We now want to see the performance using the testing set
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]


# If we like to change the way we perform cross-validation. We might change the method, we might change how we do the partitions, et cetera.
control <- trainControl(method = "cv", number = 10, p = .9) # here we 10 as the number of sample to do cross validation and estimate accuracy
train_knn_cv <- train(y ~ ., method = "knn",
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)


# plot the point estimates of the accuracy along with standard deviations.
train_knn$results %>%
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k,
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))


plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])



# Here for example we whan to smooth the result so we use another method "gamLoess".
install.packages("gam")
modelLookup("gamLoess") # we see that there are 2 parameter to optimize degree and span

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1) #span: 0.15, 0.65

train_loess <- train(y ~ .,
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test),
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1
# Note that not all parameters in machine learning algorithms are tuned. We use the train() function to only optimize parameters that are tunable.
#  like for example LDA and regression models that fit the best model using the squares estimates or maximum likelihood



# # Case Study: MNIST ----
library(tidyverse)
library(dslabs)
mnist <- read_mnist()

names(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set to increase resolution speed
set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

# Pre-processing MNIST Data ( transform (standardize, log, (predictors) or/and remove predictors that are clearly not useful or highly correlated or with very few non-unique values or close to zero variation.)
library(matrixStats)
sds <- colSds(x)# compute stadard deviation or each column and plot them
qplot(sds, bins = 256, color = "black")

library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28)) #image of the image removed columns are in red

col_index <- setdiff(1:ncol(x), nzv)
length(col_index) #how many colums are left after the removing

# we need to add colums names to the feature matrices because it's a requirement of the caret package'
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

# Model Fitting for MNIST Data

# We are going to start with knn
# 1) Optimize the number of neighbors
control <- trainControl(method = "cv", number = 10, p = .9) # CV Cross validation
train_knn <- train(x[,col_index], y,
                   method = "knn",
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)
ggplot(train_knn)

# tip test the code with only a piece of code
n <- 1000 # the number of row that we are going to use
b <- 2 # here the number of croos-validation folds that we are going to use
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)

# one we optimize de algorithm (k=3) we fit the entire data set
fit_knn <- knn3(x[ ,col_index], y,  k = 3)


y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]
# Accuracy 0.95


cm$byClass[,1:2] # From the specificity and sensitivity output coming from the confusion matrix we can see that the 8 is the harder to predict (it has the lower sensitivity) and the most commonly incorrect predicted digit is 7 (i has the lower Specificity).


#### We are going to do better with random forest (but takes more time to calculate)

library(Rborist) # this package has less features but is faster

# 1) We are going to optimize the model
control <- trainControl(method="cv", number = 5, p = 0.8) # Because with random forest, the fitting is the slowest part of the procedure we are only using 5 fold cross validation and reduce the number of final trees since we are not yet building out final model


grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], y,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000) #we'll take a random subset of observations when constructing each tree here 5000.
ggplot(train_rf) # plot the results
train_rf$bestTune # we chose the best paramaters with this code


# We are going now to optimize the final tree
fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000, # we increase the number of trees
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

# check the accuracy
y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"] # we can see its 0.951 so better than knn


# Some examples of the original image in the test?
rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn",
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)

# Variable Importance allow to better interpret the result of the random forest
# we rerun the same model but with slower package (random forest) to see the comcept of variable importane
library(randomForest)
library(tidyverse)
library(dslabs)

# Data import
mnist <- read_mnist()
index <- sample(nrow(mnist$train$images), 10000) # Where we don't filter any column/feature
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

# Model
rf <- randomForest(x, y,  ntree = 50)

# We can compute the importance of each feature using
imp <- importance(rf)
imp # We can see that the first features (the first colums) are never used in the predition algorithm

# With this data it make sens to plot an image where each feature is plotted in the location of the image
image(matrix(imp, 28, 28)) #Hot spots in the center on the image

# Show the cases where the prediction fail to predict the right number
# For Knn
p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

# For Forest plot
p_max <- predict(fit_rf, x_test[,col_index])$census
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

# Ensemble different machine learning algorithms into one (Combine  the result of different algorithms)
# we compute new class probabilities by taking the average of the class probabilities provided
# Random forest
p_rf <- predict(fit_rf, x_test[,col_index])$census
p_rf <- p_rf / rowSums(p_rf)
# Knn
p_knn <- predict(fit_knn, x_test[,col_index])
# Average
p <- (p_rf + p_knn)/2

y_pred <- factor(apply(p, 1, which.max)-1)

confusionMatrix(y_pred, y_test) # we actually improve the accuracy over both k-nearest neighbors and random forest.


# Exo
library(dslabs)
library(tidyverse)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
tissue_gene_expression
x = tissue_gene_expression$x %>% filter()
ly = levels(y)
y = tissue_gene_expression$y

pc <- prcomp(tissue_gene_expression$x) #Performs a principal components analysis
#  Plot the first two principal components with color representing tissue type.
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2],
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs,
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])

#  Redo the PCA but only after removing the center.
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2],
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# Boxplot For the first 10 PCs, make a boxplot showing the values for each tissue.
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

# Plot the percent variance explained by PC number. Hint: use the summary function.
plot(summary(pc)$importance[3,])






# Recommendation Systems ----


# Data from netflix
library(dslabs)
library(tidyverse)
library(rafalib)
data("movielens")

head(movielens)

movielens %>% # check number od user and number o fmovies
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# Use a subset of the data to see what movies the user didn't saw (NA) recomendation system mean commplet the NA
keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)
tab <- movielens %>%
  filter(userId %in% c(13:20)) %>%
  filter(movieId %in% keep) %>%
  select(userId, title, rating) %>%
  spread(title, rating)
tab %>% knitr::kable()

# Graph showing Users vs Movies
users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>%
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>%
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")



# The challenge in this machine learning algorithm is that each outcome has a different set of predictors: For example to see this, note that if we are predicting the rating for movie i by user u, in principle, all other ratings related to movie i and by user u may be used as predictors. But different users rate a different number of movies and different movies.
# We also want information for others movies that we think are similar or other user that are similar

# Some movies get rated more than others
movielens %>%
  dplyr::count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  ggtitle("Movies")


# Some users are more active than others
movielens %>%
  dplyr::count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  ggtitle("Users")



library(caret)

# we first create a test set
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]


# We need to remove users and movies that appear in the test set from the training set
test_set <- movielens[test_index,]
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Loss function we are going to quatify what it means to create a "good" algorithm
# We can interpret the residual mean squared error similar to standard deviation. It is the typical error we make when predicting a movie rating. If this number is much larger than one, we're typically missing by one or more stars rating which is not very good.

# This is a function that computes residual means squared error for a vector of ratings
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Building the Recommendation System

# Recap: k-nearest neighbors, where you found movies that were similar to each other and users that were similar to each other.

# Other classe of model matrix factorization

# Simpliest model: predict the same rating for all movies, regardless of the user and movie. (i.e the average rating of all the movies)
mu_hat <- mean(train_set$rating)
mu_hat # average = 3.5


# We compute the average in the training data and then we compute the residual mean squared error on the test set data.
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse # So we're predicting all unknown ratings with this average. we get a residual mean squared error of about 1.

# we know that the average minimizes the residual mean squared error
predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

# we create a table to store the result of each approches to compare them
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

# we can add a parameter calculated by the lm to increase the rating accuracy but it take to much time because it predict for each movie one parameter and one estimate
fit <- lm(rating ~ as.factor(userId), data = movielens)

# This solution is faster because we use mathematical proprieties to facilitate the calculation of b (the estimates) it is an approximation. We say that the estimates are just the average rating of each user for each movie minus the overall mean
mu <- mean(train_set$rating)
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black")) # Plot

# How good is our new model?
predicted_ratings <- mu + test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))# The residual mean square did drop so we see an improvement

rmse_results %>% knitr::kable()

# We can improve the model by adding the effect of the users (not all users put 5 stars when they like the movie)

# Lest see the data average rating of users
train_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>%
  filter(n()>=100) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")

# This model again is to heavy
lm(rating ~ as.factor(movieId) + as.factor(userId))

# Again we compute an approximation
user_avgs <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# See the improvement
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)



rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()


# Exo

library(tidyverse)
library(lubridate)
library(dslabs)
library(magrittr)
data("movielens")
head(movielens)

# Compute the number of ratings for each movie and then plot it against the year the movie came out
nRatings = movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# What is the average rating for the to 25 movie

movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

# stratify the post-1993 movies by ratings per year and compute their average ratings.

movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()
# Create a column date from column with seconds

movielens %<>% mutate(date = as_datetime(timestamp))

# average rating for each week
movielens %>% mutate(week = week(date)) %>%
  group_by(week) %>% summarize(rating = mean(rating)) %>%
  ggplot(aes(week, rating)) + geom_point() +
  geom_smooth() # Ivan false

movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth() # Awnser

# This column includes every genre that applies to the movie. Some movies fall under several genres. Define a category as whatever combination appears in this column. Keep only categories with more than 1,000 ratings.
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
  geom_point() +
  geom_errorbar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Regularization ----

library(dslabs)
library(tidyverse)
library(caret)
data("movielens")

# Model using only the movie effect in our model
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
mu <- mean(train_set$rating)
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
user_avgs <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",
                                     RMSE = model_2_rmse ))
# Here are 10 of the largest mistakes
# Seems that they all are obscure movies
test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%
  select(title,  residual) %>% slice(1:10) %>% knitr::kable()

# To see what's going on, let's look at the top 10 best movies in the top 10 worst movies based on the estimates of the movie effect

movie_titles <- movielens %>%
  select(movieId, title) %>%
  distinct()
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i) %>%
  slice(1:10) %>%
  knitr::kable()
# Best movies
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i) %>%
  slice(1:10) %>%
  knitr::kable()
# Worst movies
train_set %>% dplyr::count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  knitr::kable()

# We are going to see how often they were rated
train_set %>% dplyr::count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  knitr::kable()

# We can see that the worst movies and the best movies were rated by very few users (n=1) so we gave larger error so we need to be conservative when we are not sure so no prediction allowed
# Regularization permits us to penalize large estimates that come from small sample sizes. The general idea is to add a penalty for large values of b to the sum of squares equations that we minimize.

# Regularization with lambda = 3
lambda <- 3 # it is a tuning parameter and we use cross-validation in the training set only to choose it see end of the chapter
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

# To see how the estimates shrink, let's make a plot of the regularized estimate versus the least square estimates with the size of the circle telling us how large n was.
data_frame(original = movie_avgs$b_i,
           regularlized = movie_reg_avgs$b_i,
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) +
  geom_point(shape=1, alpha=0.5)

# Top 10 movies based on estimates after regularization
train_set %>%
  dplyr::count(movieId) %>%
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  knitr::kable()

# 10 worst movies
train_set %>%
  dplyr::count(movieId) %>%
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  knitr::kable()

# Did we improve the error?
predicted_ratings <- test_set %>%
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable() # yes the error is smaller


# Cross-validation to choose the lambda
lambdas <- seq(0, 10, 0.25)

mu <- mean(train_set$rating)
just_the_sum <- train_set %>%
  group_by(movieId) %>%
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>%
    left_join(just_the_sum, by='movieId') %>%
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})


qplot(lambdas, rmses) # Plot the result

lambdas[which.min(rmses)] # Show the result = 3


# Regularization of the user effect
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <-
    test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)

lambda <- lambdas[which.min(rmses)]
lambda # lambda = 3.25

# Model with regularization improve the RMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

# Exo
# Create code
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))
set.seed(1)


mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))
set.seed(1)
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#  ID of the top school
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)

# median school size
median(schools$size)
schools %>% top_n(10, score)  %>% .$size %>% median()
schools %>% top_n(-10, score) %>% .$size %>% median() # top 10 worst schools

# Plot the average score versus school size

schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)

# use regularization to pick the best schools. Regularization shrinks deviations from the average towards 0.


overall <- mean(sapply(scores, mean)) # overall mean
# Then, we need to define, for each school, how it deviates from that average.
# Write code that estimates the score above the average for each school but dividing by  n+ ??  instead of  n , with  n  the school size and  ??  a regularization parameter. Try  ??=25 .

alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

# What value of \( \alpha \) gives the minimum RMSE?
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]

# Rank the schools based on the average obtained with the best  ??
alpha <- alphas[which.min(rmse)]
score_reg <- sapply(scores, function(x)
  overall+sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

# A common mistake made when using regularization is shrinking values towards 0 that are not centered around 0. For example, if we don't subtract the overall average before shrinking, we actually obtain a very similar result. Confirm this by re-running the code from the exercise in Q6 but without removing the overall mean.

# What value of  ??  gives the minimum RMSE here?
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]

# Matrix Factorization

# We know that groups of users and groups of movies have similar rating patterns. We discover these patterns by studying the residuals obtained after fitting our model.
# 1) to studie the residual we convert the data in a matrix (each user gets a row and each movie gets a column.)

# Generating the training data
train_small <- movielens %>%
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>%
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

rownames(y)<- y[,1]
y <- y[,-1]
colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

y <- sweep(y, 1, rowMeans(y, na.rm=TRUE)) # we convert these residuals by removing the column and row averages.
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))

# Check correlation
# Here's a plot of the residuals for The Godfather and The Godfather II.They're very correlated.Whicht says that users that liked the godfather more than what the model expects them to based on the movie and user effects also like The Godfather II more than expected.

m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "You've Got Mail"
m_5 <- "Sleepless in Seattle"
qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

# we can see with pairwise that there's a positive correlation between the gangster movies Godfathers and Goodfellas, and t between the romantic comedies. We also see a negative correlation between the gangster movies and the romantic comedies.
cor(y[, c(m_1, m_2, m_3, m_4, m_5)], use="pairwise.complete") %>%
  knitr::kable()

# We use matrix factorization to take account of this structure in the data. We use some structure to predict the residuals
# We do factorization by assigning a 1 to the gangster movies and a minus one to the romantic comedies. So in this case, we can narrow down movies to two groups, gangster and romantic comedy.
# We can also reduce de users in 3 groups (1 they like gangster movies, 2 they don't, 3 they don't care)
# The main point here is that we can reconstruct this data that has 60 values with a couple of vectors totaling 17 values.

# we simulate some data for the example
set.seed(1)
options(digits = 2)
Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)

X <- jitter(P%*%t(Q))
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(aling="c")

P

# Idem but more complex we add love movies. Here we will have more parameter in the model but less than in the main data
# Here is the model Yu,i = ?? + bi + bu + pu,1q1,i + pu,2q2,i + ??i,j (pu,1q1,i + pu,2q2,i this is the factorization parameters) In netflix challenge they penalize large values of P and Q
set.seed(1)
options(digits = 2)
m_6 <- "Scent of a Woman"
Q <- cbind(c(1 , 1, 1, -1, -1, -1),
           c(1 , 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
P <- cbind(rep(c(2,0,-2), c(3,5,4)),
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(P) <- 1:nrow(X)

X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(align="c")

P

six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[,six_movies]
cor(tmp, use="pairwise.complete")

# We can usa principal component analysis (PCA) or singular value decomposition (SVD) to do the factorization from the data and not just guessing by intuition (comedie vs gangster)


# SVD and PCA

#You can think of singular value decomposition (SVD) as an algorithm that finds the vectors  p  and  q  that permit us to write the matrix of residuals  r  with  m  rows and  n  columns in the following way: ru,i = pu,1q1,i + pu,2q2,i + ... + pu,mqm,i,
# And with the the added bonus that the variability of these terms is decreasing and also that the p's are uncorrelated to each other.

# To compute the decomposition, will make all the NAs zero.
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)
dim(pca$rotation) # The vectors q are called the principal components are stored in this matrix.
dim(pca$x) # The p vectors, which are the user effects, are stored in this matrix.

# The PCA function returns a component with the variability of each of the principal components and we can access it like this and plot it.
plot(pca$sdev)

# We can also see that just with a few of these principal components we already explain a large percent of the data.
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained) # With 50 component we explain half of the data

# To see that the principal components are actually capturing something important about the data, we can make a plot of for example, the first two principal components, but now label the points with the movie
library(ggrepel)
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() +
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs,
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))
# The first principle component shows the difference between critically acclaimed movies on one side. ex: odyssey 2001, Seven (593)...
pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)
# and on the other side blockbusters. ex: Titanic shrek (4306)
pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

# We can also see that the second principle component also seems to capture structure in the data.
# If we look at one extreme of this principle component, we see artsy independent films such as Little Miss Sunshine, the Truman Show, and Slumdog Millionaire. When we look at the other extreme, we see "nerd favorites", The Lord of the Rings, Matric, Starwars...
pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)

# So using principal components analysis, we have shown that a matrix factorization approach can find important structure in our data.
# We can use the package "recommenderlab" to fit the matrix factorization that takes into account the missing data

# Exo
# dataset that represents grade scores for 100 students in 24 different subjects
set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3)
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

# Plot the grades

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

# examine the correlation between the test scores
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Exo Clustering

library(dslabs)
data("tissue_gene_expression")
#  Remove the row means and compute the distance between each observation.
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))
# hierarchical clustering plot
h <- hclust(d)
plot(h)
# Run a k-means clustering
cl <- kmeans(tissue_gene_expression$x, centers = 7)
table(cl$cluster, tissue_gene_expression$y) # Liver is classified in a single cluster roughly 20% of the time and in more than one cluster roughly 80% of the time. correct

# Select the 50 most variable genes. Make sure the observations show up in the columns, that the predictor are centered, and add a color bar to show the different tissue types.
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)
