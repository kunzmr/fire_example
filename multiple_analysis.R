library("glmnet")
library("glinternet")
library("e1071")
library("mgcv")
library("ggplot2")
library("plotly")
library("ncvreg")
library("outliers")
library("protoclust")

transformResponse = function(x, zeros = TRUE, inverse = FALSE) {
  
  if ( inverse ) {
    if ( zeros ) {
      x = exp(x) - 1
    } else {
      x = exp(x)
    }
  } else {
    if ( zeros ) {
      x = log(x + 1)
    } else {
      x = log(x)
    }
  }
  return(x)
}

x = read.csv("forestfires.csv")
# removing the x,y coordinates and the categorical variables
x = x[, -c(1:4)]
ggplot(x, aes(x = area)) + geom_histogram()
ggsave("hist_non.png", width = 5, height = 5)

x$area = transformResponse(x$area)
ggplot(x, aes(x = area)) + geom_histogram()
ggsave("hist_tran.png", width = 5, height = 5)

tmp_x = x$area
tmp_x = tmp_x[tmp_x != 0]
tmp_x = data.frame("area" = tmp_x)
ggplot(tmp_x, aes(x = area)) + geom_histogram()
ggsave("hist_non_zeros.png", width = 5, height = 5)

orig_area = transformResponse(x$area, inverse = T)
# all data:
fit = svm(area ~ RH + wind + rain + temp, data = x)
plot(predict(fit, x))
pred = transformResponse(predict(fit, x), inverse = T)
p_svm = qplot(pred, orig_area)
# matches paper RMSE
rmse_svm = sqrt(mean((orig_area - pred)^2))

pred_svm = transformResponse(predict(fit, x), inverse = T)

tmp_grub = outliers::grubbs.test(x$area)
which(x$area >= 6.995)
x = x[-239, ]
outliers::grubbs.test(x$area)
which(x$area >= 6.616)
x = x[-415, ]
outliers::grubbs.test(x$area)
# Stopping here since not significant


# initial fit
fit = lm(area ~., data = x)
# uncomment to see leverage points
#plot(fit)
# removal of outliers based on residual vs leverage
x = x[-which(rownames(x) == 500), ]
fit = lm(area ~., data = x)
# plot(fit)
x = x[-which(rownames(x) == 510), ]
fit = lm(area ~., data = x)
# no more leverage points or outliers
orig_area = transformResponse(x$area, inverse = T)

fit = svm(area ~ RH + wind + rain + temp, data = x)
plot(predict(fit, x))
pred = transformResponse(predict(fit, x), inverse = T)
p_svm = qplot(pred, orig_area)
# matches paper RMSE
rmse_svm = sqrt(mean((orig_area - pred)^2))



fit <- glm(area ~ RH + wind + rain + temp, data = x, family = "quasipoisson")
pred = transformResponse(predict(fit, x), inverse = T)
p_logistic = qplot(pred, orig_area)
rmse_logistic = sqrt(mean((orig_area - pred)^2))


fit <- lm(area ~ RH + wind + rain + temp, data = x)
pred = transformResponse(predict(fit, x), inverse = T)
p_lm = qplot(pred, orig_area)
rmse_lm = sqrt(mean((orig_area - pred)^2))

fit = mgcv::gam(area ~1 + s(wind, k =15) + s(temp, k = 15) + s(RH, k = 15) + s(DC, k = 15) + s(ISI, k = 15), data =x, method = "REML", select = TRUE)
pred = transformResponse(predict(fit, x), inverse = T)
p_gam = qplot(pred, orig_area)
rmse_gam = sqrt(mean((orig_area - pred)^2))

tmp_df = data.frame(
  "Predicted" = c(pred_svm[as.numeric(rownames(x))], pred),
  "Observed" = rep(orig_area, each = 2),
  "Type" = rep(c("SVM", "GAM"), dim(x)[1])
)
hist(pred - orig_area)
hist(pred_svm[rownames(x)] - orig_area)
ggplot(tmp_df, aes(x = Predicted, y = Observed, color = Type)) + geom_point()
ggsave("awful_pred.png", width = 5, height = 5)

response = x$area
predictors = scale(x[, -which(names(x) == "area")])

tmp_proto = protoclust(dist(predictors))

plot(tmp_proto)

fit = cv.ncvreg(predictors, response)
# interesting to note that all predictors are maintained 
fit$fit$beta[, which(fit$lambda == fit$lambda.min)]
pred = transformResponse(as.numeric(predict(fit, predictors)), inverse = T)
p_mcp = qplot(pred, orig_area)
rmse_mcp = sqrt(mean((orig_area - pred)^2))


fit = glinternet.cv(predictors, response, numLevels = rep(1, dim(predictors)[2]))
pred = transformResponse(as.numeric(predict(fit, predictors,type = "response")), inverse = T)
p_glinternet = qplot(pred, orig_area)
rmse_glinternet = sqrt(mean((orig_area - pred)^2))


