# library(devtools)
# install_github("mfasiolo/gamFactory", ref = "dev")
# install_github("mfasiolo/mgcViz")

library(gamFactory)
load("./gefCom2014.RData")

dat$TempMat <- as.matrix(temperatures) # matrix to include single index of the temperatures
dat$meanTemp <- rowMeans(dat$TempMat) # mean temperature over all 25 stations

tmp <- lapply(c(24:48, (3:7)*24), function(ii) dplyr::lag(dat$load, ii) ) ## matrix to include single index of the lags
dat$load_lags <- as.matrix(do.call(cbind, tmp))
dat <- na.omit(dat) #remove observations with missing values

## keep observations at 12
dat <- dat[dat$tod == 12, ]

formula_n <- list(load ~ dow + 
                    s(timeCount, k=4) +
                    s(toy, k = 20) +
                    s_nest(TempMat, k=21, trans = trans_linear(), m = c(4,2)) +
                    s_nest(load_lags, k=16, trans = trans_linear(pord = 1), m = c(4,2)),
                  ~1)
formula_n2 <- list(load ~ dow + 
                     s(timeCount, k=4)  +
                     s(meanTemp, k = 15) +
                     s(toy, k = 20) +
                     s(load_lags[,1], k = 10) +
                     load_lags[,2] + load_lags[,3] + load_lags[,4] + load_lags[,5] + 
                     load_lags[,6] + load_lags[,7] + load_lags[,8] + load_lags[,9] + 
                     load_lags[,10] + load_lags[,11] + load_lags[,12] + load_lags[,13] + 
                     load_lags[,14] + load_lags[,15] + load_lags[,16] + load_lags[,17] + 
                     load_lags[,18] + load_lags[,19] + load_lags[,20] + load_lags[,21] + 
                     load_lags[,22] + load_lags[,23] + load_lags[,24] + load_lags[,25] + 
                     load_lags[,26] + load_lags[,27] + load_lags[,28] + load_lags[,29] + 
                     load_lags[,30],
                   ~1)

# Divide between training and testing
# exclude "2011-12-01" because it has only one observation at 0.00
dtrain <- dat[dat$year < 2010, ]
test <- dat[(dat$year >= 2010) & (dat$date < "2011-12-01"), ]

## models fitting
library(parallel)
fits <- mclapply(list(formula_n, formula_n2), 
                 FUN = function(ff){
                   gam_nl(ff,
                          family = fam_gaussian(),
                          data = dtrain)
                 }, mc.cores = 2)

summary(fits[[1]])
summary(fits[[2]])

library(mgcViz)
library(gridExtra)
fit <- getViz(fits[[1]], nsim = 100)
fit2 <- getViz(fits[[2]], nsim = 100)

gridPrint(plot(sm(fit, 1))+l_ciPoly()+ l_fitLine() + xlab("Time") + ylab("f(Time)"),
          plot(sm(fit, 2))+l_ciPoly()+ l_fitLine() + xlab("Time of the year") + ylab("f(Time of the year)"),
          plot(sm(fit, 3))+l_ciPoly()+ l_fitLine() + 
            xlab(expression(paste("Temperatures ", {}%.%alpha[t]))) + 
            ylab(expression(paste("s(Temperatures", {}%.%alpha[t], ")"))),
          plot(sm(fit, 4))+l_ciPoly()+ l_fitLine() + 
            xlab(expression(paste("Lags", {}%.%alpha[l]))) + 
            ylab(expression(paste("s(Lags", {}%.%alpha[l], ")"))),
          ncol = 2)
plot(fit, inner = TRUE, select = 3)+xlab("Weather stations") +
  ylab(expression(paste("Temperature coefficients (", alpha[t], ")")))
plot(fit, inner = TRUE, select = 4)+xlab("Lags") +
  ylab(expression(paste("Lags coefficients (", alpha[l], ")")))

## comparison
y_test <- predict(fits[[1]], newdata = test)
y_test2 <- predict(fits[[2]], newdata = test)

paste0("With nested effects: ", sd(y_test[,1]-test$load))
paste0("With standard effects: ", sd(y_test2[,1]-test$load))




