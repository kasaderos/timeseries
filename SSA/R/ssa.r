library('httr')
library('rjson')
library(ggplot2)
days <- 14
start <- as.numeric(Sys.time() - 24 * days * 60 * 60)
url <- paste0("https://poloniex.com/public?command=returnChartData&currencyPair=BTC_ETH&start=", start, "&end=9999999999&period=300")
r <- GET(url)
p <- 50
body <- content(r)
origin <- data.frame()
for (i in 1:length(body)){
  origin <- rbind(origin, data.frame(DATE=body[[i]]$date, CLOSE=body[[i]]$close))
}
df <- origin[1 : (nrow(origin) - p), ]
head(df)

N <- nrow(df)
tau <- (N + 1) %/% 2 
X <- matrix(, nrow=0, ncol=N-tau+1)
for(i in 1:tau){
  X <- rbind(X, df$CLOSE[i:(N-tau+i)])
}

C <- X %*% t(X) * 1/(N-tau+1)

eigs <- eigen(C)
eigs$values <- sort(eigs$values, decreasing = TRUE)
r <- tau - 1

Vr <- eigs$vectors[,1:r]
Vtau_1 <- matrix(, nrow=tau-1, ncol=0)
for(i in 1:r){
  Vtau_1 <- cbind(Vtau_1, eigs$vectors[1:(tau-1),i])
}
# df[504, ] = 0.02139798

for (k in 1:p){
  Q <- as.matrix(df$CLOSE[(N-tau+1+k):nrow(df)])
  if (abs(det(Vtau_1)) < 1e-6) {
    print("det")
    break
  }
  P <- solve(Vtau_1) %*% Q
  Xr <- Vr %*% P
  x_next <- Xr[nrow(Xr)]
  if (abs(df$CLOSE[nrow(df)] - origin$CLOSE[nrow(origin)]) > 1) {
    print("eps")
    break
  }
  df <- rbind(df, data.frame(DATE=c(df$DATE[nrow(df)-1] + 1800), CLOSE=c(x_next)))
}

df1 <- origin[(nrow(origin) - 500):nrow(origin),]
df2 <- df[(nrow(df) - 500):nrow(df),]

ggplot() + 
  geom_line(data = df1, aes(x = DATE, y = CLOSE), color = "red") +
  geom_point(data = df2, aes(x = DATE, y = CLOSE), color = "blue", shape=18) +
  #xlim(1.5855e+9, 1.586e+9)+
  ylim(0.02, 0.0225) +
  geom_vline(xintercept = df2$DATE[nrow(df2) - p])
