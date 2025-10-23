##Install and Load Packages
list.of.packages <- c("GetQuandlData", "tidyquant","tidyverse", "DescTools", "AER", "sandwich", "lmtest", "car", "dplyr", "stargazer", "ggplot2", "foreign", "writexl",
                      "PerformanceAnalytics", "fPortfolio", "timeSeries", "openintro","gdata", "doBy", "psych","plm", "readxl", "zoo", "Quandl", "quantmod", "xts")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
##


minvariance <- function(assets, mu = 0.005) {
  return <- log(tail(assets, -1) / head(assets, -1))
  Q <- rbind(cov(return), rep(1, ncol(assets)), colMeans(return))
  Q <- cbind(Q, rbind(t(tail(Q, 2)), matrix(0, 2, 2)))
  b <- c(rep(0, ncol(assets)), 1, mu)
  solve(Q, b)
}


## Procedimentos para buscar de dados: https://msperlin.shinyapps.io/GetDFPData/
##Quandl.api_key("aSGyhWUzKfxhqx7zaAEs")
##IT <- Quandl(c('WIKI/AAPL.1', 'WIKI/GOOGL.1', 'WIKI/MSFT.1','WIKI/IBM.1', 'WIKI/T.1'), start_date = '2008-01-01', end_date = '2012-12-31')
##colnames(IT) = c('Date', 'AAPL', 'GOOGL','MSFT','IBM', 'T')

##IT <- read.table(file.choose(),header = TRUE, sep = ",")

tickers <- c("PRIO3.SA","VBBR3.SA","MRFG3.SA","ECOR3.SA", "CMIG4.SA", "POMO4.SA", "ENGI11.SA", "BBAS3.SA", "VLID3.SA", "KLBN4.SA")
dataEnv <- new.env()
getSymbols(tickers, from="2018-04-01", to="2025-03-25", env=dataEnv)
plist <- eapply(dataEnv, Ad)
IT <- do.call(merge, plist)
IT <- data.frame(Date=index(IT), coredata(IT))


#IT<-IT[,-1]

IT$Date <- as.Date(IT$Date)
str(IT)

## p 33
assets <- IT[, -1]
tail<-tail(assets, -1) 
head<-head(assets, -1) 
return <- log(tail(assets, -1) / head(assets, -1))

head(return)
##write_xlsx(x = return, path = "return.xlsx", col_names = TRUE)

## p.31 e p 34
## Q representa a primeira matriz do problema de otimização na p.31 
Q <- rbind(cov(return), rep(1, ncol(assets)), colMeans(return))

round(Q, 5)

Q <- cbind(Q, rbind(t(tail(Q, 2)), matrix(0, 2, 2)))
round(Q, 5)

## p.31 e p 35 
## retorno esperado do portfolio = 0,5%
mu <- 0.005
b <- c(rep(0, ncol(assets)), 1, mu)
b

w<-solve(Q, b)
w

#forma direta via função.
minvariance(IT[, -1])


#Força Bruta
# Q multiplicado por w deve ser igual a b (ver p.31) 
t(round(Q%*%w, digits = 3))
b
# A soma dos pesos dos ativos no portfolio deve ser igual a 1 
w[1]+w[2]+w[3]+w[4]+w[5]+w[6]+w[7]+w[8]+w[9]+w[10]

IT <- timeSeries(IT[, 2:11], IT[, 1])

IT <- period.apply(IT, INDEX = endpoints(IT, on = "months"), FUN = mean)

log(lag(IT) / IT)

IT_return <- returns(IT)

chart.CumReturns(IT_return, legend.loc = 'topleft', main = '')

## p 38
pspec = portfolioSpec()  #initial specification
setNFrontierPoints(pspec) = 1500  #random portfolios for the efficient frontier
Frontier = portfolioFrontier(IT_return, constraints = "LongOnly")  #strategy
Frontier
weightsPlot(Frontier)
plot(portfolioFrontier(IT_return, constraints = "LongOnly") )

##
pspec2 = portfolioSpec()

setNFrontierPoints(pspec) = 1500
boxconstraints = c("minW[1:9]=0.01", "maxW[1:9]=1")  #for minimum asset weights and maximum asset weights
eff_front3 = portfolioFrontier(IT_return, spec = pspec2, constraints = boxconstraints)
eff_front3
weightsPlot(eff_front3)
##

# https://www.rdocumentation.org/packages/fPortfolio/versions/280.73/topics/efficientPortfolio
Spec = portfolioSpec()
setSolver(Spec) = "solveRshortExact"
Frontier <- portfolioFrontier(as.timeSeries(IT_return), constraints = "LongOnly")  #strategy
frontierPlot(Frontier, col = rep('orange', 2), pch = 19)
monteCarloPoints(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)
grid()

##### Extras
frontierPoints (Frontier, frontier = 'upper')
frontierWeights<-getWeights(Frontier)
pos <- t(frontierWeights)
pos[pos < 0] <- 0
neg <- t(frontierWeights)
neg[neg>0] <- 0
dev.new()
par(xpd = T, mar = par()$mar + c(0,0,0,7))
plotRange <- c(-0.5,1.5)
barplot(pos,
        main="Composição das carteiras",
        col = c('#FFFD7D', '#E8B172','#FF8AAF', '#B4A6ED', '#A2FBFF'),
        ylim=plotRange,
        yaxt='n')
barplot(neg,
        add=TRUE,
        main="Composição das carteiras",
        col = c('#FFFD7D', '#E8B172','#FF8AAF', '#B4A6ED', '#A2FBFF'),
        ylim=rev(plotRange),
        yaxt='n')
#ajustando texto dos yticks
axis(2, at=seq(-0.5,1.5,0.25), las=2)
legend(60,
       0.75,
       lwd = 1,
       lty=1,
       colnames(frontierWeights),
       fill = c('#FFFD7D', '#E8B172','#FF8AAF', '#B4A6ED', '#A2FBFF'))
maxratioPortfolio(IT_return, spec = portfolioSpec(), constraints = "LongOnly")

frontierWeights

#####

## p 40
n <- 6; mu <- 0.005; rf<-0.0001
Q <- cbind(cov(return), rep(0, n - 1))
Q <- rbind(Q, rep(0, n))

r <- c(colMeans(return), rf)

Q <- rbind(Q, rep(1, n), r)
Q <- cbind(Q, rbind(t(tail(Q, 2)), matrix(0, 2, 2)))
b <- c(rep(0, n), 1, mu)

round(Q, 6)

b

w <- solve(Q, b)
w <- head(w, -3)
w / sum(w)

## p 41
Spec <- portfolioSpec()
setSolver(Spec) <- "solveRshortExact"
setTargetReturn(Spec) <- mean(colMeans(IT_return))
efficientPortfolio(IT_return, Spec, 'Short')
minvariancePortfolio(IT_return, Spec, 'Short')
minriskPortfolio(IT_return, Spec)
maxreturnPortfolio(IT_return, Spec)

