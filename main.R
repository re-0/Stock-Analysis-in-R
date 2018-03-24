# load libraries for data wrangling, time series analysis and visualization
require(ggplot2)
require(gridExtra)
require(scales)
require(dplyr)
# load market data
goog  <- read.csv(
  "C:/Users/.../goog.csv")

msft  <- read.csv(
  "C:/Users/.../msft.csv")

ibm   <- read.csv(
  "C:/Users/.../ibm.csv")

intc  <- read.csv(
  "C:/Users/.../intc.csv")

sp500 <- read.csv(
  "C:/Users/.../sp500.csv")

tbill <- read.csv(
  "C:/Users/.../tnx.csv")

# Uncomment to get the column names
# print(colnames(goog))

# Compute periodic returns for each stock and s&p 500
ret.goog  <- c(0, diff(goog$Close)/goog[-nrow(goog),   "Close"])
ret.msft  <- c(0, diff(msft$Close)/msft[-nrow(msft),   "Close"])
ret.ibm   <- c(0, diff(ibm$Close)/ibm[-nrow(ibm),     "Close"])
ret.intc  <- c(0, diff(intc$Close)/intc[-nrow(intc),   "Close"])
ret.sp500 <- c(0, diff(sp500$Adj.Close)/sp500[-nrow(sp500), "Adj.Close"])
ret.tbill <- c(0, diff(tbill$Close)/tbill[-nrow(tbill), "Close"])

# Add new column "Return" to Data Frames
goog  <- (goog %>%  mutate(Return = ret.goog))[-1,]
msft  <- (msft %>%  mutate(Return = ret.msft))[-1,]
ibm   <- (ibm %>%   mutate(Return = ret.ibm))[-1,]
intc  <- (intc %>%  mutate(Return = ret.intc))[-1,]
sp500 <- (sp500 %>% mutate(Return = ret.sp500))[-1,]
tbill <- (tbill %>% mutate(Return = ret.tbill))[-1,]

# Summarising the stocks and s&p 500 in a table
  stockTicker <- c("GOOG", "MSFT", "IBM", "INTC", "S&P500")
g <- goog %>%
  summarise_at(c("Open", "Close", "High", "Low", "Volume"), mean, na.rm = T)
m <- msft %>%
  summarise_at(c("Open", "Close", "High", "Low", "Volume"), mean, na.rm = T)
i <- ibm %>%
  summarise_at(c("Open", "Close", "High", "Low", "Volume"), mean, na.rm = T)
intl <- intc %>%
  summarise_at(c("Open", "Close", "High", "Low", "Volume"), mean, na.rm = T)
sp <- sp500 %>%
  summarise_at(c("Open", "Adj.Close", "High", "Low", "Volume"), mean, na.rm = T)

open  <- c(g$Open,   m$Open,   i$Open,   intl$Open,   sp$Open)
close <- c(g$Close,  m$Close,  i$Close,  intl$Close,  sp$Adj.Close)
hi    <- c(g$High,   m$High,   i$High,   intl$High,   sp$High)
lo    <- c(g$Low,    m$Low,    i$Low,    intl$Low,    sp$Low)
vol   <- c(g$Volume, m$Volume, i$Volume, intl$Volume, sp$Volume)

dataSummary <- data.frame(Symbol = stockTicker, Open = open,
           Close = close, High = hi, Low = lo, Vol = vol) %>%
                     gather(Type, Price, Open:Low) %>%
                      select(-Vol, Vol) 

# print(dataSummary)

#########
## Plotting the data
########

# Closing Prices ADD Voluem
goog$Date   <- as.Date(goog$Date)
msft$Date   <- as.Date(msft$Date)
ibm$Date    <- as.Date(ibm$Date)
intc$Date   <- as.Date(intc$Date)
sp500$Date  <- as.Date(sp500$Date)

xts.goog <- as.xts(goog[,-1], order.by = goog$Date)
  line.goog <- ggplot(xts.goog, aes(x = Index, y = goog$Close)) + geom_line() +
    labs(title = "GOOG Price", subtitle = "Mar 17, 2017-18",
        x = "Time", y = "Share Price in US$", caption = "Data Source: Yahoo! Finance") + theme_bw()

xts.msft <- as.xts(msft[,-1], order.by = msft$Date)
  line.msft <- ggplot(xts.msft, aes(x = Index, y = msft$Close)) + geom_line() +
    labs(title = "MSFT Price", subtitle = "Mar 17, 2017-18",
        x = "Time", y = "Share Price in US$", caption = "Data Source: Yahoo! Finance") + theme_bw()

xts.ibm <- as.xts(ibm[,-1], order.by = ibm$Date)
  line.ibm <- ggplot(xts.ibm, aes(x = Index, y = ibm$Close)) + geom_line() +
    labs(title = "IBM Price", subtitle = "Mar 17, 2017-18",
        x = "Time", y = "Share Price in US$", caption = "Data Source: Yahoo! Finance") + theme_bw()

xts.intc <- as.xts(intc[,-1], order.by = intc$Date)
  line.intc <- ggplot(xts.intc, aes(x = Index, y = intc$Close)) + geom_line() +
    labs(title = "INTC Price", subtitle = "Mar 17, 2017-18",
        x = "Time", y = "Share Price in US$", caption = "Data Source: Yahoo! Finance") + theme_bw()
  
grid.arrange(line.goog, line.msft, line.ibm, line.intc, nrow = 2)


# Calculating the CAPM beta for each stock
xRet_goog.rf  <- goog$Return  - tbill$Return
xRet_msft.rf  <- msft$Return  - tbill$Return
xRet_ibm.rf   <- ibm$Return   - tbill$Return
xRet_intc.rf  <- intc$Return  - tbill$Return
xRet_sp500.rf <- sp500$Return - tbill$Return

beta.goog <- summary(lm(xRet_goog.rf   ~ xRet_sp500.rf))$coefficients[2,1]
beta.msft <- summary(lm(xRet_msft.rf ~ xRet_sp500.rf))$coefficients[2,1]
beta.ibm  <- summary(lm(xRet_ibm.rf  ~ xRet_sp500.rf))$coefficients[2,1]
beta.intc <- summary(lm(xRet_intc.rf ~ xRet_sp500.rf))$coefficients[2,1]

# Computing Portfolio Weight
avg.close <- c(mean(goog$Close), mean(msft$Close), mean(ibm$Close), mean(intc$Close))
weight <- avg.close / sum(avg.close)
# 0.78316541 0.06192101 0.12304474 0.03186883

# Creating a Data Frame and Bar Plot
portfolio <- data.frame(Ticker = c("GOOG", "MSFT", "IBM", "INTC"), Weight = weight*100)

ggplot(portfolio,
                 aes(x = Ticker, y = Weight, fill = Ticker)) +
                 geom_col() + theme_bw() +
            ggtitle("Portfolio Weight",
                     subtitle = "(GOOG, MSFT, IBM, INTC)") #+
            # geom_text(
            #   aes(label = paste(round(Weight, 3), "%")),
            #   position = position_stack(vjust = 0.5),
            #   size = 8, fontface = "bold")

######
## The same data frame as pie chart. Pretty ugly though
#####
# pie.chart <- ggplot(portfolio, aes(x = "", y = Weight, fill = Ticker))+
#   geom_bar(stat = "identity") +
#   geom_text(aes(label = paste(round(Weight, 3), "%")),
#             position = position_stack(vjust = 0.5),
#             size = 10, fontface = "bold") +
#   coord_polar(theta = "y") +
#   scale_fill_brewer(palette = "Greys", direction = -1) +
#   theme_minimal() +
#   theme(axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         plot.title = element_text(size = 14, face = "bold"),
#         panel.border = element_blank(),
#         panel.grid   = element_blank(),
#         plot.caption = element_text(hjust = 0.5)) +
#   ggtitle("Portfolio Weight",
#           subtitle = "(GOOG, MSFT, IBM, INTC)")

