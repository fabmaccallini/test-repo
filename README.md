dir <- "C:\\Users\\IM49IO\\OneDrive - ING\\Documents\\R\\CBOE\\Futures\\"

# create futures names
code <- c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z") # futures name convention
futs <- expand.grid(code, c(paste0("0", 4: 9), (10: 25)))
futs <- paste0(futs[, 1], futs[, 2])
futs <- futs[futs %in% substr(list.files(paste0(dir)), 3, 5)]
# create futures list
VX.futs <- list()
library (readr)
library (xts)
for (i in 1: length(futs)) {
  file <- paste0(dir, "VX", futs[i], ".csv")
  check <- readLines(file, n = 3)
  if (substring(check[1], 1, 3) == "CFE") skip <- 1 else skip <- 0 # handle CFE occasional disclaimer
  check <- as.Date(gsub(",.*$", "", ifelse(skip, check[3], check[2])), format = "%Y-%m-%d")
  if (is.na(check)) format <- "%m/%d/%Y" else format <- "%Y-%m-%d" # handle different date format
  col.types <- cols(col_date(format = format), col_character(), col_double(), col_double(), col_double(), col_double(), col_double(), col_double(), col_integer(), col_integer(), col_integer())
  tmp <- read_csv(paste0(dir, "VX", futs[i], ".csv"), col_types = col.types, skip = skip)
  if (any(tmp[[1]] <= "2007-03-23")) tmp[tmp[[1]] <= "2007-03-23", 3: 8] <- tmp[tmp[[1]] <= "2007-03-23", 3: 8] / 10
  VX.futs[[i]] <- xts(tmp[, -c(1, 2, 10)], order.by = tmp[[1]])
}
names(VX.futs) <- futs

# load expiries from file
exp <- as.Date(read.csv(paste0(dir, "vix_expiries.txt"), FALSE)[, 1], format = "%d %B %Y")
# past expiries from dataset
VX.exp <- rep(NA, length(VX.futs))
for (i in 1: length(VX.futs)) VX.exp[i] <- max(time(VX.futs[[i]]))
VX.exp <- as.Date(VX.exp, format = "%Y-%m-%d")
VX.exp[!(VX.exp %in% exp)] <- exp[!(VX.exp %in% exp)]
  
library (PerformanceAnalytics)
library (TTR)

# Load VIX data
library (stringr)
index <- read.csv(paste0("C:\\Users\\IM49IO\\OneDrive - ING\\Documents\\R\\CBOE\\VIX\\VIX_History.csv"))
colnames(index) <- str_to_title(colnames(index))
index$Date <- as.Date(index$Date, format = "%m/%d/%Y")

# Update data
library (quantmod)
getSymbols("^VIX")
update <- cbind(time(VIX), as.data.frame(VIX[, 1: 4], row.names = NULL))
colnames(update) <- colnames(index)
index <- rbind(index, update[update$Date > max(index$Date), ])

# # update file
# write.table(update, file = "D:/DATA/Equity/Indices/VIX_daily_OHLC_1990.csv", append = TRUE, sep = ",",
#             row.names = FALSE, col.names = FALSE)

vix <- xts(index$Close, order.by = index$Date)
vix.lret <- diff(log(vix))

# Load S&P500 data
getSymbols("^GSPC")
index.lret <- diff(log(Cl(GSPC)))


### 2. Creating the trading instrument

# VIX futures start having a monthly uninterrupted chain from the Oct 2005 expiry.

start <- which(exp == max(time(VX.futs$V05)))

# futures' log returns
df.logC <- log(VX.futs[[start]]$Close)
df.logO <- log(VX.futs[[start]]$Open)
df.logS <- log(VX.futs[[start]]$Settle)
for (i in (start + 1): length(VX.futs)) {
  df.logC <- merge.xts(df.logC, log(VX.futs[[i]]$Close))
  df.logO <- merge.xts(df.logO, log(VX.futs[[i]]$Open))
  df.logS <- merge.xts(df.logS, log(VX.futs[[i]]$Settle))
}
colnames(df.logC) <- colnames(df.logC) <- colnames(df.logC) <- names(VX.futs)[start: i]
df.retC <- apply(df.logC, 2, diff) # Close to Close
df.retO <- apply(df.logO, 2, diff) # Open to Open
df.retS <- apply(df.logS, 2, diff) # Settle to Settle

df.retOC <- df.logC - df.logO # Open to Close
df.retCO <- df.retO - as.matrix(head(df.retOC, -1)) # Close to Open
df.retOS <- df.logS - df.logO # Open to Settle
df.retSO <- df.retO - as.matrix(head(df.retOS, -1)) # Settle to Open

# conformity test
if (dim(df.retC)[1] != dim(df.retO)[1]) setdiff(rownames(df.retC), rownames(df.retO))

rolls <- c(30, as.numeric(exp[- 1] - exp[-length(exp)])[-c(1: (start - 1))]) # number of roll days

df.days <- df.retC; df.days[, ] <- 0; # days to the expiry
dates <- as.Date(rownames(df.days))
for (i in start: length(exp)) {
    df.days[, i - start + 1] <- -as.numeric(dates - exp[i])
    df.days[df.days[, i - start + 1] < 0, i - start + 1] <- 0
    df.days[df.days[, i - start + 1] > rolls[i - start + 1], i - start + 1] <- 0 # count from the number of roll days
}

df.weights <- df.days; df.weights[, ] <- 0
for (i in 1: (dim(df.weights)[2] - 1)) {
    df.weights[df.days[, i] > 0, i] <- df.days[df.days[, i] > 0, i] / rolls[i]
    df.weights[df.days[, i] > 0, i + 1] <- 1 - df.weights[df.days[, i] > 0, i]
}

# # integrity test for the close
# int <- is.na(df.retC) * (df.weights > 0) # find relevant NAs
# int <- apply(int, 2, sum, na.rm = TRUE) # sum NAs per expiry
# if (sum(int) > 0) {
#     int <- max(which(int > 0)) # last expiry with NAs
#     int <- max(which(df.weights[, int] > 0)) # find the last row with NA
# } else {
#     int <- 0
# }
svix.retC <- apply(df.weights * df.retC, 1, sum, na.rm = TRUE)
svix.retC <- xts(svix.retC, order.by = as.Date(rownames(df.weights)))
# if (int > 0) svix.retC <- svix.retC[-c(1: int)] # filter incomplete series out

# # integrity test for the open
# int <- is.na(df.retO) * (df.weights > 0)
# int <- apply(int, 2, sum, na.rm = TRUE)
# if (sum(int) > 0) {
#     int <- max(which(int > 0))
#     int <- max(which(df.weights[, int] > 0))
# } else {
#     int <- 0
# }

df.weights1 <- df.weights; df.weights1[-1, ] <- df.weights1[-dim(df.weights1)[1], ] # shift the weights by one day to reflect the roll on the open the next day
svix.retO <- apply(df.weights1 * df.retO, 1, sum, na.rm = TRUE)
svix.retO <- xts(svix.retO, order.by = as.Date(rownames(df.weights1)))
# if (int > 0) svix.retO <- svix.retO[-c(1: int)] # filter incomplete series out

# on close
svix.lret <- lag(svix.retC, -1)
# on next open
# svix.ret <- lag(svix.retO, -2)
svix.lret <- svix.lret[!is.na(svix.lret)]


### 4. TERM STRUCTURE ANALYSIS
df.Cl <- Cl(get(VX.fut[start]))
for (i in (start + 1): length(VX.fut)) df.Cl <- merge.xts(df.Cl, Cl(get(VX.fut[i])))

df.tsCl <- df.Cl[, 1: 90]; df.tsCl[, ] <- NA
for (i in 1: dim(df.Cl)[1]) {
    cols <- which(expiry[start: length(expiry)] > index(df.Cl[i, ]))
    x <- c(0, expiry[start: length(expiry)][cols] - index(df.Cl[i, ])) # check if the close of the future is the same as VIX
    y <- c(index$Close[index$Date == index(df.Cl[i, ])], df.Cl[i, cols])
    if (sum(!is.na(y)) >= 5) {
        np <- ceiling(max(x[!is.na(y)] / 10)) + 1
        df.tsCl[i, 1: np] <- spline(x[!is.na(y)], y[!is.na(y)], n = np, xmin = 0, xmax = 10 * (np - 1))$y
    }
}
colnames(df.tsCl) <- seq(0, dim(df.tsCl)[2] - 1, 1) * 10
# clean
clean <- apply(is.na(df.tsCl), 1, sum) # NA rows
df.tsCl <- df.tsCl[clean < dim(df.tsCl)[2], ]
clean <- apply(is.na(df.tsCl), 2, sum) # NA columns
df.tsCl <- df.tsCl[, clean < dim(df.tsCl)[1]]
df.tsCl <- df.tsCl[, (apply(is.na(df.tsCl), 2, sum) / dim(df.tsCl)[1]) < 0.10] # remove columns with more than 10% NAs
# PCA requires clean data. To get sensible results NAs have to be replaced with sensible estimates, hence the low threshold (10%).
df.tsCl <- na.spline(df.tsCl, x = index(df.tsCl))

# PCA - Term structure
pca <- prcomp(log(na.omit(df.tsCl)), center = TRUE, scale = TRUE)
# plot(cumsum(pca$sdev ^ 2 / sum(pca$sdev ^ 2)))
# par(xaxt = "n")
# matplot(pca$rotation[, 1: 3], type = "l", lwd = 3, lty = 1, xlab = "Term (days)", ylab = "Factor loadings")
# legend("bottom", max(pca$rotation[, 1: 3]), legend = c("Level","Slope", "Curvature"), col = 1: 3, lty = 1, lwd = 3)
# par(xaxt = "s")
# axis(1, 1: dim(df.tsCl)[2], as.character(seq(0, dim(df.tsCl)[2] - 1, 1) * 10))
# # Three factors are highlited very clearly: level (PC1), slope (PC2), curvature (PC3)

# Varimax
varimax3 <- varimax(pca$rotation[, 1: 3], normalize = FALSE)
# matplot(varimax3$loadings, type = "l", lwd = 3, lty = 1, xlab = "Term (days)", ylab = "Factor loadings")
# legend("bottom", max(varimax3$loadings[, 1: 3]), legend = c("Level","Slope", "Curvature"), col = 1: 3, lty = 1, lwd = 3)
# par(xaxt = "s")
# axis(1, 1: dim(df.tsCl)[2], as.character(seq(0, dim(df.tsCl)[2] - 1, 1) * 10))

# Promax
promax3 <- promax(pca$rotation[, 1: 3])
# matplot(promax3$loadings, type = "l", lwd = 3, lty = 1, xlab = "Term (days)", ylab = "Factor loadings")


# PCA - Term structure's changes
pca.ch <- princomp(na.omit(diff(df.tsCl)), cor = TRUE) # analyse results using covariance or correlation
# summary(pca.ch) # print variance accounted for
# par(xaxt = "n")
# matplot(pca.ch$loadings[, 1: 3], type = "l", lwd = 3, lty = 1, xlab = "Term (days)", ylab = "Factor loadings")
# legend("top", legend = c("PC1","PC2", "PC3"), col = 1: 3, lty = 1, lwd = 3)
# par(xaxt = "s")
# axis(1, 1: dim(df.tsCl)[2], as.character(seq(0, dim(df.tsCl)[2] - 1, 1) * 10))
