SP500 <- read_excel("C:/Users/whitl/OneDrive/Desktop/R Files/SP500.xls")
View(SP500)
nrow(SP500)
ncol(SP500)
SP500[,c("SP500","CPI","Rate")]
SP500[c(10,100,500,1500),]
TWOTH <- SP500[ (SP500$SP500 > 2000) | (SP500$CPI < 100),]
print(TWOTH)
Q6 <- subset(SP500, (Earnings>50)&(Rate<3), select = c(SP500,Dividend))
print(Q6)
SP500$Rate <- NULL
SP500$RealPrice <- SP500$SP500*SP500$CPI/252.439
SP500$RealEarnings <- (SP500$Earnings*SP500$CPI)/252.439
SP500$PERatio <- SP500$RealPrice/SP500$RealEarnings
