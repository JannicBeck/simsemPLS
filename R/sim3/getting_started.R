loading <- matrix(0, 6, 2) #create a matrix of all 0s
loading[1:3, 1] <- NA #specify free parameters with NA
loading[4:6, 2] <- NA

loading <- matrix(0, 6, 2) #create a matrix of all 0s
loading[1:3, 1] <- c("a1", "a2", "a3") #specify free parameters with texts
loading[4:6, 2] <- c("a4", "a5", "a6")

loadingValues <- matrix(0, 6, 2)  
loadingValues[1:3, 1] <- 0.7  
loadingValues[4:6, 2] <- 0.7

LY <- bind(loading, loadingValues)

summary(LY)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1

RTE <- binds(error.cor)

latent.cor <- matrix(NA, 2, 2) #specify a 2x2 matrix of NAs
diag(latent.cor) <- 1 #set the diagonal of the matrix to 1

RPS <- binds(latent.cor, 0.5) #Defaults to making all NA values in the matrix .5

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")

summary(CFA.Model)

CFA.Model <- model.cfa(LY = LY, RPS = RPS, RTE = RTE)

dat <- generate(CFA.Model, 250, standardized = TRUE)

out <- analyze(CFA.Model, dat)

Output <- sim(1000, n=200, CFA.Model)

head(dat)

library("semPLS")
data(ECSImobi)
ECSImm
ECSIsm

CFAmm <- cbind(c("f1", "f1", "f1", "f2", "f2", "f2"),c("y1", "y2", "y3", "y4", "y5", "y6"))
CFAsm <- cbind(c("f1"), c("f2"))

CFApm <- plsm(dat, CFAsm, CFAmm)

CFAem <- sempls(CFApm, dat)

data.entry(ECSImm)




myData <- simulateData(ecsi.lavaan, sample.nobs=250L)

colnames(myData) <- ecsi.plsm$manifest
ecsi.sempls <- sempls(ecsi.plsm, myData)
