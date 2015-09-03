sigma <- rbind(c(1.000, 0.640, 0.640, 0.512, 0.512, 0.512),
               c(0.640, 1.000, 0.640, 0.512, 0.512, 0.512),
               c(0.640, 0.640, 1.000, 0.512, 0.512, 0.512),
               c(0.512, 0.512, 0.512, 1.000, 0.640, 0.640),
               c(0.512, 0.512, 0.512, 0.640, 1.000, 0.640),
               c(0.512, 0.512, 0.512, 0.640, 0.640, 1.000))

library("MASS")
X <- mvrnorm(100, rep(0, ncol(sigma)), sigma, empirical = TRUE)
colnames(X) <- c("x1", "x2", "x3", "x4", "x5", "x6")

# Lets try sempls!!
library("semPLS")

# specify dataset
dataset <- as.data.frame(X)

## specify models
# measurement (outer) model
mm <- cbind(c("y1", "y1", "y1", "y2", "y2", "y2"),
            c("x1", "x2", "x3", "x4", "x5", "x6"))

# structural (inner) model
sm <- cbind(c("y2"), c("y1"))


# create plsm object
model <- plsm(dataset, sm, mm)

# estimate the model with the pls algorithm
estim.model <- sempls(model, dataset)

library("Rgraphviz")
pathDiagram(estim.model1, file="path_model_pls2", edge.labels = "both", output.type = "graphics", digits = 3)

# show the results
estim.model

# model1
mm <- cbind(c("y1", "y1", "y1", "y2", "y2", "y2"),
            c("x1", "x2", "x3", "x4", "x5", "x6"))

lam <- c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
colnames(mm) <- c("source", "target")

# structural (inner) model
sm <- cbind(c("y2"), c("y1"))
beta <- c(0.8)
colnames(sm) <- c("source", "target")

model1.sig <- core(mm, lam, sm, beta)

estim.model1 <- model2sempls(100, model1.sig, sm, mm)
model1.coeffs <- estim.model1$coefficients$Estimate

