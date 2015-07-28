# ---- 2 var sample model ----
two.var.coeffs <- c(0.8660254, 0.8660254, 0.7, 0.65, 0.6, 0.3552925)
two.var.sig <- rbind(c(1.0, 0.5, 0.2, 0.2, 0.2),
                       c(0.5, 1.0, 0.2, 0.2, 0.2),
                       c(0.2, 0.2, 1.0, 0.23125, 0.13375),
                       c(0.2, 0.2, 0.23125, 1.0, 0.03625),
                       c(0.2, 0.2, 0.13375, 0.03625, 1.0))
colnames(two.var.sig) <- c("x1", "x2", "x3", "x4", "x5")
rownames(two.var.sig) <- colnames(two.var.sig)

mm <- cbind(c("y1", "y1", "y2", "y2", "y2"),
            c("x1", "x2", "x3", "x4", "x5"))
sm <- cbind(c("y1"), c("y2"))

# ---- 3 var sample model ----
three.var.coeffs <- c(0.7, 0.75, 0.8, 0.8, 0.8, 0.8660254, 0.8660254, 0.2381570, 0.2092895)
three.var.sig <- rbind(c(1.0, 0.5, 0.2, 0.2, 0.2, 0.2, 0.2),
                       c(0.5, 1.0, 0.2, 0.2, 0.2, 0.2, 0.2),
                       c(0.2, 0.2, 1.0, 0.23125, 0.34375, 0.2, 0.2),
                       c(0.2, 0.2, 0.23125, 1.0, 0.45625, 0.2, 0.2),
                       c(0.2, 0.2, 0.34375, 0.45625, 1.0, 0.2, 0.2),
                       c(0.2, 0.2, 0.2, 0.2, 0.2, 1.0, 0.28),
                       c(0.2, 0.2, 0.2, 0.2, 0.2, 0.28, 1.0))
colnames(three.var.sig) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
rownames(three.var.sig) <- colnames(three.var.sig)

# ---- nico sample model ----
nico.coeffs <- c(0.7, 0.65, 0.6, 0.8, 0.8, 0.76, 0.76, -0.03220357, 0.35)
nico.sig <- rbind(c(1.0, 0.1552, 0.0266, 0.0247, 0.0228, 0.2128, 0.2128),
                       c(0.1552, 1.0, 0.0266, 0.0247, 0.0228, 0.2128, 0.2128),
                       c(0.0266, 0.0266, 1.0, 0.23125, 0.13375, 0.1288, 0.1288),
                       c(0.0247, 0.0247, 0.23125, 1.0, 0.03625, 0.1196, 0.1196),
                       c(0.0228, 0.0228, 0.13375, 0.03625, 1.0, 0.1104, 0.1104),
                       c(0.2128, 0.2128, 0.1288, 0.1196, 0.1104, 1.0, 0.28),
                       c(0.2128, 0.2128, 0.1288, 0.1196, 0.1104, 0.28, 1.0))
colnames(nico.sig) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
rownames(nico.sig) <- colnames(nico.sig)

# ---- nico fixed cross correlations sample model ----
# why are lam_1_1 lam_1_2 lam_1_3 unstable because of low beta_1_3?
nico.fixc.coeffs <- c(0.7, 0.65, 0.6, 0.8, 0.8, 0.76, 0.76, -0.03220357, 0.35)
nico.fixc.sig <- rbind(c(1.0, 0.1552, 0.0247, 0.0247, 0.0247, 0.2128, 0.2128),
                       c(0.1552, 1.0, 0.0247, 0.0247, 0.0247, 0.2128, 0.2128),
                       c(0.0247, 0.0247, 1.0, 0.23125, 0.13375, 0.1196, 0.1196),
                       c(0.0247, 0.0247, 0.23125, 1.0, 0.03625, 0.1196, 0.1196),
                       c(0.0247, 0.0247, 0.13375, 0.03625, 1.0, 0.1196, 0.1196),
                       c(0.2128, 0.2128, 0.1196, 0.1196, 0.1196, 1.0, 0.28),
                       c(0.2128, 0.2128, 0.1196, 0.1196, 0.1196, 0.28, 1.0))
colnames(nico.fixc.sig) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
rownames(nico.fixc.sig) <- colnames(nico.fixc.sig)

# ---- nico fixed cross correlations and higher beta sample model ----
nico.fixc.hib.coeffs <- c(0.7, 0.65, 0.6, 0.8, 0.8, 0.76, 0.76, 0.56, 0.22)
nico.fixc.hib.sig <- rbind(c(1.0, 0.1552, 0.3, 0.3, 0.3, 0.2128, 0.2128),
                       c(0.1552, 1.0, 0.3, 0.3, 0.3, 0.2128, 0.2128),
                       c(0.3, 0.3, 1.0, 0.23125, 0.13375, 0.1196, 0.1196),
                       c(0.3, 0.3, 0.23125, 1.0, 0.03625, 0.1196, 0.1196),
                       c(0.3, 0.3, 0.13375, 0.03625, 1.0, 0.1196, 0.1196),
                       c(0.2128, 0.2128, 0.1196, 0.1196, 0.1196, 1.0, 0.28),
                       c(0.2128, 0.2128, 0.1196, 0.1196, 0.1196, 0.28, 1.0))
colnames(nico.fixc.hib.sig) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
rownames(nico.fixc.hib.sig) <- colnames(nico.fixc.hib.sig)

# ---- fixed cross and outer correlations model ----
fixc.fixo.coeffs <- c(0.8, 0.8, 0.8, 0.85, 0.85, 0.9, 0.9, 0.3, 0.20)
fixc.fixo.sig <- rbind(c(1.000, 0.620, 0.2304, 0.2304, 0.2304, 0.17595, 0.17595),
                          c(0.620, 1.000, 0.2304, 0.2304, 0.2304, 0.17595, 0.17595),
                          c(0.2304, 0.2304, 1.000, 0.460, 0.460, 0.068, 0.068),
                          c(0.2304, 0.2304, 0.460, 1.000, 0.460, 0.068, 0.068),
                          c(0.2304, 0.2304, 0.460, 0.460, 1.000, 0.068, 0.068),
                          c(0.17595, 0.17595, 0.068, 0.068, 0.068, 1.000, 0.445),
                          c(0.17595, 0.17595, 0.068, 0.068, 0.068, 0.445, 1.000))
colnames(fixc.fixo.sig) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
rownames(fixc.fixo.sig) <- colnames(fixc.fixo.sig)


# ---- fixed cross and outer correlations and independent explanatory model ----
fixc.fixo.ind.coeffs <- c(0.8, 0.8, 0.8, 0.85, 0.85, 0.9, 0.9, 0.3, 0.20)
fixc.fixo.ind.sig <- rbind(c(1.000, 0.620, 0.216, 0.216, 0.216, 0.153, 0.153),
                       c(0.620, 1.000, 0.216, 0.216, 0.216, 0.153, 0.153),
                       c(0.216, 0.216, 1.000, 0.460, 0.460, 0.000, 0.000),
                       c(0.216, 0.216, 0.460, 1.000, 0.460, 0.000, 0.000),
                       c(0.216, 0.216, 0.460, 0.460, 1.000, 0.000, 0.000),
                       c(0.153, 0.153, 0.000, 0.000, 0.000, 1.000, 0.445),
                       c(0.153, 0.153, 0.000, 0.000, 0.000, 0.445, 1.000))
colnames(fixc.fixo.ind.sig) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
rownames(fixc.fixo.ind.sig) <- colnames(fixc.fixo.ind.sig)

# ---- fixed cross correlations model ---- 1 iteration because cross correlations are fixed
nico.fixc.hib.hil.coeffs <- c(0.7, 0.75, 0.8, 0.85, 0.85, 0.9, 0.9, 0.32, 0.2)
nico.fixc.hib.hil.sig <- rbind(c(1.000, 0.620, 0.2304, 0.2304, 0.2304, 0.17595, 0.17595),
                       c(0.620, 1.000, 0.2304, 0.2304, 0.2304, 0.17595, 0.17595),
                       c(0.2304, 0.2304, 1.000, 0.23125, 0.34375, 0.068, 0.068),
                       c(0.2304, 0.2304, 0.23125, 1.000, 0.45625, 0.068, 0.068),
                       c(0.2304, 0.2304, 0.34375, 0.45625, 1.000, 0.068, 0.068),
                       c(0.17595, 0.17595, 0.068, 0.068, 0.068, 1.000, 0.445),
                       c(0.17595, 0.17595, 0.068, 0.068, 0.068, 0.445, 1.000))
colnames(nico.fixc.hib.hil.sig) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
rownames(nico.fixc.hib.hil.sig) <- colnames(nico.fixc.hib.hil.sig)

# Observation: if outer loadings are fixed, cross loadings are fixed automatically

# ---- nico sample model with higher beta ---- 2 iteration model because cross correlations are not fixed
nico.hib.coeffs <- c(0.7, 0.65, 0.6, 0.85, 0.85, 0.9, 0.9, 0.4, 0.3)
nico.hib.real.coeffs <- c(0.73, 0.65, 0.56, 0.85, 0.85, 0.9, 0.9, 0.4, 0.3)
nico.hib.sig <- rbind(c(1.0, 0.620, 0.252, 0.234, 0.216, 0.2295, 0.2295),
                       c(0.620, 1.0, 0.252, 0.234, 0.216, 0.2295, 0.2295),
                       c(0.252, 0.252, 1.0, 0.23125, 0.13375, 0.0000, 0.0000),
                       c(0.234, 0.234, 0.23125, 1.0, 0.03625, 0.0000, 0.0000),
                       c(0.216, 0.216, 0.13375, 0.03625, 1.0, 0.0000, 0.0000),
                       c(0.2295, 0.2295, 0.0000, 0.0000, 0.0000, 1.0, 0.445),
                       c(0.2295, 0.2295, 0.0000, 0.0000, 0.0000, 0.445, 1.0))
colnames(nico.hib.sig) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
rownames(nico.hib.sig) <- colnames(nico.hib.sig)

# ---- nico sample model with higher beta and weights according to correlations ----
nico.hib.w.coeffs <- c(0.7, 0.65, 0.6, 0.85, 0.85, 0.9, 0.9, 0.4, 0.3)
nico.hib.w.sig <- rbind(c(1.0, 0.620, 0.252, 0.234, 0.216, 0.2295, 0.2295),
                       c(0.620, 1.0, 0.252, 0.234, 0.216, 0.2295, 0.2295),
                       c(0.252, 0.252, 1.0, 0.1654464, 0.1386830, 0.0000, 0.0000),
                       c(0.234, 0.234, 0.1654464, 1.0, 0.1021875, 0.0000, 0.0000),
                       c(0.216, 0.216, 0.1386830, 0.1021875, 1.0, 0.0000, 0.0000),
                       c(0.2295, 0.2295, 0.0000, 0.0000, 0.0000, 1.0, 0.445),
                       c(0.2295, 0.2295, 0.0000, 0.0000, 0.0000, 0.445, 1.0))
colnames(nico.hib.w.sig) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
rownames(nico.hib.w.sig) <- colnames(nico.hib.w.sig)

mm <- cbind(c("y1", "y1", "y2", "y2", "y2", "y3", "y3"),
            c("x1", "x2", "x3", "x4", "x5", "x6", "x7"))
sm <- cbind(c("y2", "y3"), c("y1", "y1"))
library("MASS")
nobs <- 100
X <- mvrnorm(nobs, rep(0, ncol(three.var.sig)), three.var.sig, empirical = TRUE)
library("semPLS")
dataset <- as.data.frame(X)
model <- plsm(dataset, sm, mm)
estim.model <- sempls(model, dataset)

nmonte <- 300

three.var <- monte_carlo(nmonte, nobs, "semPLS", three.var.sig, model, three.var.coeffs)
nico <- monte_carlo(nmonte, nobs, "semPLS", nico.sig, model, nico.coeffs)
nico.fixc <- monte_carlo(nmonte, nobs, "semPLS", nico.fixc.sig, model, nico.fixc.coeffs)
nico.fixc.hib <- monte_carlo(nmonte, nobs, "semPLS", nico.fixc.hib.sig, model, nico.fixc.hib.coeffs)
fixc.fixo <- monte_carlo(nmonte, nobs, "semPLS", fixc.fixo.sig, model, fixc.fixo.coeffs)
fixc.fixo.ind <- monte_carlo(nmonte, nobs, "semPLS", fixc.fixo.ind.sig, model, fixc.fixo.ind.coeffs)
nico.fixc.hib.hil <- monte_carlo(nmonte, nobs, "semPLS", nico.fixc.hib.hil.sig, model, nico.fixc.hib.hil.coeffs)
nico.hib <- monte_carlo(nmonte, nobs, "semPLS", nico.hib.sig, model, nico.hib.coeffs)
nico.hib.w <- monte_carlo(nmonte, nobs, "semPLS", nico.hib.w.sig, model, nico.hib.w.coeffs)


coeff_plot(three.var)
coeff_plot(nico)
coeff_plot(nico.fixc)
coeff_plot(nico.fixc.hib)
coeff_plot(fixc.fixo)
coeff_plot(fixc.fixo.ind)
coeff_plot(nico.fixc.hib.hil)
coeff_plot(nico.hib)
coeff_plot(nico.hib.w)

parallelplot.simsemPLS(three.var, subset = 1:ncol(three.var$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsemPLS(nico, subset = 1:ncol(nico$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsemPLS(nico.fixc, subset = 1:ncol(nico.fixc$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsemPLS(nico.fixc.hib, subset = 1:ncol(nico.fixc.hib$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsemPLS(fixc.fixo, subset = 1:ncol(fixc.fixo$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsemPLS(fixc.fixo.ind, subset = 1:ncol(fixc.fixo.ind$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsemPLS(nico.fixc.hib.hil, subset = 1:ncol(nico.fixc.hib.hil$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsemPLS(nico.hib, subset = 1:ncol(nico.hib$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsemPLS(nico.hib.w, subset = 1:ncol(nico.hib.w$t), reflinesAt = c(-1, 0, 1))



library("ggplot2")
library("reshape2")

# split to submodels
mm1 <- melt(nico.hib.w$t[, -(4:9)])
mm2 <- melt(nico.hib.w$t[, -c((1:3),(6:9))])
mm3 <- melt(nico.hib.w$t[, -c((1:5),(8:9))])
sm <- melt(nico.hib.w$t[, -(1:7)])

ggplot(aes(x=value, colour=Var2), data=mm1) + geom_density()
ggplot(aes(x=value, colour=Var2), data=mm2) + geom_density()
ggplot(aes(x=value, colour=Var2), data=mm3) + geom_density()
ggplot(aes(x=value, colour=Var2), data=sm) + geom_density()


library("Rgraphviz")
pathDiagram(estim.model, file="simple_model", edge.labels = "both", output.type = "graphics", digits = 6)



