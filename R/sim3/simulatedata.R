# function from the lavaan package
#
simulateData <- function (model = NULL, model.type = "sem", meanstructure = FALSE, 
          int.ov.free = TRUE, int.lv.free = FALSE, fixed.x = FALSE, 
          orthogonal = FALSE, std.lv = TRUE, auto.fix.first = FALSE, 
          auto.fix.single = FALSE, auto.var = TRUE, auto.cov.lv.x = TRUE, 
          auto.cov.y = TRUE, ..., sample.nobs = 500L, ov.var = NULL, 
          group.label = paste("G", 1:ngroups, sep = ""), skewness = NULL, 
          kurtosis = NULL, seed = NULL, empirical = FALSE, return.type = "data.frame", 
          return.fit = FALSE, debug = FALSE, standardized = FALSE) 
{
    if (!is.null(seed)) 
        set.seed(seed)
    if (!exists(".Random.seed", envir = .GlobalEnv)) 
        runif(1)
    RNGstate <- .Random.seed
    if (is.list(model)) {
        if (!is.null(model$lhs) && !is.null(model$op) && !is.null(model$rhs) && 
                !is.null(model$free)) {
            lav <- model
        }
        else if (is.character(model[[1]])) {
            stop("lavaan ERROR: model is a list, but not a parameterTable?")
        }
    }
    else {
        lav <- lavaanify(model = model, meanstructure = meanstructure, 
                         int.ov.free = int.ov.free, int.lv.free = int.lv.free, 
                         fixed.x = fixed.x, orthogonal = orthogonal, std.lv = std.lv, 
                         auto.fix.first = auto.fix.first, auto.fix.single = auto.fix.single, 
                         auto.var = auto.var, auto.cov.lv.x = auto.cov.lv.x, 
                         auto.cov.y = auto.cov.y, ngroups = length(sample.nobs))
    }
    if (debug) {
        cat("initial lav\n")
        print(as.data.frame(lav))
    }
    idx <- which(lav$op == "=~" & is.na(lav$ustart))
    if (length(idx) > 0L) {
        if (standardized) {
            lav$ustart[idx] <- 0.7
        }
        else {
            lav$ustart[idx] <- 1
        }
    }
    idx <- which(lav$op == "~~" & is.na(lav$ustart) & lav$lhs == 
                     lav$rhs)
    if (length(idx) > 0L) 
        lav$ustart[idx] <- 1
    idx <- which(lav$op == "~" & is.na(lav$ustart))
    if (length(idx) > 0L) {
        warning("lavaan WARNING: some regression coefficients are unspecified and will be set to zero")
    }
    idx <- which(is.na(lav$ustart))
    if (length(idx) > 0L) 
        lav$ustart[idx] <- 0
    if (debug) {
        cat("lav + default values\n")
        print(as.data.frame(lav))
    }
    if (standardized) {
        lambda.idx <- which(lav$op == "=~")
        if (any(lav$ustart[lambda.idx] >= 1)) {
            warning("lavaan WARNING: standardized=TRUE but factor loadings are >= 1.0")
        }
        reg.idx <- which(lav$op == "~")
        if (any(lav$ustart[reg.idx] >= 1)) {
            warning("lavaan WARNING: standardized=TRUE but regression coefficients are >= 1.0")
        }
        lav2 <- lav
        ngroups <- max(lav$group)
        ov.names <- vnames(lav, "ov")
        ov.nox <- vnames(lav, "ov.nox")
        lv.names <- vnames(lav, "lv")
        lv.y <- vnames(lav, "lv.y")
        ov.var.idx <- which(lav$op == "~~" & lav$lhs %in% ov.nox & 
                                lav$rhs == lav$lhs)
        lv.var.idx <- which(lav$op == "~~" & lav$lhs %in% lv.y & 
                                lav$rhs == lav$lhs)
        if (any(lav2$user[c(ov.var.idx, lv.var.idx)] > 0L)) {
            warning("lavaan WARNING: if residual variances are specified, please use standardized=FALSE")
        }
        lav2$ustart[c(ov.var.idx, lv.var.idx)] <- 0
        fit <- lavaan(model = lav2, sample.nobs = sample.nobs, 
                      ...)
        Sigma.hat <- computeSigmaHat(lavmodel = fit@Model)
        ETA <- computeVETA(lavmodel = fit@Model, lavsamplestats = fit@SampleStats)
        if (debug) {
            cat("Sigma.hat:\n")
            print(Sigma.hat)
            cat("Eta:\n")
            print(ETA)
        }
        for (g in 1:ngroups) {
            var.group <- which(lav$op == "~~" & lav$lhs %in% 
                                   ov.nox & lav$rhs == lav$lhs & lav$group == g)
            ov.idx <- match(ov.nox, ov.names)
            lav$ustart[var.group] <- 1 - diag(Sigma.hat[[g]])[ov.idx]
        }
        if (length(lv.y) > 0L) {
            for (g in 1:ngroups) {
                var.group <- which(lav$op == "~~" & lav$lhs %in% 
                                       lv.y & lav$rhs == lav$lhs & lav$group == g)
                eta.idx <- match(lv.y, lv.names)
                lav$ustart[var.group] <- 1 - diag(ETA[[g]])[eta.idx]
            }
        }
        if (debug) {
            cat("after standardisation lav\n")
            print(as.data.frame(lav))
        }
    }
    if (!is.null(ov.var)) {
        lav$ustart <- unstandardize.est.ov(partable = lav, ov.var = ov.var)
        if (debug) {
            cat("after unstandardisation lav\n")
            print(as.data.frame(lav))
        }
    }
    fit <- lavaan(model = lav, sample.nobs = sample.nobs, ...)
    Sigma.hat <- computeSigmaHat(lavmodel = fit@Model)
    Mu.hat <- computeMuHat(lavmodel = fit@Model)
    if (fit@Model@categorical) {
        TH <- computeTH(lavmodel = fit@Model)
    }
    if (debug) {
        cat("\nModel-implied moments (before Vale-Maurelli):\n")
        print(Sigma.hat)
        print(Mu.hat)
        if (exists("TH")) 
            print(TH)
    }
    ngroups <- length(sample.nobs)
    X <- vector("list", length = ngroups)
    out <- vector("list", length = ngroups)
    for (g in 1:ngroups) {
        COV <- Sigma.hat[[g]]
        if (empirical) {
            COV <- COV * sample.nobs[g]/(sample.nobs[g] - 1)
        }
        if (is.null(skewness) && is.null(kurtosis)) {
            X[[g]] <- MASS::mvrnorm(n = sample.nobs[g], mu = Mu.hat[[g]], 
                                    Sigma = COV, empirical = empirical)
        }
        else {
            Z <- ValeMaurelli1983(n = sample.nobs[g], COR = cov2cor(COV), 
                                  skewness = skewness, kurtosis = kurtosis, debug = debug)
            TMP <- scale(Z, center = FALSE, scale = 1/sqrt(diag(COV)))[, 
                                                                       , drop = FALSE]
            X[[g]] <- sweep(TMP, MARGIN = 2, STATS = Mu.hat[[g]], 
                            FUN = "+")
        }
        ov.ord <- vnames(lav, type = "ov.ord", group = g)
        if (length(ov.ord) > 0L) {
            ov.names <- vnames(lav, type = "ov", group = g)
            for (o in ov.ord) {
                o.idx <- which(o == ov.names)
                th.idx <- which(lav$op == "|" & lav$lhs == o & 
                                    lav$group == g)
                th.val <- c(-Inf, sort(lav$ustart[th.idx]), +Inf)
                X[[g]][, o.idx] <- as.integer(cut(X[[g]][, o.idx], 
                                                  th.val))
            }
        }
        if (return.type == "data.frame") 
            X[[g]] <- as.data.frame(X[[g]])
    }
    if (return.type == "matrix") {
        if (ngroups == 1L) {
            return(X[[1L]])
        }
        else {
            return(X)
        }
    }
    else if (return.type == "data.frame") {
        Data <- X[[1L]]
        if (ngroups > 1L) {
            for (g in 2:ngroups) {
                Data <- rbind(Data, X[[g]])
            }
            Data$group <- rep(1:ngroups, times = sample.nobs)
        }
        var.names <- vnames(fit@ParTable, type = "ov", group = 1L)
        if (ngroups > 1L) 
            var.names <- c(var.names, "group")
        names(Data) <- var.names
        if (return.fit) {
            attr(Data, "fit") <- fit
        }
        return(Data)
    }
    else if (return.type == "cov") {
        if (ngroups == 1L) {
            return(cov(X[[1L]]))
        }
        else {
            cov.list <- lapply(X, cov)
            return(cov.list)
        }
    }
}