
eblupRB<-function (formula, vardir,weight, MAXITER = 100, PRECISION = 1e-04, data) 
{
  method = "REML"
  result <- list(eblup = NA, fit = list(method = method, convergence = TRUE, 
                                        iterations = 0, estcoef = NA, refvar = NA, goodness=NA))
  namevar <- deparse(substitute(vardir))
  nameweight <- deparse(substitute(weight))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit, 
                               data)
    X <- model.matrix(formula, data)
    vardir <- data[, namevar]
    weight <- data[, nameweight]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  y <- formuladata[, 1]
  if (attr(attributes(formuladata)$terms, "response") == 1) 
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0) 
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir))) 
    stop("Argument vardir=", namevar, " contains NA values.")
  m <- length(y)
  p <- dim(X)[2]
  Xt <- t(X)
  if (method == "REML") {
    Aest.REML <- 0
    Aest.REML[1] <- median(vardir)
    k <- 0
    diff <- PRECISION + 1
    while ((diff > PRECISION) & (k < MAXITER)) {
      k <- k + 1
      Vi <- 1/(Aest.REML[k] + vardir)
      XtVi <- t(Vi * X)
      Q <- solve(XtVi %*% X, tol = 1e-30)
      P <- diag(Vi) - t(XtVi) %*% Q %*% XtVi
      Py <- P %*% y
      s <- (-0.5) * sum(diag(P)) + 0.5 * (t(Py) %*% Py)
      F <- 0.5 * sum(diag(P %*% P))
      Aest.REML[k + 1] <- Aest.REML[k] + s/F
      diff <- abs((Aest.REML[k + 1] - Aest.REML[k])/Aest.REML[k])
    }
    A.REML <- max(Aest.REML[k + 1], 0)
    result$fit$iterations <- k
    if (k >= MAXITER && diff >= PRECISION) {
      result$fit$convergence <- FALSE
      return(result)
    }
    Vi <- 1/(A.REML + vardir)
    XtVi <- t(Vi * X)
    Q <- solve(XtVi %*% X, tol = 1e-30)
    beta.REML <- Q %*% (XtVi %*% y)
    varA <- 1/F
    std.errorbeta <- sqrt(diag(Q))
    tvalue <- beta.REML/std.errorbeta
    pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)
    Xbeta.REML <- X %*% beta.REML
    resid <- y - Xbeta.REML
    loglike <- (-0.5) * (sum(log(2 * pi * (A.REML + vardir)) + 
                               (resid^2)/(A.REML + vardir)))
    AIC <- (-2) * loglike + 2 * (p + 1)
    BIC <- (-2) * loglike + (p + 1) * log(m)
    goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
    coef <- data.frame(beta = beta.REML, std.error = std.errorbeta, 
                       tvalue, pvalue)
    variance <- A.REML
    EBLUP <- Xbeta.REML + A.REML * Vi * resid
  }
  result$fit$estcoef <- coef
  result$fit$refvar <- variance
  result$fit$goodness <- goodness
  
  weight_p0<-cbind(weight*y, weight*EBLUP)
  ratio_benchmark<-colSums(weight_p0)[1]/colSums(weight_p0)[2]
  est_eblup_kec_rb<-data.frame(EBLUP,EBLUP*ratio_benchmark)
  colnames(est_eblup_kec_rb)<-c("EBLUP","EBLUP BENCHMARK")
  result$eblup <- est_eblup_kec_rb
  
  return(result)
}

eblupDB<-function (formula, vardir,weight, MAXITER = 100, PRECISION = 1e-04, data) 
{
  method = "REML"
  result <- list(eblup = NA, fit = list(method = method, convergence = TRUE, 
                                        iterations = 0, estcoef = NA, refvar = NA, goodness=NA))
  namevar <- deparse(substitute(vardir))
  nameweight <- deparse(substitute(weight))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit, 
                               data)
    
    X <- model.matrix(formula, data)
    vardir <- data[, namevar]
    weight <- data[, nameweight]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  y <- formuladata[, 1]
  if (attr(attributes(formuladata)$terms, "response") == 1) 
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0) 
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir))) 
    stop("Argument vardir=", namevar, " contains NA values.")
  m <- length(y)
  p <- dim(X)[2]
  Xt <- t(X)
  if (method == "REML") {
    Aest.REML <- 0
    Aest.REML[1] <- median(vardir)
    k <- 0
    diff <- PRECISION + 1
    while ((diff > PRECISION) & (k < MAXITER)) {
      k <- k + 1
      Vi <- 1/(Aest.REML[k] + vardir)
      XtVi <- t(Vi * X)
      Q <- solve(XtVi %*% X, tol = 1e-30)
      P <- diag(Vi) - t(XtVi) %*% Q %*% XtVi
      Py <- P %*% y
      s <- (-0.5) * sum(diag(P)) + 0.5 * (t(Py) %*% Py)
      F <- 0.5 * sum(diag(P %*% P))
      Aest.REML[k + 1] <- Aest.REML[k] + s/F
      diff <- abs((Aest.REML[k + 1] - Aest.REML[k])/Aest.REML[k])
    }
    A.REML <- max(Aest.REML[k + 1], 0)
    result$fit$iterations <- k
    if (k >= MAXITER && diff >= PRECISION) {
      result$fit$convergence <- FALSE
      return(result)
    }
    Vi <- 1/(A.REML + vardir)
    XtVi <- t(Vi * X)
    Q <- solve(XtVi %*% X)
    beta.REML <- Q %*% (XtVi %*% y)
    varA <- 1/F
    std.errorbeta <- sqrt(diag(Q))
    tvalue <- beta.REML/std.errorbeta
    pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)
    Xbeta.REML <- X %*% beta.REML
    resid <- y - Xbeta.REML
    loglike <- (-0.5) * (sum(log(2 * pi * (A.REML + vardir)) + 
                               (resid^2)/(A.REML + vardir)))
    AIC <- (-2) * loglike + 2 * (p + 1)
    BIC <- (-2) * loglike + (p + 1) * log(m)
    goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
    coef <- data.frame(beta = beta.REML, std.error = std.errorbeta, 
                       tvalue, pvalue)
    variance <- A.REML
    EBLUP <- Xbeta.REML + A.REML * Vi * resid
  }
  result$fit$estcoef <- coef
  result$fit$refvar <- variance
  result$fit$goodness <- goodness
  
  ###PERUBAHAN DARI SINI
  
  weight_p0<-cbind(weight*y, weight*EBLUP)
  
  diff_benchmark<-colSums(weight_p0)[1]-colSums(weight_p0)[2]
  est_eblup_kec_db<-data.frame(EBLUP,EBLUP+diff_benchmark)
  colnames(est_eblup_kec_db)<-c("EBLUP","EBLUP BENCHMARK")
  result$eblup <- est_eblup_kec_db
  result$benchmark<-diff_benchmark
  
  ###SAMPAI SINI
  return(result)
}

eblupWFQ<-function (formula, vardir,weight, MAXITER = 100, PRECISION = 1e-04, omega_is="se", data) 
{
  method = "REML"
  result <- list(eblup = NA, fit = list(method = method, convergence = TRUE, 
                                        iterations = 0, estcoef = NA, refvar = NA, goodness=NA))
  namevar <- deparse(substitute(vardir))
  nameweight <- deparse(substitute(weight))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit, 
                               data)
    
    X <- model.matrix(formula, data)
    
    vardir <- data[, namevar]
    weight <- data[, nameweight]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  y <- formuladata[, 1]
  if (attr(attributes(formuladata)$terms, "response") == 1) 
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0) 
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir))) 
    stop("Argument vardir=", namevar, " contains NA values.")
  m <- length(y)
  p <- dim(X)[2]
  Xt <- t(X)
  if (method == "REML") {
    Aest.REML <- 0
    Aest.REML[1] <- median(vardir)
    k <- 0
    diff <- PRECISION + 1
    while ((diff > PRECISION) & (k < MAXITER)) {
      k <- k + 1
      Vi <- 1/(Aest.REML[k] + vardir)
      XtVi <- t(Vi * X)
      Q <- solve(XtVi %*% X)
      P <- diag(Vi) - t(XtVi) %*% Q %*% XtVi
      Py <- P %*% y
      s <- (-0.5) * sum(diag(P)) + 0.5 * (t(Py) %*% Py)
      F <- 0.5 * sum(diag(P %*% P))
      Aest.REML[k + 1] <- Aest.REML[k] + s/F
      diff <- abs((Aest.REML[k + 1] - Aest.REML[k])/Aest.REML[k])
    }
    A.REML <- max(Aest.REML[k + 1], 0)
    result$fit$iterations <- k
    if (k >= MAXITER && diff >= PRECISION) {
      result$fit$convergence <- FALSE
      return(result)
    }
    Vi <- 1/(A.REML + vardir)
    XtVi <- t(Vi * X)
    Q <- solve(XtVi %*% X)
    beta.REML <- Q %*% (XtVi %*% y)
    varA <- 1/F
    std.errorbeta <- sqrt(diag(Q))
    tvalue <- beta.REML/std.errorbeta
    pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)
    Xbeta.REML <- X %*% beta.REML
    resid <- y - Xbeta.REML
    loglike <- (-0.5) * (sum(log(2 * pi * (A.REML + vardir)) + 
                               (resid^2)/(A.REML + vardir)))
    AIC <- (-2) * loglike + 2 * (p + 1)
    BIC <- (-2) * loglike + (p + 1) * log(m)
    goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
    coef <- data.frame(beta = beta.REML, std.error = std.errorbeta, 
                       tvalue, pvalue)
    variance <- A.REML
    EBLUP <- Xbeta.REML + A.REML * Vi * resid
  }
  result$fit$estcoef <- coef
  result$fit$refvar <- variance
  result$fit$goodness <- goodness
  
  ###PERUBAHAN DARI SINI
  
  weight_p0<-cbind(weight*y, weight*EBLUP)
  result$coba<-weight_p0
  diff_benchmark<-colSums(weight_p0)[1]-colSums(weight_p0)[2]
  
  if (omega_is != "se" & omega_is != "mse") 
    stop(" omega=\"", omega_is, "\" must be \"se\", \"mse\"\"\".")
  if(omega_is=="se"){
    omega<-vardir
    
    
  }else{
    if(omega_is=="mse"){
      mse_temp<-mseFH(formula = formula, vardir = vardir)
      omega<-mse_temp$mse
      
    }
  }
  weight2<-weight^2
  
  lambda<-(omega * weight)/sum(weight2*omega)
  
  diff_benchmark<-colSums(weight_p0)[1]-colSums(weight_p0)[2]
  est_eblup_kec_wfq<-data.frame(EBLUP,EBLUP+(lambda*diff_benchmark))
  
  colnames(est_eblup_kec_wfq)<-c("EBLUP","EBLUP BENCHMARK")
  result$eblup <- est_eblup_kec_wfq
  
  ###SAMPAI SINI
  
  return(result)
}

eblupYR<-function (formula, vardir,weight, MAXITER = 100, PRECISION = 1e-04, data) 
{
  method = "REML"
  result <- list(eblup = NA, fit = list(method = method, convergence = TRUE, 
                                        iterations = 0, estcoef = NA, refvar = NA, goodness = NA))
  namevar <- deparse(substitute(vardir))
  nameweight <- deparse(substitute(weight))
  
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit, 
                               data)
    
    X <- model.matrix(formula, data)
    vardir <- data[, namevar]
    weight <- data[, nameweight]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  y <- formuladata[, 1]
  if (attr(attributes(formuladata)$terms, "response") == 1) 
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0) 
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir))) 
    stop("Argument vardir=", namevar, " contains NA values.")
  m <- length(y)
  p <- dim(X)[2]
  Xt <- t(X)
  if (method == "REML") {
    Aest.REML <- 0
    Aest.REML[1] <- median(vardir)
    k <- 0
    diff <- PRECISION + 1
    while ((diff > PRECISION) & (k < MAXITER)) {
      k <- k + 1
      Vi <- 1/(Aest.REML[k] + vardir)
      XtVi <- t(Vi * X)
      Q <- solve(XtVi %*% X, tol=1e-30)
      P <- diag(Vi) - t(XtVi) %*% Q %*% XtVi
      Py <- P %*% y
      s <- (-0.5) * sum(diag(P)) + 0.5 * (t(Py) %*% Py)
      F <- 0.5 * sum(diag(P %*% P))
      Aest.REML[k + 1] <- Aest.REML[k] + s/F
      diff <- abs((Aest.REML[k + 1] - Aest.REML[k])/Aest.REML[k])
    }
    A.REML <- max(Aest.REML[k + 1], 0)
    result$fit$iterations <- k
    if (k >= MAXITER && diff >= PRECISION) {
      result$fit$convergence <- FALSE
      return(result)
    }
    
    Vi <- 1/(A.REML + vardir)
    Bd <- vardir/(A.REML + vardir)
    
    ###PERUBAHAN DARI SINI (PART 1)
    XtBdW <- t(Bd * weight * X)
    Q2 <- solve(XtBdW %*% X, tol=1e-30)
    #beta.REML2 <- Q2 %*% (XtVi2 %*% y)
    beta.REML2 <- Q2 %*% XtBdW%*%y
    
    ###PERUBAHAN SAMPAI SINI (PART 1)
    
    XtVi <- t(Vi * X)
    Q <- solve(XtVi %*% X, tol=1e-30)
    beta.REML <- Q %*% (XtVi %*% y)
    
    varA <- 1/F
    std.errorbeta <- sqrt(diag(Q))
    tvalue <- beta.REML/std.errorbeta
    pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)
    
    ###PERUBAHAN DARI SINI (PART 2)
    
    Xbeta.REML <- X %*% beta.REML
    Xbeta.REML2 <- X %*% beta.REML2
    
    ###PERUBAHAN SAMPAI SINI (PART 2)
    
    
    resid <- y - Xbeta.REML
    resid2 <- y - Xbeta.REML2
    loglike <- (-0.5) * (sum(log(2 * pi * (A.REML + vardir)) + 
                               (resid^2)/(A.REML + vardir)))
    AIC <- (-2) * loglike + 2 * (p + 1)
    BIC <- (-2) * loglike + (p + 1) * log(m)
    goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
    coef <- data.frame(beta = beta.REML, std.error = std.errorbeta, 
                       tvalue, pvalue)
    variance <- A.REML
    
    ###PERUBAHAN DARI SINI (PART 3)
    EBLUP <- Xbeta.REML + A.REML * Vi * resid
    EBLUP2 <- Xbeta.REML2 + A.REML * Vi * resid2
    #EBLUP2 <- (1-Bd)*y+Bd*Xbeta.REML2
    #print(cbind(EBLUP2,EBLUP3))
    ###PERUBAHAN SAMPAI SINI (PART 3)
  }
  
  result$fit$estcoef <- coef
  result$fit$refvar <- variance
  result$fit$goodness <- goodness
  
  ###PERUBAHAN DARI SINI (PART 4)
  hasil<-data.frame(EBLUP,EBLUP2)
  colnames(hasil)<-c("EBLUP","EBLUP BENCHMARK")
  result$eblup <- hasil
  ###PERUBAHAN SAMPAI SINI (PART 4)
  
  return(result)
}


eblupAUG<-function (formula, vardir, weight , MAXITER = 100, PRECISION = 1e-04, data) 
{
  method = "REML"
  result <- list(eblup = NA, fit = list(method = method, convergence = TRUE, 
                                        iterations = 0, estcoef = NA, refvar = NA, goodness = NA))
  namevar <- deparse(substitute(vardir))
  nameweight <- deparse(substitute(weight))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit, 
                               data)
    
    X <- model.matrix(formula, data)
    vardir <- data[, namevar]
    weight <- data[, nameweight]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  y <- formuladata[, 1]
  if (attr(attributes(formuladata)$terms, "response") == 1) 
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0) 
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir))) 
    stop("Argument vardir=", namevar, " contains NA values.")
  m <- length(y)
  p <- dim(X)[2]
  Xt <- t(X)
  if (method == "REML") {
    Aest.REML <- 0
    Aest.REML[1] <- median(vardir)
    k <- 0
    diff <- PRECISION + 1
    while ((diff > PRECISION) & (k < MAXITER)) {
      k <- k + 1
      Vi <- 1/(Aest.REML[k] + vardir)
      XtVi <- t(Vi * X)
      Q <- solve(XtVi %*% X, tol = 1e-30)
      P <- diag(Vi) - t(XtVi) %*% Q %*% XtVi
      Py <- P %*% y
      s <- (-0.5) * sum(diag(P)) + 0.5 * (t(Py) %*% Py)
      F <- 0.5 * sum(diag(P %*% P))
      Aest.REML[k + 1] <- Aest.REML[k] + s/F
      diff <- abs((Aest.REML[k + 1] - Aest.REML[k])/Aest.REML[k])
    }
    A.REML <- max(Aest.REML[k + 1], 0)
    result$fit$iterations <- k
    if (k >= MAXITER && diff >= PRECISION) {
      result$fit$convergence <- FALSE
      return(result)
    }
    
    Vi <- 1/(A.REML + vardir)
    Bd <- vardir/(A.REML + vardir)
    
    XtVi <- t(Vi * X)
    Q <- solve(XtVi %*% X, tol = 1e-30)
    beta.REML <- Q %*% (XtVi %*% y)
    
    varA <- 1/F
    std.errorbeta <- sqrt(diag(Q))
    tvalue <- beta.REML/std.errorbeta
    pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)
    
    Xbeta.REML <- X %*% beta.REML
    
    resid <- y - Xbeta.REML
    loglike <- (-0.5) * (sum(log(2 * pi * (A.REML + vardir)) + 
                               (resid^2)/(A.REML + vardir)))
    AIC <- (-2) * loglike + 2 * (p + 1)
    BIC <- (-2) * loglike + (p + 1) * log(m)
    goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
    coef <- data.frame(beta = beta.REML, std.error = std.errorbeta, 
                       tvalue, pvalue)
    variance <- A.REML
    
    ###PERUBAHAN DARI SINI (PART 3)
    EBLUP <- Xbeta.REML + A.REML * Vi * resid
    ###PERUBAHAN SAMPAI SINI (PART 3)
    
    #EBLUP AUG#
    Xaug<-cbind(X, weight*vardir)
    paug <- dim(Xaug)[2]
    Xaugt <- t(Xaug)
    
    Aest.REML <- 0
    Aest.REML[1] <- median(vardir)
    k <- 0
    diff <- PRECISION + 1
    while ((diff > PRECISION) & (k < MAXITER)) {
      k <- k + 1
      Vi <- 1/(Aest.REML[k] + vardir)
      XaugtVi <- t(Vi * Xaug)
      Q <- solve(XaugtVi %*% Xaug, tol = 1e-30)
      P <- diag(Vi) - t(XaugtVi) %*% Q %*% XaugtVi
      Py <- P %*% y
      s <- (-0.5) * sum(diag(P)) + 0.5 * (t(Py) %*% Py)
      F <- 0.5 * sum(diag(P %*% P))
      Aest.REML[k + 1] <- Aest.REML[k] + s/F
      diff <- abs((Aest.REML[k + 1] - Aest.REML[k])/Aest.REML[k])
    }
    A.REML <- max(Aest.REML[k + 1], 0)
    result$fit$iterations <- k
    if (k >= MAXITER && diff >= PRECISION) {
      result$fit$convergence <- FALSE
      return(result)
    }
    
    Vi <- 1/(A.REML + vardir)
    Bd <- vardir/(A.REML + vardir)
    
    XaugtVi <- t(Vi * Xaug)
    Q <- solve(XaugtVi %*% Xaug, tol = 1e-30)
    beta.REML <- Q %*% (XaugtVi %*% y)
    
    varA <- 1/F
    std.errorbeta <- sqrt(diag(Q))
    tvalue <- beta.REML/std.errorbeta
    pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)
    
    Xaugbeta.REML <- Xaug %*% beta.REML
    
    resid <- y - Xaugbeta.REML
    loglike <- (-0.5) * (sum(log(2 * pi * (A.REML + vardir)) + 
                               (resid^2)/(A.REML + vardir)))
    AIC <- (-2) * loglike + 2 * (paug + 1)
    BIC <- (-2) * loglike + (paug + 1) * log(m)
    goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
    coef <- data.frame(beta = beta.REML, std.error = std.errorbeta, 
                       tvalue, pvalue)
    variance <- A.REML
    
    ###PERUBAHAN DARI SINI (PART 3)
    EBLUPaug <- Xaugbeta.REML + A.REML * Vi * resid
    ###PERUBAHAN SAMPAI SINI (PART 3)
    
  }
  
  result$fit$estcoef <- coef
  result$fit$refvar <- variance
  result$fit$goodness <- goodness
  
  ###PERUBAHAN DARI SINI (PART 4)
  hasil<-data.frame(EBLUP,EBLUPaug)
  colnames(hasil)<-c("EBLUP","EBLUP BENCHMARK")
  result$eblup <- hasil
  ###PERUBAHAN SAMPAI SINI (PART 4)
  
  return(result)
}

##################MSE#############################

mseDB<-function (formula, vardir, MAXITER = 100, PRECISION = 1e-04, weight, data) 
{
  method = "REML"
  result <- list(est = NA, mse = NA)
  
  namevar <- deparse(substitute(vardir))
  nameweight <- deparse(substitute(weight))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit, 
                               data)
    
    X <- model.matrix(formula, data)
    vardir <- data[, namevar]
    weight <- data[, nameweight]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  y <- formuladata[, 1]
  if (attr(attributes(formuladata)$terms, "response") == 1) 
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0) 
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir))) 
    stop("Argument vardir=", namevar, " contains NA values.")
  vardir<-na.omit(vardir)
  result$est <- eblupFH(y ~ X - 1, vardir)
  if (result$est$fit$convergence == FALSE) {
    warning("The fitting method does not converge.\n")
    return(result)
  }
  A <- result$est$fit$refvar
  m <- dim(X)[1]
  p <- dim(X)[2]
  g1d <- rep(0, m)
  g2d <- rep(0, m)
  g3d <- rep(0, m)
  g4d <- rep(0, m)
  mse2d <- rep(0, m)
  Vi <- 1/(A + vardir)
  Bd <- vardir/(A + vardir)
  SumAD2 <- sum(Vi^2)
  XtVi <- t(Vi * X)
  Q <- solve(XtVi %*% X, tol = 1e-30)
  
  ##EDIT
  
  g4.a<-sum(weight^2*Bd^2/(Vi))
  
  g4.b<-0
  for (i in 1:length(weight)) {
    xdi <- matrix(X[i, ], nrow = 1, ncol = p)
    for (j in 1:length(weight)) {
      xdj <- matrix(X[j, ], nrow = 1, ncol = p)
      g4.b<-g4.b+weight[i]*weight[j]*Bd[i]*Bd[j]*(xdi %*% Q %*% t(xdj))
    }
  }
  
  ###
  
  if (method == "REML") {
    VarA <- 2/SumAD2
    for (d in 1:m) {
      g1d[d] <- vardir[d] * (1 - Bd[d])
      xd <- matrix(X[d, ], nrow = 1, ncol = p)
      g2d[d] <- (Bd[d]^2) * xd %*% Q %*% t(xd)
      g3d[d] <- (Bd[d]^2) * VarA/(A + vardir[d])
      ##TAMBAHAN KOMPONEN g4
      g4d <- g4.a-g4.b
      mse2d[d] <- g1d[d] + g2d[d] + 2 * g3d[d] +g4d
    }
  }
  result$mse <- mse2d
  result$g4.a<-g4.a
  result$g4.b<-g4.b
  return(result)
}

mseYR<-function (formula, vardir, MAXITER = 100, PRECISION = 1e-04, weight, data) 
{
  method = "REML"
  result <- list(est = NA, mse = NA)
  namevar <- deparse(substitute(vardir))
  nameweight <- deparse(substitute(weight))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit, 
                               data)
    X <- model.matrix(formula, data)
    vardir <- data[, namevar]
    weight <- data[, nameweight]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  y <- formuladata[, 1]
  if (attr(attributes(formuladata)$terms, "response") == 1) 
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0) 
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir))) 
    stop("Argument vardir=", namevar, " contains NA values.")
  result$est <- eblupFH(y ~ X - 1, vardir)
  if (result$est$fit$convergence == FALSE) {
    warning("The fitting method does not converge.\n")
    return(result)
  }
  A <- result$est$fit$refvar
  m <- dim(X)[1]
  p <- dim(X)[2]
  g1d <- rep(0, m)
  g2d <- rep(0, m)
  g3d <- rep(0, m)
  mse2d <- rep(0, m)
  Vi <- 1/(A + vardir)
  Bd <- vardir/(A + vardir)
  SumAD2 <- sum(Vi^2)
  BdW<-Bd*weight
  WBdGamma<-weight^2*Bd^2/(1-Bd)
  
  BdWXtX<-((t(X*BdW)) %*% X)
  g2.a<-solve(BdWXtX, tol = 1e-30)
  g2.b<-((t(X*WBdGamma)) %*% X)
  
  VB<-(A*g2.a)%*%g2.b%*%g2.a
  
  if (method == "REML") {
    VarA <- 2/SumAD2
    for (d in 1:m) {
      g1d[d] <- vardir[d] * (1 - Bd[d])
      xd <- matrix(X[d, ], nrow = 1, ncol = p)
      g2d[d] <- (Bd[d]^2) * xd %*% VB %*% t(xd)
      g3d[d] <- (Bd[d]^2) * VarA/(A + vardir[d])
      mse2d[d] <- g1d[d] + g2d[d] + 2 * g3d[d]
    }
  }
  result$mse <- mse2d
  return(result)
}

mseAUG<-function (formula, vardir, weight, MAXITER = 100, PRECISION = 1e-04, data) 
{
  method = "REML"
  result <- list(est = NA, mse = NA)
  namevar <- deparse(substitute(vardir))
  nameweight <- deparse(substitute(weight))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit, 
                               data)
    X <- model.matrix(formula, data)
    vardir <- data[, namevar]
    weight <- data[, nameweight]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  y <- formuladata[, 1]
  if (attr(attributes(formuladata)$terms, "response") == 1) 
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0) 
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir))) 
    stop("Argument vardir=", namevar, " contains NA values.")
  result$est <- eblupAUG(y ~ X - 1, vardir, weight)
  if (result$est$fit$convergence == FALSE) {
    warning("The fitting method does not converge.\n")
    return(result)
  }
  A <- result$est$fit$refvar
  X<-cbind(X, vardir*weight)
  m <- dim(X)[1]
  p <- dim(X)[2]
  g1d <- rep(0, m)
  g2d <- rep(0, m)
  g3d <- rep(0, m)
  mse2d <- rep(0, m)
  Vi <- 1/(A + vardir)
  Bd <- vardir/(A + vardir)
  SumAD2 <- sum(Vi^2)
  XtVi <- t(Vi * X)
  Q <- solve(XtVi %*% X, tol = 1e-30)
  if (method == "REML") {
    VarA <- 2/SumAD2
    for (d in 1:m) {
      g1d[d] <- vardir[d] * (1 - Bd[d])
      xd <- matrix(X[d, ], nrow = 1, ncol = p)
      g2d[d] <- (Bd[d]^2) * xd %*% Q %*% t(xd)
      g3d[d] <- (Bd[d]^2) * VarA/(A + vardir[d])
      mse2d[d] <- g1d[d] + g2d[d] + 2 * g3d[d]
    }
  }
  result$mse <- mse2d
  return(result)
}


eblupDB2<-function (formula,  vardir_kec , idkab, vardir_kab , weight_kab, direct_kab, MAXITER = 100, PRECISION = 1e-04, B = 0,  data_kec, data_kab) {
  method = "REML"
  
  
  # Container Start ---------------------------------------------------------
  result <- list(eblup = NA, fit = list(method = method, convergence = TRUE, 
                                        iterations = 0, estcoef = NA, refvar = NA))
  
  # Container End -----------------------------------------------------------
  
  
  
  # Get Variabel Start ------------------------------------------------------
  namevarkec <- deparse(substitute(vardir_kec))
  namekab <- deparse(substitute(idkab))
  
  if (!missing(data_kec)) {
    formuladata <- model.frame(formula, na.action = na.omit, 
                               data_kec)
    X <- model.matrix(formula, data_kec)
    vardir_kec <- data_kec[, namevarkec]
    idkab <- data_kec[, namekab]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  
  namevarkab<-deparse(substitute(vardir_kab))
  nameweightkab<-deparse(substitute(weight_kab))
  namedirkab<-deparse(substitute(direct_kab))
  if (!missing(data_kab)) {
    vardir_kab <- data_kab[, namevarkab]
    weight_kab <- data_kab[, nameweightkab]
    direct_kab <- data_kab[, namedirkab]
  }
  
  y <- formuladata[, 1]
  if (attr(attributes(formuladata)$terms, "response") == 1) 
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0) 
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir_kec))) 
    stop("Argument vardir_kec=", namevarkec, " contains NA values.")
  
  # Get Variabel End --------------------------------------------------------
  
  
  
  # REML Start --------------------------------------------------------------
  m <- length(y)
  p <- dim(X)[2]
  Xt <- t(X)
  
  Aest.REML <- 0
  Aest.REML[1] <- median(vardir_kec)
  k <- 0
  diff <- PRECISION + 1
  while ((diff > PRECISION) & (k < MAXITER)) {
    k <- k + 1
    Vi <- 1/(Aest.REML[k] + vardir_kec)
    XtVi <- t(Vi * X)
    Q <- solve(XtVi %*% X)
    P <- diag(Vi) - t(XtVi) %*% Q %*% XtVi
    Py <- P %*% y
    s <- (-0.5) * sum(diag(P)) + 0.5 * (t(Py) %*% Py)
    F <- 0.5 * sum(diag(P %*% P))
    Aest.REML[k + 1] <- Aest.REML[k] + s/F
    diff <- abs((Aest.REML[k + 1] - Aest.REML[k])/Aest.REML[k])
  }
  
  A.REML <- max(Aest.REML[k + 1], 0)
  result$fit$iterations <- k
  if (k >= MAXITER && diff >= PRECISION) {
    result$fit$convergence <- FALSE
    return(result)
  }
  # REML End ----------------------------------------------------------------
  
  
  
  # Beta Start --------------------------------------------------------------
  Vi <- 1/(A.REML + vardir_kec)
  XtVi <- t(Vi * X)
  Q <- solve(XtVi %*% X)
  beta.REML <- Q %*% (XtVi %*% y)
  varA <- 1/F
  std.errorbeta <- sqrt(diag(Q))
  tvalue <- beta.REML/std.errorbeta
  pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)
  
  xkab<-aggregate(x=list(X), by=list(idkab), mean)
  xkab2<-as.matrix(xkab[,-1])
  
  # Beta End ----------------------------------------------------------------
  
  
  
  # EBLUP Start -------------------------------------------------------------
  Xbeta.REML <- xkab2 %*% beta.REML
  ykab<-direct_kab
  gammakab<-A.REML/(A.REML+vardir_kab)
  u_kab<-gammakab*(ykab-xkab2%*%beta.REML)
  
  EBLUP <- Xbeta.REML+ u_kab
  # EBLUP End ---------------------------------------------------------------
  
  
  
  # Benchmark Start ---------------------------------------------------------
  weight_p0<-cbind(weight_kab*ykab, weight_kab*EBLUP)
  diff_benchmark<-colSums(weight_p0)[1]-colSums(weight_p0)[2]
  est_eblup_kec_db<-data.frame(EBLUP,EBLUP+diff_benchmark)
  colnames(est_eblup_kec_db)<-c("EBLUP","EBLUP BENCHMARK")
  # Benchmark End -----------------------------------------------------------
  
  
  
  # Result Start ------------------------------------------------------------
  coef <- data.frame(beta = beta.REML, std.error = std.errorbeta, tvalue, pvalue)  
  variance <- A.REML
  
  result$eblup <- est_eblup_kec_db
  result$fit$estcoef <- coef
  result$fit$refvar <- variance
  # Result End --------------------------------------------------------------
  
  return(result)
}

mseDB2<-function (formula,  vardir_kec , idkab, vardir_kab , weight_kab, direct_kab, MAXITER = 100, PRECISION = 1e-04, B = 0,  data_kec, data_kab) 
{
  method="REML"
  result <- list(est = NA, mse = NA)
  
  # Get Variabel Start ------------------------------------------------------
  namevarkec <- deparse(substitute(vardir_kec))
  namekab <- deparse(substitute(idkab))
  
  if (!missing(data_kec)) {
    formuladata <- model.frame(formula, na.action = na.omit, 
                               data_kec)
    X <- model.matrix(formula, data_kec)
    vardir_kec <- data_kec[, namevarkec]
    idkab <- data_kec[, namekab]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  
  namevarkab<-deparse(substitute(vardir_kab))
  nameweightkab<-deparse(substitute(weight_kab))
  namedirkab<-deparse(substitute(direct_kab))
  if (!missing(data_kab)) {
    vardir_kab <- data_kab[, namevarkab]
    weight_kab <- data_kab[, nameweightkab]
    direct_kab <- data_kab[, namedirkab]
  }
  
  y <- formuladata[, 1]
  if (attr(attributes(formuladata)$terms, "response") == 1) 
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0) 
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir_kec))) 
    stop("Argument vardir_kec=", namevarkec, " contains NA values.")
  
  # Get Variabel End --------------------------------------------------------
  vardir_kec<-na.omit(vardir_kec)
  
  
  # REML Start --------------------------------------------------------------
  m <- length(y)
  p <- dim(X)[2]
  Xt <- t(X)
  
  Aest.REML <- 0
  Aest.REML[1] <- median(vardir_kec)
  k <- 0
  diff <- PRECISION + 1
  while ((diff > PRECISION) & (k < MAXITER)) {
    k <- k + 1
    Vi <- 1/(Aest.REML[k] + vardir_kec)
    XtVi <- t(Vi * X)
    Q <- solve(XtVi %*% X)
    P <- diag(Vi) - t(XtVi) %*% Q %*% XtVi
    Py <- P %*% y
    s <- (-0.5) * sum(diag(P)) + 0.5 * (t(Py) %*% Py)
    F <- 0.5 * sum(diag(P %*% P))
    Aest.REML[k + 1] <- Aest.REML[k] + s/F
    diff <- abs((Aest.REML[k + 1] - Aest.REML[k])/Aest.REML[k])
  }
  
  A.REML <- max(Aest.REML[k + 1], 0)
  result$fit$iterations <- k
  if (k >= MAXITER && diff >= PRECISION) {
    result$fit$convergence <- FALSE
    return(result)
  }
  # REML End ----------------------------------------------------------------
  
  
  
  # Beta Start --------------------------------------------------------------
  Vi <- 1/(A.REML + vardir_kec)
  XtVi <- t(Vi * X)
  Q <- solve(XtVi %*% X)
  beta.REML <- Q %*% (XtVi %*% y)
  varA <- 1/F
  std.errorbeta <- sqrt(diag(Q))
  tvalue <- beta.REML/std.errorbeta
  pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)
  
  xkab<-aggregate(x=list(X), by=list(idkab), mean)
  xkab2<-as.matrix(xkab[,-1])
  
  # Beta End ----------------------------------------------------------------
  
  
  
  # EBLUP Start -------------------------------------------------------------
  Xbeta.REML <- xkab2 %*% beta.REML
  ykab<-direct_kab
  gammakab<-A.REML/(A.REML+vardir_kab)
  u_kab<-gammakab*(ykab-xkab2%*%beta.REML)
  
  EBLUP <- Xbeta.REML+ u_kab
  # EBLUP End ---------------------------------------------------------------
  
  A <- A.REML
  X<-xkab2
  m <- dim(X)[1]
  p <- dim(X)[2]
  g1d <- rep(0, m)
  g2d <- rep(0, m)
  g3d <- rep(0, m)
  g4d <- rep(0, m)
  mse2d <- rep(0, m)
  mse2d_eblup <- rep(0, m)
  Vi <- 1/(A + vardir_kab)
  Bd <- vardir_kab/(A + vardir_kab)
  SumAD2 <- sum(Vi^2)
  XtVi <- t(Vi * X)
  Q <- solve(XtVi %*% X, tol = 1e-30)
  
  ##EDIT
  
  g4.a<-sum(weight_kab^2*Bd^2/(Vi))
  
  g4.b<-0
  for (i in 1:length(weight_kab)) {
    xdi <- matrix(X[i, ], nrow = 1, ncol = p)
    for (j in 1:length(weight_kab)) {
      xdj <- matrix(X[j, ], nrow = 1, ncol = p)
      g4.b<-g4.b+weight_kab[i]*weight_kab[j]*Bd[i]*Bd[j]*(xdi %*% Q %*% t(xdj))
    }
  }
  
  ###
  
  if (method == "REML") {
    VarA <- 2/SumAD2
    for (d in 1:m) {
      g1d[d] <- vardir_kab[d] * (1 - Bd[d])
      xd <- matrix(X[d, ], nrow = 1, ncol = p)
      g2d[d] <- (Bd[d]^2) * xd %*% Q %*% t(xd)
      g3d[d] <- (Bd[d]^2) * VarA/(A + vardir_kab[d])
      ##TAMBAHAN KOMPONEN g4
      g4d <- g4.a-g4.b
      mse2d[d] <- g1d[d] + g2d[d] + 2 * g3d[d] +g4d
      mse2d_eblup[d] <- g1d[d] + g2d[d] + 2 * g3d[d] 
    }
  }
  result$mse_benchmark <- mse2d
  result$mse_eblup <- mse2d_eblup
  result$g4.a<-g4.a
  result$g4.b<-g4.b
  return(result)
}

eblupDB3<-function (formula, vardir, weight, idprov ,  MAXITER = 100, PRECISION = 1e-04,
                    B = 0, data)
{
  result <- list(eblup = NA, fit = list(convergence = TRUE,
                                        iterations = 0, estcoef = NA, refvar = NA, goodness = NA))
  
  namevar <- deparse(substitute(vardir))
  nameweight <- deparse(substitute(weight))
  nameidprov <- deparse(substitute(idprov))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit,
                               data)
    
    X <- model.matrix(formula, data)
    vardir <- data[, namevar]
    weight <- data[, nameweight]
    idprov <- data[, nameidprov]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  vardir<-as.vector(vardir)
  y <- formuladata[, 1]
  if (attr(attributes(formuladata)$terms, "response") == 1)
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0)
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir)))
    stop("Argument vardir=", namevar, " contains NA values.")
  m <- length(y)
  p <- dim(X)[2]
  Xt <- t(X)
  Aest.REML <- 0
  Aest.REML[1] <- median(vardir)
  k <- 0
  diff <- PRECISION + 1
  while ((diff > PRECISION) & (k < MAXITER)) {
    k <- k + 1
    Vi <- 1/(Aest.REML[k] + vardir)
    XtVi <- t(Vi * X)
    Q <- solve(XtVi %*% X)
    P <- diag(Vi) - t(XtVi) %*% Q %*% XtVi
    Py <- P %*% y
    s <- (-0.5) * sum(diag(P)) + 0.5 * (t(Py) %*% Py)
    F <- 0.5 * sum(diag(P %*% P))
    Aest.REML[k + 1] <- Aest.REML[k] + s/F
    diff <- abs((Aest.REML[k + 1] - Aest.REML[k])/Aest.REML[k])
  }
  A.REML <- max(Aest.REML[k + 1], 0)
  result$fit$iterations <- k
  if (k >= MAXITER && diff >= PRECISION) {
    result$fit$convergence <- FALSE
    return(result)
  }
  Vi <- 1/(A.REML + vardir)
  XtVi <- t(Vi * X)
  Q <- solve(XtVi %*% X)
  beta.REML <- Q %*% (XtVi %*% y)
  varA <- 1/F
  std.errorbeta <- sqrt(diag(Q))
  tvalue <- beta.REML/std.errorbeta
  pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)
  Xbeta.REML <- X %*% beta.REML
  resid <- y - Xbeta.REML
  loglike <- (-0.5) * (sum(log(2 * pi * (A.REML + vardir)) +
                             (resid^2)/(A.REML + vardir)))
  AIC <- (-2) * loglike + 2 * (p + 1)
  BIC <- (-2) * loglike + (p + 1) * log(m)
  goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
  coef <- data.frame(beta = beta.REML, std.error = std.errorbeta,
                     tvalue, pvalue)
  variance <- A.REML
  EBLUP <- Xbeta.REML + A.REML * Vi * resid
  
  result$fit$estcoef <- coef
  result$fit$refvar <- variance
  result$fit$goodness <- goodness
  
  ###PERUBAHAN DARI SINI
  
  dfbench<-data.frame(id=idprov, y=y, eblup=EBLUP, weight=weight)
  eblup_db<-data.frame(eblup=NA,benchmark=NA)
  for (idprov in unique(dfbench$id)) {
    temp<-dfbench[which(dfbench$id==idprov),]
    weight_p0<-cbind(temp$weight*temp$y, temp$weight*temp$eblup)
    diff_benchmark<-colSums(weight_p0)[1]-colSums(weight_p0)[2]
    temp_hasil<-data.frame(eblup=temp$eblup,benchmark=temp$eblup+diff_benchmark)  
    eblup_db<-rbind(eblup_db,temp_hasil)
  }
  eblup_db<-eblup_db[-1,]
  
  ###SAMPAI SINI
  
  result$eblup<-eblup_db
  return(result)
}

mseDB3<-function (formula, vardir, idprov,MAXITER = 100, PRECISION = 1e-04,
                  B = 0,weight, data)
{
  result <- list(est = NA, mse = NA)
  namevar <- deparse(substitute(vardir))
  nameweight <- deparse(substitute(weight))
  
  namevar <- deparse(substitute(vardir))
  nameweight <- deparse(substitute(weight))
  nameidprov <- deparse(substitute(idprov))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit,
                               data)
    
    X <- model.matrix(formula, data)
    vardir <- data[, namevar]
    weight <- data[, nameweight]
    idprov <- data[, nameidprov]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  y <- formuladata[, 1]
  if (attr(attributes(formuladata)$terms, "response") == 1)
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0)
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir)))
    stop("Argument vardir=", namevar, " contains NA values.")
  vardir<-na.omit(vardir)
  result$est <- eblupFH(y ~ X - 1, vardir, "REML",MAXITER,
                        PRECISION, B)
  if (result$est$fit$convergence == FALSE) {
    warning("The fitting method does not converge.\n")
    return(result)
  }
  A <- result$est$fit$refvar
  m <- dim(X)[1]
  p <- dim(X)[2]
  
  
  Vi <- 1/(A + vardir)
  Bd <- vardir/(A + vardir)
  
  
  ##EDIT
  
  dfmse<-data.frame(id=idprov, weight=weight, Bd=Bd, Vi=Vi, vardir=vardir, formuladata)
  dfx<-data.frame(id=idprov, X)
  mse_db<-data.frame(eblup=NA,benchmark=NA)
  for (idprov in unique(dfmse$id)) {
    temp_mse<-dfmse[which(dfmse$id==idprov),]
    temp_X<-dfx[which(dfx$id==idprov),]
    temp_X<-temp_X[,-1]
    temp_X<-as.matrix(temp_X)
    
    g1d <- rep(0, nrow(temp_mse))
    g2d <- rep(0, nrow(temp_mse))
    g3d <- rep(0, nrow(temp_mse))
    g4d <- rep(0, nrow(temp_mse))
    mse2d_eblup <- rep(0, nrow(temp_mse))
    mse2d_benchmark <- rep(0, nrow(temp_mse))
    
    SumAD2 <- sum(temp_mse$Vi^2)
    XtVi <- t(temp_mse$Vi * temp_X)
    #XtVi<-as.matrix(XtVi)
    cat("Provinsi ",idprov,": \n")
    Q <- solve(XtVi %*% temp_X, tol=1e-30)
    #print(XtVi %*% temp_X)
    cat("Berhasil"," \n")
    
    g4.a<-sum(temp_mse$weight^2*temp_mse$Bd^2/(temp_mse$Vi))
    
    g4.b<-0
    for (i in 1:length(temp_mse$weight)) {
      xdi <- matrix(temp_X[i, ], nrow = 1, ncol = p)
      for (j in 1:length(temp_mse$weight)) {
        xdj <- matrix(temp_X[j, ], nrow = 1, ncol = p)
        g4.b<-g4.b+temp_mse$weight[i]*temp_mse$weight[j]*temp_mse$Bd[i]*temp_mse$Bd[j]*(xdi %*% Q %*% t(xdj))
      }
    }
    
    VarA <- 2/SumAD2
    for (d in 1:nrow(temp_mse)) {
      g1d[d] <- temp_mse$vardir[d] * (1 - temp_mse$Bd[d])
      xd <- matrix(temp_X[d, ], nrow = 1, ncol = p)
      g2d[d] <- (temp_mse$Bd[d]^2) * xd %*% Q %*% t(xd)
      g3d[d] <- (temp_mse$Bd[d]^2) * VarA/(A + temp_mse$vardir[d])
      ##TAMBAHAN KOMPONEN g4
      g4d <- g4.a-g4.b
      mse2d_benchmark[d] <- g1d[d] + g2d[d] + 2 * g3d[d] +g4d
      
    }
    
    mse2d_eblup_hit<-mseFH(formula=formula, vardir=vardir, data=temp_mse)
    mse2d_eblup<-mse2d_eblup_hit$mse
    temp_hasil<-data.frame(eblup=mse2d_eblup,benchmark=mse2d_benchmark)  
    mse_db<-rbind(mse_db,temp_hasil)
    
  }
  mse_db<-mse_db[-1,]
  
  
  
  result$mse <- mse_db
  result$g4.a<-g4.a
  result$g4.b<-g4.b
  return(result)
}