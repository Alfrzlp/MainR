eblupDBgab<-function (formula, vardir, weight, idprov ,  MAXITER = 100, PRECISION = 1e-04,
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
  
  eblup_db<-cbind(dfbench$id, y, eblup_db)
  colnames(eblup_db)<-c("idprov","Direct","EBLUP","EBLUP BENCHMARK")
  ###SAMPAI SINI
  result$eblup<-eblup_db
  return(result)
}

mseDBgab<-function (formula, vardir, idprov,MAXITER = 100, PRECISION = 1e-04,
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
  
  dfnew<-data.frame(formuladata,vardir=vardir)
  mse_eblup<-mseFH(formula=formula, vardir=vardir, data=dfnew)
  
  
  ##EDIT
  
  dfmse<-data.frame(id=idprov, weight=weight, Bd=Bd, Vi=Vi, vardir=vardir, formuladata)
  dfx<-data.frame(id=idprov, X)
  mse_db<-data.frame(benchmark=NA)
  for (idprov in unique(dfmse$id)) {
    temp_mse<-dfmse[which(dfmse$id==idprov),]
    temp_X<-dfx[which(dfx$id==idprov),]
    temp_X<-temp_X[,-1]
    temp_X<-as.matrix(temp_X)
    
    g4d <- rep(0, nrow(temp_mse))
    mse2d_benchmark <- rep(0, nrow(temp_mse))
    
    SumAD2 <- sum(temp_mse$Vi^2)
    XtVi <- t(temp_mse$Vi * temp_X)
    #XtVi<-as.matrix(XtVi)
    cat("Provinsi ",idprov,": \n")
    #print(XtVi %*% temp_X)
    Q <- solve(XtVi %*% temp_X, tol=1e-30)
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
      ##TAMBAHAN KOMPONEN g4
      g4d <- g4.a-g4.b
      mse2d_benchmark[d] <- g4d
    }
    
    #mse2d_eblup_hit<-mseFH(formula=formula, vardir=vardir, data=temp_mse)
    #mse2d_eblup<-mse2d_eblup_hit$mse
    temp_hasil<-data.frame(benchmark=mse2d_benchmark)  
    mse_db<-rbind(mse_db,temp_hasil)
    
  }
  mse_db<-mse_db[-1,]
  
  mse_db<-cbind(vardir, mse_eblup$mse,      mse_eblup$mse+mse_db)
  mse_db<-cbind(dfmse$id,mse_db)
  
  colnames(mse_db)<-c("idprov","vardir","EBLUP","EBLUP BENCHMARK")
  
  mse_db<-as.data.frame(mse_db)
  result$mse <- mse_db
  return(result)
}