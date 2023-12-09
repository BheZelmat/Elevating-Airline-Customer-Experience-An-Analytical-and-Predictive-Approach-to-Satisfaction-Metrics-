# loads/installs libraries as needed
my.library <- function(package, ..., character.only = FALSE) {
  if (!character.only)
    package <- as.character(substitute(package))
  yesno <- require(package, ..., character.only = TRUE)
  if (!yesno) {
    try(install.packages(package, dependencies = TRUE))
    yesno <- require(package, ..., character.only = TRUE)
  }
  invisible(yesno)
} 

## Plotting
my.library("ggplot2")
my.library("scales")
my.library(RColorBrewer)

## Data manipulation
my.library("dplyr")

## Information Value/ WoE
my.library("Information")

my.library("pROC")

my.library("bestglm")

my.library("rpart")
my.library("rpart.plot")

## VISUALISATION
### Barplot
barp <- function(Data,x,freq="freq",main="") {
  require(dplyr)
  X = Data[[x]]
  if (freq == "freq" | freq == "count") {
    p <- ggplot(Data,aes(x=X, fill=as.factor(X))) + geom_bar() + theme_bw() +
      labs(x=x, fill=x, y="count") + theme(legend.position="none")
  } else {
    Y<- as.data.frame(table(X))
    p <- ggplot(Y,aes(x=X, y=Freq/sum(Y$Freq), fill=Y$X)) + geom_bar(stat="identity") + 
      theme_bw() +
      labs(x=x, fill=x, y="Relative Frequencies") + 
      theme(legend.position="none")
  }
  
  if (main != "") {
    p <- p + ggtitle(label=main) + theme(plot.title = element_text(hjust = 0.5))
  }
  print(p)
}

### Class Conditional Barplot
cc_barplot <- function(Data,x,y, freq = "condProb", main = "") {
  Y <- Data[[y]]
  X <- Data[[x]]
  
  #require(graphics)
  # my_bar <- barplot(X, beside = TRUE, xlab ="student" , ylim=c(0,1), 
  #         col = c("red","darkblue"), 
  #         ylab = "Relative Frequency")
  
  require(ggplot2)
  if (freq == "count" | freq =="freq") {
    p <- ggplot(Data, aes(x = X, fill = Y)) + geom_bar(position = "dodge") + 
      labs(fill=y, x=x, y="count") + theme_bw() 
  } else if (freq=="relfreq") {
    
    tab <- table(Y,X)
    cl <- colSums(tab)
    for (i in 1:ncol(tab)) {
      tab[,i] <- tab[,i]/cl[i]
    }
    Y <- as.data.frame(tab)
    p <- ggplot(Y, aes(x=X, y=Freq, fill=Y)) + geom_col(position = "dodge") + 
      labs(fill=y, x=x, y="Relative Frequency") + theme_bw() +
      geom_text(aes(x=X, y=Freq+ 0.03, label=signif(Freq,2)), position=position_dodge(width=0.9))
    
  } else {
    tab <- table(Y,X)
    cl <- rowSums(tab)
    for (i in 1:nrow(tab)) {
      tab[i,] <- tab[i,]/cl[i]
    }
    Y <- as.data.frame(tab)
    p <- ggplot(Y, aes(x=X, y=Freq, fill=Y)) + geom_col(position = "dodge") + 
      labs(fill=y, x=x, y=paste0("P(",x," | ",y,")")) + theme_bw() +
      geom_text(aes(x=X, y=Freq+ 0.03, label=signif(Freq,2)), position=position_dodge(width=0.9)) 
    #ggtitle(paste("Conditional Probability of ",x,"given",y))
  }
  
  if (main != "") {
    p <- p + ggtitle(label=main) + theme(plot.title = element_text(hjust = 0.5))
  }
  print(p)
}

### Class Conditional Histogram
cc_hist <- function(Data,x,y, freq="cond", breaks="Sturges", main="") {
  # class label column
  Y <- Data[[y]]
  if (is.factor(Y)) {
    l <- levels(Y)
  } else {
    l <- unique(Y)
  }
  # Predictor column
  X <- Data[[x]]
  
  #browser()
  rangeX <- c(min(X),max(X))
  rangeY <- c(0,0)
  
  out <- list()
  # Compute relative frequencies
  for (i in 1:length(l)) {
    out[[i]] <- hist(X[Y==l[i]], plot=FALSE, breaks=breaks)
    
    if (freq=="cond") {
      out[[i]]$counts = out[[i]]$counts/sum(out[[i]]$counts)
    }
    
    if (max(out[[i]]$counts) > rangeY[2]) { 
      rangeY[2] = max(out[[i]]$counts)
    }
  }
  
  if (freq == "cond") {
    yl = "Relative Frequency of Conditional Distribution"
    fr = TRUE
  } else if (freq=="freq") {
    yl = "Frequency" 
    fr = TRUE
  } else {
    yl = "Density" 
    fr = FALSE
  }
  
  # Set colours
  cols <- brewer.pal(n=max(3,length(l)), "Dark2")
  if (length(l) < 3) {
    cols <- c(rgb(1,0,0,0.5), rgb(0,0,1,0.5))
  }
  
  plot(out[[1]], freq=fr, xlim=rangeX, ylim=rangeY, col=alpha(cols[1],0.3),cex=2.5, 
       main= ifelse(main=="",paste("Histogram of",x,"conditional on",y), main), 
       ylab=yl,xlab=x)
  for (i in 2:length(l)) {
    plot(out[[2]], freq=fr,add=TRUE, xlim=rangeX, ylim=rangeY, col=alpha(cols[i],0.3), cex=2.5)
  }
  
  legend("topright",legend=as.factor(l), lty=rep(1,length(l)), 
         lwd=rep(5,length(l)), col=alpha(cols, 0.3))
}

### Boxplot
cc_boxplot <- function(Data,x,y,main="") {
  X <- Data[[x]]
  Y <- as.factor(Data[[y]])
  #if (!is.factor(Y)) { stop("y has to be a categorical variable")  }
  
  p <- ggplot(Data,aes(x=Y,y=X,fill=Y)) + geom_boxplot(notch=TRUE) +
    labs(x=y,y=x) + theme_bw() + theme(legend.position="none") 
  
  if (main != "") {
    p <- p + ggtitle(label=main) + theme(plot.title = element_text(hjust = 0.5))
  }
  print(p)
}


############## INFORMATION VALUE
# functions for WoE replacement
WOE_encode_df <- function(X,IV) {
  Y <- X
  var_names <- names(IV$Tables)
  for (i in var_names) {
    Y[i] <- WOE_encode(X[[i]], data.frame(IV$Tables[i]))
  }
  return(Y)
}

# replace single column
WOE_encode <- function(x,iv_table) {
  ny <- vector("numeric", length(x))
  # Loop over bins
  for (j in 1:nrow(iv_table)) {
    
    if (iv_table[j,1] == "NA"  | is.na(iv_table[j,1])) {
      # Missing values
      ny[which(is.na(x))] <- iv_table[j,4]
      
    } else if (is.factor(x)) {
      ny[which(x == iv_table[j,1])] <- iv_table[j,4]
      
    } else {
      if (j < nrow(iv_table)) {
        lower <- as.double( gsub("\\[([^,]+),[^,]+","\\1",iv_table[j,1]) )
        #upper <- as.double( gsub("\\[([^,]+),([^,]+)\\]","\\2",iv_table[j,1]))
        upper <- as.double( gsub("\\[([^,]+),([^,]+)\\]","\\1",iv_table[j+1,1]))
        ny[which(x>=lower & x<upper)] <- iv_table[j,4]
      } else{
        lower <- as.double( gsub("\\[([^,]+),[^,]+","\\1",iv_table[j,1]) )
        ny[which(x>=lower)] <- iv_table[j,4]
      }
    }
  }
  return(ny)
}

# Bin rather than substitute with WOE value
WOE_bin <- function(x,iv_table) {
  if (is.factor(x)) {
    return(x)
  }
  
  ny <- vector("numeric", length(x))
  # Loop over bins
  for (j in 1:nrow(iv_table)) {
    
    if (iv_table[j,1] == "NA"  | is.na(iv_table[j,1])) {
      # Missing values
      #ny[which(is.na(x))] <- 0 (it's already 0 so do nothing)
      
    } else {
      if (j < nrow(iv_table)) {
        lower <- as.double( gsub("\\[([^,]+),[^,]+","\\1",iv_table[j,1]) )
        #upper <- as.double( gsub("\\[([^,]+),([^,]+)\\]","\\2",iv_table[j,1]))
        upper <- as.double( gsub("\\[([^,]+),([^,]+)\\]","\\1",iv_table[j+1,1]))
        ny[which(x>=lower & x<upper)] <- j
      } else{
        lower <- as.double( gsub("\\[([^,]+),[^,]+","\\1",iv_table[j,1]) )
        ny[which(x>=lower)] <- j
      }
    }
  }
  return(factor(ny))
}



WOE_bin_df <- function(X,IV) {
  Y <- X
  var_names <- names(IV$Tables)
  for (i in var_names) {
    df <- data.frame( IV$Tables[i] )
    Y[i] <- WOE_bin(X[[i]], data.frame(IV$Tables[i]))
  }
  return(Y)
}


stepCV <- function(Data, response="y", direction="forward",
                   K=10, foldid=c(), perf="error", threshold=0.5)
{
  # response variable
  y1 <- Data[[response]]
  # if response is a factor make it binary
  if (is.factor(y1)) {
    y1 <- 1*(y1 == levels(y1)[2]) 
  }
  # predictors
  xs <- names(Data)[!names(Data) %in% response]
  n <- length(xs)
  
  # evaluation function
  fperf <- function(actual,prob, M="error", thr=threshold) {
    if (M=="auc") {
      out <- pROC::roc(actual, prob, plot=FALSE, quiet=TRUE)$auc
    } else {
      out <- mean(1*(prob > thr) != actual)
    }
    return(out)
  }
  
  # Folds 
  if (length(foldid) == nrow(Data)) {
    K = max(foldid)
    if (K <=1 | any(foldid<1)) {
      stop("Specified foldid is inappropriate")
    }
  } else {
    foldid <- sample(1:K, size = nrow(Data), replace = TRUE)
  }
  
  cv.function <- function(form, Data, y, folds) {
    nK <- max(folds)
    error <- rep(0,nK)
    for (i in 1:nK) {
      # fit model on all but i-th partition
      m <- glm(as.formula(form), data=Data[folds!=i, ], family=binomial)
      # estimate probabilities on i-th partition
      probs <- predict(m, newdata=Data[folds==i, ], type="response")
      # evaluate performance
      error[i] <- fperf(y[folds==i],probs, M=perf, thr=threshold)
    }
    return(list("CV"=mean(error), "sdCV"=sd(error)))
  }
  
  # Initialise output data.frame
  out <- data.frame(matrix(c(rep(1,1+n), rep(0,(1+n)*(n+3))), nrow=1+n),
                    row.names = c(0:n))
  names(out) <- c("Intercept",xs,"logLikelihood","CV","sdCV")
  
  # Null model
  form <- paste(response,"~ 1")
  p <- cv.function(form, Data, y1, foldid)
  out$CV[1] <- p$CV
  out$sdCV[1] <- p$sdCV
  out$logLikelihood[1] <- logLik(glm(as.formula(form),data=Data,family=binomial))
  
  # Full model
  full <- paste(response,"~ 1 +", paste(xs,collapse = "+"))
  p <- cv.function(full, Data, y1, foldid)
  out$CV[1+n] <- p$CV
  out$sdCV[1+n] <- p$sdCV
  out$logLikelihood[1+n] <- logLik(glm(as.formula(full),data=Data,family=binomial))
  out[n+1, 2:(n+1)] <-1 
  
  # Forward Selection
  if (direction!="backward") {
    # Forward: At each step select model with highest likelihood
    c <- 2;
    while (length(xs) > 1) {
      bestL <- -Inf
      bestj <- 0
      for (j in 1:length(xs)) {
        # construct formula
        form1 <- paste(form, "+",xs[j])
        m <- glm(as.formula(form1), data = Data, family=binomial)
        #cat(xs[j],":",logLik(m),"\n")
        if (logLik(m) > bestL) {
          bestL <- logLik(m)
          bestj <- j
        }
      }
      # Include selected variable in formula
      form <- paste(form,"+",xs[bestj])
      
      # Variable xs[bestj] will be included in all subsequent models
      out[[xs[bestj]]][c:nrow(out)] <- 1
      
      # Remove selected variable from list
      xs <- xs[!(xs %in% xs[bestj])]
      
      # Estimate CV performance of selected model
      p <- cv.function(form, Data, y1, foldid)
      out$CV[c] <- p$CV
      out$sdCV[c] <- p$sdCV
      out$logLikelihood[c] <- bestL
      c <- c+1
    } 
    
  } else {
    # Backward Selection
    # revise out
    out[2:n, 2:(n+1)] <- 1
    c <- nrow(out)-1
    while (length(xs) > 1) {
      bestL <- -Inf
      bestj <- 0
      for (j in 1:length(xs)) {
        form1 <- paste(response, "~ 1 +", paste(xs[-j], collapse = "+"))
        m <- glm(as.formula(form1), data = Data, family=binomial)
        # cat(c ,": Removed ", xs[j],":",logLik(m)," ")
        if (logLik(m) > bestL) {
          bestL <- logLik(m)
          bestj <- j
        }
      }
      # Variable xs[bestj] will be excluded from all subsequent models
      out[[xs[bestj]]][1:c] <- 0
      
      # Remove selected variable from list
      xs <- xs[!(xs %in% xs[bestj])]
      
      # Estimate CV performance of selected model
      if (length(xs) > 1) {
        form1 <- paste(response, "~ 1 +", paste(xs, collapse = "+"))
      } else {
        form1 <- paste(response, "~ 1 +", xs)
      }
      p <- cv.function(form1, Data, y1, foldid)
      out$CV[c] <- p$CV
      out$sdCV[c] <- p$sdCV
      out$logLikelihood[c] <- bestL
      c <- c-1
    }
  }
  
  # 1 SD rule
  if (perf=="auc") {
    me <- which.max(out$CV)
    sel <- which((out$CV >= out$CV[me] - out$sdCV[me])==TRUE)[1]
  } else {
    me <- which.min(out$CV)
    sel <- which((out$CV <= out$CV[me] + out$sdCV[me])==TRUE)[1]
  }
  # get names of predictors again
  xs <- names(Data)[!names(Data) %in% response]
  # 1. Row of out containing predictors (excluding Intercept and CV info)
  # 2. Select variables for which the entry in out is 1
  # 3. Get variable names
  xsel <- xs[ out[sel,2:(ncol(out)-3)] == 1 ]
  bestF <- paste(response,"~", paste(xsel, collapse = "+"))
  
  # add asterisk to row.name of selected model
  rownames(out)[sel] <- paste0(rownames(out)[sel],"*")
  
  return( list("BestModel" = glm(as.formula(bestF), data=Data, family=binomial),
               "Subsets" = out,
               "K"=K,
               "criterion"=perf))  
}




####### Plots 2-dimensional decision boundary for logistic regression
log.dec.bound <- function(model,threshold,X)
{
  f <- function(x2, x1, model,thr) {
    out <- predict(model, newdata=data.frame("x1"=x1,"x2"=x2), type="link")
    return(out + log(1/threshold -1))
  }
  
  N <- 200
  x1 <- seq(from=min(X[,1]), to=max(X[,1]), length.out = N)
  x2.range <- range(X[,2]) + c(-1,1)
  
  Y <- data.frame("x"=x1,"y"=x1)
  for (i in 1:N) {
    #browser()
    y1 <- f(x2.range[1], x1[i], model, threshold)
    y2 <- f(x2.range[2], x1[i], model, threshold)
    #cat( y1, y2, "\n")
    if (y1 * y2 > 0) {
      
      Y[i,2] = ifelse(y1>0, x2.range[1], x2.range[2])
      
    } else {
      Y[i,2] <- uniroot(f, interval = x2.range, x1=x1[i],
                        model=model, thr=threshold, 
                        tol=1.0e-6)$root     
    }
  }
  return(Y)
}