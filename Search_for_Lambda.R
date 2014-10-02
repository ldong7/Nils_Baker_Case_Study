# search for best lambda for boxcox transformation

linearBC <- function(data){
  dataframe <- data.frame(lambda=numeric(0), SSE = numeric(0))
  for (lambda in seq(from = -3, to =3, by =0.1)){
    k2 <- geometric.mean(data[1])
    k1 <- 1/(lambda*k2^(lambda-1))
    if (lambda == 0){
      w <- k2 * log(data[1])
    }
    else{
      w <- k1*((data[1])^lambda -1)
    }
    w <- cbind(w, data[2])
    colnames(w) <- c('w', 'x')
    SLR <- lm(w~x, data = w)
    options(warn=-1)
    SSE <- anova(SLR)[["Sum Sq"]][2]
    row <- cbind(lambda, SSE)
    dataframe <- rbind(dataframe, row)
  }
  optimal <- subset(dataframe, dataframe$SSE==min(dataframe$SSE))
  return(optimal$lambda[1])
}

bisectionBC <- function(data){
  dataframe <- data.frame(lambda=numeric(0), SSE = numeric(0))
  up <- 3.0
  low <- -3.0
  mid <- 0
  while ( (up-low) >= 0.1) {   
    k2 <- geometric.mean(data[1])
    k1_up <- 1/(up*k2^(up-1))
    k1_low <- 1/(low*k2^(low-1))
    k1_mid <- 1/(mid*k2^(mid-1))        
    if (up == 0 & low ==0){
      w_up <- k2 * log(data[1])
      w_low <- k2 * log(data[1])
    }
    else if (up == 0){
      w_up <- k2 * log(data[1])
      w_low <- k1_low*((data[1])^low - 1)
    }
    else if (low == 0){
      w_low <- k2 * log(data[1])
      w_up <- k1_up*((data[1])^up - 1)
    }
    else{
      w_low <- k1_low*((data[1])^low - 1)
      w_up <- k1_up*((data[1])^up - 1)
    }   
    w_low <- cbind(w_low, data[2])
    w_up <- cbind(w_up, data[2])
    colnames(w_low) <- c('w', 'x')
    colnames(w_up) <- c('w', 'x')
    SLR_low <- lm(w~x, data = w_low)
    SLR_up <- lm(w~x, data = w_up)
    options(warn=-1)
    SSE_low <- anova(SLR_low)[["Sum Sq"]][2]
    SSE_up <- anova(SLR_up)[["Sum Sq"]][2]    
    k1_mid <- 1/(mid*k2^(mid-1))
    
    if (mid == 0){
      w_mid <- k2 * log(data[1])
    }
    else{
      w_mid <- k1_mid*((data[1])^mid - 1)
    }    
    w_mid <- cbind(w_mid, data[2])
    colnames(w_mid) <- c('w', 'x')
    SLR_mid <- lm(w~x, data = w_mid)
    options(warn=-1)
    SSE_mid <- anova(SLR_mid)[["Sum Sq"]][2]    
    if (abs(SSE_low-SSE_mid) <= abs(SSE_up-SSE_mid)){
      low <- low
      up <- mid
    }
    else{
      low <- mid
      up <- up
    }
    mid <- (up+low)/2
  }
  return(mid)
}