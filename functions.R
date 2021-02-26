library(tidyverse)
library(tidymodels)
#Henry Shiffer 1003866881

fit_logistic_lasso <- function(x, y,lambda, beta0 = NULL, eps = 0.0001, iter_max = 100) {
  ## The fit model for the logistic lasso model using coordinate descent on
  ## the penalized iteratively reweighted least squares algorithm. The model
  ## takes the arguments x, y, lambda, beta0, eps, and iter_max and returns
  ## a list containing beta, the intercept, lambda, whether or not the model
  ## converged, the factor levels, the number of iterations, and the error 
  ## produced from the gradient.
  ## 
  ## Input:
  ##    -x: Matrix of predictors (not including the intercept).
  ##    -y: Vector of data.
  ##    -lambda: The penalty parameter, as the model is a lasso model.
  ##    -beta0: The initial guess of beta, set to NULL 
  ##    -eps: The algorithm stopping critereon. Set to 0.0001.
  ##    -iter_max: The maximium number of iterations for the algorithm.
  ## Output:
  ##    -ret: a list containing:
  ##       (beta, intercept, lambda, converged (TRUE or FALSE),
  ##          fact_levels (the factor levels), iter (amount of iterations if takes the algorithm)) 
  ##  
  ## Example:
  ## library(tidyverse)
  ## library(tidymodels)
  ## n <- 1000
  ## x <- x = seq(-3,3, length.out = n)
  ## y <- rbinom(n,size = 1, prob = 1/(1 + exp(-w+2*x)) )%>% as.numeric %>% factor
  ## ret <- fit_logistic_lasso(x, y, lambda = 0.10)
  
  n <- dim(x)[1] #registering n
  p <- dim(x)[2] #registering p
  if (!is.null(beta0)) { #registering beta0
    beta <- beta0
  }else {
    beta0 <- rep(0, p)
    beta <- beta0
  }
  fct_levels <- levels(y) #compute the factor levels
  y <- as.numeric(y) - 1
  
  x_beta0 <- (x %*% beta) %>% as.numeric #compute XB
  p1 <- 1/(1 + exp(-x_beta0))
  
  intercept0 <- 0 
  for (i in 1:iter_max){ # 1st loop through i's
    w <- p1 * (1 - p1) #compute weight w in loop
    z <- x_beta0 + (y - p1)/w #compute z
    r <- z - x %*% beta 
    intercept <- sum(w*r)/sum(w) #computing intercept
    for(j in 1:p) { #2nd loop through j's
      rj = z - intercept - x[,-j] %*% beta[-j] #computing rj for coordinate descent
      if (t(x[,j])%*%(rj*w) > 0){ #coordinate descent condition 1
        beta[j] <- max((abs(t(x[,j])%*%(rj*w)) - (n*lambda)), 0) / sum(w*(x[,j])^2)
      }
      else if (t(x[,j])%*%(rj*w) < 0){ #coordinate descent condition 2
        beta[j] <- - max((abs(t(x[,j])%*%(rj*w)) - (n*lambda)), 0) / sum(w*(x[,j])^2) 
      }
    }
    x_beta0 <- x %*% beta %>% as.numeric #update x_beta0
    p1 <- 1/(1 + exp(-x_beta0))  #update p1
    names(beta) <- colnames(x) #name the betas
    
    #convergence check
    if ((max(abs(intercept - intercept0), max(abs(beta - beta0)))) < eps) {
      return(list(intercept=intercept, beta = beta,
                  lambda = lambda,converged = TRUE,
                  fact_levels = fct_levels,
                  iter = i))
    }
    beta0 <- beta # keep track of the old beta for convergence check
    intercept0 <- intercept #keep track of the old intercept for convergence check
  }
  return(list(intercept=intercept, beta = beta,
              lambda = lambda,converged = FALSE,
              fact_levels = fct_levels,
              iter = i))
  
}





predict_logistic_lasso <- function(object, new_x) {
  ## Takes the return object of the fit_logistic_lasso function which contains beta, 
  ## the intercept, the factor levels and lambda, and new data that is used to
  ## predict a logistic lasso at more than one data point, returning intercept and beta
  ##
  ## Input:
  ## - object: The return value from fit_logistic_lasso which must contain
  ##           the intercept, beta and factor levels returned from 
  ##           the fit_logistic_lasso function call.
  ##
  ## - new_x: The new data that we are trying to predict at.
  ## Output: 
  ## - The predictions of the fit_logistic_lasso function.
  ## Example: 
  ## library(tidyverse)
  ## library(tidymodels)
  ## new_x <- data
  ## object <- fit_logistic_lasso(x, y, lambda = 0.10) 
  ## ret2 <- predict_logistic_lasso(object, new_x)
  
  #adding intercept to XBeta
  numeric_pred <- (object$intercept + (new_x %*% object$beta) >= 0) %>% as.numeric
  return(object$fact_levels[numeric_pred + 1] %>% factor)
}
