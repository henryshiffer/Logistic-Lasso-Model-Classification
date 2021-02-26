library(tidyverse)
library(tidymodels)
#Henry Shiffer 1003866881


logistic_lasso <- function(mode = "classification", penalty) {
  ## This function registers the logistic lasso model, and specifies the mode the
  ## model is working in (classification), and the penalty. These two arguments
  ## must be specified to allow the model to work.
  ##  Input:
  ##    -mode: The mode the model is in, which is classification for the logistic_lasso
  ##           model.
  ##    -penalty: The penalty parameter, as the model is a lasso model.
  ## Output:
  ##    -No output.
  ## Example:
  ## library(tidyverse)
  ## library(tidymodels)
  ## logistic_lasso(mode = "classification", penalty = 0.10 )
  args <- list(penalty = rlang::enquo(penalty))
  new_model_spec("logistic_lasso",
                 args = args,
                 mode = mode, 
                 eng_args = NULL,
                 method = NULL,
                 engine = NULL)
}



set_new_model("logistic_lasso")
set_model_mode(model = "logistic_lasso", 
               mode = "classification")
set_model_engine("logistic_lasso", 
                 mode = "classification",
                 eng = "fit_logistic_lasso")

set_dependency("logistic_lasso", 
               eng = "fit_logistic_lasso", 
               pkg = "base")

set_model_arg( 
  model = "logistic_lasso",
  eng = "fit_logistic_lasso",
  parsnip = "penalty", 
  original = "lambda", 
  func = list(pkg = "dials", fun = "penalty"), 
  has_submodel = FALSE 
)

set_encoding(
  model = "logistic_lasso",
  eng = "fit_logistic_lasso",
  mode = "classification",
  options = list(predictor_indicators = "traditional", 
                 compute_intercept = TRUE, 
                 remove_intercept = TRUE, 
                 allow_sparse_x = FALSE)
)


set_fit(
  model = "logistic_lasso",
  eng = "fit_logistic_lasso",
  mode = "classification",
  value = list(interface = "matrix",
               protect = c("x", "y"),
               func = c(fun = "fit_logistic_lasso"),
               defaults = list()))

set_pred(
  model = "logistic_lasso",
  eng = "fit_logistic_lasso",
  mode = "classification",
  type = "class",
  value = list(pre = NULL,post = NULL,func = c(fun = "predict_logistic_lasso"),
               args = list(object = expr(object$fit),
                           new_x = expr(as.matrix(new_data[, names(object$fit$beta)]))))
)



update.logistic_lasso <- function(object, penalty) {
  ## The finalize workflow function calls the function
  ## update so we need to include a update function for
  ## the logistic_lasso model. This function creates a new
  ## spec that has the final parameter. 
  ##  Input:
  ##    - object: Output from logistic_lasso.
  ##    - penalty: The penalty parameter.
  ##  Output:
  ##      -No output.
  ##  Example:
  ##  library(tidyverse)
  ##  library(tidymodels)
  ##  object <- logistic_lasso(classification, penalty = 0.3)
  ##  update.logistic_lasso(object, penalty = 0.3)
  
  if(!is.null(penalty)) {
    object$args <- list(penalty = enquo(penalty))
  }
  new_model_spec("logistic_lasso", args = object$args,
                 eng_args = NULL,
                 mode = "classification",
                 method = NULL,
                 engine = object$engine)
}


















