
x <- best_fit$model
newdata <- data.frame(wind_turbines_cumulative5000_end_construction = seq(0, 8000, length.out = 200))
condition <- c('landcover_cc_grouped_end = "open lands"')
data <- data_ready_calving_ssf_2h
type <- "exponential"
baseline <- "zero"
zoi <- TRUE
zoi_shape <- "exp_decay"
which_cumulative <- "cumulative"
type_feature <- "point"
n_features <- 1
wmean <- TRUE

predict.model <- function(x,
                          newdata,
                          data = NULL,
                          type = c("linear", "exponential", "exp", "logit", "cloglog")[1],
                          wmean = TRUE,
                          wq_probs = NULL,
                          include = "all",
                          baseline = c("median", "mean", "zero")[1],
                          zoi = FALSE,
                          zoi_shape = c("exp_decay", "gaussian_decay", "linear_decay", "threshold_decay")[1],
                          which_cumulative = "cumulative",
                          type_feature = c("point", "line", "area")[1],
                          type_feature_recompute = FALSE,
                          n_features = 1,
                          zoi_limit = 0.05, # for computing line ZOI
                          resolution = 100, # resolution for the raster created in create_line_feature_zoi
                          line_value = 1, # value sey to the linear inftastructure raster created in create_line_feature_zoi
                          ...) {
  
  # store new data entered
  dfvar <- newdata
  
  # formula
  f <- x$formula
  
  # get variables
  wcols <- oneimpact::extract_response_strata(f, covars = TRUE)
  
  # case
  case <- wcols$response
  # get variable referring to strata
  strat <- wcols$strata
  
  # relevant columns
  all_vars <- all.vars(f)
  
  # covariates summary
  f2 <- as.formula(paste0(wcols$response, " ~ -1 + ", wcols$covars))
  
  all_covars <- grep(wcols$strata, all_vars[-1], invert = TRUE, value = TRUE)
  # get predictors
  data_covs <- data[, all_covars]
  # select numeric predictors to be standardized
  numeric_covs <- (sapply(data_covs, is.numeric) == TRUE)
  
  # if(standardize != FALSE) {
  # now we compute the means and sds in all cases, to check
  # for missing variables/infrastructures
  
  # get standardization parms
  data_covs_num <- data_covs[, numeric_covs]
  # standardize
  data_covs_num_std <- lapply(1:ncol(data_covs_num), function(i) scale(data_covs_num[,i]))
  # register mean and sd
  covs_mean_sd <- data.frame(do.call("rbind", lapply(1:length(data_covs_num_std), function(i)
    sapply(c("scaled:center", "scaled:scale"), function(m) attr(data_covs_num_std[[i]], m)))))
  rownames(covs_mean_sd) <- colnames(data_covs_num)
  colnames(covs_mean_sd) <- c("mean", "sd")
  
  # is data numeric?
  classes_numeric <- sapply(data[,all_vars], is.numeric)
  # numeric variables
  data_summary_num <- as.data.frame(apply(na.omit(as.matrix(data[,all_vars[classes_numeric == TRUE]])), 2, oneimpact::data_summary))
  # character variables - use mode
  data_summary_ch <- as.data.frame(apply(na.omit(as.matrix(data[,all_vars[classes_numeric == FALSE]])), 2, oneimpact::data_summary_char))
  names(data_summary_ch) <- all_vars[classes_numeric == FALSE]
  if(nrow(data_summary_ch) > 0) {
    data_summary <- cbind(data_summary_num, data_summary_ch)[order(c(which(classes_numeric == TRUE), which(classes_numeric == FALSE)))]
  } else {
    data_summary <- data_summary_num
  }
  
  # baselines for plotting and predicting!! mean? median?
  # define the baseline
  bs <- baseline
  if (baseline[1] == "median"){
    baseline <- data_summary[rownames(data_summary) == "50%",]
  } else {
    if(baseline[1] == "mean") {
      baseline <- data_summary[rownames(data_summary) == "mean",]
    } else {
      if (baseline[1] == "zero"){
        baseline <- data_summary[5,]
        baseline[1, 1+which(numeric_covs)] <- 0 # zero for numeric ones
        if(!zoi) baseline[1, 1+which(!numeric_covs)] <- sapply(unname(which(!numeric_covs)),
                                                               function(z) unique(data[,names(numeric_covs[z])])[1])
      } else {
        stop(paste0("Invalid 'baseline' parameter: ", baseline, ". Pleas re-define."))
      }
    }
  }
  
  # is the variable a ZOI variable?
  if (!zoi){
    
    # new data
    newdata <- baseline
    newdata <- newdata[rep(1, nrow(dfvar)),]
    newdata[, names(dfvar)] <- dfvar
    
  } else{
    
    # ZOI radii
    zois <- names(data_summary)[grep(names(dfvar)[1], names(data_summary))]
    # zois <- as.numeric(gsub(names(dfvar)[1], "", zois))
    zoi_radii <- as.numeric(gsub("\\D", "", zois))
    
    # compute ZOI for the intended distances
    dfvar2 <- dfvar[, rep(1, length(zoi_radii)), drop = FALSE]
    is_cumulative <- grepl(pattern = which_cumulative, zois)
    fact <- rep(1, length(zoi_radii))
    if(type_feature == "line") fact <- create_linear_feature_zoi(radii = zoi_radii,
                                                                 type = zoi_shape,
                                                                 radius_max = max(zoi_radii),
                                                                 type_feature_recompute = type_feature_recompute,
                                                                 zoi_limit = zoi_limit,
                                                                 res = resolution,
                                                                 value = line_value)
    dfvar2 <- as.data.frame(do.call("cbind", lapply(c(1:ncol(dfvar2)), function(i) {
      n_feat <- ifelse(is_cumulative[i], n_features, 1)
      n_feat*fact[i]*oneimpact::dist_decay(dfvar2[,i], radius = zoi_radii[i], type = zoi_shape) })))
    names(dfvar2) <- zois#paste0(names(dfvar)[1], zoi_radii)
    
    # cumulative vars
    #dfvar2[,grepl("cumulative", colnames(dfvar2))]*100
    
    # new data
    newdata <- baseline
    newdata <- newdata[rep(1, nrow(dfvar)),]
    newdata[, names(dfvar2)] <- dfvar2
    #if (ncol(dfvar)==2){newdata[,names(dfvar)[2]] <- dfvar[,2]}
  }
  
  # make sure prediction works even if categorical variables are constant
  # get that from the summary instead of dat, where from? maybe a new object
  if(!(zoi & bs == "zero")) { # other variables are ignored in the case of zoi plots
    for(i in which(!x$numeric_covs)) {
      if(class(data[,names(x$numeric_covs[i])]) == "factor") {
        newdata[,i+1] <- factor(newdata[,i+1], levels = levels(data[,names(x$numeric_covs[i])])) # factor
      } else {
        newdata[,i+1] <- factor(newdata[,i+1], levels = sort(unique(data[,names(x$numeric_covs[i])]))) # character
      }
    }
  }
  
  # predict for new data set
  
  # predict only for those variables/coefs of interest for ZOI variables
  include <- if(zoi) colnames(dfvar) else include
  # predict
  pred <- predict.values(f,
                         newdata = newdata,
                         coefs = coef,
                         condition = condition,
                         weights = 1,
                         type = type,
                         wmean = wmean,
                         wq_probs = wq_probs,
                         include = include)
  
  pred
}

coefs <- coef(x)
tidy(x, conf.int = TRUE)

predict.values <- function(x,
                            newdata,
                            coefs,
                            weights = 1,
                            type = c("linear", "exponential", "exp", "logit", "cloglog")[1],
                            wmean = TRUE,
                            wq_probs = NULL,
                            include = "all",
                            ...) {
  
  # formula with not strata
  wcols <- oneimpact::extract_response_strata(x, covars = TRUE)
  formula_no_strata <- as.formula(paste0(wcols$response, " ~ -1+", wcols$covars))
  
  # repeat weights == 1 if necessary
  if(length(weights) == 1 & is.matrix(coefs)) {
    weights <- coefs[1,]
    weights[] <- 1/length(weights)
  }
  
  # condition
  if(condition != "") {
    vars_condition <- strsplit(condition, "=")
    
    values_condition <- trimws(sapply(vars_condition, last))
    vars_condition <- trimws(sapply(vars_condition, first))
  }
  # coefs
  # coefs <- x$coef
  
  # subset of variables to be included
  if (include[1] != "all"){
    
    if(length(values_condition) > 0) {
      include <- c(include, values_condition)
    }
    
    # terms
    include_terms <- paste0(include, collapse = "|")
    # subset coefficients
    if(is.matrix(coefs)) {
      coef_names <- rownames(coefs)
      coefs <- coefs[grepl(include_terms, coef_names), ]
    } else {
      coef_names <- names(coefs)
      coefs <- coefs[grepl(include_terms, coef_names)]
    }
    
    coefs <- coefs[c(1, 2)]
    coefs[1] <- sum(coefs)
    
    # re-set model matrix
    form_parts <- as.character(formula_no_strata)[3] |>
      gsub(pattern = "\n", replacement = "") |>
      strsplit(split = "+", fixed = TRUE)
    form_parts_subset <- sapply(form_parts, grep, pattern = include_terms, value = TRUE)[1]
    form_include <- as.formula(paste0(" ~ -1 + ", paste(form_parts_subset, collapse = " + ")))
    
    nd <- newdata[, c(grep(include_terms, colnames(newdata), value = TRUE)), drop = FALSE]
    mm <- model.matrix(form_include, nd)
  } else {
    # model matrix
    mm <- model.matrix(formula_no_strata, newdata)
  }
  
  # prediction
  pred <- mm %*% coefs[1]
  
  # if wq_probs are provided, the weighted quantiles are computed
  # if wmean is providade, the weighted mean is computed
  # if none is provided, the raw prediction is shown
  if (!is.null(wq_probs)){
    preddf <- data.frame(t(apply(pred, 1, DescTools::Quantile,
                                 weights = weights,
                                 type = 5,
                                 probs=wq_probs)))
    # preddf <- data.frame(t(apply(pred, 1, modi::weighted.quantile,
    #                              w = x$weights,
    #                              prob=wq_probs)))
    
    ########### error here
    # 50: In regularize.values(x, y, ties, missing(ties), na.rm = na.rm) :
    #   collapsing to unique 'x' values
    # when using weights = x$weights
    colnames(preddf) <- paste0("quantile:", wq_probs)
    rownames(preddf) <- NULL
    if (wmean){ preddf$mean <- as.vector(pred %*% weights) }
  }else{
    if (wmean){
      preddf <- data.frame(mean = as.vector(pred %*% weights))
    }else{
      preddf <- pred
    }
  }
  
  # should result be in linear or exp scale?
  if (type == "exponential" | type == "exp") {
    preddf <- exp(preddf)
    
  } else {
    if (type == "logit") {
      preddf <- 1 / (1 + exp(-preddf))#log(preddf/(1-preddf))
    } else {
      if (type == "cloglog") {
        preddf <- 1 - exp(-exp(preddf))#log(preddf/(1-preddf))
      }
    }
  }
  
  # return prediction
  return(preddf)
}
