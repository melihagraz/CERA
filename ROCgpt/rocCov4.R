library(cutpointr )
library(pROC)


ROC_fin <- function(data, x, y, method = c("youden_topleft", "cutoff", "maximized", "min_pvalue"),
                    index = NULL, cutoff = NULL,
                    constrain_metric = NULL, min_cons = NULL) {
  library(dplyr)                    
  # if the method is best you can specify the index = c("youden","closest.topleft")
  # cutoff = NULL #you can specify the cutoff
  # data = aSAH
  # x="s100b"
  # y="outcome"
  # index = "youden"
  
  
  # Print data, x, and y
  
  
  x1 <- data[[x]]
  y1 <- data[[y]]
  
  
  
  # Add more print statements
  
  
  roc.s100b <- roc(y1, x1, ci=TRUE,ci.alpha = ci_value, stratified = FALSE, plot = FALSE) 
  
  
  
  auc.s100b <- round(roc.s100b$auc[1],4)
  ci_L<-round(roc.s100b$ci[1],4)
  ci_U<-round(roc.s100b$ci[3],4)
  
  if (method == "youden_topleft") {
    main_res <- round(coords(roc.s100b, "best", ret=c("threshold",  "sensitivity", "specificity", 
                                                      "ppv", "npv", "accuracy", "auc"),  
                             best.method = index, transpose = FALSE), 4)
    main_res = cbind(main_res, auc = auc.s100b, ci_low = ci_L, ci_up = ci_U)
    
    main_res = list(best_res = main_res, pl = roc.s100b)
    
  } else if (method == "cutoff") {
    main_res <- round(coords(roc.s100b, cutoff, ret=c("threshold",  "sensitivity", "specificity", 
                                                      "ppv", "npv", "accuracy"),  
                             best.method = index, transpose = FALSE), 4)
    main_res = cbind(main_res, auc = auc.s100b, ci_low = ci_L, ci_up = ci_U)
    main_res = list(specific_cutoff = main_res, pl = roc.s100b)
    
    
  }else if (method == "maximized"){
    
    
    if (constrain_metric == "specificity") {
      main_res <- data.frame(cut_off = numeric(),
                             Sen = numeric(),
                             Spec = numeric(),
                             PPV = numeric(),
                             NPV = numeric(),
                             Acc = numeric(),
                             Minimum_Specificity = numeric())
      
      
      #for (ix in 1:length(min_cons)) {
      # constrain specif
      df_ns<-data.frame(Dependent= data[,y], X1 = data[,x])
      
      df_ns <- df_ns[complete.cases(df_ns), ]
      
      
      cp <- cutpointr( x = df_ns[,2], class = df_ns[,1],
                       method = maximize_metric,
                       metric = sens_constrain,
                       constrained_metric =  specificity,
                       min_constrain = min_cons)
      
      
      
      
      test<-summary(cp)
      
      cutf<-test$confusion_matrix[[1]][[1]]#cutoff
      tp<-test$confusion_matrix[[1]][[2]]
      fn<-test$confusion_matrix[[1]][[3]]
      fp<-test$confusion_matrix[[1]][[4]]
      tn<-test$confusion_matrix[[1]][[5]]
      sens<-(tp)/(tp+fn) 
      spec<-(tn)/(tn+fp) 
      PPV<-(tp)/(tp+fp)
      NPV<-(tn)/(tn+fn)
      Acc = (tp+tn)/(tp+fp+tn+fn)
      
      main_res[1,1] <- cutf
      main_res[1,2] <- sens
      main_res[1,3] <- spec
      main_res[1,4] <- PPV
      main_res[1,5] <- NPV
      main_res[1,6] <- Acc
      main_res[1,7] <- min_cons
      
      main_res_max_sens<-main_res
    }else{
      
      main_res <- data.frame(cut_off = numeric(),
                             Sen = numeric(),
                             Spec = numeric(),
                             PPV = numeric(),
                             NPV = numeric(),
                             Acc = numeric(),
                             Minimum_Sensitivity = numeric())
      
      #  for (ix in 1:length(min_cons)) {
      # constrain specif
      df_ns<-data.frame(Dependent= data[,y], X1 = data[,x])
      df_ns <- df_ns[complete.cases(df_ns), ]
      
      cp <- cutpointr(x = df_ns[,2], class = df_ns[,1],
                      method = maximize_metric,
                      metric = spec_constrain,
                      constrained_metric =  sensitivity,
                      min_constrain = min_cons)
      
      
      
      test<-summary(cp)
      cutf<-test$confusion_matrix[[1]][[1]]#cutoff
      tp<-test$confusion_matrix[[1]][[2]]
      fn<-test$confusion_matrix[[1]][[3]]
      fp<-test$confusion_matrix[[1]][[4]]
      tn<-test$confusion_matrix[[1]][[5]]
      sens<-(tp)/(tp+fn) 
      spec<-(tn)/(tn+fp) 
      PPV<-(tp)/(tp+fp)
      NPV<-(tn)/(tn+fn)
      Acc = (tp+tn)/(tp+fp+tn+fn)
      
      main_res[1,1] <- cutf
      main_res[1,2] <- sens
      main_res[1,3] <- spec
      main_res[1,4] <- PPV
      main_res[1,5] <- NPV
      main_res[1,6] <- Acc
      main_res[1,7] <- min_cons
      round(main_res,4)
      main_res_max_spec<-main_res
    }
    main_res <- if (constrain_metric == "specificity") main_res_max_sens else main_res_max_spec
    main_res = cbind(main_res, auc = auc.s100b, ci_low = ci_L, ci_up = ci_U)
    main_res = list(specific_cutoff = main_res, pl = roc.s100b)                   
  } else {
    
    
    # Example data
    Biomarker_X <- x1
    Y <- y1
    
    # Compute ROC curve
    roc.s100b <-roc(y1, x1, ci=TRUE,ci.alpha = ci_value, stratified = FALSE, plot = FALSE) 
    # roc_obj <- roc(Y, Biomarker_X)
    roc_obj<-roc.s100b
    
    # Find optimal cut-off value using minimum p-value method
    min_pvalue_cutoff <- function(roc_obj) {
      p_values <- sapply(roc_obj$thresholds, function(threshold) {
        contingency_table <- table(Y, Biomarker_X > threshold)
        if (nrow(contingency_table) < 2 || ncol(contingency_table) < 2) {
          return(NA)
        }
        fisher.test(contingency_table)$p.value
      }, simplify = "numeric")
      optimal_indices <- which(p_values == min(p_values, na.rm = TRUE))
      return(roc_obj$thresholds[optimal_indices[1]]) # Return only the first optimal cutoff value
    }
    
    optimal_cutoff <- min_pvalue_cutoff(roc_obj)
    optimal_cutoff
    
    # Check if the optimal_cutoff is Inf, indicating there was an issue
    if (is.infinite(optimal_cutoff)) {
      cat("Unable to find optimal cutoff value.\n")
    } else {
      # Calculate performance measures
      confusion_matrix <- table(Y, Biomarker_X > optimal_cutoff)
      if (nrow(confusion_matrix) < 2 || ncol(confusion_matrix) < 2) {
        cat("Confusion matrix is not complete.\n")
        
      } else {
        sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
        specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
        accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
        PPV <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
        NPV <- confusion_matrix[1, 1] / sum(confusion_matrix[, 1])
        
        #cat("Sensitivity:", sensitivity, "\nSpecificity:", specificity, "\nAccuracy:", accuracy, "\nPPV:", PPV, "\nNPV:", NPV, "\n")
        
        # Calculate AUC and its confidence intervals
        auc_obj <- auc(roc_obj)
        ci_obj <- ci.auc(roc_obj)
        #cat("AUC:", auc_obj, "\nConfidence interval:", ci_obj[1], "-", ci_obj[3], "\n")
        
        # Create a data frame to store performance measures
        main_res <- data.frame(cutoff = numeric(), sensitivity = numeric(), specificity = numeric(),
                               PPV = numeric(), NPV = numeric(), accuracy = numeric(), 
                               AUC = numeric(), AUC_lower = numeric(), AUC_upper = numeric(),
                               stringsAsFactors = FALSE)
        
        # Add performance measures and AUC to the main_res data frame
        main_res[1, "cutoff"] <- optimal_cutoff
        main_res[1, "sensitivity"] <- sensitivity
        main_res[1, "specificity"] <- specificity
        main_res[1, "PPV"] <- PPV
        main_res[1, "NPV"] <- NPV
        main_res[1, "accuracy"] <- accuracy
        main_res[1, "AUC"] <- auc_obj
        main_res[1, "AUC_lower"] <- ci_obj[1]
        main_res[1, "AUC_upper"] <- ci_obj[3]
        main_res = list(best_res = main_res, pl = roc.s100b)
      }
    }
    # Print the optimal cutoff, performance measures
    
  }
  
  return(main_res)
  
}

