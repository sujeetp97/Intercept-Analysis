get_simulated_data <- function(n_rows){
  cat("\nGenerating Simulated Data:\n")
  cat("\nCreating input variables                                                     ")
  set.seed(12345567)
  x1 <- sample(x = c(100:100000000), size = n_rows, replace = T)
  x2 <- sample(x = c(100000:100000000), size = n_rows, replace = T)
  x3 <- sample(x = c(5, 20, 50, 100, 400, 1000), size = n_rows, replace = T)
  x4 <- sample(x = seq(-10, 10, 0.001), size = n_rows, replace = T)
  x5 <- sample(x = c(-100000:100000000), size = n_rows, replace = T)
  
  x1 <- sample(x = x1, size = n_rows, replace = T)
  x2 <- sample(x = x2, size = n_rows, replace = T)
  x3 <- sample(x = x3, size = n_rows, replace = T)
  x4 <- sample(x = x4, size = n_rows, replace = T)
  x5 <- sample(x = x4, size = n_rows, replace = T)
  cat("\nCreating output variable y                                                     ")
  y <- 
    1000 + 
    (x1 * 10000) +
    (x2 * 9000) + 
    ((x3 ^ 3) * (x4 * 100)) + 
    (x4 ^ 3) +
    (x5 * 9000)
  
  cat("\nAdding noise to input xs                                                     ")
  x1_w_noise <- x1 + (sample(x = c(-100:10), size = n_rows, replace = T))
  x2_w_noise <- x2 + (sample(x = c(-100:10000), size = n_rows, replace = T))
  x3_w_noise <- x3 + (sample(x = c(-20:100), size = n_rows, replace = T))
  x4_w_noise <- x4 + (sample(x = seq(0,3,0.01), size = n_rows, replace = T))
  x5_w_noise <- x5 + (sample(x = c(-10000:10000), size = n_rows, replace = T))
  
  cat("\nCreating data.table and adding extra variables                               ")
  dt <- as.data.table(data.frame(
    y,
    x1 = x1_w_noise,
    x2 = x2_w_noise,
    x3 = x3_w_noise,
    x4 = x4_w_noise,
    x5 = x5_w_noise))
  set.seed(5555555)
  dt[, x6 := sample(x = c(100:100000000), size = n_rows, replace = T)]
  dt[, x7 := sample(x = c(100000:100000000), size = n_rows, replace = T)]
  dt[, x8 := sample(x = c(5, 20, 50, 100, 400, 1000), size = n_rows, replace = T)]
  dt[, x9 := sample(x = seq(-10, 10, 0.001), size = n_rows, replace = T)]
  dt[, x10 := sample(x = c(-100000:100000000), size = n_rows, replace = T)]
  cat("\nComplete                                                                     ")
  return(dt)
  
}


show_simulated_data_variables <- function(dt){
  Sys.sleep(2)
  plot(dt$y~dt$x1, main = "y ~ x1")
  Sys.sleep(2)
  plot(dt$y~dt$x6, main = "y ~ x6")
  Sys.sleep(2)
  plot(dt$y~dt$x2, main = "y ~ x2")
  Sys.sleep(2)
  plot(dt$y~dt$x7, main = "y ~ x7")
  Sys.sleep(2)
  plot(dt$y~dt$x3, main = "y ~ x3")
  Sys.sleep(2)
  plot(dt$y~dt$x8, main = "y ~ x8")
  Sys.sleep(2)
  plot(dt$y~dt$x4, main = "y ~ x4")
  Sys.sleep(2)
  plot(dt$y~dt$x9, main = "y ~ x9")
  Sys.sleep(2)
  plot(dt$y~dt$x5, main = "y ~ x5")
  Sys.sleep(2)
  plot(dt$y~dt$x10, main = "y ~ x10")
  Sys.sleep(2)
}


get_intercept_analysis_dt <- function(dt){
  cat("\nIntercept Analysis:\n")
  cat("\nPreparing All Combinations of inputs                                                     ")
  imp_col_indices <- c(1, 2, 3, 4, 5)
  random_col_indices <- c(6, 7, 8, 9, 10)
  all_x_indices <- c(imp_col_indices, random_col_indices)
  
  x_combinations <- list()
  for(index in c(1:10)){
    combs <- combn(x = all_x_indices, m = index, simplify = FALSE)
    for(sub_index in c(1:NROW(combs))){
      x_combinations[[NROW(x_combinations) + 1]] <- combs[[sub_index]]
    }
  }
  
  cat("\n")
  intercepts <- c()
  for(combination in x_combinations){
    cat(paste("\rRegression Modeling for all input combinations - ", (NROW(intercepts)+1), " / ", NROW(x_combinations)))
    cat("                                 ")
    
    temp_dt <- dt[, c("y", paste("x", as.vector(unlist(combination)), sep = "")), with = F]
    mod_obj <- lm(y ~ ., temp_dt)  
    intercepts <- c(intercepts, coefficients(mod_obj)[[1]])
    
  }
  
  cat("\nCalculating Variable Importance scores                                                   ")
  num_imp_cols <- unlist(lapply(X = x_combinations, FUN = function (X){
    return(NROW(X[X < 6]))
  }))
  
  per_imp_cols <- unlist(lapply(X = x_combinations, FUN = function (X){
    return(NROW(X[X < 6])/NROW(X))
  }))
  
  imp_cols_effect <- num_imp_cols * per_imp_cols
  
  intercept_analysis_dt <- as.data.table(data.frame(
    intercepts,
    num_imp_cols,
    per_imp_cols,
    imp_cols_effect
  ))
  
  mod_obj <- lm(y ~ ., dt)  
  summary(mod_obj)
  
  
  coeff_estimates <- as.vector(unlist(mod_obj$coefficients[2:6]))
  std_errs <- as.vector(unlist(coef(summary(mod_obj))[,2][2:6]))
  t_values <- as.vector(unlist(coef(summary(mod_obj))[,3][2:6]))
  p_values <- as.vector(unlist(coef(summary(mod_obj))[,4][2:6]))
  
  t_values[t_values < 0.001] <- 0.001
  p_values[p_values < 0.001] <- 0.001
  
  combination_strength <- c()
  for(combination in x_combinations){
    combination <- combination[combination < 6]
    combination_strength <- c(combination_strength, 
                              sum(((combination * abs(t_values[combination]))/p_values[combination]), na.rm = T))
  }
  
  
  intercept_analysis_dt[, combination_strength := combination_strength]
  cat("\nComplete                                                                                        ")
  return(intercept_analysis_dt)
}
