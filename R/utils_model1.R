library(matlib)

sum_neighbors <- function(v_list,relations, t, agent, v0_value) {
  
  t0 <- t - 1
  neighbors <- which(relations[agent,] < t0)
  
  neighbors_lags <- relations[agent, neighbors]
  lags <- unique(neighbors_lags)
  result_x <- 0
  result_y <- 0
  for (i in lags) {
    t1 <- t0 - i
    result_x <- result_x + sum(v_list[[t1]]$x_normal[which(relations[agent,] == i)])
    result_y <- result_y + sum(v_list[[t1]]$y_normal[which(relations[agent,] == i)])
  }
  result_x <- result_x / length(neighbors)
  result_y <- result_y / length(neighbors)
  phi <- atan2(result_y, result_x) *180 /pi
  noise <- etha * floor(runif(1, 0, 360))
  new_phi <- phi + noise
  v_value <- v_size(result_x, result_y)
  vx_result <-  cos((pi * new_phi)/180) * v0_value
  vy_result <- sin((pi * new_phi)/180) * v0_value
  result <- list(v_x = result_x, v_y = result_y, size = v_value, x_normal = vx_result, y_normal = vy_result)
  return(result)
}


sum_neighbors_intrinsic <- function(v_list,relations, t, agent, v0_value) {
  
  t0 <- t - 1
  neighbors <- which(relations[agent,] < t0)
  
  neighbors_lags <- relations[agent, neighbors]
  lags <- unique(neighbors_lags)
  result_x <- 0
  result_y <- 0
  for (i in lags) {
    t1 <- t0 - i
    result_x <- result_x + (sum(v_list[[t1]]$x_normal[which(relations[agent,] == i)]) + ((etha * runif(1, 0, 1)) / (length(which(relations[agent,] == i)))))
    result_y <- result_y + (sum(v_list[[t1]]$y_normal[which(relations[agent,] == i)]) + ((etha * runif(1, 0, 1)) / (length(which(relations[agent,] == i)))))
  }
  result_x <- result_x / length(neighbors)
  result_y <- result_y / length(neighbors)
  phi <- atan2(result_y, result_x) *180 /pi
  # noise <- etha * floor(runif(1, 0, 360))
  # new_phi <- phi + noise
  v_value <- v_size(result_x, result_y)
  vx_result <-  cos((pi * phi)/180) * v0_value
  vy_result <- sin((pi * phi)/180) * v0_value
  result <- list(v_x = result_x, v_y = result_y, size = v_value, x_normal = vx_result, y_normal = vy_result)
  return(result)
}


v_size <- function(vx, vy) {
  result <- sqrt(vx^2 + vy^2)
  return(result)
}


order_parameter <- function(v_list) {
  
  v_total <- (sqrt((sum(v_list$x_normal))^2 + (sum(v_list$y_normal))^2))/length(v_list$x_normal)
  return(v_total)
}





