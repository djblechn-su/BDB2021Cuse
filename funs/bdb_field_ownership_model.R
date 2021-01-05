# Field Ownership - Run Field Ownership Model (Source: https://github.com/burrisk/Big-Data-Bowl)

bdb_field_ownership_model <- function(e){
  # Functions Taken from Kyle Burris' Field Ownership Model https://github.com/burrisk/Big-Data-Bowl
  bounds <- c(2, 2, 9, 2 * pi)
  
  time <- function(route_pars, start_pos, end_pos, bounds, inits, gradient = T){
    decel_constant <- bounds[1]; accel_constant <- bounds[2]; max_speed <- bounds[3]
    turn_speed <- route_pars[1]; turn_radius <- route_pars[2]; total_turn <- route_pars[3];
    accel_time <- route_pars[4]
    init_speed <- inits[1]; init_angle <- inits[2]
    decel_time <- (log(init_speed) - log(turn_speed)) / decel_constant
    turn_time <- abs(total_turn) * turn_radius / turn_speed
    objective <- decel_time + turn_time + accel_time
    if (gradient){
      grad <- c(-1 / (decel_constant * turn_speed) - abs(total_turn) * turn_radius / turn_speed ^ 2,
                abs(total_turn) / turn_speed,
                sign(total_turn) * turn_radius / turn_speed,
                1)
      return(list(
        "objective" = objective,
        "gradient" = grad
      ))
    }
    return(list("objective" = objective))
  }
  
  movement_constraints <- function(route_pars, bounds, inits, gradient = F){
    # Unpack parameters
    turn_speed <- route_pars[1]; turn_radius <- route_pars[2]
    ca <- turn_speed ^ 2 / turn_radius
    
    constraints <- c(ca, -ca)
    if (gradient){
      grad <- rbind(c(2 * turn_speed / turn_radius, -(turn_speed / turn_radius) ^ 2, 0, 0),
                    c(-2 * turn_speed / turn_radius, (turn_speed / turn_radius) ^ 2, 0, 0))
      return(list("constraints" = constraints,
                  "jacobian" = grad))
    } else{
      return(list("constraints" = constraints))
    }
  }
  
  equality_function <- function(route_pars, start_pos, end_pos, bounds, inits, gradient = T){
    d <- displacement(route_pars, bounds, inits, gradient = T)
    objective <- d$objective + start_pos - end_pos
    grad <- d$gradient
    return(list("constraints" = objective, "jacobian" = grad))
  }
  
  inequality_function <- function(route_pars, start_pos, end_pos, bounds, inits, gradient = T){
    mov <- movement_constraints(route_pars, bounds, inits, gradient = T)
    return(list("constraints" = c(mov$constraints[1] - bounds[4], mov$constraints[2] - bounds[4]), "jacobian" = mov$jacobian))
  }
  
  min_time_dist <- function(distance, velocity, angle, bounds){
    start_pos <- c(10.3, 27.2)
    end_pos <- c(10.3 + distance, 27.2)
    inits <- c(velocity, angle)
    time <- min_time(start_pos, end_pos, bounds, inits)$time
    return(time)
  }
  
  min_time <- function(start_pos, end_pos, bounds, inits){
    x0 <- start_pos[1]; y0 <- start_pos[2]; x1 <- end_pos[1]; y1 <- end_pos[2]
    init_speed <- inits[1]; init_angle <- inits[2]
    diff_x <- x1 - x0; diff_y <- y1 - y0
    find_angle <- function(diff_x, diff_y, init_angle){
      distance <- sqrt(diff_x^2 + diff_y^2)
      atan2(cos(init_angle) * diff_y - diff_x * sin(init_angle),
            diff_x * cos(init_angle) + diff_y * sin(init_angle))
    }
    
    angle_with_velocity <- find_angle(diff_x, diff_y, init_angle)
    clockwise_turn <- 2 * (angle_with_velocity < 0) - 1
    route_init <- feasible_route_BFGS(start_pos, end_pos, bounds, inits, clockwise_turn)
    if (route_init$convergence == 0){
      return(list("route" = rep(NA, 4), "time" = NA, "init_fail" = T))
    }
    
    solution <- nloptr(route_init$route, eval_f = time, lb = c(0, 0, min(-2 * pi * clockwise_turn, 0), 0),
                       ub = c(inits[1], Inf, max(-2 * pi * (clockwise_turn), 0), Inf),
                       eval_g_ineq = inequality_function,
                       eval_g_eq = equality_function,
                       start_pos = start_pos, end_pos = end_pos, bounds = bounds, inits = inits,
                       gradient = T,
                       opts = list("algorithm" = "NLOPT_LD_SLSQP", "local_opts" =
                                     list("algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1e-7),
                                   "xtol_rel" = 1e-7, "maxeval" = 1000))
    
    if (sum(abs(displacement(solution$solution, bounds, inits)$objective -
                (end_pos - start_pos))) > 0.1){
      return(list("route" = rep(NA, 4), "time" = NA, "init_fail" = F))
    }
    if (sum(solution$solution == route_init$route) == 4){
      return(list("route" = rep(NA, 4), "time" = NA, "init_fail" = F))
    }
    
    return(list("route" = solution$solution, "time" = solution$objective,
                "init_fail" = F))
  }
  
  feasible_route_BFGS <- function(start_pos, end_pos, bounds, inits, clockwise_turn = 1){
    x0 <- start_pos[1]; y0 <- start_pos[2]; x1 <- end_pos[1]; y1 <- end_pos[2]
    decel_constant <- bounds[1]; accel_constant <- bounds[2]; max_speed <- bounds[3]; max_ca <- bounds[4]
    init_speed <- inits[1]; init_angle <- inits[2]
    
    turn_speed <- init_speed / 10; turn_radius <- 2 * turn_speed ^ 2 / max_ca
    f <- function(pars){
      sum((displacement(c(turn_speed, turn_radius, pars[1], pars[2]), bounds, inits)$objective -
             (end_pos - start_pos))^2)
    }
    g <- function(pars){
      2 *  matrix((displacement(c(turn_speed, turn_radius, pars[1], pars[2]), bounds, inits)$objective
                   - (end_pos - start_pos)), ncol = 2) %*%
        displacement_gradient(c(turn_speed, turn_radius, pars[1], pars[2]), bounds, inits)[, 3:4]
    }
    if (clockwise_turn != 1){
      k <- optim(c(pi / 2, 1), f, g, method = "L-BFGS-B", lower = c(0, 0), upper = c(2 * pi, Inf),
                 control = list("maxit" = 500))
    } else{
      k <- optim(c(-pi / 2, 1), f, g, method = "L-BFGS-B", lower = c(-2 * pi, 0), upper = c(0, Inf),
                 control = list("maxit" = 500))
    }
    route_pars <- c(turn_speed, turn_radius, k$par[1], k$par[2])
    convergence <- ifelse(k$convergence == 0, 1, 0)
    
    return(list("route" = route_pars, "convergence" = convergence))
  }
  
  displacement <- function(route_pars, bounds, inits, gradient = F){
    # Extract Parameters
    decel_constant <- bounds[1]; accel_constant <- bounds[2]; max_speed <- bounds[3];
    turn_speed <- route_pars[1]; turn_radius <- route_pars[2]; total_turn <- route_pars[3];
    accel_time <- route_pars[4]
    init_speed <- inits[1]; init_angle <- inits[2]
    rotation_angle <- init_angle + total_turn
    
    x_decel <- cos(init_angle) / decel_constant * (init_speed - turn_speed)
    x_turn <- turn_radius * sign(total_turn) * (sin(rotation_angle) - sin(init_angle))
    x_accel <- cos(rotation_angle) * (max_speed * accel_time + (turn_speed - max_speed) / accel_constant
                                      * (1 - exp(-accel_constant * accel_time)))
    y_decel <- sin(init_angle) / decel_constant * (init_speed - turn_speed)
    y_turn <- -turn_radius * sign(total_turn) * (cos(rotation_angle) - cos(init_angle))
    y_accel <- sin(rotation_angle) * (max_speed * accel_time + (turn_speed - max_speed) / accel_constant
                                      * (1 - exp(-accel_constant * accel_time)))
    objective <- c(x_decel + x_turn + x_accel, y_decel + y_turn + y_accel)
    
    if (gradient){
      grad <- matrix(nrow = 2, ncol = 4)
      
      grad[1, 1] <- -cos(init_angle) / decel_constant +
        cos(rotation_angle) / accel_constant * (1 - exp(-accel_constant * accel_time))
      grad[1, 2] <- sign(total_turn) * (sin(rotation_angle) - sin(init_angle))
      grad[1, 3] <- turn_radius * sign(total_turn) * cos(rotation_angle)  - sin(rotation_angle) *
        (max_speed * accel_time + (turn_speed - max_speed) / accel_constant
         * (1 - exp(-accel_constant * accel_time)))
      grad[1, 4] <- cos(rotation_angle) * ((turn_speed - max_speed) * exp(-accel_constant * accel_time)
                                           + max_speed)
      grad[2, 1] <- -sin(init_angle) / decel_constant +
        sin(rotation_angle) / accel_constant * (1 - exp(-accel_constant * accel_time))
      grad[2, 2] <-  -sign(total_turn) * (cos(rotation_angle) - cos(init_angle))
      grad[2, 3] <-  turn_radius * sign(total_turn) * sin(rotation_angle) +
        cos(rotation_angle) * (max_speed * accel_time + (turn_speed - max_speed) / accel_constant
                               * (1 - exp(-accel_constant * accel_time)))
      grad[2, 4] <- sin(rotation_angle) * ((turn_speed - max_speed) * exp(-accel_constant * accel_time)
                                           + max_speed)
      return(list("objective" = objective, "gradient" = grad))
    } else{
      return(list("objective" = objective))
    }
  }
  
  displacement_gradient <- function(route_pars, bounds, inits){
    # Extract Parameters
    decel_constant <- bounds[1]; accel_constant <- bounds[2]; max_speed <- bounds[3];
    turn_speed <- route_pars[1]; turn_radius <- route_pars[2]; total_turn <- route_pars[3];
    accel_time <- route_pars[4]
    init_speed <- inits[1]; init_angle <- inits[2]
    rotation_angle <- init_angle + total_turn
    grad <- matrix(nrow = 2, ncol = 4)
    grad[1, 1] <- -cos(init_angle) / decel_constant +
      cos(rotation_angle) / accel_constant * (1 - exp(-accel_constant * accel_time))
    grad[1, 2] <- sign(total_turn) * (sin(rotation_angle) - sin(init_angle))
    grad[1, 3] <- turn_radius * sign(total_turn) * cos(rotation_angle)  - sin(rotation_angle) *
      (max_speed * accel_time + (turn_speed - max_speed) / accel_constant
       * (1 - exp(-accel_constant * accel_time)))
    grad[1, 4] <- cos(rotation_angle) * ((turn_speed - max_speed) * exp(-accel_constant * accel_time)
                                         + max_speed)
    grad[2, 1] <- -sin(init_angle) / decel_constant +
      sin(rotation_angle) / accel_constant * (1 - exp(-accel_constant * accel_time))
    grad[2, 2] <-  -sign(total_turn) * (cos(rotation_angle) - cos(init_angle))
    grad[2, 3] <-  turn_radius * sign(total_turn) * sin(rotation_angle) +
      cos(rotation_angle) * (max_speed * accel_time + (turn_speed - max_speed) / accel_constant
                             * (1 - exp(-accel_constant * accel_time)))
    grad[2, 4] <- sin(rotation_angle) * ((turn_speed - max_speed) * exp(-accel_constant * accel_time)
                                         + max_speed)
    
    return(grad)
  }
  
  # Fit Neural Network
  N <- 15000
  x <- rtruncnorm(N, a = 0, b = Inf, mean = 20, sd = 20)
  y <- runif(N, -pi, pi)
  v <- runif(N, 0.05, bounds[3])
  z <- sapply(1:N, function(i){
    min_time_dist(x[i], v[i], y[i], bounds)
  })
  
  mat <- tibble(distance = x, speed = v, angle = y, time = z)
  
  times <- mat %>% na.omit()
  
  train <- times %>% sample_frac(0.9)
  
  val <- times %>% anti_join(train)
  
  X_train <- train[, 1:3] %>% as.matrix()
  
  X_val <- val[, 1:3] %>% as.matrix()
  
  Y_train <- train[, 4, drop = F] %>% as.matrix()
  
  Y_val <- val[,4, drop = F] %>% as.matrix()
  
  callbacks_list <- list(
    callback_reduce_lr_on_plateau(
      monitor = "val_loss",
      factor = 0.1,
      patience = 10
    ),
    callback_early_stopping(
      monitor = "val_loss",
      patience = 15
    )
  )
  
  model <- keras_model_sequential() 
  
  model %>%
    layer_dense(units = 64, input_shape = c(3)) %>%
    layer_activation('relu') %>%
    layer_dense(units = 32, input_shape = c(3)) %>%
    layer_activation('relu') %>%
    layer_dense(units = 1) %>%
    layer_activation('linear')
  
  model %>% compile(optimizer = optimizer_adam(), loss = 'mse')
  
  model %>% fit(X_train,
        Y_train,
        epochs = e,
        batch_size = 2 ^ 6,
        validation_data = list(X_val, Y_val),
        callbacks = callbacks_list)
  
  return(model)
}
