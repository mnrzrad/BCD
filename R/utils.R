normalize_constant_BBCD <- function(n1, n2, p1, p2, lambda) {
  x_vals <- 0:n1
  y_vals <- 0:n2
  log_x_terms <- dbinom(x_vals, n1, p1, log = TRUE)
  log_y_terms <- dbinom(y_vals, n2, p2, log = TRUE)
  log_x_matrix <- matrix(rep(log_x_terms, each = length(y_vals)),
                         nrow = length(y_vals), ncol = length(x_vals))
  log_y_matrix <- matrix(rep(log_y_terms, times = length(x_vals)),
                         nrow = length(y_vals), ncol = length(x_vals))
  xy_matrix <- outer(y_vals, x_vals, function(y, x) x * y)
  log_lambda_matrix <- xy_matrix * log(lambda)
  log_total_matrix <- log_x_matrix + log_y_matrix + log_lambda_matrix
  max_log <- max(log_total_matrix)
  log_sum <- max_log + log(sum(exp(log_total_matrix - max_log)))
  return(exp(-log_sum))
}

normalize_constant_BGCD <- function(q1, q2, q3, max_iter = 100, tol = 1e-12) {
  if (any(c(q1, q2, q3) <= 0) || any(c(q1, q2, q3) >= 1)) return(Inf)

  x <- 0:max_iter
  y <- 0:max_iter

  mat <- outer(x, y, function(x, y) {
    log_term <- x * log(q1) + y * log(q2) + (x * y) * log(q3)
    exp(log_term)
  })

  total <- sum(mat)
  if (!is.finite(total) || total < tol) return(Inf)

  return(1 / total)
}

normalize_constant_BPCD <- function(lam1,lam2,lam3, max_iter = 10000) {
  norm_const <- 0
  for (y in 0:max_iter) {
      term <- ((lam2^y)/factorial(y)) * exp(lam1*(lam3^y))
      if (term < .Machine$double.eps)
        break
      norm_const <- norm_const + term
    }
  return(1 / norm_const)
}

create_observed_table <- function(data, min_x = NULL, min_y = NULL) {
  data <- as.data.frame(data)
  X <- data[,1]
  Y <- data[,2]
  if (is.null(min_x)) min_x <- min(X)
  if (is.null(min_y)) min_y <- min(Y)
  x_range <- max(X) - min_x + 1
  y_range <- max(Y) - min_y + 1
  obs_table <- matrix(0, nrow = x_range, ncol = y_range)
  for (i in 1:length(X)) {
    x_idx <- X[i] - min_x + 1
    y_idx <- Y[i] - min_y + 1
    obs_table[x_idx, y_idx] <- obs_table[x_idx, y_idx] + 1
  }
  rownames(obs_table) <- min_x:max(X)
  colnames(obs_table) <- min_y:max(Y)
  return(obs_table)
}

create_expected_table <- function(data = NULL,
                                  distribution,
                                  params,
                                  min_x = NULL, max_x = NULL,
                                  min_y = NULL, max_y = NULL) {
  data <- as.data.frame(data)
  if (!is.null(data) && (is.null(min_x) || is.null(max_x) || is.null(min_y) || is.null(max_y))) {
    X <- data[,1]
    Y <- data[,2]
    if (is.null(min_x)) min_x <- min(X)
    if (is.null(max_x)) max_x <- max(X)
    if (is.null(min_y)) min_y <- min(Y)
    if (is.null(max_y)) max_y <- max(Y)
  }
  sample_size <- length(X)
  x_range <- max_x - min_x + 1
  y_range <- max_y - min_y + 1
  exp_table <- matrix(0, nrow = x_range, ncol = y_range)
  for (i in 1:x_range) {
    for (j in 1:y_range) {
      x_val <- min_x + i - 1
      y_val <- min_y + j - 1
      if (distribution == "BBCD") {
        prob <- dbinomBCD(x_val, y_val, params$n1, params$n2, params$p1, params$p2, params$lambda)
      } else if (distribution == "BGCD") {
        prob <- dgeomBCD(x_val, y_val, params$q1, params$q2, params$q3)
      } else if (distribution == "BPCD") {
        prob <- dpoisBCD(x_val, y_val, params$lambda1, params$lambda2, params$lambda3)
      } else {
        stop("Unknown distribution type. Use 'BBCD', 'BGCD', or 'BPCD'")
      }
      exp_table[i, j] <- prob * sample_size
    }
  }
  rownames(exp_table) <- min_x:max_x
  colnames(exp_table) <- min_y:max_y
  return(exp_table)
}

freeman_tukey_test <- function(observed, expected, distribution, num_params) {
  obs_flat <- as.vector(observed)
  exp_flat <- as.vector(expected)
  T_squared <- 4 * sum((sqrt(obs_flat) - sqrt(exp_flat))^2)
  df <- (nrow(observed) * ncol(observed)) - num_params - 1
  p_value <- 1 - stats::pchisq(T_squared, df)
  return(list(statistic = T_squared,
              df = df,
              p_value = p_value))
}
