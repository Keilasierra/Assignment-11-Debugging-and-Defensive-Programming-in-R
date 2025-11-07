# Assignment-11-Debugging-and-Defensive-Programming-in-R
https://keilasierra4.wordpress.com/2025/11/07/assignment-11-debugging-and-defensive-programming-in-r/

# Assignment #11: Debugging and Defensive Programming in R

# Step 1: Reproduce the Error #
set.seed(123)
test_mat <- matrix(rnorm(50), nrow = 10)
tukey_multiple(test_mat)

# Step 2: Define tukey.outlier() helper ###
tukey.outlier <- function(v, k = 1.5) {
  stopifnot(is.numeric(v))
  qs <- stats::quantile(v, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  iqr <- qs[2] - qs[1]
  lower <- qs[1] - k * iqr
  upper <- qs[2] + k * iqr
  (v < lower) | (v > upper)
}

# Step 3: Diagnose the Bug #
# The problem was that I used "&&" instead of "&".
# In R, "&&" only checks the first element, so it gives one TRUE or FALSE
# instead of checking each value in the column. That’s why the function didn’t
# work right. To fix it, I need to use "&" because it compares every element
# one by one (element-wise) instead of just the first one.

# Step 4: Fix the Code ###
tukey_multiple <- function(x) {
  outliers <- array(TRUE, dim = dim(x))
  for (j in 1:ncol(x)) {
    outliers[, j] <- outliers[, j] & tukey.outlier(x[, j])  
  }
  outlier.vec <- vector("logical", length = nrow(x))
  for (i in 1:nrow(x)) {
    outlier.vec[i] <- all(outliers[i, ])
  }
  return(outlier.vec)
}

# Validate the fix
set.seed(123)
test_mat <- matrix(rnorm(50), nrow = 10)
tukey_multiple(test_mat)

#Step 5: Defensive Enhancements 
corrected_tukey_safe <- function(x, k = 1.5) {
  if (is.data.frame(x)) {
    if (!all(vapply(x, is.numeric, logical(1)))) {
      stop("All columns must be numeric for Tukey outlier detection.")
    }
    x <- as.matrix(x)
  }
  if (!is.matrix(x) || !is.numeric(x)) {
    stop("`x` must be a numeric matrix (or a data frame with only numeric columns).")
  }
  outliers <- array(TRUE, dim = dim(x))
  for (j in seq_len(ncol(x))) {
    outliers[, j] <- outliers[, j] & tukey.outlier(x[, j], k = k)
  }
  unname(apply(outliers, 1L, all))
}

