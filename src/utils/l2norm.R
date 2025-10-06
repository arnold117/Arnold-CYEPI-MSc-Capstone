# src/utils/l2norm.R - L2 Normalization Utility Functions

# Define the function to perform row-wise L2 normalization
l2_norm <- function(x, epsilon = 1e-12) {
  norm_val <- sqrt(sum(x^2))

  if (any(is.na(x))) {
    return(rep(NA, length(x)))
  }

  # If vector is near zero, return NA
  if (norm_val < epsilon) {
    return(rep(NA, length(x)))
  }

  return(x / norm_val)
}

# Function to calculate intensity (L2 norm) for each row
calc_intensity <- function(x) {
  sqrt(sum(x^2))
}

# Example usage:
# # Apply normalization row-wise
# df_normed <- t(apply(df, 1, l2_norm))
#
# # Calculate intensity for each row
# intensity <- apply(df, 1, calc_intensity)