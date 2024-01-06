## 1
# Create a matrix
set.seed(2022)
A = matrix(sample(0:3, 9, replace = TRUE), nrow = 3)

# Declare the function
my_colSums = function(A) {
  # Pre-allocate solution
  res = numeric(ncol(A))
  
  # For the sake of exercise, use a for loop to calculate the sum of columns
  # For larger matrices, apply is faster
  for (i in 1:ncol(A)) {
    res[i] = sum(A[, i])
  }
  
  # Return results
  return(res)
}

# Call function
my_colSums(A)

## 2
# Declare function
my_isSymmetric = function(A) {
  # Function returns TRUE if matrix is symmetric and FALSE otherwise
  # 1. Check if square
  if (!(ncol(A) == nrow(A))) {
    return(FALSE)
    # 2. Check if symmetric (transpose matrix is identical)
  } else if (!(identical(A, t(A)))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Create symmetric matrix
As = matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1), ncol = 3)

# Call function
my_isSymmetric(A)
my_isSymmetric(As)

## 3
# Declare function
my_recurmatpow = function(A, k) {
  # This is a recursive implementation that only runs log_2(k) matrix multiplications
  # First declare fixed results
  if (k == 1) {
    # A^1 = A
    return(A)
  } else if (k == 2) {
    # A^2 = A * A
    return(A %*% A)
  } else if (k == 0) {
    # Just for consistency. This case will never occur in recursive calls,
    # only if the function is called with the initial argument k = 0
    # A^0 = I
    diag(ncol(A))
  }
  
  # Calculate all other results for k > 2 recursively, make sure for compatibility with odd k
  # In case k is even
  # A^k = (A^(k/2)) * (A^(k/2))
  # Odd
  # A^k = (A^(k %/% 2)) * (A^((k %/% 2) + 1))
  return(my_recurmatpow(A, floor(k/2)) %*% my_recurmatpow(A, ceiling(k/2)))
}

# Call function
my_recurmatpow(A, 5)
