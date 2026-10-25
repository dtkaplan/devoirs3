#' Convert a character string to a hash value
#'
#' @param s A character string to hash
#' @return A numeric hash value
rolling_hash <- function(s) {
  p <- 31
  m <- 1e9 + 7
  
  hash_value <- 0
  p_pow <- 1
  
  # Split the string into individual characters
  chars <- strsplit(s, "")[[1]]
  
  for (char in chars) {
    # Convert character to its ASCII code
    # Adjusting so 'a' = 1, 'b' = 2, etc.
    char_val <- utf8ToInt(char) - utf8ToInt('a') + 1
    
    # Calculate hash and keep it within modulo bounds
    hash_value <- (hash_value + char_val * p_pow) %% m
    p_pow <- (p_pow * p) %% m
  }
  
  return(hash_value)
}