###############################################################
# R function to perform a Wald test of joint significance
#
# PARAMETERS:
# varcov : variance-covariance matrix
# coeff  : vector of coefficients
#
#
# NOTES:
# [1] dimension of 'varcov' should equal the length of 'coeff'
# [2] assumes that all elements of 'coeff' are equal to zero
#
###############################################################

wald <- function( varcov , coeff ) {
  
  b <- as.matrix( coeff )
  wdf <- nrow( varcov )
  
  chi2 <- t( b ) %*% solve( varcov ) %*% b
  
  pvalue <- pchisq( chi2[ 1 , 1 ] , df = wdf , lower.tail = FALSE )
  
  return_list <- list( "chi2" = chi2[ 1 , 1 ] , 
                       "df" = wdf , 
                       "p" = pvalue )
  
  return( return_list )
  
}
