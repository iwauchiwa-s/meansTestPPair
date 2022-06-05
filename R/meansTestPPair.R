#' @title Significance test for difference between 2 mean values of pair groups
#' @description \code{meansTestPPair} statistical test for mean values
#'
#' @importFrom stats pt
#' @importFrom stats qt
#' @importFrom stats dnorm
#' @importFrom graphics curve
#' @importFrom graphics legend
#' @param nd1 number of group
#' @param av1 mean value of group 1
#' @param sd1 standard deviation of group 1
#' @param av2 mean value of group 2
#' @param sd2 standard deviation of group 2
#' @param sd3 standard deviation of pair differences
#' @return Deviation Test
#' @export
#' @examples
#' # meansTestPPair(25, 50, 10, 60, 15, 10)

meansTestPPair <- function(nd1, av1, sd1, av2, sd2, sd3){
  dav <- abs(av1-av2)
  var1 <- sd1^2
  var2 <- sd2^2
  var3 <- sd3^2
  sdpool <- sqrt ( ( var1 + var2) / 2 )
  cohen_d <- dav/sd3
  dof <- nd1-1
  t <- (abs(av1-av2))/sqrt(var3/nd1)
  pv <- pt(-t,df=dof)*2
  cl_l <- (av2-av1)-qt(0.975,dof)*sqrt(var3/nd1)
  cl_u <- (av2-av1)+qt(0.975,dof)*sqrt(var3/nd1)
  if (pv <= 0.05){
    txj <- 1 # significant
  }
  else{
    txj <- 0 # insignificant
  }

  # x-axix settings
  xmn <- min(c(av1,av2))-max(c(sd1,sd2))*4
  xmx <- max(c(av1,av2))+max(c(sd1,sd2))*4

  n <- 1000
  x1 <- seq(xmn, xmx, length=n)
  mx1 <- max( dnorm(x1,av1,sd1) )
  mx2 <- max( dnorm(x1,av2,sd2) )
  mx <- max(mx1,mx2) * 1.1


  # draw the normal distributions
  curve(dnorm(x,av1,sd1),xmn,xmx,col = "blue",lwd=1,xlab="", ylab="", ylim=c(0,mx))
  curve(dnorm(x,av2,sd2),xmn,xmx,add = TRUE, col = "red",lwd=1)
  legend("topleft",
         legend=c("1", "2"),
         lty=c(1,1),
         col=c("blue", "red")
  )
  return(list(Deviation=dav, Cohens_d=cohen_d, t_value=t, P_value=pv, lower_lim=cl_l, upper_lim=cl_u, judge=txj))
}
