# prueba de hipotesis para saber si las varianzas son iguales o no
# H0: s1=s2   vs    Ha: s1 != s2


Vp <- function(s1,s2,n,m){
  fc <- (s1/s2)^2
  vp <- pf(fc,n-1,m-1)+pf(1/fc,m-1,n-1,lower.tail = F)
  return(vp)
}
#Vp(s1,s2,n,m)
Vp(1.2,1.4,11,11)

                  #miu,s
x <- rnorm(1000000,119,1.2)
y <- rnorm(1000000,113,1.4)

var.test(x,y,alternative ="two.sided")
