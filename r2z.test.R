r2z.test <- function(r1,r2,n1,n2) {
  z1=log(((1+r1)/(1-r1))**0.5);
  z2=log(((1+r2)/(1-r2))**0.5); 
  se=((1/(n1-3))+(1/(n2-3)))**0.5;
  t=abs(z1-z2)/se; 
  p=2*(1-pnorm(t));
  out <- c(t=t,p=p)
  return(out)
}

r2z.test(0.058  , -0.003, 35, 868)

####
rho <- -0.061; se <- 0.058
paste0(rho, ", ",round(rho-1.96*se,3), "~", round(rho+1.96*se,3))
