############################################
# Test for group homogeneity 
  # If the smallest pvalue is greater than (\alpha ) the group is considered homogeneous

# Require Utest.R
# Require Bn.R

# imput 
   # md is a nxn matrix of distances. n=n1+n2.
   # numB = number of bootstraps (we recomend 1000<numB<10000)
# output
   # the smallest Utest's pvalue of all (2^(n-1)-n-1) two-subgroups arrengement  


homogeneity.test=function(md,numB){
  n=dim(md)[2]
  pv=matrix(0,ncol=(n+2),nrow=1)
  for (sn1 in 2:floor(n/2)){
    comb=combn(n,sn1) 
    for (nts in 1:dim(comb)[2]){
      g1=comb[,nts]
      g2=c(1:n)[-g1]
      ngv=c(length(g1),length(g2))
      mdnew=md[c(g1,g2),c(g1,g2)]
      ptest=Utest(ngv,numB,mdnew)
      pv=rbind(pv,c(ptest,g1,g2,length(g1))) 
    }
  }
  pv=pv[-1,]
  p=pv[order(pv[,1]),]
  return(p[1,1])
}
#Example 1 
   # x=matrix(c(rnorm(25,0),rnorm(30,10)),nrow=5)
   ## Not Run. Here n=11 and the group is not homogeneous
   # md=as.matrix(dist(t(x)))
   # homogeneity.test(md,1000)

#Example 2 
# x=matrix(c(rnorm(25,0),rnorm(30,0)),nrow=5)
## Not Run. Here n=11 and the group is  homogeneous
# md=as.matrix(dist(t(x)))
# homogeneity.test(md,1000)