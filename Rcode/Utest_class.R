########################################
# test for classification of new element X in one of two groups
   # H0: The element X is not well classified 
   # H1: The element X is well classified 

# Riquire  Bn.R

# imput 
  # ngv=c(n1,n2), n1=number of elements in the group 1 and n2=number of elements in the group 2
  # md is a (n+1 x n+1) matrix of distances.  n = n1+n2.
      # If n1 is the size of group 1, n2 is the size of group 2
      # and X is the element to be classified, md must be calculated
      # putting the data in order that the n1 elents of group 1 go first
      # than X  and than the n2 elements of group 2.
  # numB = number of bootstraps (we recomend 1000<numB<10000)
# output
  # p.value based on bootstraps  
Uteste_class= function(ngv,numB,md)
{   ng1 = ngv[1]
    ng2 = ngv[2]
  if (dim(md)[1]!=(ng1+ng2)){
    print("md must be (n+1 x n+1) matrix. 
           If n1 is the size of group 1, n2 is the size of group 2
           and X is the element to be classified, md must be calculated
           putting the data in order that the n1 elents of group 1 go first
           than X  and than n2 elements of group 2")
  }

  B = rep(0,numB)
  B1_0=Bn(c(ng1+1,ng2),md)
  B2_0=Bn(c(ng1,1+ng2),md)
  D = B1_0-B2_0
  for(i in 1:numB)
  {
    vaux1 = floor(runif(ng1,1,ng1+1))
    vaux2 = floor(runif(ng2,ng1+1,ng2+ng1+2))
    vaux3 = floor(runif(1,ng1+1,ng2+ng1+2))
    mataux = md[c(vaux1,vaux3,vaux2),c(vaux1,vaux3,vaux2)]   
    ngv1=c(ng1+1,ng2)
    B1=Bn(ngv1,mataux)
    ngv2=c(ng1,ng2+1)
    B2=Bn(ngv2,mataux)
    B[i]=B1-B2
  }
  mean(B>D)
}
