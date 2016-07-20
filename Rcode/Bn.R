################################
# The test statistic Bn
# a mesure of distances between and within groups

# imput 
  # ngv=c(n1,n2), n1=number of elements in the group 1 and n2=number of elements in the group 2
  # md is a nxn matrix of distances. n=n1+n2.
# Output
  # a value for the statistic Bn
Bn = function(ngv,md)     
  {
  ng=sum(ngv)
  maux1=matrix(0, nrow=ng, ncol=ng)
  for (i in 1:ngv[1])
  {
    for (j in (ngv[1]+1):ng)                                                    ##Dist?ncia entre os grupos
    {
      maux1[i,j] = md[i,j]
    }
  }
  
  maux2=matrix(0, ngv[1], ngv[1])
  for (i in 1:(ngv[1]-1))
  {
    for (j in (i+1):ngv[1])
    {
      maux2[i,j] = md[i,j]                                                        ##Dist?ncia dentro do grupo 1
    }
  }
  
  maux3=matrix(0, ng, ng)
  for (i in (ngv[1]+1):(ng-1))
  {
    for (j in (i+1):ng)
    {
      maux3[i,j] = md[i,j]                                                        ##Dist?ncia dentro do grupo 2
    }
  }
  
  a1 = (1/(ngv[1]*ngv[2]))*sum(maux1)
  a2 = (2/(ngv[1]*(ngv[1]-1)))*sum(maux2)
  a3 = (2/(ngv[2]*(ngv[2]-1)))*sum(maux3)
  Bn = (ngv[1]*ngv[2]/(ng*(ng-1)))*(2*a1-a2-a3)
  Bn
}
