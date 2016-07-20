########################################
# test for homogeneity of two groups

# Riquire  Bn.R

# imput 
   # ngv=c(n1,n2), n1=number of elements in the group 1 and n2=number of elements in the group 2
   # md is a nxn matrix of distances. n=n1+n2.
   # numB = number of bootstraps (we recomend 1000<numB<10000)
# output
   # p.value based on bootstraps 

Utest = function(ngv,numB,md)
{
  ng = sum(ngv)
  comp = Bn(ngv,md)
  vaux = rep(0,ng)
  mataux = matrix(0,ng,ng)
  absoluto = rep(0,numB)
  
  for(i in 1:numB)
  {
    vaux = floor(runif(ng,1,(ng+1)))
    mataux = md[vaux,vaux]                                                    ##Completa a matriz com todas as poss?veis combina??es de valores do vetor "Vaux" por linhas
    aux = Bn(ngv,mataux)
    if(aux<=comp)
    {
      absoluto[i]=1
    }
    else
    {
      absoluto[i]=0
    }
  }
  p.value = 1 - (sum(absoluto))/numB
  p.value
}

