#return  p-value for max padronized Bn 
max_stdBn=function(Fobj,n){
# test for max Bn (Fobj)
  1-exp(  (2^(n-1)-n-1)*pnorm(-Fobj,log.p=TRUE) )
}