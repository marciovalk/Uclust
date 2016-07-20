####################################
# Test for group homogeneity 
   # applies the maximun homogeneity test for the configuration of the max padronized Bn
   # repeat the optimization algorithm n times and chooses the maximum 

# Require optimBn.R
# Require Bn.R
# Require objstdBn.R
# Require max_stdBn.R
# Require boot_sigma.R

# imput 
  # md is a nxn matrix of distances. n=n1+n2.
  # rep is the number of independent runs of the algorithm to take care of local maximum 
# output

homogeneity.max.test=function(rep=5,md){
   Fobj=vector()
   n=dim(md)[1]
   grupo1=list()
   
    for (i in 1:rep){
ans=optimBn(md)
Fobj[i]=ans$Fobj[length(ans$Fobj)]
grupo1[[i]]=ans$grupo1
}
   minFobj=min(Fobj)
   p.homo=max_stdBn(minFobj,n)
   return(list(Fobj,"Fobj",grupo1,"grupo1",p.homo,"p.homo"))
    }