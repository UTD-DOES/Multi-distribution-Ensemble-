library('Rsolnp')
library('fBasics')
root<-"C:/Users/mxs165231/Desktop/combine_prob/"
dataA<-read.csv(paste0(root,'gamma/Forecast_prob_10526_gamma.csv'))
dataB<-read.csv(paste0(root,'gaussian/Forecast_prob_10526_gaussian.csv'))
dataC<-read.csv(paste0(root,'laplace/Forecast_prob_10526_laplace.csv'))
n2=620   #n2 stands for the data used for training optimal weights
qA<-dataA[1:n2,8:ncol(dataA)]
qB<-dataB[1:n2,8:ncol(dataB)]
qC<-dataC[1:n2,8:ncol(dataC)]
x<-dataA[,6]
k<-rep(0,99)       #setup an empty vector to store pinball loss at each percentile (1-99)
p<-matrix(0,nrow=n2,ncol=2)  #build a matrix to place pinballloss for each point estimation and optimal standard deviation
w<-vector(length=1)

f<-function(w){for(i in 1:n2){
  for(j in 1:99){
    if(x[i]-w[1]*qA[i,j]-w[2]*qB[i,j]-w[3]*qC[i,j]<0){k[j]<-k[j]+(1-j/100)*(w[1]*qA[i,j]+w[2]*qB[i,j]+w[3]*qC[i,j]-x[i])}    
    else
    {k[j]<-k[j]+(j/100)*(x[i]-w[1]*qA[i,j]-w[2]*qB[i,j]-w[3]*qC[i,j])}
  }
  #formulate the objective function
}
  sum(k)
}
equal<-function(w){
  w[1]+w[2]+w[3]
}
results<-solnp(c(1/3,1/3,1/3),f,eqfun=equal,eqB=1,LB=rep(0,3),UB=rep(1,3))
optimal_par<-results$pars
optimal_value<-results$vscale

c<-optimal_par
z<-matrix(0,nrow=(nrow(dataA)-n2),ncol=99)      #build a matrix to place probabilistic forecasting results
qAnew<-dataA[(1+n2):nrow(dataA),8:ncol(dataA)]
qBnew<-dataB[(1+n2):nrow(dataA),8:ncol(dataB)]
qCnew<-dataC[(1+n2):nrow(dataA),8:ncol(dataC)]
for(i in 1:(nrow(dataA)-n2)){
  for(j in 1:99){
    z[i,j]<-c[1]*qAnew[i,j]+c[2]*qBnew[i,j]+c[3]*qCnew[i,j]
  }
}
l_new<-data.frame(cbind(dataA[(1+n2):nrow(dataA),1:7],z))  ###new probabilistic forecast
m2<-dataA[(n2+1):nrow(dataA),6]      #observation
k<-rep(0,99)  #build a vector to save pinball loss
for(i in 1:(nrow(dataA)-n2)){
  f<-{
    for(j in 1:99){
      if(m2[i]-z[i,j]<0){k[j]<-k[j]+(1-j/100)*(z[i,j]-m2[i])}    ##laplace distribution s is standard deviation
      else
      {k[j]<-k[j]+(j/100)*(m2[i]-z[i,j])}
    }
    sum(k)                               
  }}
opt_loss_combine<-f/(nrow(dataA)-n2)   #calculate pinball loss metrics              
normalized_loss_combine<-opt_loss_combine/max(dataA[,6])   #calculate normalized pinball loss                                                          #new pinball loss calculation

########single method#################
###gaussian####
k<-rep(0,99)  #build a vector to save pinball loss
for(i in 1:(nrow(dataA)-n1)){
  f<-{
    for(j in 1:99){
      if(m2[i]-qAnew[i,j]<0){k[j]<-k[j]+(1-j/100)*(qAnew[i,j]-m2[i])}    ##laplace distribution s is standard deviation
      else
      {k[j]<-k[j]+(j/100)*(m2[i]-qAnew[i,j])}
    }
    sum(k)                               
  }}
opt_loss_gaussian<-f/(nrow(dataA)-n1)   #calculate pinball loss metrics              
normalized_loss_gaussian<-opt_loss_gaussian/max(dataA[,6])   #calculate normalized pinball loss 
#####laplace#########
k<-rep(0,99)  #build a vector to save pinball loss
for(i in 1:(nrow(dataA)-n1)){
  f<-{
    for(j in 1:99){
      if(m2[i]-qCnew[i,j]<0){k[j]<-k[j]+(1-j/100)*(qCnew[i,j]-m2[i])}    ##laplace distribution s is standard deviation
      else
      {k[j]<-k[j]+(j/100)*(m2[i]-qCnew[i,j])}
    }
    sum(k)                               
  }}
opt_loss_laplace<-f/(nrow(dataA)-n1)   #calculate pinball loss metrics              
normalized_loss_laplace<-opt_loss_laplace/max(dataA[,6])   #calculate normalized pinball loss 

