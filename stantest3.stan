data{
  int n;
  real rwsMLE_shape;
  real rwsMLE_shapeSE;
  real rwsMLE_scale;
  real rwsMLE_scaleSE;
  vector[n] rwsY;
}

transformed data{
  //adjust location to lognormal
  real<lower=0> rwsLocation;
  real<lower=0> rwsLocationSE;
  real<lower=0> rwsLocationTau;
  
  //adjust scale to lognormal
  real<lower=0> rwsScale ;
  real<lower=0> rwsScaleSE ;
  real<lower=0> rwsScaleTau;
  rwsLocation =log(rwsMLE_shape^2/sqrt(rwsMLE_shapeSE^2+rwsMLE_shape^2));
  rwsLocationSE = sqrt(log(1+rwsMLE_shapeSE^2/rwsMLE_shape^2));
  rwsLocationTau = (1/rwsLocationSE)^2;
  rwsScale = log(rwsMLE_scale^2/sqrt(rwsMLE_scaleSE^2+rwsMLE_scale^2));
  rwsScaleSE = sqrt(log(1+rwsMLE_scaleSE^2/rwsMLE_scale^2));
  rwsScaleTau = (1/rwsScaleSE)^2;
}
//Parameters
parameters{
  //priors from MLE
  real<lower=0> alpha;
  real<lower=0> sigma;
}

//Model
model{
  //Priors
  alpha ~ lognormal(rwsLocation,rwsLocationTau);
  sigma ~ lognormal(rwsScale,rwsScaleTau);
  
  //Likelihood 
  rwsY ~ weibull(alpha,sigma);
}
