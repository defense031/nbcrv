data{
  int n;
  real jbpdsMLE_shape;
  real jbpdsMLE_shapeSE;
  real jbpdsMLE_scale;
  real jbpdsMLE_scaleSE;
  vector[n] jbpdsY;
}

transformed data{
  //adjust location to lognormal
  real<lower=0> jbpdsLocation;
  real<lower=0> jbpdsLocationSE;
  real<lower=0> jbpdsLocationTau;
  
  //adjust scale to lognormal
  real<lower=0> jbpdsScale ;
  real<lower=0> jbpdsScaleSE ;
  real<lower=0> jbpdsScaleTau;
  jbpdsLocation =log(jbpdsMLE_shape^2/sqrt(jbpdsMLE_shapeSE^2+jbpdsMLE_shape^2));
  jbpdsLocationSE = sqrt(log(1+jbpdsMLE_shapeSE^2/jbpdsMLE_shape^2));
  jbpdsLocationTau = (1/jbpdsLocationSE)^2;
  jbpdsScale = log(jbpdsMLE_scale^2/sqrt(jbpdsMLE_scaleSE^2+jbpdsMLE_scale^2));
  jbpdsScaleSE = sqrt(log(1+jbpdsMLE_scaleSE^2/jbpdsMLE_scale^2));
  jbpdsScaleTau = (1/jbpdsScaleSE)^2;
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
  alpha ~ lognormal(jbpdsLocation,jbpdsLocationTau);
  sigma ~ lognormal(jbpdsScale,jbpdsScaleTau);
  
  //Likelihood 
  jbpdsY ~ weibull(alpha,sigma);
}
