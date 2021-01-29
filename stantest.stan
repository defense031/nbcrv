data{
      int n;
      real cbmsMLE_shape;
      real cbmsMLE_shapeSE;
      real cbmsMLE_scale;
      real cbmsMLE_scaleSE;
      vector[n] cbmsY;
    }
    
    transformed data{
          //adjust location to lognormal
          real<lower=0> cbmsLocation;
          real<lower=0> cbmsLocationSE;
          real<lower=0> cbmsLocationTau;
          
          //adjust scale to lognormal
          real<lower=0> cbmsScale ;
          real<lower=0> cbmsScaleSE ;
          real<lower=0> cbmsScaleTau;
          cbmsLocation =log(cbmsMLE_shape^2/sqrt(cbmsMLE_shapeSE^2+cbmsMLE_shape^2));
          cbmsLocationSE = sqrt(log(1+cbmsMLE_shapeSE^2/cbmsMLE_shape^2));
          cbmsLocationTau = (1/cbmsLocationSE)^2;
          cbmsScale = log(cbmsMLE_scale^2/sqrt(cbmsMLE_scaleSE^2+cbmsMLE_scale^2));
          cbmsScaleSE = sqrt(log(1+cbmsMLE_scaleSE^2/cbmsMLE_scale^2));
          cbmsScaleTau = (1/cbmsScaleSE)^2;
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
    alpha ~ lognormal(cbmsLocation,cbmsLocationTau);
    sigma ~ lognormal(cbmsScale,cbmsScaleTau);
    
    //Likelihood 
    cbmsY ~ weibull(alpha,sigma);
}
