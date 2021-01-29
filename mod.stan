
    data{
      int n;
      real cbmsMLE_shape;
      real cbmsMLE_shapeSE;
      real cbmsMLE_scale;
      real cbmsMLE_scaleSE;
      vector[n] cbmsY;
    }
    
    //Parameters
    parameters{
      //priors from MLE
        real alpha;
        real sigma;
    }
    
    transformed parameters{
          tau=(1/sigma)^2;
          //adjust location to lognormal
          cbmsLocation<-log(cbmsMLE_shape^2/sqrt(cbmsMLE_shapeSE^2+cbmsMLE_shape^2));
          cbmsLocationSE<-sqrt(log(1+cbmsMLE_shapeSE^2/cbmsMLE_shape^2));
          
          //adjust scale to lognormal
          cbmsScale<-log(cbmsMLE_scale^2/sqrt(cbmsMLE_scaleSE^2+cbmsMLE_scale^2));
          cbmsScaleSE<-sqrt(log(1+cbmsMLE_scaleSE^2/cbmsMLE_scale^2));
    }
    
  //Model
    model{
    //Priors
    alpha ~ lognormal(cbmsLocation,cbmsLocationSE);
    sigma ~ lognormal(cbmsMLE_scale,cbmsMLE_scaleSE);
    
    //Likelihood 
    cbmsY ~ weibull(alpha,tau);
    }
  generated quantities{
    }
