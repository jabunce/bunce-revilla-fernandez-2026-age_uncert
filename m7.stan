

functions {
  // McElreath 2020, pg 512
  vector merge_missing( array[] int miss_indexes , vector x_obs , vector x_miss ) {
    int N = dims(x_obs)[1];             // total number of rows of observations and missing values
    int N_miss = dims(x_miss)[1];       // number of rows of missing observations
    vector[N] merged;                   // vector of merged data and variables to estimate
    merged = x_obs;

    for ( i in 1:N_miss )
      merged[ miss_indexes[i] ] = x_miss[i]; // for each element of "merged" with an NA, replace with an element of the vector of variables to estimate "x_miss"

    return merged;
  } //merge_missing


  real comp_mod_h( vector muQ, vector muK, vector muH, vector muI, real t ) {

    real mu; // height at time t

    if ( t <= muI[1] ) {
      mu = 0.012 + ( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1]);
    } else if (t < muI[2]) {
      mu = 0.012 + ( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1]) +
                   ( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2]);
    } else if (t <= muI[3]) {
      mu = 0.012 + ( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1]) +
                   ( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2]) +
                   ( 2*muH[3]/muK[3] * ( 1 - exp(muK[3]*muQ[3]*( muI[2] - t )/( 1 + 2*muQ[3] )) ) )^(1/muQ[3]);
    } else if (t <= muI[4]) {
      mu = 0.012 + ( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1]) +
                   ( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2]) +
                   ( 2*muH[3]/muK[3] * ( 1 - exp(muK[3]*muQ[3]*( muI[2] - t )/( 1 + 2*muQ[3] )) ) )^(1/muQ[3]) +
                   ( 2*muH[4]/muK[4] * ( 1 - exp(muK[4]*muQ[4]*( muI[3] - t )/( 1 + 2*muQ[4] )) ) )^(1/muQ[4]);
    } else {
      mu = 0.012 + ( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1]) +
                   ( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2]) +
                   ( 2*muH[3]/muK[3] * ( 1 - exp(muK[3]*muQ[3]*( muI[2] - t )/( 1 + 2*muQ[3] )) ) )^(1/muQ[3]) +
                   ( 2*muH[4]/muK[4] * ( 1 - exp(muK[4]*muQ[4]*( muI[3] - t )/( 1 + 2*muQ[4] )) ) )^(1/muQ[4]) +
                   ( 2*muH[5]/muK[5] * ( 1 - exp(muK[5]*muQ[5]*( muI[4] - t )/( 1 + 2*muQ[5] )) ) )^(1/muQ[5]);
    }; //else


    return mu;

  } //comp_mod_h



  real comp_mod_w( vector muQ, vector muK, vector muH, vector muI, real t) {

    real mu;      // weight at time t

    if ( t <= muI[1] ) {
      mu = 1.02e-6 + pi()*( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1] + 2);
    } else if (t <= muI[2]) {
      mu = 1.02e-6 + pi()*( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1] + 2) +
                    pi()*( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2] + 2);
    } else if (t <= muI[3]) {
      mu = 1.02e-6 + pi()*( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1] + 2) +
                    pi()*( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2] + 2) +
                    pi()*( 2*muH[3]/muK[3] * ( 1 - exp(muK[3]*muQ[3]*( muI[2] - t )/( 1 + 2*muQ[3] )) ) )^(1/muQ[3] + 2);
    } else if (t <= muI[4]) {
      mu = 1.02e-6 + pi()*( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1] + 2) +
                    pi()*( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2] + 2) +
                    pi()*( 2*muH[3]/muK[3] * ( 1 - exp(muK[3]*muQ[3]*( muI[2] - t )/( 1 + 2*muQ[3] )) ) )^(1/muQ[3] + 2) +
                    pi()*( 2*muH[4]/muK[4] * ( 1 - exp(muK[4]*muQ[4]*( muI[3] - t )/( 1 + 2*muQ[4] )) ) )^(1/muQ[4] + 2);    
    } else {
      mu = 1.02e-6 + pi()*( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1] + 2) +
                    pi()*( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2] + 2) +
                    pi()*( 2*muH[3]/muK[3] * ( 1 - exp(muK[3]*muQ[3]*( muI[2] - t )/( 1 + 2*muQ[3] )) ) )^(1/muQ[3] + 2) +
                    pi()*( 2*muH[4]/muK[4] * ( 1 - exp(muK[4]*muQ[4]*( muI[3] - t )/( 1 + 2*muQ[4] )) ) )^(1/muQ[4] + 2) +
                    pi()*( 2*muH[5]/muK[5] * ( 1 - exp(muK[5]*muQ[5]*( muI[4] - t )/( 1 + 2*muQ[5] )) ) )^(1/muQ[5] + 2);
    }; //else


    return mu;

  } //comp_mod_w

}



data {
  int<lower=0> N;                           // number of observations
  int<lower=0> J;                           // number of people measured
  int<lower=0> E;                           // number of ethnic groups
  array[N] int<lower=0> ID;                 // vector of person IDs for each measurement
  vector<lower=0>[N] age;                   // vector of ages for each observation (in years since conception)
  vector<lower=0>[N] height;                // vector of observations (in cm)
  vector<lower=0>[N] weight;                // vector of observations (in g), 1e10 indicates NA
  array[N] int<lower=1,upper=E+1> ethnicity;  // vector of ethnicities for each observation: 1=Ber, 2=Mat
  array[J] int<lower=1,upper=E+1> ethIndiv;   // vector of ethnicities for each individual j: 1=Ber, 2=Mat

  vector[20] meansL;                  // means from PriorPredictIndivStdev

  vector<lower=0>[20] gexpL;          // rate (=1/mean) for exponential hyperprior on group-level offset stdev (from PriorPredictIndivStdev)
  vector<lower=0>[20] iexpL;          // mean and stdev for hyperprior on indiv-level offset stdev
  vector<lower=0>[20] ihsdL;

  real<lower=0> CholEtaG;             // eta parameter for lkj_corr_cholesky (from PriorPredictIndivStdev)
  real<lower=0> CholEtaI;

  real<lower=0> muLsigma_h;           // mean measurement error stdev on log scale
  real<lower=0> muLsigma_w;

  vector<lower=0,upper=2025>[N] obsyear;            // year for each observation
  vector<lower=0,upper=10>[J] sdID;                 // stdev of Normal around recorded conception year for each individual = (recorded age)*0.1 
  vector<lower=0,upper=2025>[J] recConcepYear;      // recorded year of conception for each individual
  vector<lower=-1,upper=1>[N] firstObs;             // code if each observation is first observation: 1=first real observation, 0=not first observation, -1=artificial conception observation

  int<lower=0> w_num_miss;              // number of missing weights
  array[w_num_miss] int w_missidx;      // vector of indices of missing weights
}

transformed data {
  vector[20] zeros = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]';         // column vector of 0s, note transpose at the end to turn into row vectors
  vector[20] ones =  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]';         // column vector of 1s
  vector[N] log_height;                            
  vector[N] log_weight;

  log_height = log(height);
  log_weight = log(weight);
}

parameters {
  array[E] vector[20] zGrp;                         // ethnic group offset z-scores to each parameter: E-array of column 15-vectors, one vector for each ethnicity
  vector<lower=0>[20] offsetGroupStdevs;            // stdevs for group-mean offsets to the overall mean (across groups) trajectory for the group, for each of the 20 parameters
  cholesky_factor_corr[20] L_G;                     // cholesky factor 20x20 matrix, for correlation matrix for mean offsets

  array[J] vector[20] zInd;                         // individual offset z-scores to each parameter: J-array of column 20-vectors, one vector for each individual
  array[E] vector<lower=0>[20] offsetIndivStdevs;   // stdevs for mean offsets to the mean group trajectory for each individual, for each of the 20 parameters: E-array of 20-vectors, one vector for each ethnic group
  array[E] cholesky_factor_corr[20] L_I;            // E-array of cholesky factor 20x20 matrices, for correlation matrix among mean (across individuals within ethnic groups) parameter offsets

  real<lower=0,upper=0.005> Lsigma_h;        // variance of the logged normal distribution, upper limit of 0.005 yields 95% of observations within 2*stdev = 1cm of actual height at 100cm, within 0.5kg of actual weight at 50kg
  real<lower=0> Lsigma_w;        // 0.5  0.13

  vector<lower=1900,upper=2025>[J] concepyear;           // estimated year of conception for each individual

  vector[w_num_miss] log_weight_impute;        // vector of variables representing missing log weights to impute
}

transformed parameters {
  
  matrix[E,20] GrpOffset;            // matrix of random offsets for each ethnic group to each of the 20 paramters, rows = groups, cols = parameters
  matrix[J,20] IndOffset;            // matrix of random offsets for each individual to each of the 20 paramters, rows = indivs, cols = parameters
                                     // Stan manual pg 150-151, Rethinking pg 409
  vector<lower=0,upper=200>[N] EstAge;   // estimated age for each observation


  for (e in 1:E) {
    GrpOffset[e] = (diag_pre_multiply(offsetGroupStdevs, L_G) * zGrp[e])';                           //create cholesky factor for cov matrix and multiply by vector of offset z-scores, then transpose. This makes an 20-vector for each ethnic group e
  } // for e


  for (j in 1:J) {
    IndOffset[j] = (diag_pre_multiply(offsetIndivStdevs[ethIndiv[j]], L_I[ethIndiv[j]]) * zInd[j])';  //create cholesky factor for group-specific cov matrix and multiply by vector of offset z-scores, then transpose. This makes a 20-vector for each individual j
  } // for j


  for (n in 1:N) {
    if ( firstObs[n] != -1 && ethnicity[n] == 2) {    // if a real observation and MAtsigenka
      EstAge[n] = obsyear[n] - concepyear[ID[n]];
      if ( EstAge[n] < 0) {                           // if sampled conception estimate > moment of observation, make age at that observation slightly greater than 0 (conception)  
        EstAge[n] = 0.0001;
      } // if
    } else {                                          // if an artificial observation at conception and/or Berkeley make age at that observation 0 
      EstAge[n] = 0;
    } // else
  } // for n

}


model {

  vector[N] mu_h;                      // mean of un-logged normal distribution
  vector[N] mu_w;

  array[J] vector[5] Q;                // temporary holders for linear sub-model for each parameter
  array[J] vector[5] K;
  array[J] vector[5] H;
  array[J] vector[5] I;

  vector[N] log_weight_merge;          // vector merging log weights and log_weight_impute variables 

  Lsigma_h ~ exponential(1/muLsigma_h);       // where parameter of exponential = lambda = 1/mean.
  Lsigma_w ~ exponential(1/muLsigma_w);

  zGrp ~ multi_normal(zeros, diag_matrix(ones));      // E-array of group offset z-scores, sample 20 at a time from a normal
  zInd ~ multi_normal(zeros, diag_matrix(ones));      // J-array of individual offset z-scores

  offsetGroupStdevs ~ normal(gexpL,0.001); //0.00001   exponential(1/gexpL);           // hyper-priors for stdevs of group offsets to parameters, on log scale, mean of exponential(beta) = 1/beta
  L_G ~ lkj_corr_cholesky(CholEtaG);                  // cholesky factor 20x20 matrix, lower the eta to allow more extreme correlations, Rethinking pg 394

  for (eth in 1:E) {
    offsetIndivStdevs[eth] ~ normal(iexpL,ihsdL);           // E-array of hyper-priors for stdevs of individual offsets to parameters
    L_I[eth] ~ lkj_corr_cholesky(CholEtaI);                 // E-array of cholesky factor 20x20 matrices
  } // for e


  concepyear ~ normal(recConcepYear, sdID);


  for (n in 1:N) {

    for (z in 1:5) {
      Q[ID[n],z] = exp( meansL[z+0] + GrpOffset[ethnicity[n],z+0] + IndOffset[ID[n],z+0] );
      K[ID[n],z] = exp( meansL[z+5] + GrpOffset[ethnicity[n],z+5] + IndOffset[ID[n],z+5] );
      H[ID[n],z] = K[ID[n],z]/2 + meansL[z+10] + GrpOffset[ethnicity[n],z+10] + IndOffset[ID[n],z+10];
      I[ID[n],z] = exp( meansL[z+15] + GrpOffset[ethnicity[n],z+15] + IndOffset[ID[n],z+15] );
    } // for z


    if ( ethnicity[n] == 2) {                   // if Matsigenka use estimated age
        mu_h[n] = comp_mod_h( Q[ID[n]], K[ID[n]], H[ID[n]], I[ID[n]], EstAge[n] );
        mu_w[n] = comp_mod_w( Q[ID[n]], K[ID[n]], H[ID[n]], I[ID[n]], EstAge[n] );
    } else {                                     // if Berkeley use exact age
        mu_h[n] = comp_mod_h( Q[ID[n]], K[ID[n]], H[ID[n]], I[ID[n]], age[n] );
        mu_w[n] = comp_mod_w( Q[ID[n]], K[ID[n]], H[ID[n]], I[ID[n]], age[n] );
    } // else



  } // for n


  log_height ~ normal( log(mu_h), Lsigma_h );

  log_weight_merge = merge_missing(w_missidx, log_weight, log_weight_impute);    // merge log weights and log_weight_impute variables 
  log_weight_merge ~ normal( log(mu_w), Lsigma_w );                              // where an element of log_weight_merge is a variable, this is the prior. Where an element of log_weight_merge is data, the model learns the values of mu_w and Lsigma_w consistent with it.

}


generated quantities{

  vector[N] mu_h;                      // mean of un-logged normal distribution
  vector[N] mu_w;

  vector[N] log_lik;                 // for WAIC

  matrix[20,20] G_cor_mat;           // 20x20 correlation matrix for parameters across individuals
  matrix[20,20] G_cov_mat;           // 20x20 variance-covariance matrix for parameters across individuals
  array[E] matrix[20,20] I_cor_mat;  
  array[E] matrix[20,20] I_cov_mat; 

  array[J] vector[5] Q;             // temporary holders for linear sub-model for each parameter
  array[J] vector[5] K;
  array[J] vector[5] H;
  array[J] vector[5] I;

  array[E] vector[5] mQ;            // just group offsets: E=1 US, E=2 Matsigenka
  array[E] vector[5] mK;
  array[E] vector[5] mH;
  array[E] vector[5] mI;

  vector[N] obsyearout;

  vector[w_num_miss] weight_impute;

  obsyearout = obsyear;             // the simulated observation year data, for plotting


  for (n in 1:N) {

    for (z in 1:5) {
      mQ[ethnicity[n],z] = exp( meansL[z+0] + GrpOffset[ethnicity[n],z+0] );
      mK[ethnicity[n],z] = exp( meansL[z+5] + GrpOffset[ethnicity[n],z+5] );
      mH[ethnicity[n],z] = mK[ethnicity[n],z]/2 + meansL[z+10] + GrpOffset[ethnicity[n],z+10];
      mI[ethnicity[n],z] = exp( meansL[z+15] + GrpOffset[ethnicity[n],z+15] );

      Q[ID[n],z] = exp( meansL[z+0] + GrpOffset[ethnicity[n],z+0] + IndOffset[ID[n],z+0] );
      K[ID[n],z] = exp( meansL[z+5] + GrpOffset[ethnicity[n],z+5] + IndOffset[ID[n],z+5] );
      H[ID[n],z] = K[ID[n],z]/2 + meansL[z+10] + GrpOffset[ethnicity[n],z+10] + IndOffset[ID[n],z+10];
      I[ID[n],z] = exp( meansL[z+15] + GrpOffset[ethnicity[n],z+15] + IndOffset[ID[n],z+15] );
    } // for z


      if ( ethnicity[n] == 2) {                   // if Matsigenka use estimated age
        mu_h[n] = comp_mod_h( Q[ID[n]], K[ID[n]], H[ID[n]], I[ID[n]], EstAge[n] );
        mu_w[n] = comp_mod_w( Q[ID[n]], K[ID[n]], H[ID[n]], I[ID[n]], EstAge[n] );
      } else {                                     // if Berkeley use exact age
        mu_h[n] = comp_mod_h( Q[ID[n]], K[ID[n]], H[ID[n]], I[ID[n]], age[n] );
        mu_w[n] = comp_mod_w( Q[ID[n]], K[ID[n]], H[ID[n]], I[ID[n]], age[n] );
      } // else

    // log_lik[n] = normal_lpdf( log_height[n] | log(mu[n]), Lsigma );      // for WAIC function

  } // for n


  G_cor_mat = L_G * L_G';                                    //reconstruct the correlation matrix to look at in output

  G_cov_mat = diag_pre_multiply(offsetGroupStdevs, L_G) * 
              diag_pre_multiply(offsetGroupStdevs, L_G)';    //construct cov matrix from cholesky factors of cov matrix to look at in output


  for (eth in 1:E) {

    I_cor_mat[eth] = L_I[eth] * L_I[eth]'; 

    I_cov_mat[eth] = diag_pre_multiply(offsetIndivStdevs[eth], L_I[eth]) * 
                     diag_pre_multiply(offsetIndivStdevs[eth], L_I[eth])';
  
  } // for eth
  
  weight_impute = exp(log_weight_impute);
}

