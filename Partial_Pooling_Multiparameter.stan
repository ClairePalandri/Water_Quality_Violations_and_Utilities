data {
int<lower=1> N;  //The number of data points
int<lower=1> K;  //The number of covarites plus intercept.
int<lower=1> J;  // The number of groups or states of hierarhicy
matrix[N,K] X;   // The Data Matrix
int<lower=1> id[N];  //vector of department indices.
int<lower=0, upper=1> z[N];  //The response variable
}

parameters {
real beta[K];  //The national level coefficients
vector[K] beta_group[J]; //matrix of group-level regression coefficients
real<lower=0, upper=1> nu;
real<lower=0,upper=1> nu_group[J];
}


model {
//priors
for(j in 1:J){beta_group[j] ~ normal(beta,100);}
for(j in 1:J){nu_group[j] ~ beta(((1-nu)*nu-(nu*(1-nu)/J))*nu/(nu*(1-nu)/J),((1-nu)*nu-(nu*(1-nu)/J))*nu*(1-nu)/(nu*nu*(1-nu)/J)); }
beta ~ normal(0,10);
nu ~ beta(1,1);   

//model 
for(n in 1:N){
z[n] ~ bernoulli((nu_group[id[n]]*exp(X[n] * beta_group[id[n]]))/(exp(X[n] * beta_group[id[n]])+1));
}
}
