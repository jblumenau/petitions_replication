## Define model

stancode_double_gamma_with_party <- "

data {

int<lower = 1> N_obs; // Number of observations
int<lower = 1> N; // Number of mp-by-terms
int<lower = 1> M; // Number of mps overall
int<lower = 1> D; // Number of debates
int<lower = 1> K; // Number of topics
int<lower = 1> P; // Number of parties
int<lower = 1> sigma_dim; // Dimensions of covariance matrix
int nId[N_obs];  // Unique mp-term id
int mId[N_obs];  // Unique mp id
int dId[N_obs]; // Unique debate id
int Y[N_obs]; // Response
row_vector[K] X[N_obs]; // Topic proportions
real X_sig[N_obs]; // Signatures
real X_sig_commons[N_obs]; // Signatures for commons debates
real X_sig_wminhall[N_obs]; // Signatures for wminhall debates
real Xi_margin[N]; // Electoral margin of MP i
real Xi_new_mp[N]; // Is MP i new to the House?
real Xi_cabinet[N]; // Has MP i served in the cabinet during this parliamentary term?
matrix[N,P] Xi_party_matrix; // Which party does MP i belong to?
real Xd_petition[D]; // Is this a petition debate?

}

parameters {

real alpha_0; // Grand intercept
vector[sigma_dim] alpha_gamma_individual[N];
vector[sigma_dim-1] mu_gamma;
vector[sigma_dim-1] mu_gamma_margin;
vector[sigma_dim-1] mu_gamma_cabinet;
vector[sigma_dim-1] mu_gamma_new_mp;
matrix[P, sigma_dim-1] mu_gamma_party; 

real mu_alpha; // second level mp random effect intercept
real mu_alpha_new_mp; // second level intercept for new mp
real mu_alpha_cabinet; // second level intercept for cabinet minister
real mu_alpha_margin; // second level intercept for electoral margin
vector[P] mu_alpha_party; // second level intercept for parties

real mu_delta; // second level debate random effect intercept
real mu_delta_petition; // second level debate effect for petition debates

vector[K] Beta[M]; // MP-topic coefficients
real delta[D]; // Debate fixed-effect

real<lower = 0> sigma_beta[K]; // Beta variance
real<lower = 0> sigma_delta; // delta variance
real<lower = 0> sigma_alpha; // delta variance
real<lower = 0> sigma_gamma_commons; // delta variance
real<lower = 0> sigma_gamma_wminhall; // delta variance

}

model {

real mu[N_obs];
matrix[N,sigma_dim] alpha_gamma_fitted;

for(i in 1:N_obs)  {

mu[i] = (alpha_0 + // Grand intercept
alpha_gamma_individual[nId[i],1] + // MP random effect
delta[dId[i]] + // Debate random effect
dot_product(Beta[mId[i],], X[i,]) + // MP-topic random slopes
alpha_gamma_individual[nId[i],2] * X_sig_commons[i] + // MP-signature random slope (Commons)
alpha_gamma_individual[nId[i],3] * X_sig_wminhall[i]); // MP-signature random slope (Wminhall)

}

Y ~ bernoulli_logit(mu);

// Construct fitted values for MP-signature random slopes and MP random effects

for(m in 1:N) {

alpha_gamma_fitted[m,1] = mu_alpha + mu_alpha_new_mp*Xi_new_mp[m] + mu_alpha_cabinet*Xi_cabinet[m] + mu_alpha_margin*Xi_margin[m] + dot_product(mu_alpha_party, Xi_party_matrix[m,]);

for(k in 1:2){

alpha_gamma_fitted[m,k+1] = mu_gamma[k] + mu_gamma_margin[k]*Xi_margin[m] + mu_gamma_cabinet[k]*Xi_cabinet[m] + mu_gamma_new_mp[k]*Xi_new_mp[m] + dot_product(mu_gamma_party[,k], Xi_party_matrix[m,]);

}
}

// Draw MP signature slopes and random intercepts from multivariate normal
for(m in 1:N) {

alpha_gamma_individual[m,1] ~ normal(alpha_gamma_fitted[m,1], sigma_alpha);
alpha_gamma_individual[m,2] ~ normal(alpha_gamma_fitted[m,2], sigma_gamma_commons);
alpha_gamma_individual[m,3] ~ normal(alpha_gamma_fitted[m,3], sigma_gamma_wminhall);

}


// Prior for grand intercepts
alpha_0 ~ normal(0, 10);

// Priors for debate intercepts
for(d in 1:D){
delta[d] ~ normal(mu_delta + mu_delta_petition*Xd_petition[d], sigma_delta);
}


// Priors for topic-mp coefficients

for(k in 1:K){
Beta[,k] ~ normal(0, sigma_beta[k]);
}

// Hyperpriors for MP-effects and signature coefficients

mu_alpha ~ normal(0, 2);
mu_alpha_new_mp ~ normal(0, 2);
mu_alpha_cabinet ~ normal(0, 2);
mu_alpha_margin ~ normal(0, 2);
for(p in 1:P) mu_alpha_party[p] ~ normal(0, 2);

mu_gamma ~ normal(0,2);
mu_gamma_margin ~ normal(0,2);
mu_gamma_cabinet ~ normal(0,2);
mu_gamma_new_mp ~ normal(0,2);
for(p in 1:P) mu_gamma_party[p,1] ~ normal(0, 2);
for(p in 1:P) mu_gamma_party[p,2] ~ normal(0, 2);

// Hyperpriors for debate coefficients

mu_delta ~ normal(0, 2);
mu_delta_petition ~ normal(0, 2);

}

generated quantities{

real log_likelihood;
real deviance;
log_likelihood = 0;
for(i in 1:N_obs)  {

log_likelihood = log_likelihood + bernoulli_logit_lpmf(Y[i] | (alpha_0 + 
alpha_gamma_individual[nId[i],1] +
delta[dId[i]] +
dot_product(Beta[mId[i],], X[i,]) +
alpha_gamma_individual[nId[i],2] * X_sig_commons[i] +
alpha_gamma_individual[nId[i],3] * X_sig_wminhall[i]));

}

deviance = -2*log_likelihood;


}

"