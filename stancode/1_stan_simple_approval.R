## Define model

stancode_simple <- "

data {

int<lower = 1> N_obs; // Number of observations
int<lower = 1> N; // Number of mps
int<lower = 1> D; // Number of debates
int<lower = 1> P; // Number of parties
int nId[N_obs];  // Unique mp id
int dId[N_obs]; // Unique debate id
int Y[N_obs]; // Response
vector[N_obs] X_sig; // Signatures
real Xi_margin[N]; // Electoral margin of MP i
real Xi_new_mp[N]; // Is MP i new to the House?
real Xi_cabinet[N]; // Has MP i served in a frontbench position during this parliamentary term?
matrix[N,P] Xi_party_matrix; // Which party does MP i belong to?

}

parameters {

real alpha_0; // Grand intercept
vector[N] gamma; // Signature coefficients (one per MP)

real<lower = 0> sigma_gamma; // gamma variance

real mu_gamma; // second level signature coefficent intercept
real mu_gamma_margin; // second level slope for electoral margin
real mu_gamma_new_mp; // second level slope for new mp
real mu_gamma_cabinet; // second level slope for minister
vector[P] mu_gamma_party; // second level slope for parties

}

model {
real mu[N_obs];

for(i in 1:N_obs)  {

mu[i] = (alpha_0 + 
         gamma[nId[i]] * X_sig[i]);

}



Y ~ bernoulli_logit(mu);

// Prior for grand intercepts
alpha_0 ~ normal(0, 10);

// Prior for MP-effects and signature coefficients
for(m in 1:N){
  gamma[m] ~ normal(mu_gamma + mu_gamma_margin*Xi_margin[m] + mu_gamma_new_mp*Xi_new_mp[m] + mu_gamma_cabinet*Xi_cabinet[m] + dot_product(mu_gamma_party, Xi_party_matrix[m,]), sigma_gamma);
}

// Hyperpriors

mu_gamma ~ normal(0, 2);
mu_gamma_margin ~ normal(0, 2);
mu_gamma_new_mp ~ normal(0, 2);
mu_gamma_cabinet ~ normal(0, 2);
for(p in 1:P) mu_gamma_party[p] ~ normal(0, 2);

}


generated quantities{

real log_likelihood;
real deviance;

log_likelihood = bernoulli_logit_lpmf(Y | alpha_0 + gamma[nId] .* X_sig);
deviance = -2*log_likelihood;

}
"

