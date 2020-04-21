## Define model

stancode_simple <- "

data {

int<lower = 1> N_obs; // Number of observations
int<lower = 1> N; // Number of mps
int<lower = 1> D; // Number of debates
int<lower = 1> K; // Number of topics
int<lower = 1> P; // Number of parties
int nId[N_obs];  // Unique mp id
int dId[N_obs]; // Unique debate id
int Y[N_obs]; // Response
row_vector[K] X[N_obs]; // Topic proportions
vector[N_obs] X_sig; // Signatures
real Xi_margin[N]; // Electoral margin of MP i
real Xi_new_mp[N]; // Is MP i new to the House?
real Xi_cabinet[N]; // Has MP i served in the government during this parliamentary term?
matrix[N,P] Xi_party_matrix; // Which party does MP i belong to?
real Xd_petition[D]; // Is this a petition debate?

}

parameters {

real alpha_0; // Grand intercept
vector[N] gamma; // Signature coefficients (one per MP)

real<lower = 0> sigma_gamma; // gamma variance

real mu_gamma; // second level signature coefficent intercept
real mu_gamma_margin; // second level slope for electoral margin
real mu_gamma_new_mp; // second level slope for new mp
real mu_gamma_cabinet; // second level slope for minister
vector[P] mu_gamma_party; 

}

model {
real gamma_mean[N];

Y ~ bernoulli_logit(alpha_0 + gamma[nId] .* X_sig);

// Prior for grand intercepts
alpha_0 ~ normal(0, 2);

// Prior for MP-effects and signature coefficients
for(m in 1:N) gamma_mean[m] = mu_gamma + mu_gamma_margin*Xi_margin[m] + mu_gamma_new_mp*Xi_new_mp[m] + mu_gamma_cabinet*Xi_cabinet[m] + dot_product(mu_gamma_party, Xi_party_matrix[m,]);

gamma ~ normal(gamma_mean, sigma_gamma);


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

log_likelihood = bernoulli_logit_lpmf(Y | (alpha_0 + gamma[nId] .* X_sig));
deviance = -2*log_likelihood;

}
"

