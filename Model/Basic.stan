data {
	int<lower=1> N;
	int<lower=1> NT;
	int<lower=1,upper=NT> HT[N];
	int<lower=1,upper=NT> AT[N];
	int<lower=0> HT_G[N];
	int<lower=0> AT_G[N];
}
parameters{
	real Base_H;
	real Base_A;

	real Skill[NT];
}
transformed parameters{
	real HT_L[N];
	real AT_L[N];

	for (n in 1:N){
		HT_L[n] <- exp(Base_H + Skill[HT[n]] - Skill[AT[n]]);
		AT_L[n] <- exp(Base_A + Skill[AT[n]] - Skill[HT[n]]);
	}
}
model{
	Skill ~ normal(0,4);
	Skill[1] ~ normal(0,0.001);
	
	HT_G ~ poisson(HT_L);
	AT_G ~ poisson(AT_L);

}