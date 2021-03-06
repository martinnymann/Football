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

	real Skill_free[NT-1];
}
transformed parameters{
	vector[NT] Skill;
	Skill[1] <- 0;
	for (i in 1:(NT-1)){
		Skill[i+1] <- Skill_free[i];	
	}
	
}
model{
	vector[N] HT_L;
	vector[N] AT_L;

	Skill_free ~ normal(0,4);
	
	for (n in 1:N){
		HT_L[n] <- exp(Base_H + Skill[HT[n]] - Skill[AT[n]]);
		AT_L[n] <- exp(Base_A + Skill[AT[n]] - Skill[HT[n]]);
	}


	HT_G ~ poisson(HT_L);
	AT_G ~ poisson(AT_L);

}