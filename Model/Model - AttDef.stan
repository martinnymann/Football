data {
	int<lower=1> N;
	int<lower=1> NT;
	int<lower=1,upper=NT> HT[N];
	int<lower=1,upper=NT> AT[N];
	int<lower=0> HT_G[N];
	int<lower=0> AT_G[N];
}
parameters{
	real HomeAdvantage;

	vector[NT] Skill_free_Att;
	vector[NT] Skill_free_Def;

}
transformed parameters{
	matrix[NT,2] Skill;
	// col(Skill,1) <- Skill_free_Att-mean(Skill_free_Att);
	// col(Skill,2) <- Skill_free_Def-mean(Skill_free_Def);

	for (s in 1:NT){
	Skill[s,1] <- Skill_free_Att[s]-mean(Skill_free_Att);
	Skill[s,2] <- Skill_free_Def[s]-mean(Skill_free_Def);
	}
}
model{
	vector[N] HT_L;
	vector[N] AT_L;

	Skill_free_Att ~ normal(0,4);
	Skill_free_Def ~ normal(0,4);
	
	for (n in 1:N){
		HT_L[n] <- exp(HomeAdvantage + Skill[HT[n],1] - Skill[AT[n],2]);
		AT_L[n] <- exp(Skill[AT[n],1] - Skill[HT[n],2]);
	}


	HT_G ~ poisson(HT_L);
	AT_G ~ poisson(AT_L);

}