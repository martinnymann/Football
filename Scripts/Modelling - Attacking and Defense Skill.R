load("Data/Processed Data/MS.RData")

SingleSeason <- MS[Season=="1112/E0",]

SingleSeason[,HT_N:=as.numeric(factor(HomeTeam))]
SingleSeason[,AT_N:=as.numeric(factor(AwayTeam))]

MD <- list(N=dim(SingleSeason)[1],
           NT=length(unique(SingleSeason$HT_N)),
           HT=as.integer(SingleSeason$HT_N),
           AT=as.integer(SingleSeason$AT_N),
           HT_G=SingleSeason$FTHG,
           AT_G=SingleSeason$FTAG)


system.time(SS_Fit <- stan(file="Model/Model - AttDef.stan",
                             seed=42,
                             iter = 2500, 
                             chains = 4,
                             data=MD))





NS <- paste0("Skill[",1:length(unique(SingleSeason$HT_N)),"]")
S <- levels(factor(SingleSeason$HomeTeam))


x <- extract(SS_Fit,pars="Skill")[[1]]

# x[1,1,1] # 1. Interation 2. Team 3. Att vs. Def
# x[1,1,2] # 

DT_Att <- data.table(x[,,1])
DT_Def <- data.table(x[,,2])

setnames(DT_Att,S)
setnames(DT_Def,S)

DT_Att[,Skill:="Attack"]
DT_Def[,Skill:="Defence"]

Ex_Skill <- rbind(DT_Att,DT_Def)


GD  <- Ex_Skill[,lapply(.SD,mean),by="Skill"]

GD <- dcast(melt(GD,id.vars="Skill"),variable~Skill,value.var="value")

ggplot(GD,aes(x=Defence,y=Attack))+
  theme_bw()+
  geom_text(aes(label=variable))
