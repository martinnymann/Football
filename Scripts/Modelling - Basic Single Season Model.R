load("Data/Processed Data/MS.RData")

SingleSeason <- MS[Season=="1011/E0",]

SingleSeason[,HT_N:=as.numeric(factor(HomeTeam))]
SingleSeason[,AT_N:=as.numeric(factor(AwayTeam))]

MD <- list(N=dim(SingleSeason)[1],
           NT=length(unique(SingleSeason$HT_N)),
           HT=as.integer(SingleSeason$HT_N),
           AT=as.integer(SingleSeason$AT_N),
           HT_G=SingleSeason$FTHG,
           AT_G=SingleSeason$FTAG)


system.time(SS_Fit <- stan(file="Model/Basic.stan",
                           seed=42,
                           pars=c("Base_H","Base_A","Skill"),
                           iter = 2500, 
                           chains = 4,
                           data=MD))





NS <- paste0("Skill[",1:length(unique(SingleSeason$HT_N)),"]")
S <- levels(factor(SingleSeason$HomeTeam))


Ex_skill <- data.table(extract(SS_Fit,pars="Skill")[[1]])
setnames(Ex_skill,S)


GF <-data.table(ggs(SS_Fit,family="Skill"))

GF[,Parameter:=factor(Parameter,levels=NS,labels=S)]


ggplot(GF,aes(x=value,y=..density..))+
  theme_bw()+
  geom_histogram(col="white",fill="skyblue",binwidth=0.05)+
  facet_wrap(~Parameter)

ggplot(data=Ex_skill[,list(x=`Man United`-`Man City`)],aes(x=x))+
  geom_histogram(col="white",fill="skyblue",binwidth=0.05)+
  theme_bw()

ProbMatrix <- data.table(expand.grid(TeamA=S,TeamB=S,stringsAsFactors=F))

ProbMatrix[,
           Probs:=apply(ProbMatrix,1,function(l){
             x1 <- l[1]
             x2 <- l[2]
             mean(Ex_skill[,x1,with=FALSE]-Ex_skill[,x2,with=FALSE]>0)
})]

ProbMatrix <- dcast(as.data.frame(ProbMatrix),TeamA~TeamB,value.var="Probs")

View(ProbMatrix)
