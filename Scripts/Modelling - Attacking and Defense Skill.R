load("Data/Processed Data/MS.RData")

SingleSeason <- MS[Season=="1112/E0",]

SingleSeason[,HT_N:=as.numeric(factor(HomeTeam))]
SingleSeason[,AT_N:=as.numeric(factor(AwayTeam))]
SingleSeason[,variable:=paste0("Game - ",1:380)]

MD <- list(N=dim(SingleSeason)[1],
           NT=length(unique(SingleSeason$HT_N)),
           HT=as.integer(SingleSeason$HT_N),
           AT=as.integer(SingleSeason$AT_N),
           HT_G=SingleSeason$FTHG,
           AT_G=SingleSeason$FTAG)


system.time(SS_Fit <- stan(file="Model/Model - Refactored.stan",
                             seed=42,
                             iter = 500, 
                             chains = 4,
                             data=MD))


NS <- paste0("Skill[",1:length(unique(SingleSeason$HT_N)),"]")
S <- levels(factor(SingleSeason$HomeTeam))


Ext <- extract(SS_Fit,pars="Skill")[[1]]

# x[1,1,1] # 1. Interation 2. Team 3. Att vs. Def
# x[1,1,2] # 


# SS_Fit@model_pars
# ggs_caterpillar(ggs(S=SS_Fit,family="Skill"))+scale_x_continuous(limit=c(-1,1))



DT_Att <- data.table(Ext[,,1])
DT_Def <- data.table(Ext[,,2])

setnames(DT_Att,S)
setnames(DT_Def,S)

DT_Att[,Skill:="Attack"]
DT_Def[,Skill:="Defence"]

Ex_Skill <- rbind(DT_Att,DT_Def)


GD  <- Ex_Skill[,lapply(.SD,mean),by="Skill"]

GD <- dcast(melt(GD,id.vars="Skill"),variable~Skill,value.var="value")


Cairo_png("Output/Modelling Results/Skills.png",width=10,height=10)
print(
  ggplot(GD,aes(x=Defence,y=Attack))+
    theme_bw()+
    geom_text(aes(label=variable))+
    ggtitle("Inital Model Results - 2011/2012")+
    scale_y_continuous(limit=c(-0.6,0.6))+
    scale_x_continuous(limit=c(-0.6,0.6))
)
dev.off()


