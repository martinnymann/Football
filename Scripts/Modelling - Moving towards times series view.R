load("Data/Processed Data/MS.RData")

ModelTemplate <- stan(file="Model/Model - AttDef.stan",
                      seed=42,
                      iter = 0, 
                      chains = 0)

#Ignore error message about no samples, we haven't supplied data yet.

SingleDiv <- MS[Div=="E0",]
Season <- unique(SingleDiv$Season)

Results <- NULL
for (Ss in Season){#Ss <-Season[1]
  print(paste0("Estimating Season - ",Ss))
  MDT <- MS[Season==Ss,]
  
  MDT[,HT_N:=as.numeric(factor(HomeTeam))]
  MDT[,AT_N:=as.numeric(factor(AwayTeam))]
  
  MD <- list(N=dim(MDT)[1],
             NT=length(unique(MDT$HT_N)),
             HT=as.integer(MDT$HT_N),
             AT=as.integer(MDT$AT_N),
             HT_G=MDT$FTHG,
             AT_G=MDT$FTAG,
             CouNames=levels(factor(MDT$HomeTeam)))
  
  SS_Fit <- stan(fit=ModelTemplate,
                 seed=42,
                 iter = 2500, 
                 chains = 4,
                 data=MD)
  
  S <- MD$CouNames
  
  Ext <- extract(SS_Fit,pars="Skill")[[1]]
  DT_Att <- data.table(Ext[,,1])
  DT_Def <- data.table(Ext[,,2])
  
  setnames(DT_Att,S)
  setnames(DT_Def,S)
  
  DT_Att[,Skill:="Attack"]
  DT_Def[,Skill:="Defence"]
  
  Ex_Skill <- rbind(DT_Att,DT_Def)
  
  Ex_Skill[,Season:=substr(Ss,start=1,stop=4)]
  
  Results[Ss] <- list(Ex_Skill)
  
}

RDT <- data.table(ldply(Results,function(r){melt(r,id.vars=c("Skill","Season"))}))
setnames(RDT,"variable","Team")

SkillValues <- RDT[, list(metric1=mean(value)), keyby = list(Team,Skill,Season)
                   ][CJ(unique(Team), unique(Skill), unique(Season)), allow.cartesian = T
                     ][,setNames(as.list(metric1), paste(Skill)), by = list(Team,Season)]


SkillValues[,Team_SN:=substr(Team,1,3)]
SDD <- fread(input="Data/Reference Tables/SeasonToDate.csv",colClasses=c("character","character"))
SDD[,dummyDate:=as.Date(dummyDate,"%d/%m/%Y")]

setkey(SkillValues,Season)
setkey(SDD,Season)

SkillValues <- SDD[SkillValues]

ggplot(SkillValues[Season!="1314",],aes(x=Defence,y=Attack))+theme_bw()+geom_text(aes(label=Team_SN))+
  facet_wrap(~Season)

SkillValues[,Season:=paste0("20",substr(Season,1,2),"-20",substr(Season,3,4))]
SkillValues[,Year:=as.numeric(substr(Season,1,4))]


M <- gvisMotionChart(data=SkillValues[Season!="2013-2014",],
                     idvar="Team",
                     timevar="Year",
                     xvar="Defence",
                     yvar="Attack",
                     options=list(
                       state='{"showTrails":false};'
                       )
                     )


plot(M)
cat(M$html$chart, file="Output/Modelling Results/GoogleVisChart_A.html")


