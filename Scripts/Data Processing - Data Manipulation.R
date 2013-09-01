OriginalObjects <- ls()

load("Data/Raw Data/RawImport.RData")

RecentYears <- RawImport[as.numeric(substr(names(RawImport),1,1)) < 3]

ColNames <- melt(llply(RecentYears,names))
ColNames$Year <- sapply(strsplit(ColNames$L1,split="/",fixed=T),"[",1)
ColNames$Division <- sapply(strsplit(ColNames$L1,split="/",fixed=T),"[",2)

write.csv(dcast(ColNames[ColNames$Division=="E0",],value~L1,fun.aggregate=length),
          file=paste0(getwd(),"/Output/EDA/EachYearsStatistics.csv"),row.names=F)


VoI <- c("Season","Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR","HS","AS","HST","AST")

RecentYears <- ldply(names(RecentYears),function(ndf){
  df <- RecentYears[[ndf]]
  df$Season <- ndf
  df[,VoI]
  })

MS <- data.table(RecentYears,key=c("Season","Div","HomeTeam","AwayTeam"))

MS <- MS[HomeTeam!="",]

save(MS,file="Data/Processed Data/MS.RData")

keep(list=OriginalObjects,sure=TRUE)