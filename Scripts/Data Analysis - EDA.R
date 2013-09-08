load(file="Data/Processed Data/MS.RData")

# HomeGoals <- MS[,list(Goals=FTHG),by=c("Season","Div","Date","HomeTeam")]
# AwayGoals <- MS[,list(Goals=FTAG),by=c("Season","Div","Date","AwayTeam")]


MS[,CGHTS:=.N,by=c("Season","HomeTeam")]



# cairo_pdf(filename="Away Goals.pdf",width=15,height=15)
# d_ply(.data=MS,.variables=.(Div),function(gd){
#   print(
#     ggplot(gd,aes(x=FTHG,fill=Div))+geom_histogram(binwidth = 1)+scale_x_discrete() + theme_bw() + facet_wrap(~HomeTeam+CGHTS)
#     )
# })
# dev.off()

# lapply(MS,FUN=function(x){sum(is.na(x))})


MS <- MS[complete.cases(MS),]

c <- MS[Div=="E0",list(FTHG)]

c <- c$FTHG

c <- prop.table(table(c))


gd3 <- data.frame(x=as.integer(names(c)),t=as.numeric(c))


a <- fitdistr(x=b,densfun="Poisson")
b <- fitdistr(x=b,densfun="negative binomial")

a <- a$estimate
b <- b$estimate


a[names(a)=="lambda"]
b[names(b)=="size"]
b[names(b)=="mu"]

a_max <- qpois(p=1-1e-7,lambda=a[names(a)=="lambda"])+1
b_max <- qnbinom(p=1-1e-7,size=b[names(b)=="size"],mu=b[names(b)=="mu"])+1

dpois(x=seq(0,a_max),lambda=a[names(a)=="lambda"])
dnbinom(x=seq(0,b_max),size=b[names(b)=="size"],mu=b[names(b)=="mu"])
        

gd1 <- data.frame(x=seq(0,a_max),p=dpois(x=seq(0,a_max),lambda=a[names(a)=="lambda"]))
gd2 <- data.frame(x=seq(0,b_max),d=dnbinom(x=seq(0,b_max),size=b[names(b)=="size"],mu=b[names(b)=="mu"]))


head(gd1)
head(gd2)
head(gd3)

g <- ggplot()

g + geom_bar(data=gd1,aes(x=x,y=p),stat="identity") + 
  + geom_bar(data=gd1,aes(x=x,y=p),stat="identity") + 
  + geom_bar(data=gd1,aes(x=x,y=p),stat="identity") 
  

A <- ggplot(gd)+theme_bw()+scale_x_discrete(limit=0:12)+
  geom_histogram(fill="red",alpha=0.2,aes(x=a),binwidth=1)+
  geom_histogram(col="black",alpha=0.2,aes(x=t),binwidth=1)+
  ggtitle("Poisson")


B <- ggplot(gd)+theme_bw()+scale_x_discrete(limit=0:12)+
  geom_histogram(fill="blue",alpha=0.2,aes(x=b),binwidth=1)+
  geom_histogram(col="black",alpha=0.2,aes(x=t),binwidth=1)+
  ggtitle("Negative Binomial")


gridExtra::grid.arrange(A,B)

MS[,Score:=paste0(FTHG,":",FTAG)]

dcast(MS[Season=="1213/E0",],HomeTeam~AwayTeam,value.var="Score")

gd <- MS[Season=="1213/E0",]

head(gd)

.Score <- function(HT,AT,R){
  if (R == "D"){
    HTS <- 1
    ATS <- 1
  } else if (R =="A") {
    HTS <- 0
    ATS <- 3
  } else if (R == "H") {
    HTS <- 3
    ATS <- 0
  } else {
    HTS <- NA
    ATS <- NA
  }
  list(Team=c(HT,AT),Points=c(HTS,ATS))
}

EoTP <- gd[, .Score(HomeTeam,AwayTeam,FTR),by=1:nrow(gd)][,list(TotalPoints=sum(Points)),by="Team"]

EoTP <- EoTP[order(-TotalPoints)]
EoTP[,Rank:=.I]


gd[,HomeTeamN:=factor(HomeTeam,levels=EoTP[,Team][nrow(EoTP):1])]
gd[,AwayTeamN:=factor(AwayTeam,levels=EoTP[,Team])]

EoTP[,Team]
reorder(EoTP[,Team])

nrow(EoTP):1
seq_along(EoTP[,Team])

ggplot(gd,aes(x=AwayTeamN,y=HomeTeamN))+geom_tile(aes(fill=FTR))+geom_text(aes(label=Score))+theme_bw()


table(MS$Season)


