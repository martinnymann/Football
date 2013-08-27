OriginalObjects <- ls()
BaseLocation <- "http://www.football-data.co.uk/mmz4281/"


CSVLocations <- read.csv(file="Data/Reference Tables/RawCSVLocations.csv")
CSVLocations <- as.character(CSVLocations[regexpr(pattern="mmz4281",text=CSVLocations[,1],fixed=T)>0,])
CSVLocations <- sapply(strsplit(x=CSVLocations,split="mmz4281/",fixed=T),"[",2)
CSVLocations <- sapply(strsplit(x=CSVLocations,split=".csv",fixed=T),"[",1)
NamesCSV <-CSVLocations
CSVLocations <- paste0(BaseLocation,NamesCSV,".csv")


RawImport <- llply(.data=CSVLocations,
                   .fun=function(l){
                     print(l)
                     read.csv(l,check.names=FALSE,stringsAsFactors=FALSE)
                   }
                   )
names(RawImport) <- NamesCSV


save(RawImport,file="Data/Raw Data/RawImport.RData")

keep(list=OriginalObjects,sure=TRUE)