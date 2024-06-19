library(data.table)
library(mountSTAT)

devtools::load_all("~/simPop")

path <- file.path(mountMeth(),"Gussenbauer/uROS2021")

data(eusilcS)
setDT(eusilcS)

eusilcS[,db090_10:=db090*10]
eusilcS[,db090_25:=db090*25]
eusilcS[,db090_50:=db090*50]
eusilcS[,db090_100:=db090*100]
eusilcS[,db090_200:=db090*200]

eusilcS[,rb050_10:=rb050*10]
eusilcS[,rb050_25:=rb050*25]
eusilcS[,rb050_50:=rb050*50]
eusilcS[,rb050_100:=rb050*100]
eusilcS[,rb050_200:=rb050*200]

eusilcT_10 <- simEUSILC(dataS=eusilcS,wh="db090_10",wp="rb050_10")
eusilcT_25 <- simEUSILC(dataS=eusilcS,wh="db090_25",wp="rb050_25")
eusilcT_50 <- simEUSILC(dataS=eusilcS,wh="db090_50",wp="rb050_50")
eusilcT_100 <- simEUSILC(dataS=eusilcS,wh="db090_100",wp="rb050_100")
eusilcT_200 <- simEUSILC(dataS=eusilcS,wh="db090_200",wp="rb050_200")


save(eusilcT_10,file=file.path(path,"simPop_Test_Calibrations_10_start.RData"))

save(eusilcT_10,eusilcT_25,eusilcT_50,eusilcT_100,eusilcT_200,file=file.path(path,"simPop_Test_Calibrations.RData"))
load(file.path(path,"simPop_Test_Calibrations.RData"))

# calibrate
data(eusilcP) 
eusilcP$age_cut <- cut(eusilcP$age,breaks=c(-Inf,5,10,15,20,25,30,35,40,45,50,55,60,65,Inf))
margins <- as.data.frame(
  xtabs(rep(1, nrow(eusilcP)) ~ eusilcP$region + eusilcP$gender + eusilcP$citizenship))
colnames(margins) <- c("db040", "rb090", "pb220a", "Freq")
setDT(margins)
margins[,Freq:=Freq*10]

simPop_adj <- list()
# Rprof(file="simPop_Test_Calibrations_10.out",memory.profiling=TRUE)
runTime <- rep(0,30)
for(i in 1:30){
  t <- Sys.time()
  # gc1 <- gc(reset = TRUE)
  simPop_adj_i <- calibPop(eusilcT_10, split="db040", temp=1, epsP.factor=0.025,persTables=margins, nr_cpus = 1,verbose = FALSE,
                           sizefactor = 4)
  marg1 <- simPop_adj_i@pop@data[,.N,by=c("db040", "rb090", "ageCat", "pb220a")]
  # gc2 <- gc()
  # cat(sprintf("mem: %.1fMb.\n", sum(gc2[,6] - gc1[,2])))
  runTime[i] <- as.numeric(Sys.time() - t,units="secs")
  simPop_adj <- c(simPop_adj,list(marg1))
}
# Rprof(NULL)
save(simPop_adj,runTime,file=file.path(path,"simPop_Test_Calibrations_10.RData"))
rm(simPop_adj);gc()


margins <- as.data.frame(
  xtabs(rep(1, nrow(eusilcP)) ~ eusilcP$region + eusilcP$gender + eusilcP$citizenship))
colnames(margins) <- c("db040", "rb090", "pb220a", "Freq")
setDT(margins)
margins[,Freq:=Freq*25]
simPop_adj <- list()
runTime <- rep(0,30)
for(i in 1:30){
  t <- Sys.time()
  simPop_adj_i <- calibPop(eusilcT_25, split="db040", temp=1, epsP.factor=0.025,persTables=list(margins), nr_cpus = 1,verbose = FALSE,
                           sizefactor = 4)
  marg1 <- simPop_adj_i@pop@data[,.N,by=c("db040", "rb090", "ageCat", "pb220a")]
  runTime[i] <- as.numeric(Sys.time() - t,units="secs")
  simPop_adj <- c(simPop_adj,list(marg1))
}
save(simPop_adj,runTime,file=file.path(path,"simPop_Test_Calibrations_25.RData"))
rm(simPop_adj);gc()


margins <- as.data.frame(
  xtabs(rep(1, nrow(eusilcP)) ~ eusilcP$region + eusilcP$gender + eusilcP$citizenship))
colnames(margins) <- c("db040", "rb090", "pb220a", "Freq")
setDT(margins)
margins[,Freq:=Freq*50]
simPop_adj <- list()
runTime <- rep(0,30)
for(i in 1:30){
  t <- Sys.time()
  simPop_adj_i <- calibPop(eusilcT_50, split="db040", temp=1, epsP.factor=0.025,persTables=list(margins), nr_cpus = 1,verbose = FALSE,
                           sizefactor = 4)
  marg1 <- simPop_adj_i@pop@data[,.N,by=c("db040", "rb090", "ageCat", "pb220a")]
  runTime[i] <- as.numeric(Sys.time() - t,units="secs")
  simPop_adj <- c(simPop_adj,list(marg1))
}
save(simPop_adj,runTime,file=file.path(path,"simPop_Test_Calibrations_50.RData"))
rm(simPop_adj);gc()


margins <- as.data.frame(
  xtabs(rep(1, nrow(eusilcP)) ~ eusilcP$region + eusilcP$gender + eusilcP$citizenship))
colnames(margins) <- c("db040", "rb090", "pb220a", "Freq")
setDT(margins)
margins[,Freq:=Freq*100]
simPop_adj <- list()
runTime <- rep(0,30)
for(i in 1:30){
  t <- Sys.time()
  simPop_adj_i <- calibPop(eusilcT_100, split="db040", temp=1, epsP.factor=0.025,persTables=list(margins), nr_cpus = 1,verbose = FALSE,
                           sizefactor = 4)
  marg1 <- simPop_adj_i@pop@data[,.N,by=c("db040", "rb090", "ageCat", "pb220a")]
  runTime[i] <- as.numeric(Sys.time() - t,units="secs")
  simPop_adj <- c(simPop_adj,list(marg1))
}
save(simPop_adj,runTime,file=file.path(path,"simPop_Test_Calibrations_100.RData"))
rm(simPop_adj);gc()


margins <- as.data.frame(
  xtabs(rep(1, nrow(eusilcP)) ~ eusilcP$region + eusilcP$gender + eusilcP$citizenship))
colnames(margins) <- c("db040", "rb090", "pb220a", "Freq")
setDT(margins)
margins[,Freq:=Freq*200]
simPop_adj <- list()
runTime <- rep(0,30)
for(i in 21:30){
  t <- Sys.time()
  simPop_adj_i <- calibPop(eusilcT_200, split="db040", temp=1, epsP.factor=0.025,persTables=list(margins), nr_cpus = 1,verbose = FALSE,
                           sizefactor = 4)
  marg1 <- simPop_adj_i@pop@data[,.N,by=c("db040", "rb090", "ageCat", "pb220a")]
  runTime[i] <- as.numeric(Sys.time() - t,units="secs")
  simPop_adj <- c(simPop_adj,list(marg1))
}
save(simPop_adj,runTime,file=file.path(path,"simPop_Test_Calibrations_200.RData"))
rm(simPop_adj);gc()

