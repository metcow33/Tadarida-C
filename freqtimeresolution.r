rm(list = ls())
.libPaths("C:/Documents/R files")

setwd("C:/Documents/Data/small dataset")


Ftot=list.files("C:/Documents/Data/small dataset",recursive=T,full.names=T)
Wav=grepl(".wav",Ftot)
file.remove(subset(Ftot,!Wav))


library(seewave)
library(tuneR)

snd<-list.files(path= "C:/Documents/Data/small dataset", recursive = T, pattern = ".wav$")

#change sampling rate of sound files to alter time/frequency resolution in Tadarida


for(i in 1: length(snd)){
  nm<-(paste0("C:/Documents/Data/small dataset/", snd[i]))
  
  a<-readWave(nm)
  
  a@samp.rate<-a@samp.rate*5
  
  writeWave(a, filename = nm , extensible=FALSE)
}



SubFolder=list.dirs("C:/Documents/Data/small dataset",recursive=F)
Newname=paste0(SubFolder,"_temp")
file.rename(from=SubFolder,to=Newname)

################################################################################################
#check all files treated by Tadarida

folders<-list.dirs(path="C:/Documents/Data/Tad_Dat_freqm_thresholds_2", recursive = F, full.names = F)

wavlist<-list()
for(i in 1:length(folders)){
  wv<-list.files(path = paste0("C:/Documents/Data/Tad_Dat_freqm_thresholds_2/", folders[i]), recursive=F,full.names=F, pattern=".wav$")
 wv<-gsub(".wav$", "", wv)
   wavlist[[i]]<-wv
}

talist<-list()


for(i in 1:length(folders)){
  ta<-list.files(path = paste0("C:/Documents/Data/Tad_Dat_freqm_thresholds_2/", folders[i], "/txt"), recursive=F,full.names=F, pattern=".ta$")
  ta<-gsub(".ta$", "", ta)
    talist[[i]]<-ta
}


for(i in 1:length(wavlist)){
if(identical(talist[[i]], wavlist[[i]])==F){print(paste0("ERROR: ",i, "_",folders[[i]]))}
}
#######################################################################################################
#match labels from previous database to database with new freq resolution

library(Hmisc)       
library(data.table)

#list of point measurement files from previous time/freq resolution
talist<-list.files(path="D:/Documents/Data/Tad Dat No Type pre10.12.18/Tadarida Dat No Type", pattern=".ta$", full.names = T, recursive = T)

#list of point measurement files from new time/freq resolution
talist2<-list.files(path="C:/Documents/Data/Tad_Dat_freqm_thresholds_2", pattern=".ta$", full.names = T, recursive = T)

#for testing
test=(basename(talist)==basename(talist2))
summary(test)

#blank list to store matrix of matches for each file
tstlist<-list()
#loop to match points in each point measurement file
otlist<-list()

for (i in 1:length(talist)){
  #read in old file  
  oldta<-read.csv(talist[i], sep="\t")
  #read in corresponding new file
  newta<-read.csv(talist2[i], sep="\t")
  
  #divide time values from old file to match with new time/freq resolution
  oldta$StTime <- oldta$StTime/5
  oldta$PosMP <- oldta$PosMP
  oldta$Dur <-oldta$Dur/5
  
  #multiply freq values from old file to match with new time/freq resolution 
  oldta$FreqMP<-oldta$FreqMP*5
  
  #create a variable fro the time of the peak freq for each point in old file
  oldta$Time<-oldta$StTime+(oldta$PosMP)*oldta$Dur
  
  #create a matrix from old file of peak freq and time of peak freq (Time) for each point
  ot<-subset(oldta,select=c("Time","FreqMP"))

  otlist[[i]]<-ot
    
  #create a variable for the time of the peak freq for each point in new file
  newta$Time<-newta$StTime+(newta$PosMP)*newta$Dur
  
  #create a matrix from new file of peak freq and time of peak freq (Time) for each point
  nt<-subset(newta,select=c("Time","FreqMP"))
  
  #look for points to match between the files
  tst<-find.matches(ot, nt, tol=c(35, 1.75), maxmatch=1)
  
  tstlist[[i]]<-tst
}

######################################################

save(tstlist,file="tst.list")
fwrite(data.frame(talist), file="talist.txt")

for (i in 3000:3010){
#plot the matches to see how well the matches worked
table(tstlist[[i]][1])
test=subset(otlist[[i]],tstlist[[i]][1]!="")

plot(otlist[[i]],ylim=c(0,25))
points(nt,col=2)
test=subset(otlist[[i]],tstlist[[i]][[1]]!="")
points(test,col=3)
}

