ClassifC1="ClassifC1.r"
AggContacts="AggContacts.r"
AggNbSp="AggNbSp.r"
#ClassifC1="C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/ClassifC1.r"
#AggContacts="C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/AggContacts.r"
#AggNbSp="C:/Users/Yves Bas/Documents/Tadarida/AggNbSp.r"




#get arguments from the command line
args <- commandArgs(trailingOnly = TRUE)
#uncomment the following lines if you prefer to do not use R in command line
#args[1]="C:/wamp64/www/wavtri_test2ndSp/txt"
#args="E:/PI_CDL1804/5ae195607dcf87000f45bbfb/txt"
#args[2]="ClassifEsp_LF_180320.learner"
#args[2]="ClassifEsp_LF_180129.learner"
#args[2]="ClassifEspFrance180303.learner"
#args[3]="tabase3_LFXC"
#args[3]=""

#options (HPF = filtre passe-haut / Reduc = r�duction des features par DFA)
#args[4]=8 #HPF
#args[5]=T #Reduc - obsolete
#args[6]=T #TC
#args[7]=500 #block size
#args[10]="SpeciesList.csv" #species list
#args[11]="CNS_tabase3HF_France_IdConc.learner" #name of the species number" classifier
#args[12]=T #if species number should be filtered or not



tadir=args[1]
talistot=list.files(tadir,pattern=".ta$",full.names=T)
block=as.numeric(args[7])


for (r in 1:ceiling(length(talistot)/block))
{
args[8]=block*(r-1)+1 #start number
args[9]=block*r #end number
source(ClassifC1)
print(paste(r,ceiling(length(talistot)/block),Sys.time()))
}

source(AggContacts)


source(AggNbSp)