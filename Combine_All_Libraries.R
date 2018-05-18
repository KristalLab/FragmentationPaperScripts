#Author: Jeremy Koelmel
#Date: 05/05/2018
## This part of the program reformats all libraries into 1.

##remove all R objects from memory
rm(list = ls())
# Note looks for a Libraries/Neg; Libraries/Pos; and Output folder in this folder
Directory<-"C:/Users/Jeremy/Desktop/Desktop/ForOthers/Bowden/2018_4_14_FragmentPaper/Software_Fragments_Precursor"
DirNeg<-paste(Directory,"/Libraries/Neg/",sep="")
DirPos<-paste(Directory,"/Libraries/Pos/",sep="")
DirOut<-paste(Directory,"/Output/",sep="")
PosTRUE<-TRUE
#Code to combine all libraries into one library for each polarity
if (PosTRUE==FALSE){
  Files<-list.files(DirNeg)
  Dir<-DirNeg
  output_file<-"AllLibsNeg.csv"
} else {
  Files<-list.files(DirPos)
  Dir<-DirPos
  output_file<-"AllLibsPos.csv"
}
#for loop 
i<-10
for (i in 1:length(Files)){
  #print(Files[i])
  CurrentTable<-read.csv(paste(Dir,Files[i],sep=""), sep=",", na.strings="NA", dec=".", strip.white=TRUE,header=FALSE, check.names = FALSE)
  TableAllCurrent<-matrix(0,(nrow(CurrentTable)-1)*(ncol(CurrentTable)-2),6)
  # Get Class In Column 1
  TableAllCurrent[,2]<-as.character(CurrentTable[1,1])
  # Get Adduct In Column 2
  TableAllCurrent[,3]<-as.character(CurrentTable[1,2])
  # Repeating Lipid Name
  TableAllCurrent[,1]<-as.character(CurrentTable[2:nrow(CurrentTable),1])
  # Repeating Lipid precursor mass
  TableAllCurrent[,4]<-as.numeric(as.character(CurrentTable[2:nrow(CurrentTable),2]))
  # Fragment name and mass
  if (ncol(CurrentTable)>2) {
    Start<-1
    for (n in 3:ncol(CurrentTable)){
      #Fragment Name
      TableAllCurrent[Start:(nrow(CurrentTable)-2+Start),5]<-as.character(CurrentTable[1,n])
      #Fragment Mass
      TableAllCurrent[Start:(nrow(CurrentTable)-2+Start),6]<-as.numeric(as.character(CurrentTable[2:nrow(CurrentTable),n]))
      Start<-Start+(nrow(CurrentTable)-1)
    } 
  } else {
    TableAllCurrent[,5]<-"None"
    TableAllCurrent[,6]<-0.01
  }
  if(i>1){
    TableAll<-rbind(TableAll,TableAllCurrent)
  } else {
    TableAll<-TableAllCurrent
  }
}
write.table(TableAll, paste(DirOut, output_file, sep=""), sep=",", col.names=FALSE, row.names=FALSE, quote=TRUE, na="NA")


