#Author: Jeremy Koelmel
#Date: 05/05/2018
## This part of the program reformats all libraries into 1.

##remove all R objects from memory
rm(list = ls())
ppm_Window<-10
PPM_CONST <- (10^6 + ppm_Window/2) / 10^6

# Note looks for a Output/AllLisNeg.csv /Pos.csv folder in this folder
Directory<-"C:/Users/Jeremy/Desktop/Desktop/ForOthers/Bowden/2018_4_14_FragmentPaper/Software_Fragments_Precursor"
Dir_Out_In<-paste(Directory,"/Output/",sep="")
DirLibraryNeg<-paste(Directory,"/Output/AllLibsNeg.csv",sep="")
DirLibraryPos<-paste(Directory,"/Output/AllLibsPos.csv",sep="")
PosTRUE<-TRUE
#Code to combine all libraries into one library for each polarity
if (PosTRUE==FALSE){
  DirLib<-DirLibraryNeg
  output_file<-"PrecFragOverlap_Neg.csv"
} else {
  DirLib<-DirLibraryPos
  output_file<-"PrecFragOverlap_Pos.csv"
}
#Just for testing
#Library<-read.csv("C:/Users/Jeremy/Desktop/Desktop/ForOthers/Bowden/2018_4_14_FragmentPaper/Software_Fragments_Precursor/Output/AllLibsNegLPL_test.csv",sep=",", na.strings="NA", dec=".", strip.white=TRUE,header=FALSE)
# Read in library of all precursors and fragments
Library<-read.csv(DirLib,sep=",", na.strings="NA", dec=".", strip.white=TRUE,header=FALSE)
Library<-as.matrix(Library)
#Make a library of only unique precursors 
PrecLibrary<-Library[which(!duplicated(Library[,1])),1:4]
PrecLibrary<-cbind(PrecLibrary,matrix(0,nrow(PrecLibrary),4))
#Make a library of only unique Fragments
FragLibrary<-Library[which(!duplicated(Library[,c(2,3,5,6)])),c(1,5,6)]
#Remove library (takes up a tone of space)
remove(Library)
PrecMZs<-as.numeric(as.character(PrecLibrary[,4]))
FragMZs<-as.numeric(as.character(FragLibrary[,3]))
PrecNames<-as.character(FragLibrary[,1])
FragNames<-as.character(FragLibrary[,2])
i <- 1 #for test
for (i in 1:nrow(PrecLibrary)){
  MZ_Conditional <- (FragMZs - abs(FragMZs - FragMZs*PPM_CONST) < PrecMZs[i]) * (PrecMZs[i] < (FragMZs + abs(FragMZs - FragMZs*PPM_CONST)))
  MatchedIndex<-which(MZ_Conditional == 1)
  ppmError<-10^6*(abs(PrecMZs[i]-FragMZs[MatchedIndex])/PrecMZs[i])
  PrecLibrary[i,5]<-paste(PrecNames[MatchedIndex],collapse=" | ")
  PrecLibrary[i,6]<-paste(FragNames[MatchedIndex],collapse=" | ")
  PrecLibrary[i,7]<-paste(FragMZs[MatchedIndex],collapse=" | ")
  PrecLibrary[i,8]<-paste(ppmError,collapse=" | ")
}
print(PrecOverlap<-(sum(PrecLibrary[,5]!="")/nrow(PrecLibrary))*100)

write.table(PrecLibrary, paste(Dir_Out_In, output_file, sep=""), sep=",", col.names=FALSE, row.names=FALSE, quote=TRUE, na="NA")


