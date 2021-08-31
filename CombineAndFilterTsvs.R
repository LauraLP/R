library(ggplot2)
library(ggthemes)
library(tidyverse)
library(tidyr)

summary<-function(dfile){
  dataframe <- read.delim(dfile)
  dataframe$file<-c(basename(dfile))
  dataframe$ID<-c(basename(dfile))
  dataframe<- filter(dataframe, MAF < 0.01 | is.na(MAF))
  dataframe<- filter(dataframe, Alt.Depth > 2)
  dataframe<- filter(dataframe, Consequences != 'synonymous_variant' | is.na(Consequences))
  dataframe<- filter(dataframe, Consequences != 'intron_variant' | is.na(Consequences))
  dataframe<- filter(dataframe, Consequences != 'upstream_gene_variant' | is.na(Consequences))
  dataframe<- filter(dataframe, Consequences != 'downstream_gene_variant' | is.na(Consequences) )
  dataframe<- filter(dataframe, Sift != 'tolerated' , Polyphen != 'benign')
  dataframe<-separate(data = dataframe, col = ID, into = c("Patient","sample","type", "timepoint"), sep = "\\-")
  dataframe<-separate(data = dataframe, col = file, into = c("file","variantcaller","type", "extension"), sep = "\\.")
  #vapply(strsplit(file(),"."), `[`, 1, FUN.VALUE=character(1))
  dataframe$key<-c(paste(dataframe$HGVSc,dataframe$file,dataframe$Consequences, sep='-'))
 
  
  write.table(dataframe, "/Users/lauralopezpascua/Analysis/DLBLC/CRUK/mutect_combined_filtered.tsv", sep = "\t", col.names = !file.exists("/Users/lauralopezpascua/Analysis/DLBLC/CRUK/test_combined_filtered.tsv"), append = T,row.names = FALSE,quote = FALSE)
  return(dataframe)}
filenames <- list.files("/Users/lauralopezpascua/Analysis/DLBLC/CRUK/mutect/annotation", pattern="*.tsv", full.names=TRUE)
res<-lapply(filenames,summary)
res