library(readr)
library(plyr)
library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)

#Decoding done base on table provided by Goldman and Birney
dna_encoding <- list("A" = c("C" = 0,"G" = 1,"T" = 2),
                     "C" = c("G" = 0,"T" = 1,"A" = 2),
                     "G" = c("T" = 0,"A" = 1,"C" = 2),
                     "T" = c("A" = 0,"C" = 1,"G" = 2))

huff3_cd <- read_delim("data/View_huff3.cd.new.correct", 
                       delim = "\t", escape_double = FALSE, 
                       col_names = FALSE, na = "NA", comment = "--", 
                       trim_ws = TRUE)

decode_dna <- function(input){
  #Generate a vector using input
  input <- str_split(input, pattern = "")[[1]]
  #Set initial nucleotide as A
  last_nt <- "A"
  #Now convert each charater into a number
  s4 <- c()
  for(i in 1:length(input)){
    #Convert the number based on the last nucleotide
    s4 <- c(s4,dna_encoding[[last_nt]][input[i]])
    #Set new nt for next loop
    last_nt <- input[i]
  }
  
  ### Get length of message 
  s2 <- s4[1:25]
  #Check where 000s stop and reverse order
  s2 <- s2[25:min(which(s2 != 0))]
  #Convert back to base 10
  for(i in 1:length(s2)){s2[i] = s2[i] * 3^(i-1)}
  n <- sum(s2)
  #Use the information to extract only the meaningful part of the message
  s1 <- s4[26:(25 + n )]
  
  ###Now decode message 
  decoded_message <- data.frame(dna=character(),base3=character(),decoded=character())
  #Run a while loop to identify the characters based on the table
  while(length(s1) >4){
    #Get first 5 and 6 elements
    five_numbers <- paste(s1[1:5],collapse = "") 
    six_numbers <- paste(s1[1:6],collapse = "") 
    #Check if code is on table
    if(five_numbers %in%  huff3_cd$X4){
      #Save info
      decoded_message[nrow(decoded_message) +1,] <- c(paste(names(s1[1:5]),collapse = ""),huff3_cd[huff3_cd$X4 == five_numbers,c("X4","X2")])
      #Delete already decoded elements
      s1 <- s1[-(1:5)]
      }else{
        #Save info
        decoded_message[nrow(decoded_message) +1,] <- c(paste(names(s1[1:6]),collapse = ""),huff3_cd[huff3_cd$X4 == six_numbers,c("X4","X2")])
        #Delete already decoded elements
        s1 <- s1[-(1:6)]
      }
  }
  decoded_message$position <- 1:nrow(decoded_message)
  #Create bins to spread the message in case is too big
  decoded_message$bin <- cut(decoded_message$position,breaks = seq(0,nrow(decoded_message)+26,by=26))
  #Change shape to long format be used for plotting
  decoded_message <- melt(decoded_message, id.vars = c("position","bin"), 
                          variable.name = "Code", 
                          value.name = "Value")
  decoded_message$Code <- factor(decoded_message$Code,levels=c("dna","base3","decoded"))
  return(decoded_message)
}


# message_decoded <- decode_dna(input = "CGTACGTACGTACGTACGTAGTCGCACTACACAGTCGACTACGCTGTACTGCAGAGTGCTGTCTCACGTGATGACGTGCTGCATGATATCTACAGTCATCGTCTATCGAGATACGCTACGTACGT")
# message_decoded <- decode_dna(input = "CGTACGTACGTACGTACGACGACGTGCATCATATCGTCTGTGTACTATGATCGCAGCGCGTGTACTATGATACGCTCGTATCTATCATAGTGTACTCGACTATGATATCTATATATATGATGTGTAGTACAGACAGTCTCACGTGACATCAGTGCACGCGATATCGACATGACATGATCGCTAGTCGCTCGACATCGCAGCTGACAGTCGACAGACTATGATACGATATCGTACAGACATGTGCTGACATCACACGTCTCAGTGCTATGATACGATATCTACGTGAGTGCTAGAGCACAGTACGCTATGATCGCAGCGCGTGTACTATGATATATGTGTGCGATCGACATGCGTAGTCGATCGTATGTACTACGCTACGATGTACTATGATATCTATATATATGATCGCAGCGCGTGTACTATGATCGCATGTGTACAGACTAGTCTATGATACGAGTGCAGTGCTATGATGTGTAGTACAGACAGTCTCACGTGACATCAGTGCACGCGATATCGACATGACATGATCGCTAGTCGCTCGACATCGCAGCTGACAGTCGACAGACTATGATACGATATCGTACAGTGCTGACATCGTGTCACAGTACGATGTACTATGATGTGACGTATCGCAGCGAGATATGATACACTGTGACGTATCATCGTATCTACGCGACATCACAGACACACACATCACTAGAGTCATCGATGTGCTGTCTCAGACAGCGCTACGTGACAGACGCAGCGAGATACGATGTGACATCGTCGCATCGTCTGCGTCGTACGTACGTACGTACGTA")


# ggplot(message_decoded,aes(x=position,y=Code))+
#   geom_tile(aes(fill = Code),color="black")+ #width=2, height=1
#   geom_text(aes(size= Code,label = Value))+
#   scale_size_manual(values=c(3,3,6))+
#   scale_fill_manual(values=c("lightgreen","#FF8488","lightblue")) +
#   theme_minimal() +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         # axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         legend.position = "none",
#         strip.background = element_blank(),
#         strip.text.x = element_blank())+
#   facet_wrap(~bin,scales = "free",ncol = 1)
