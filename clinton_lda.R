#these are the emails released by Hillary Clinton during her time as secretary of state
#obtained from kaggle

emails<-read.csv('emails.csv')
library(RTextTools)
library(topicmodels)
library(tm)
library(plyr)

#remove common stop words that show up in emails
remove_sw<-function(sw,dtmatrix){
  q<-sapply(sw, function(x,w){ #apply function over list of stop words
    ifelse(any(x==w),which(x==w), NA)#find indices where stop words are
  },w=colnames(dtmatrix)) 
  return(dtmatrix[,-c(na.omit(q))])} #return the dtmatrix minus the omitted (stop) words

#create custom stop word list
sw<-c('one','new','american','percent','time','say','will','state','govern',
      'offic','said','fyi','see','can','call','get','want','know','like','think','just',
      'tomorrow','unit','meet','work','want','also','now','thank','updat','year','subject',
      'doc','agreement','may','back','need','today','talk','good','date','right','draft','email','clinton')

#create document-term matrix
emails$ExtractedSubject<-as.vector(emails$ExtractedSubject)
emails$ExtractedBodyText<-as.vector(emails$ExtractedBodyText)
email_matrix<-create_matrix(c(emails$ExtractedSubject,emails$ExtractedBodyText),language='english',removePunctuation=TRUE,stemWords=TRUE,removeNumbers = TRUE,toLower=TRUE)

#remove custom stop words
email_matrix_2<-remove_sw(sw,email_matrix)

#remove empty documents
rowTotal<-apply(email_matrix_2,1,sum)
email_matrix_3<-email_matrix_2[rowTotal>0,]

#create Latent Dirichlet Allocation topic model
lda.1<-LDA(email_matrix_3,5)
terms(lda.1,10)
#Topic 1       Topic 2   Topic 3  Topic 4     Topic 5     
#[1,] "secur"       "sid"     "israel" "secretari" "obama"     
#[2,] "afghanistan" "cheryl"  "presid" "depart"    "republican"
#[3,] "haiti"       "speech"  "obama"  "room"      "democrat"  
#[4,] "report"      "huma"    "world"  "hous"      "parti"     
#[5,] "offici"      "let"     "peopl"  "arriv"     "vote"      
#[6,] "afghan"      "ask"     "women"  "benghazi"  "senat"     
#[7,] "pakistan"    "monday"  "polici" "rout"      "presid"    
#[8,] "militari"    "pls"     "isra"   "privat"    "hous"      
#[9,] "mcchrystal"  "discuss" "china"  "inform"    "elect"     
#[10,] "forc"        "mill"    "peac"   "confer"    "polit"     

#topic 1:middle east and china

#topic 2:israel/palestinian conflict

#topic 3:congressional vote/benghazi

#topic 4:scheduling/logistics

#topic 5:developing nations + social programs


#TO DO:
#find frequency of topics per email in corpus

#cbind to original csv

#graph frequency of topic over dates