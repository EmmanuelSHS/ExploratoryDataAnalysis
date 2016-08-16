randomName<-function(n=1,syllables=3){
  vowels<-c("a","e","i","o","u","y")
  consonants<-setdiff(letters,vowels)
  replicate(n,
            paste(
              rbind(sample(consonants,syllables,replace=TRUE),
                    sample(vowels,syllables,replace=TRUE)),
              sep='',collapse='')
  )
}

set.seed(42)

nFamily<-20
nItemPerFamily<-sample(1:6,nFamily,replace=TRUE)
nValues<-3

df_test<-data.frame(
  family=rep(randomName(nFamily),nItemPerFamily),
  item=randomName(sum(nItemPerFamily),2))

df_test<-cbind(df_test,as.data.frame(matrix(runif(nrow(df_test)*nValues),nrow=nrow(df_test),ncol=nValues)))


df_test<-melt(df_test,c("family","item"),variable_name="score") # from wide to long
p<-polarHistogram(df_test,familyLabel=FALSE)
print(p)
