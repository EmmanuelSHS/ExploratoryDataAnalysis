setwd("~/Documents/cuw4701_edav/final_project/")

cleand = df[df$Created.Date>"2015-01-01",]
cleand = cleand[cleand$Created.Date<"2016-01-01",]
cleand = cleand[cleand$Agency.Name=="Department of Transportation",]
cleand = cleand[,c(1:16, 18, 25:26)]
cleand = cleand[!is.na(cleand$Created.Date),]
write.csv(cleand, "cleaned_dot_2015.csv")
