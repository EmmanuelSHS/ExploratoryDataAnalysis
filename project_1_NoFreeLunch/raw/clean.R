#Set your working directory
setwd('Documents/cuw4701_edav/')

#Load CSV file into Data Frame
df = read.csv("Survey+Response.csv")

col.list = c("Matlab", "R", "Github", "Excel", "SQL", "RStudio", "ggplot2", "shell (terminal / command line)", "C/C++", "Python", "Stata", "LaTeX", "XML", "Web: html css js", "google drive (formerly docs)", "Sweave/knitr","dropbox", "SPSS", "regular expressions (grep)", "lattice" )

#Count Columns with NAs
na.check = df%>%is.na() %>% apply(2,sum)

#Remove NAs
df_clean = df[,which(na.check==0)]

#Create colums initializing at 0
df_clean[,col.list] = 0

for(i in col.list){
  #Need an If Statement because of R vs RStudio. 
  if(i == "R"){ 
    #Use Reg expressions "R,|R$" which looks for "R," and for "R$" which means there is nothing after R (line 87 caused this issue)
    fnd = "R,|R$"
    #try to find fnd within the vector, return Row # if True
    rows = grep(pattern = fnd, x = df$Experiences.with.tools)
  }else{
    #Same as above
    fnd = paste(i, sep = "")
    rows = grep(pattern = fnd, x = df$Experiences.with.tools, fixed = TRUE)
  }
  df[rows,i] = 1
}
