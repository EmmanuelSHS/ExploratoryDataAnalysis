setwd("~/Desktop")
data=read.csv("project3.csv")
data=na.omit(data)




#colnames(crime_2010)=c('Murder','Negligent Manslaughter','Forcible Sex Offense','Nonforcible Sex Offense','Robbery','Aggravated Assault','Burglary','Motor Vehicle Theft','Arson')



##Circlize plot
library('circlize')
m=cor(crime_2010)
chordDiagram(m)


##Trellis plot

#Columbia Univerty 
Columbia_index=grepl("Columbia University",data[,2])
Columbia_data=data[Columbia_index,][,13:39]
Columbia_data=colSums(Columbia_data)
Columbia_people=data[Columbia_index,][,12]
Columbia_people=Columbia_people[1]+Columbia_people[5]

#Cornell University 
Cornell_index=grepl("Cornell University",data[,2])
Cornell_data=data[Cornell_index,][,13:39]
Cornell_people=data[Cornell_index,][,12]


#Dartmouth College 
Dartmouth_index=grepl("Dartmouth College",data[,2])
Dartmouth_data=data[Dartmouth_index,][,13:39]
Dartmouth_people=data[Dartmouth_index,][,12]

#Brown University
Brown_index=data[,2]=='Brown University'
Brown_data=data[Brown_index,][,13:39]
Brown_people=data[Brown_index,][,12]

#University of Pennsylvania 
Pennsylvania_index=data[,2]=='University of Pennsylvania'
Pennsylvania_data=data[Pennsylvania_index,][,13:39]
Pennsylvania_people=data[Pennsylvania_index,][,12]

#Harvard University
Harvard_index=grepl("Harvard University",data[,2])
Harvard_data=data[Harvard_index,][,13:39]
Harvard_data=colSums(Harvard_data)
Harvard_people=data[Harvard_index,][,12][1]

#Yale University
Yale_index=data[,2]=='Yale University'
Yale_data=data[Yale_index,][,13:39]
Yale_people=data[Yale_index,][,12]


#Princeton University
Princeton_index=grepl("Princeton University",data[,2])
Princeton_data=data[Princeton_index,][,13:39]
Princeton_data=colSums(Princeton_data)
Princeton_people=data[Princeton_index,][,12][1]

#Ivy League
Ivy_league_matrix=c(Columbia_data,
                             Cornell_data,
                             Dartmouth_data,
                             Brown_data,
                             Pennsylvania_data,
                             Harvard_data,
                             Yale_data,
                             Princeton_data)



schools=c('Columbia University',
          'Cornell University',
          'Dartmouth College',
          'Brown University',
          'University of Pennsylvania',
          'Harvard University',
          'Yale University',
          'Princeton University')


universities=rep(schools,each=27)
years=rep(rep(c(2010,2011,2012),each=9),8)
type=rep(c('Murder','Negligent Manslaughter','Forcible Sex Offense','Nonforcible Sex Offense','Robbery','Aggravated Assault','Burglary','Motor Vehicle Theft','Arson'),24)
university_people_number=c(Columbia_people,
                      Cornell_people,
                      Dartmouth_people,
                      Brown_people,
                      Pennsylvania_people,
                      Harvard_people,
                      Yale_people,
                      Princeton_people)
People_number=rep(university_people_number,each=27)
crime_per_1000_person=1000*unlist(matrix(Ivy_league_matrix,ncol=1))/People_number
  
Ivy_league_data=data.frame(universities,years,type, matrix(Ivy_league_matrix,ncol=1),People_number,crime_per_1000_person)
colnames(Ivy_league_data)=c('University','Year','type','number',"number_of_people",'crime_per_1000_people')

##Trellis plot of total crime number
library(lattice)
dotplot(type~number|University,data=Ivy_league_data,group=Year,pch=c(1,2,3),col=c('blue','red','orange'),layout=c(2,4),main='Ivy League Crime Investigation',ylab='Crime Type',
        key=list(space='right',transparent=T,points=list(pch=c(1,2,3),col=c('blue','red','orange')),text=list(c('2010','2011','2012'))))

#seperated for each year
dotplot(type~number|University,data=Ivy_league_data[Ivy_league_data$Year==2010,],layout=c(2,4),main='Ivy League 2010 Crime Investigation',ylab='Crime Type')
dotplot(type~number|University,data=Ivy_league_data[Ivy_league_data$Year==2011,],layout=c(2,4),main='Ivy League 2011 Crime Investigation',ylab='Crime Type')
dotplot(type~number|University,data=Ivy_league_data[Ivy_league_data$Year==2012,],layout=c(2,4),main='Ivy League 2012 Crime Investigation',ylab='Crime Type')

##Trellis plot of crime per 1000 people
dotplot(type~crime_per_1000_people|University,data=Ivy_league_data,group=Year,pch=c(1,2,3),col=c('blue','red','orange'),layout=c(2,4),main='Ivy League Crime Investigation',ylab='Crime Type',
        key=list(space='right',transparent=T,points=list(pch=c(1,2,3),col=c('blue','red','orange')),text=list(c('2010','2011','2012'))))

#seperated for each year
dotplot(type~crime_per_1000_people|University,data=Ivy_league_data[Ivy_league_data$Year==2010,],layout=c(2,4),main='Ivy League 2010 Crime Investigation',ylab='Crime Type')
dotplot(type~crime_per_1000_people|University,data=Ivy_league_data[Ivy_league_data$Year==2011,],layout=c(2,4),main='Ivy League 2011 Crime Investigation',ylab='Crime Type')
dotplot(type~crime_per_1000_people|University,data=Ivy_league_data[Ivy_league_data$Year==2012,],layout=c(2,4),main='Ivy League 2012 Crime Investigation',ylab='Crime Type')


##explore NYC crime
nyc_data=read.csv("Felony.csv")
nyc_data=na.omit(nyc_data)
nyc_data=nyc_data[nyc_data$OccurrenceYear==2015,]
data1=nyc_data[,c(5,12,16)]
district=rownames(table(data1$Borough))[-1]
occur_month=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep')
crime_type=c('GRAND LARCENY','FELONY ASSAULT','ROBBERY','BURGLARY')

number_list=c()
i=1
for (x in occur_month){
  for (y in crime_type){
    for (z in district){
      number=sum(data1$OccurrenceMonth==x & data1$Offense==y & data1$Borough==z)
      number_list[i]=number
      i=i+1
    }
  }
}

month_list=rep(c(1:9),each=20)
crime_type_list=rep(rep(crime_type,each=5),9)
district_list=rep(district,36)
data1=data.frame(format(month_list,format="b%"),crime_type_list,district_list,number_list)
colnames(data1)=c('Month','Crime_Type','District','Number')

library(ggplot2)
library(reshape)

ggplot(data1, aes(x = Month, y=Number, fill = District)) +
  geom_bar(width = 1, position="identity", stat="identity", color="black") + 
  scale_y_sqrt() + #facet_grid: Lay out panels in a grid
  facet_grid(. ~ Crime_Type, scales="free",labeller = label_both) + coord_polar(start=3*pi/2) +
  ggtitle("2015 January-September NYC Crime Investigation") + 
  scale_fill_brewer(palette = "RdYlGn")+
  xlab("")



