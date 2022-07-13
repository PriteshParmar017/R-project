library(tidyverse)
library(ggplot2)

data<-read_csv('C:/Users/Admin/Downloads/Salary_Dataset_with_Extra_Features.csv')

out<-data%>%arrange(desc(Salary))
out%>%head()

data<-data%>%filter(Salary!=90000000)%>%arrange(desc(Salary))
ggplot(data)+geom_point(aes(Rating,Salary))
ggplot(data)+geom_boxplot(aes(as.character(Rating),Salary))

data%>%group_by(Rating)%>%summarize(n=n())%>%ggplot()+geom_bar(aes(Rating,n),stat='identity')

data%>%filter(Salary>=5000000,`Company Name`!='Amazon')%>%
  ggplot()+geom_bar(aes(reorder(`Company Name`,Salary,median),Salary,fill=Salary),stat='identity')+
  coord_flip()+labs(title='Company salary',x='Company Name')+
  scale_fill_gradient2(low = "white",high = "blue")+guides(fill=F)

table(data['Location'])
avg_sly<-data%>%group_by(`Job Title`)%>%
  summarize(avg_salary=sum(Salary)/n())
avg_sly<-avg_sly%>%filter(avg_salary>=4000000)%>%arrange(desc(avg_salary))
ggplot(avg_sly)+
  geom_bar(aes(reorder(`Job Title`,avg_salary,median),avg_salary,fill=avg_salary),stat='identity')+
  coord_flip()+labs(title='job title top average salary',x='job title')+guides(fill=F)


data%>%group_by(`Job Roles`)%>%
  summarize(avg_salary=sum(Salary)/n())%>%ggplot()+
  geom_bar(aes(reorder(`Job Roles`,avg_salary,median),avg_salary,fill=`Job Roles`),stat='identity')+
  coord_flip()+labs(title='job roles top average salary',x='job roles')+guides(fill=F)
data%>%group_by(`Job Roles`)%>%
  summarize(avg_salary=sum(Salary)/n(),n=n())


data%>%group_by(`Employment Status`)%>%
  summarize(avg_salary=sum(Salary)/n())%>%
  ggplot()+geom_bar(aes(`Employment Status`,avg_salary,fill=`Employment Status`),stat = 'identity')+
  guides(fill=F)+labs(title='salary of each status')

dt_location<-data%>%group_by(Location)%>%
  summarize(avg_salary=sum(Salary)/n(),avg_rating=round(sum(Rating)/n(),1),n=n())%>%
  arrange(desc(avg_salary))
dt_location<-dt_location%>%mutate(percent=round(n/sum(n),3)*100)
dt_location%>%head(10)
ggplot(dt_location,aes(x=percent,y='',fill=Location))+
  geom_bar(stat = 'identity',position = 'stack')+coord_polar()+
  geom_text(aes(label = percent),
            position = position_stack(vjust = 0.5))+
  labs(title='percent of every location')