klibrary(ggplot2)
library(dplyr)
library(plyr)
library(lubridate)
dataset<-read_csv("USvideos.csv")

#fixing the dataset

dataset1<-distinct(dataset,title,.keep_all=TRUE)

dataset$category_id[dataset$category_id=="1"]<-"Film & Animation"
dataset$category_id[dataset$category_id=="2"]<-"Autos & Vehicles"
dataset$category_id[dataset$category_id=="10"]<-"Music"
dataset$category_id[dataset$category_id=="15"]<-"Pets & Animals"
dataset$category_id[dataset$category_id=="17"]<-"Sports"
dataset$category_id[dataset$category_id=="18"]<-"Short Movies"
dataset$category_id[dataset$category_id=="19"]<-"Travel & Events"
dataset$category_id[dataset$category_id=="20"]<-"Gaming"
dataset$category_id[dataset$category_id=="21"]<-"Videoblogging"
dataset$category_id[dataset$category_id=="22"]<-"People & Blogs"
dataset$category_id[dataset$category_id=="23"]<-"Comedy"
dataset$category_id[dataset$category_id=="24"]<-"Entertainment"
dataset$category_id[dataset$category_id=="25"]<-"News & Politics"
dataset$category_id[dataset$category_id=="26"]<-"Howto & Style"
dataset$category_id[dataset$category_id=="27"]<-"Education"
dataset$category_id[dataset$category_id=="28"]<-"Science & Technology"
dataset$category_id[dataset$category_id=="29"]<-"Nonprofits & Activism"
dataset$category_id[dataset$category_id=="30"]<-"Movies"
dataset$category_id[dataset$category_id=="31"]<-"Anime/Animation"
dataset$category_id[dataset$category_id=="32"]<-"Action/Adventure"
dataset$category_id[dataset$category_id=="33"]<-"Classics"
dataset$category_id[dataset$category_id=="34"]<-"Comedy"
dataset$category_id[dataset$category_id=="35"]<-"Documentary"
dataset$category_id[dataset$category_id=="36"]<-"Drama"
dataset$category_id[dataset$category_id=="37"]<-"Family"
dataset$category_id[dataset$category_id=="38"]<-"Foreign"
dataset$category_id[dataset$category_id=="39"]<-"Horror"
dataset$category_id[dataset$category_id=="40"]<-"Sci-Fi/Fantasy"
dataset$category_id[dataset$category_id=="41"]<-"Thriller"
dataset$category_id[dataset$category_id=="42"]<-"Shorts"
dataset$category_id[dataset$category_id=="43"]<-"Shows"
dataset$category_id[dataset$category_id=="44"]<-"Trailers"



#basic statistics
nrow(dataset1)

table(dataset1$category_id)
popularity<-pie((table(dataset1$category_id)))

summary(dataset1)


sd(dataset$views)
var(dataset$views)

sd(dataset$likes)
var(dataset$likes)

sd(dataset$comment_count)
var(dataset$comment_count)


ggplot(dataset1,aes(views))+geom_histogram()+xlim(0,2e+07)+ylim(0,1700)

#Linear model
linearmodel<-lm(views~likes,data=dataset1)
linearmodel2<-lm(views~comment_count,data=dataset1)
summary(linearmodel)
plot(linearmodel)

summary(linearmodel2)
plot(linearmodel2)

ggplot(dataset1,aes(likes,views))+geom_point()+geom_smooth(method="lm")
ggplot(dataset1,aes(comment_count,views))+geom_point()+geom_smooth(method="lm")
#data summary

#More data summary
attach(dataset1)
bestviews<-dataset1[order(-views),]
bestviews$title[1:15]
bestviews$views[1:15]

bestviews$category_id[1:15]
popularityview<-pie((table(bestviews$category_id[1:15])))


bestlikes<-dataset1[order(-likes),]
bestlikes$title[1:15]
bestlikes$category_id[1:15]
pie(table(bestlikes$category_id[1:15]))


mostcomments<-dataset1[order(-comment_count),]
mostcomments$title[1:15]
mostcomments$category_id[1:15]
pie(table(mostcomments$category_id[1:15]))

ggplot(dataset1, aes(views,likes)) + 
  geom_point(aes(color=category_id),size=1)




ent=dataset1 %>% filter(category_id=='Entertainment')
mus=dataset1 %>% filter(category_id=='Music')


summary(ent)
summary(mus)
t.test(ent$views,mus$views,mu=0)




#It seems that entertainment is most popular in trending section with music coming in second place.





