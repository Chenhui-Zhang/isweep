##Trivium Property sweeper##
##Developed by Chenhui Zhang##
##to make sweeping faster##
##Only used for the purpose of daily sweeping##
##Not all sites on the sweeping list are included due to the constraints of their crawler agreements
##Any change of the original website may cause bugs
##As of 2022-03-04, it works well :)
##Love you all##
#Read me======================================================
#Please install packages for the first time
#eg. uncomment the line below and run
#install.packages("rvest")
library(rvest)
library(readr)
library(stringr)
library(tidyverse)
library(dplyr)
library(utils)
library(purrr)
library(tibble)
library(forcats)
#Set the working directory to the folder containing this file
#You will find the final output file in the same folder

setwd("D:/Trivium/isweep")

#Input the date====

#Change the numbers to the date of today
#Remember the "0", it's "03" not "3"
date<-data.frame(Year="2022",Month="03",Day="29")

#Then 
#Ignore everything below and run==============================

date$Date<-paste(date$Year,date$Month,date$Day,sep="-")
#The Paper====
#Get the entire webpage
url_thepaper<-"https://www.thepaper.cn/list_25433"
web_thepaper<-read_html(url_thepaper)
#Equivalent to 
#web<-read_html("https://www.thepaper.cn/list_25433")
#Get title
news_thepaper<-web_thepaper%>%html_nodes("div.news_li h2 a")
title_thepaper<-news_thepaper%>%html_text()
#Inspect the title
#head(title_thepaper,10)
#Get link
link_thepaper<-news_thepaper%>%html_attrs()
#head(link_thepaper,10)
link_thepaper1<-c(1:length(link_thepaper))
for (i in 1:length(link_thepaper)){
  link_thepaper1[i]<-link_thepaper[[i]][1]
}
#Combine newsforward with link1 to get detailed link2
link_thepaper2<-paste("https://www.thepaper.cn/",link_thepaper1,sep="")
#Get date and time
adate<-as.data.frame(c(1:length(link_thepaper2)))
for (i in 1:length(link_thepaper2)){
  alink<-link_thepaper2[i]
  aweb<-read_html(alink)
  anews<-aweb%>%html_nodes("div.newscontent div.news_about p")
  anews<-anews[2]
  atext<-html_text(anews)
  aaaaa<-as.character(as.Date(str_extract(atext,"\\d{4}-\\d{2}-\\d{2}"), format="%Y-%m-%d"))
  adate[i,]<-aaaaa
}
names(adate)<-c("Date")
adf<-data.frame(Title=title_thepaper,Link=link_thepaper2,Date=adate)
df_thepaper<-data.frame(Title=adf$Title,Link=adf$Link)[which(adf$Date==date$Date),]
df<-df_thepaper


#MoHURD====
#1
url_mohurd<-"https://www.mohurd.gov.cn/xinwen/gzdt/index.html"
web_mohurd<-read_html(url_mohurd)
news_mohurd<-web_mohurd%>%html_nodes("div.linkList-section-wrapper ul li a")
title_mohurd<-news_mohurd%>%html_text()
link_mohurd<-news_mohurd%>%html_attrs()
link_mohurd1<-c(1:length(link_mohurd))
for (i in 1:length(link_mohurd)){
  link_mohurd1[i]<-link_mohurd[[i]][1]
}

#Get date
adate_mh<-web_mohurd%>%html_nodes("div.linkList-section-wrapper ul li span.date-info")
adate_mh2<-adate_mh%>%html_text()
adf_mh<-data.frame(Title=title_mohurd,Link=link_mohurd1,Date=adate_mh2)
df_mohurd1<-data.frame(Title=adf_mh$Title,Link=adf_mh$Link)[which(adf_mh$Date==date$Date),]
#2
url_mohurd2<-"https://www.mohurd.gov.cn/xinwen/dfxx/index.html"
web_mohurd2<-read_html(url_mohurd2)
news_mohurd2<-web_mohurd2%>%html_nodes("div.linkList-section-wrapper a")
title_mohurd2<-news_mohurd2%>%html_text()
link_mohurd2<-news_mohurd2%>%html_attrs()
link_mohurd22<-c(1:length(link_mohurd2))
for (i in 1:length(link_mohurd2)){
  link_mohurd22[i]<-link_mohurd2[[i]][1]
}
middf<-data.frame(title_mohurd2,link_mohurd22)
adf_mh2<-head(middf,-14)
#Get date
adate_mh21<-web_mohurd2%>%html_nodes("div.linkList-section-wrapper ul li span.date-info")
adate_mh22<-adate_mh21%>%html_text()
adf_mh3<-data.frame(Title=adf_mh2$title_mohurd2,Link=adf_mh2$link_mohurd22,Date=adate_mh22)
df_mohurd2<-data.frame(Title=adf_mh3$Title,Link=adf_mh3$Link)[which(adf_mh3$Date==date$Date),]

df_mohurd<-rbind(df_mohurd1,df_mohurd2)
df_mohurd$Title<-str_trim(df_mohurd$Title,"both")
df<-rbind(df,df_mohurd)

#Yicai====
url_yicai<-"https://www.yicai.com/news/loushi/"
web_yicai<-url_yicai%>%read_html()
news_yicai<-web_yicai%>%html_nodes("div.m-con h2")
title_yicai<-news_yicai%>%html_text()
link_yicai<-web_yicai%>%html_nodes("div.m-con a")
link_yicai1<-link_yicai%>%html_attrs()
link_yicai2<-c(1:length(link_yicai1))
for (i in 1:length(link_yicai1)){
  link_yicai2[i]<-link_yicai1[[i]][1]
}
link_yicai3<-paste("https://www.yicai.com",link_yicai2,sep="")
#Get date
adate_yc<-web_yicai%>%html_nodes("div.author.f-cb span")
adate_yc1<-adate_yc%>%html_text()
adate_yc2<-as.character(as.Date(str_extract(adate_yc1,"\\d{2}-\\d{2}"), format="%m-%d"))
adf_yc<-data.frame(Title=title_yicai,Link=link_yicai3,Date=adate_yc2)
df_yicai<-data.frame(Title=adf_yc$Title,Link=adf_yc$Link)[which(adf_yc$Date==date$Date | is.na(adf_yc$Date)==TRUE),]
df<-rbind(df,df_yicai)

#21jingji====
url_21jingji<-"http://www.21jingji.com/channel/Property/"
web_21jingji<-url_21jingji%>%read_html()
news_21jingji<-web_21jingji%>%html_nodes("div.news a h2")
title_21jingji<-news_21jingji%>%html_text()
link_21jingji<-web_21jingji%>%html_nodes("div.news a")
link_21jingji1<-link_21jingji%>%html_attrs()
link_21jingji2<-c(1:length(link_21jingji1))
for (i in 1:length(link_21jingji1)){
  link_21jingji2[i]<-link_21jingji1[[i]][1]
}
#Get date
adate_21jingji<-web_21jingji%>%html_nodes("div.title span")
adate_21jingji1<-adate_21jingji%>%html_text()
adate_21jingji2<-as.character(as.Date(str_extract(adate_21jingji1,"\\d{2}-\\d{2}-\\d{2}"), format="%y-%m-%d"))
adf_21jingji<-data.frame(Title=title_21jingji,Link=link_21jingji2,Date=adate_21jingji2)
df_21jingji<-data.frame(Title=adf_21jingji$Title,Link=adf_21jingji$Link)[which(adf_21jingji$Date==date$Date),]
df<-rbind(df,df_21jingji)

#gov.cn====
url_govcn<-"http://sousuo.gov.cn/s.htm?t=govall&advance=false&n=&timetype=&mintime=&maxtime=&sort=&q=%E6%88%BF"
web_govcn<-url_govcn%>%read_html()
news_govcn<-web_govcn%>%html_nodes("div.result h3 a")
title_govcn<-news_govcn%>%html_text()
link_govcn<-news_govcn%>%html_attrs()
link_govcn1<-c(1:length(link_govcn))
for (i in 1:length(link_govcn)){
  link_govcn1[i]<-link_govcn[[i]][1]
}
#Get date
adate_govcn<-web_govcn%>%html_nodes("div.result span")
adate_govcn1<-adate_govcn%>%html_text()
adate_govcn2<-str_extract(adate_govcn1,"\\d{4}.\\d{2}.\\d{2}")
adate_govcn3<-gsub("\\.","-",adate_govcn2)
adate_govcn4<-na.omit(adate_govcn3)
adate_govcn5<-head(adate_govcn4,-4)
adf_govcn<-data.frame(Title=title_govcn,Link=link_govcn1)
adf_govcn1<-adf_govcn[which(grepl("http",adf_govcn$Link,fixed = TRUE)),]
adf_govcn2<-data.frame(Title=adf_govcn1$Title,Link=adf_govcn1$Link,Date=adate_govcn5)
df_govcn<-data.frame(Title=adf_govcn2$Title,Link=adf_govcn2$Link)[which(adf_govcn2$Date==date$Date),]
df<-rbind(df,df_govcn)

#infzm====
#1
url_infzm1<-"http://www.infzm.com/search?k=%E6%88%BF%E5%9C%B0"
web_infzm1<-url_infzm1%>%read_html()
news_infzm1<-web_infzm1%>%html_nodes("div.nfzm-content-item__inner h5")
title_infzm1<-news_infzm1%>%html_text()
link_infzm1<-web_infzm1%>%html_nodes("ul.nfzm-list.ui-line li a")
link_infzm2<-link_infzm1%>%html_attrs()
link_infzm3<-paste("http://www.infzm.com",link_infzm2,sep="")
adate_infzm<-c(1:length(link_infzm3))
for (i in 1:length(link_infzm3)){
  link_infzm4<-link_infzm3[i]
  aweb_infzm<-link_infzm4%>%read_html()
  adate_infzm1<-aweb_infzm%>%html_nodes("div.nfzm-content__meta span.nfzm-content__publish")
  adate_infzm1<-adate_infzm1%>%as.character()
  adate_infzm[i]<-str_extract(adate_infzm1,"\\d{4}-\\d{2}-\\d{2}")
}
adf_infzm<-data.frame(Title=title_infzm1,Link=link_infzm3,Date=adate_infzm)

df_infzm1<-data.frame(Title=adf_infzm$Title,Link=adf_infzm$Link)[which(adf_infzm$Date==date$Date),]
names(df_infzm1)<-c("Title", "Link")
df_infzm1$Title<-str_trim(df_infzm1$Title,"both")
df<-rbind(df,df_infzm1)

#dayoo====
#1
url_dayoo1<-"https://life.dayoo.com/house/154823.shtml"
web_dayoo1<-url_dayoo1%>%read_html()
news_dayoo1<-web_dayoo1%>%html_nodes("div.news-item h2 a")
title_dayoo1<-news_dayoo1%>%html_text()
link_dayoo1<-news_dayoo1%>%html_attrs()
link_dayoo11<-c(1:length(link_dayoo1))
for (i in 1:length(link_dayoo1)){
  link_dayoo11[i]<-link_dayoo1[[i]][1]
}
adate_dayoo<-web_dayoo1%>%html_nodes("div.news-item div.news-time")
adate_dayoo1<-adate_dayoo%>%html_text()
adate_dayoo2<-str_extract(adate_dayoo1,"\\d{4}-\\d{2}-\\d{2}")
adf_dayoo<-data.frame(Title=title_dayoo1,Link=link_dayoo11,Date=adate_dayoo2)

df_dayoo1<-data.frame(Title=adf_dayoo$Title,Link=adf_dayoo$Link)[which(adf_dayoo$Date==date$Date),]
#2
url_dayoo2<-"https://life.dayoo.com/house/154824.shtml"
web_dayoo2<-url_dayoo2%>%read_html()
news_dayoo2<-web_dayoo2%>%html_nodes("div.news-item h2 a")
title_dayoo2<-news_dayoo2%>%html_text()
link_dayoo2<-news_dayoo2%>%html_attrs()
link_dayoo21<-c(1:length(link_dayoo2))
for (i in 1:length(link_dayoo2)){
  link_dayoo21[i]<-link_dayoo2[[i]][1]
}
adate_dayoo3<-web_dayoo2%>%html_nodes("div.news-item div.news-time")
adate_dayoo4<-adate_dayoo3%>%html_text()
adate_dayoo5<-str_extract(adate_dayoo4,"\\d{4}-\\d{2}-\\d{2}")
adf_dayoo2<-data.frame(Title=title_dayoo2,Link=link_dayoo21,Date=adate_dayoo5)
df_dayoo2<-data.frame(Title=adf_dayoo2$Title,Link=adf_dayoo2$Link)[which(adf_dayoo2$Date==date$Date),]
df_dayoo<-rbind(df_dayoo1,df_dayoo2)
df<-rbind(df,df_dayoo)

#chinajsb====
#1
url_chinajsb1<-"http://m.chinajsb.cn/show.php?s=/Search/index&value=%E6%88%BF%E5%9C%B0"
web_chinajsb1<-url_chinajsb1%>%read_html()
news_chinajsb1<-web_chinajsb1%>%html_nodes("div.acticleList ul li a p.title")
title_chinajsb1<-news_chinajsb1%>%html_text()
link_chinajsb1<-web_chinajsb1%>%html_nodes("div.acticleList li a")
link_chinajsb11<-link_chinajsb1%>%html_attrs()
link_chinajsb12<-c(1:length(link_chinajsb11))
for (i in 1:length(link_chinajsb11)){
  link_chinajsb12[i]<-link_chinajsb11[[i]][1]
}
adate_chinajsb<-web_chinajsb1%>%html_nodes("div.acticleList ul li p.time")
adate_chinajsb1<-adate_chinajsb%>%html_text()
adate_chinajsb2<-str_extract(adate_chinajsb1,"\\d{4}-\\d{2}-\\d{2}")
adf_chinajsb<-data.frame(Title=title_chinajsb1,Link=link_chinajsb12,Date=adate_chinajsb2)

df_chinajsb1<-data.frame(Title=adf_chinajsb$Title,Link=adf_chinajsb$Link)[which(adf_chinajsb$Date==date$Date),]
#2
url_chinajsb2<-"http://m.chinajsb.cn/show.php?s=/Search/index&value=%E6%88%BF%E5%9C%B0%E4%BA%A7"
web_chinajsb2<-url_chinajsb2%>%read_html()
news_chinajsb2<-web_chinajsb2%>%html_nodes("div.acticleList ul li a p.title")
title_chinajsb2<-news_chinajsb2%>%html_text()
link_chinajsb2<-web_chinajsb2%>%html_nodes("div.acticleList li a")
link_chinajsb21<-link_chinajsb2%>%html_attrs()
link_chinajsb22<-c(1:length(link_chinajsb21))
for (i in 1:length(link_chinajsb21)){
  link_chinajsb22[i]<-link_chinajsb21[[i]][1]
}
adate_chinajsb3<-web_chinajsb2%>%html_nodes("div.acticleList ul li p.time")
adate_chinajsb4<-adate_chinajsb3%>%html_text()
adate_chinajsb5<-str_extract(adate_chinajsb4,"\\d{4}-\\d{2}-\\d{2}")
adf_chinajsb2<-data.frame(Title=title_chinajsb2,Link=link_chinajsb22,Date=adate_chinajsb5)
df_chinajsb2<-data.frame(Title=adf_chinajsb2$Title,Link=adf_chinajsb2$Link)[which(adf_chinajsb2$Date==date$Date),]
df_chinajsb<-rbind(df_chinajsb1,df_chinajsb2)
df<-rbind(df,df_chinajsb)


#caixinglobal====
url_cx<-"https://u.caixinglobal.com/htm/search.html?keyword=%22real%20estate%22"
web_cx<-url_cx%>%read_html()
news_cx<-web_cx%>%html_nodes("div.list-news-txt dt")
title_cx<-news_cx%>%html_text()
link_cx1<-web_cx%>%html_nodes("div.list-news-txt dt a")
link_cx2<-link_cx1%>%html_attrs()
link_cx3<-c(1:length(link_cx2))
for (i in 1:length(link_cx2)){
  link_cx3[i]<-link_cx2[[i]][1]
}
adate_cx<-str_extract(link_cx3,"\\d{4}-\\d{2}-\\d{2}")
adf_cx<-data.frame(Title=title_cx,Link=link_cx3,Date=adate_cx)
df_cx<-data.frame(Title=adf_cx$Title,Link=adf_cx$Link)[which(adf_cx$Date==date$Date),]
df<-rbind(df,df_cx)
write_excel_csv(df,"D:/Trivium/isweep/daily_property_sweep.csv")

## Trivium Finance
## If you ask, 
## It also works on Trivium Finance site!
## Uncomment the lines below and try :)
# urltf<-"https://finance.triviumchina.com/"
# webtf<-urltf%>%read_html()
# newstf<-webtf%>%html_nodes("div.trivium-favorite-js h2 a")
# titletf<-newstf%>%html_text()
# titletfdf<-as.data.frame(titletf)
# titletfdf

## Thank you for running this code! 
## This is my first time writing a crawler. 
## Sometimes the code is a bit verbose. 
## I'll try to make it better next time. 
## Wish you all the best!