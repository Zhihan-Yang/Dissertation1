
library(ggplot2)
library(ggmap)
library(sp)
library(maptools)
library(maps)
library(readxl)
library(MASS)
library(nnet)

data <- read_excel("~/project1/Data for enteric virus MSc.xlsx")
mydata <- data[-c(42:55)]
names(mydata)[59] <- "Aichivirus"
names(mydata)[61] <- "Astrovirus"
names(mydata)[62] <- "Adenovirus"
names(mydata)[12] <- "length_of_stay"
names(mydata)

#-----------------------------------------------   Q1  -----------------------------------------------


###############################   map    ################

com=rep(0,times=nrow(mydata))
uncom=rep(0,times=nrow(mydata))
com_index=c(69,64,72,59,62,61)
uncom_index=c(42:58,60,63,65:68,70,71)
for (i in 1:nrow(mydata)){
  if(sum(mydata[i,com_index])<12){
    com[i]=1
  }
  if(sum(mydata[i,uncom_index])<50){
    uncom[i]=1
  }
  
}
mydata$com=com
mydata$uncom=uncom

com_data=mydata[which(mydata$com==1), ]
uncom_data=mydata[which(mydata$uncom==1), ]


world<-map_data("world")
vietnam<-subset(world, region %in% c("Vietnam"))

xmin<-102
xmax<-110
ymin<-8
ymax<-25

p1=ggplot() + geom_polygon(data = vietnam, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax))+
  geom_point(aes(x=com_data$LONGITUDE,y=com_data$LATITUDE),alpha=0.2,color="red")+
  scale_size(range=c(1,1))+
  labs(title='common virus')+
  theme(text=element_text(size=10),plot.title = element_text(hjust = 0.5))
p1  

p2=ggplot() + geom_polygon(data = vietnam, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax))+
  geom_point(aes(x=uncom_data$LONGITUDE,y=uncom_data$LATITUDE),alpha=0.2,color="blue")+
  scale_size(range=c(1,1))+
  labs(title='uncommon virus')+
  theme(text=element_text(size=10),plot.title = element_text(hjust = 0.5))
p2

###############################   commom and uncommon virus distriburion     ########################

names(mydata[com_index])
com_vir = c("Rotavirus","Norovirus","Sapovirus","Aichivirus","Adenovirus","Astrovirus")

count_com_vir=rep(0,times=length(com_vir))

for(i in 1:nrow(mydata)){
  for(j in 1:length(com_vir)){
    if(mydata[i,com_index[j]]==1){
      count_com_vir[j]=count_com_vir[j]+1
    }
  }
}
count_com_vir

com_virus_sum_data<-data.frame(common_virus=com_vir,count=count_com_vir)
p3=ggplot(com_virus_sum_data,aes(x=common_virus,y=count)) + 
  geom_bar(stat="identity")   + 
  theme(axis.text.x = element_text(angle=25, hjust=0.5, vjust=.5,size=12),axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))
p3


data1=mydata[c(13,14,42,44,48,50,52,54,56,58,60:63,65:73,75:79,81:84)]
names(mydata[uncom_index])

uncom_vir=c( "Alphapapillomavirus" ,"Alphapolyomavirus","Alphatorquevirus", "Betapapillomavirus" , "Betapolyomavirus", "Betatorquevirus", "Bocaparvovirus","Cardiovirus","Circovirus", "Cosavirus", "Cytomegalovirus",  "Enterovirus", "Gammatorquevirus", "Gemycircularvirus","Gemykibivirus", "Gemykrogvirus", "Husavirus",  "Lymphocryptovirus","Morbillivirus", "Parechovirus","Picobirnavirus","Porprismacovirus", "Protoparvovirus", "Rubulavirus","Salivirus")


count_uncom_vir=rep(0,times=length(uncom_vir))

for(i in 1:nrow(mydata)){
  for(j in 1:length(uncom_vir)){
    if(mydata[i,uncom_index[j]]==1){
      count_uncom_vir[j]=count_uncom_vir[j]+1
    }
  }
}
count_uncom_vir

uncom_virus_sum_data<-data.frame(uncommon_virus=uncom_vir,count=count_uncom_vir)
p4=ggplot(uncom_virus_sum_data,aes(x=uncommon_virus,y=count)) + 
  geom_bar(stat="identity")   + 
  theme(axis.text.x = element_text(angle=25, hjust=0.5, vjust=.5,size=12),axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))

p4

###############################   DongThap city and other city #######################################

CentrallyCity_name = unique(mydata[,13])      #City name
CentrallyCity_num = nrow(CentrallyCity_name)
ProvincialCity_name = unique(mydata[which(mydata[,13] == "Dong Thap"),14]) # City name in Dong Thap
ProvincialCity_num = nrow(ProvincialCity_name)
City_num = CentrallyCity_num+ProvincialCity_num-1

A = as.list(ProvincialCity_name)
ProvincialCity_name_list = A[[1]]
A = as.list(CentrallyCity_name)
CentrallyCity_name_list = A[[1]]

a1 = rep(0,times=length(com_vir))
b1 <- matrix(rep(a1,times=ProvincialCity_num),ncol=6) 

for (i in 1:ProvincialCity_num){
  for (j in 1:length(com_vir)){
    b1[i,j] = nrow(mydata[which((mydata$CentrallyCity == "Dong Thap") & (mydata$ProvincialCity == ProvincialCity_name_list[i]) & (mydata[,com_index[j]] == 1)), ])
  }
}
DongThap_virus_sum_data<-data.frame(DongThap_ProcincialCity=ProvincialCity_name_list,virus_count=b1)

colors <- c("aquamarine4","burlywood4","red3","darkorange","yellow","cornflowerblue")

barplot(t(b1),main="Common Virus",names.arg=ProvincialCity_name_list,xlab="DongThap",ylab="count",col=colors,border=NA,width=0.5)
legend("right", com_vir, cex=1, fill=colors)

a2 = rep(0,times=length(com_vir))
b2 <- matrix(rep(a1,times=CentrallyCity_num),ncol=6) 

for (i in 2:CentrallyCity_num){
  for (j in 1:length(com_vir)){
    b2[i,j] = nrow(mydata[which((mydata$CentrallyCity ==CentrallyCity_name_list[i]) &  (mydata[,com_index[j]] == 1)), ])
  }
}
CentrallyCity_virus_sum_data<-data.frame(Vietnam_CentrallyCity=CentrallyCity_name_list,virus_count=b2)
barplot(t(b2),main="Common Virus",names.arg=CentrallyCity_name_list,xlab="Vietnam",ylab="count",col=colors,border=NA,width=0.5)
legend("left", com_vir, cex=1, fill=colors)

a2 = rep(0,times=length(uncom_vir))
b2 <- matrix(rep(a2,times=7),ncol=length(uncom_vir)) 

for (i in 2:CentrallyCity_num){
  for (j in 1:length(uncom_vir)){
    b2[i,j] = nrow(mydata[which((mydata$CentrallyCity ==  CentrallyCity_name_list[i])  & (mydata[,uncom_index[j]] == 1)), ])
  }
}

b2

DongThap_virus_sum_data<-data.frame(CentrallyCity=CentrallyCity_name_list,virus_count=b2)

colors <- c("darkolivegreen","bisque","aquamarine4","burlywood4","red3",
            "darkorange","yellow","cornflowerblue","darkblue","coral",
            "blueviolet","darkgoldenrod2","grey","black","green",
            "cadetblue1","blue","brown","azure","azure4",
            "orchid2","steelblue4","yellow3","cyan")

barplot(t(b2),main="Uncommon Virus",names.arg=CentrallyCity_name_list,xlab="CentrallyCity",ylab="count",col=colors,border=NA,width=0.1)

# Add the legend to the chart.
legend("right", uncom_vir, cex=0.6, fill=colors)


a2 = rep(0,times=length(uncom_vir))
b2 <- matrix(rep(a2,times=9),ncol=length(uncom_vir)) 

for (i in 1:ProvincialCity_num){
  for (j in 1:length(uncom_vir)){
    b2[i,j] = nrow(mydata[which((mydata$ProvincialCity ==  ProvincialCity_name_list[i])  & (mydata[,uncom_index[j]] == 1)), ])
  }
}

b2

DongThap_virus_sum_data<-data.frame(ProvincialCityCity=ProvincialCity_name_list,virus_count=b2)

colors <- c("darkolivegreen","bisque","aquamarine4","burlywood4","red3",
            "darkorange","yellow","cornflowerblue","darkblue","coral",
            "blueviolet","darkgoldenrod2","grey","black","green",
            "cadetblue1","blue","brown","azure","azure4",
            "orchid2","steelblue4","yellow3","cyan")

barplot(t(b2),main="Uncommon Virus",names.arg=ProvincialCity_name_list,xlab="Dong Thap",ylab="count",col=colors,border=NA,width=0.1)

# Add the legend to the chart.
legend("right", uncom_vir, cex=0.6, fill=colors)




#-----------------------------------------------   Q2  -----------
names(mydata)
data2 <- mydata[c(3,8,9,17,31:36,38:41,74)]
names(data2)
class(data2$SiteRecruitment)
data2$SiteRecruitment=as.factor(data2$SiteRecruitment)
class(data2$Gender)
data2$Gender=as.factor(data2$Gender)

class(data2$ContactDiar)
for(i in 1:nrow(data2)){
  if(data2$ContactDiar[i]==2 | data2$ContactDiar[i]==9){
    data2$ContactDiar[i]=0
  }
}
data2$ContactDiar=as.logical(data2$ContactDiar)

class(data2$KeepAnimal)
for(i in 1:nrow(data2)){
  if(data2$KeepAnimal[i]==2){
    data2$KeepAnimal[i]=0
  }
}
data2$KeepAnimal=as.logical(data2$KeepAnimal)

class(data2$KillingAnimal)
for(i in 1:nrow(data2)){
  if(data2$KillingAnimal[i]==2){
    data2$KillingAnimal[i]=0
  }
}
data2$KillingAnimal=as.logical(data2$KillingAnimal)

class(data2$EatCookRawMeat)
for(i in 1:nrow(data2)){
  if(data2$EatCookRawMeat[i]==2){
    data2$EatCookRawMeat[i]=0
  }
}
data2$EatCookRawMeat=as.logical(data2$EatCookRawMeat)

class(data2$IF_Bacterial)
for(i in 1:nrow(data2)){
  if(data2$IF_Bacterial[i]==2){
    data2$IF_Bacterial[i]=0
  }
}
data2$IF_Bacterial=as.logical(data2$IF_Bacterial)

class(data2$is_coinf)
data2$is_coinf=as.numeric(data2$is_coinf)
data2$is_coinf[is.na(data2$is_coinf)] <- 0
#####删除{
data2$is_coinf=as.factor(data2$is_coinf)
#####删除}
####原始数据探索
names(data2)

## 平均Age
ave_age <- rep(0,times=6)
for(i in 1:6){
  ave_age[i] <- colSums(data2[which(data2$is_coinf==i-1),1])/nrow(data2[which(data2$is_coinf==i-1),1])
}
ave_age

p_SiteRecruitment_2<- rep(0,times=6)
for(i in 1:6){
  p_SiteRecruitment_2[i] <- nrow(data2[which((data2$is_coinf==i-1)&(data2$SiteRecruitment==2)),])/nrow(data2[which(data2$is_coinf==i-1),])
}
round(p_SiteRecruitment_2,2)

p_SiteRecruitment_4<- rep(0,times=6)
for(i in 1:6){
  p_SiteRecruitment_4[i] <- nrow(data2[which((data2$is_coinf==i-1)&(data2$SiteRecruitment==4)),])/nrow(data2[which(data2$is_coinf==i-1),])
}
round(p_SiteRecruitment_4,2)

p_SiteRecruitment_5<- rep(0,times=6)
for(i in 1:6){
  p_SiteRecruitment_5[i] <- nrow(data2[which((data2$is_coinf==i-1)&(data2$SiteRecruitment==5)),])/nrow(data2[which(data2$is_coinf==i-1),])
}
round(p_SiteRecruitment_5,2)

p_SiteRecruitment_6<- rep(0,times=6)
for(i in 1:6){
  p_SiteRecruitment_6[i] <- nrow(data2[which((data2$is_coinf==i-1)&(data2$SiteRecruitment==6)),])/nrow(data2[which(data2$is_coinf==i-1),])
}
round(p_SiteRecruitment_6,2)

p_gender_male <- rep(0,times=6)
for(i in 1:6){
  p_gender_male[i] <- nrow(data2[which((data2$is_coinf==i-1)&(data2$Gender==1)),])/nrow(data2[which(data2$is_coinf==i-1),])
}
round(p_gender_male,2)

p_ContactDiar <- rep(0,times=6)
for(i in 1:6){
  p_ContactDiar[i] <- nrow(data2[which((data2$is_coinf==i-1)&(data2$ContactDiar==TRUE)),])/nrow(data2[which(data2$is_coinf==i-1),])
}
round(p_ContactDiar,2)


p_w_a <- rep(0,times=6)
p_water_animal <- matrix(rep(p_w_a ,times=10),ncol=6) 
for(i in 1:10){#10个变量
  for(j in 1:6){
    p_water_animal[i,j] <- nrow(data2[which((data2$is_coinf==j-1)&(data2[4+i]==TRUE)),])/nrow(data2[which(data2$is_coinf==j-1),])
  }
}
round(p_water_animal,2)

####模型
data2$is_coinf <- relevel(data2$is_coinf, ref=1)
model <- multinom(is_coinf ~ .-is_coinf, data=data2)
summary(model)
zvalues <- summary(model)$coefficients / summary(model)$standard.errors
pnorm(abs(zvalues), lower.tail=FALSE)*2
anova(model)

model_ordinal <- polr(is_coinf ~ .-is_coinf, data=data2)
summary(model_ordinal)

###common virus
data3 <- mydata[c(3,8,9,17,31:36,38:41,com_index)]
names(data3)

class(data3$SiteRecruitment)
data3$SiteRecruitment=as.factor(data3$SiteRecruitment)
class(data3$Gender)
data3$Gender=as.factor(data3$Gender)

class(data3$ContactDiar)
for(i in 1:nrow(data3)){
  if(data3$ContactDiar[i]==2 | data3$ContactDiar[i]==9){
    data3$ContactDiar[i]=0
  }
}
data3$ContactDiar=as.logical(data3$ContactDiar)

class(data3$KeepAnimal)
for(i in 1:nrow(data3)){
  if(data3$KeepAnimal[i]==2){
    data3$KeepAnimal[i]=0
  }
}
data3$KeepAnimal=as.logical(data3$KeepAnimal)

class(data3$KillingAnimal)
for(i in 1:nrow(data3)){
  if(data3$KillingAnimal[i]==2){
    data3$KillingAnimal[i]=0
  }
}
data3$KillingAnimal=as.logical(data3$KillingAnimal)

class(data3$EatCookRawMeat)
for(i in 1:nrow(data3)){
  if(data3$EatCookRawMeat[i]==2){
    data3$EatCookRawMeat[i]=0
  }
}
data3$EatCookRawMeat=as.logical(data3$EatCookRawMeat)

class(data3$IF_Bacterial)
for(i in 1:nrow(data3)){
  if(data3$IF_Bacterial[i]==2){
    data3$IF_Bacterial[i]=0
  }
}
data3$IF_Bacterial=as.logical(data3$IF_Bacterial)

for(i in 1:nrow(data3)){
  if(data3$Rotavirus[i]==2){
    data3$Rotavirus[i]=0
  }
  if(data3$Norovirus[i]==2){
    data3$Norovirus[i]=0
  }
  if(data3$Sapovirus[i]==2){
    data3$Sapovirus[i]=0
  }
  if(data3$Aichivirus[i]==2){
    data3$Aichivirus[i]=0
  }
  if(data3$Adenovirus[i]==2){
    data3$Adenovirus[i]=0
  }
  if(data3$Astrovirus[i]==2){
    data3$Astrovirus[i]=0
  }
}
data3$Rotavirus=as.logical(data3$Rotavirus)
data3$Norovirus=as.logical(data3$Norovirus)
data3$Sapovirus=as.logical(data3$Sapovirus)
data3$Aichivirus=as.logical(data3$Aichivirus)
data3$Adenovirus=as.logical(data3$Adenovirus)
data3$Astrovirus=as.logical(data3$Astrovirus)

model1 <- glm(Rotavirus ~ Age+SiteRecruitment+Gender+ContactDiar+Tap+Well+Rain+River+Pond+Bottled+KeepAnimal+KillingAnimal+EatCookRawMeat+IF_Bacterial,data=data3,family="binomial")
summary(model1)
anova(model1)
plot(model1)
model2 <- glm(Norovirus ~ Age+SiteRecruitment+Gender+ContactDiar+Tap+Well+Rain+River+Pond+Bottled+KeepAnimal+KillingAnimal+EatCookRawMeat+IF_Bacterial,data=data3,family="binomial")
summary(model2)
plot(model2)
model3 <- glm(Sapovirus ~ Age+SiteRecruitment+Gender+ContactDiar+Tap+Well+Rain+River+Pond+Bottled+KeepAnimal+KillingAnimal+EatCookRawMeat+IF_Bacterial,data=data3,family="binomial")
summary(model3)
plot(model3)
model4 <- glm(Aichivirus ~ Age+SiteRecruitment+Gender+ContactDiar+Tap+Well+Rain+River+Pond+Bottled+KeepAnimal+KillingAnimal+EatCookRawMeat+IF_Bacterial,data=data3,family="binomial")
summary(model4)
plot(model4)
model5 <- glm(Adenovirus ~ Age+SiteRecruitment+Gender+ContactDiar+Tap+Well+Rain+River+Pond+Bottled+KeepAnimal+KillingAnimal+EatCookRawMeat+IF_Bacterial,data=data3,family="binomial")
summary(model5)
plot(model5)
model6 <- glm(Astrovirus ~ Age+SiteRecruitment+Gender+ContactDiar+Tap+Well+Rain+River+Pond+Bottled+KeepAnimal+KillingAnimal+EatCookRawMeat+IF_Bacterial,data=data3,family="binomial")
summary(model6)
plot(model6)
ave_age <- rep(0,times=6)
for(i in 1:6){
  ave_age[i] <- colSums(data3[which(data3[14+i]==TRUE),1])/nrow(data3[which(data3[14+i]==TRUE),1])
}
round(ave_age,0)

p_SiteRecruitment_2<- rep(0,times=6)
for(i in 1:6){
  p_SiteRecruitment_2[i] <- nrow(data3[which((data3[14+i]==TRUE)&(data3$SiteRecruitment==2)),])/nrow(data3[which(data3[14+i]==TRUE),])
}
round(p_SiteRecruitment_2,2)

p_SiteRecruitment_4<- rep(0,times=6)
for(i in 1:6){
  p_SiteRecruitment_4[i] <- nrow(data3[which((data3[14+i]==TRUE)&(data3$SiteRecruitment==4)),])/nrow(data3[which(data3[14+i]==TRUE),])
}
round(p_SiteRecruitment_4,2)

p_SiteRecruitment_5<- rep(0,times=6)
for(i in 1:6){
  p_SiteRecruitment_5[i] <- nrow(data3[which((data3[14+i]==TRUE)&(data3$SiteRecruitment==5)),])/nrow(data3[which(data3$SiteRecruitment==5),])
}
round(p_SiteRecruitment_5,2)


p_gender_male <- rep(0,times=6)
for(i in 1:6){
  p_gender_male[i] <- nrow(data3[which((data3[i+14]==TRUE)&(data3$Gender==1)),])/nrow(data3[which(data3[i+14]==TRUE),])
}
round(p_gender_male,2)


p_SiteRecruitment_6<- rep(0,times=6)
for(i in 1:6){
  p_SiteRecruitment_6[i] <- nrow(data3[which((data3[14+i]==TRUE)&(data3$SiteRecruitment==6)),])/nrow(data3[which(data3[14+i]==TRUE),])
}
round(p_SiteRecruitment_6,2)

p_w_a <- rep(0,times=6)
p_water_animal <- matrix(rep(p_w_a ,times=11),ncol=6) 
for(i in 1:11){#10个变量
  for(j in 1:6){
    p_water_animal[i,j] <- nrow(data3[which((data3[j+14]==TRUE)&(data3[3+i]==TRUE)),])/nrow(data3[which(data3[j+14]==TRUE),])
  }
}
round(p_water_animal,2)

names(data3)
#########------- for common virus-----------
names(mydata)
data2_2 <- mydata[c(3,8,9,12,17,31:36,38:41,74,com_index)]#???
data2_2 <- data2#???
data2_2$com <- com
data2_2$Rotavirus <- mydata$Rotavirus
data2_2$Norovirus <- mydata$Norovirus
data2_2$Sapovirus <- mydata$Sapovirus
data2_2$Aichivirus <- mydata$Aichivirus
data2_2$Adenovirus <- mydata$Adenovirus
data2_2$Astrovirus <- mydata$Astrovirus

data2_1 <- data2_2[which(mydata$com==1), ]
data2_1=data2_2[-c(16)]
names(data2_2)
#model <- multinom(Rotavirus ~ .-c(Rotavirus,Norovirus,Sapovirus,Aichivirus,Adenovirus,Astrovirus), data=data2_1)
model <- multinom(Rotavirus ~ Age+SiteRecruitment+Gender+length_of_stay+ContactDiar+
                    Tap+Well+Rain+River+Pond+Bottled+KeepAnimal+KillingAnimal+EatCookRawMeat+
                    IF_Bacterial, data=data2_1)

summary(model)
zvalues <- summary(model)$coefficients / summary(model)$standard.errors
pnorm(abs(zvalues), lower.tail=FALSE)*2


#----------------------------------------------- Q3-------------------------
data3 <- mydata[c(18:22,74)]
names(data3)
class(data3$BloodStool)
for(i in 1:nrow(data3)){
  if(data3$BloodStool[i]==9 | data3$BloodStool[i]==2){
    data3$BloodStool[i]=0
  }
}
data3$BloodStool=as.logical(data3$BloodStool)

for(i in 1:nrow(data3)){
  if(data3$MucoidStool[i]==9 | data3$MucoidStool[i]==2){
    data3$MucoidStool[i]=0
  }
}
data3$MucoidStool=as.logical(data3$MucoidStool)

for(i in 1:nrow(data3)){
  if(data3$AbdominalPain[i]==9 | data3$AbdominalPain[i]==2){
    data3$AbdominalPain[i]=0
  }
}
data3$AbdominalPain=as.logical(data3$AbdominalPain)

for(i in 1:nrow(data3)){
  if(data3$ThreeDaysFever[i]==9 | data3$ThreeDaysFever[i]==2){
    data3$ThreeDaysFever[i]=0
  }
}
data3$ThreeDaysFever=as.logical(data3$ThreeDaysFever)

data3$NumberDiarEpi=as.numeric(data3$NumberDiarEpi)
data3$NumberDiarEpi[is.na(data3$NumberDiarEpi)] <- 7

data3$is_coinf=as.numeric(data3$is_coinf)
data3$is_coinf[is.na(data3$is_coinf)] <- 0
data3$is_coinf=as.factor(data3$is_coinf)
###########  6种coinfection
model <- multinom(is_coinf ~ .-is_coinf, data=data3)
summary(model)
zvalues <- summary(model)$coefficients / summary(model)$standard.errors
pnorm(abs(zvalues), lower.tail=FALSE)*2

model_polr <- polr(is_coinf ~ .-is_coinf, data=data3)
summary(model_polr)
pt(3.41434, 400-3, lower.tail=FALSE)*2

########## Yes/No coinfection
yorn_coinf <- rep(0,times=nrow(data3))
for(i in 1:nrow(data3)){
  if(data3[i,6]==2 |data3[i,6]==3 |data3[i,6]==4|data3[i,6]==5){
    yorn_coinf[i]=1
  }else{
    yorn_coinf[i]=0
  }
}
data3$yorn_coinf <- yorn_coinf
class(data3$yorn_coinf)
data3$yorn_coinf=as.logical(data3$yorn_coinf)
yorn_model <- glm(yorn_coinf ~ BloodStool+MucoidStool+NumberDiarEpi+AbdominalPain+ThreeDaysFever,data=data3,family="binomial")
summary(yorn_model)
par(mfcol=c(1,2))
plot(yorn_model)
anova(yorn_model)
###画图
Coinfection <- c("Yes","No","Total")
Symptoms <- c("MucoidStool","MucoidStool","MucoidStool","ThreeDaysFever","ThreeDaysFever","ThreeDaysFever")
Probability <- c(0.205,0.12959,0.149929,0.5842,0.4429,0.4809)
cabbage_exp <- data.frame(Symptoms,Probability,Coinfection)

ggplot(cabbage_exp, aes(x = Symptoms, y = Probability, fill = Coinfection)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_fill_brewer(palette=8)+
  #scale_fill_brewer("",values = c("Coin(Yes)" = "firebrick1","Coin(No)" = "cyan3","Total"="yellow"))+
  labs(title='MucoidStool and ThreeDaysFever')+
  #scale_fill_brewer("",values = c("Coin(Yes)" = "firebrick1","Coin(No)" = "cyan3","Total"="yellow"))+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(hjust=0.5, vjust=.5,size=12),axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))



data4 <- mydata[c(com_index,uncom_index)]
data4 <- data.frame(data3,data4)
for(i in 1:nrow(data4)){
  for(j in 7:37)
    if(data4[i,j]==2){
      data4[i,j]=0
    }
}
names(data4)
Rotavirus_Sym <- glm(Rotavirus ~ BloodStool+MucoidStool+NumberDiarEpi+AbdominalPain+ThreeDaysFever,data=data4,family="binomial")
Norovirus_Sym <- glm(Norovirus ~ BloodStool+MucoidStool+NumberDiarEpi+AbdominalPain+ThreeDaysFever,data=data4,family="binomial")
Sapovirus_Sym <- glm(Sapovirus ~ BloodStool+MucoidStool+NumberDiarEpi+AbdominalPain+ThreeDaysFever,data=data4,family="binomial")
Aichivirus_Sym <- glm(Aichivirus ~ BloodStool+MucoidStool+NumberDiarEpi+AbdominalPain+ThreeDaysFever,data=data4,family="binomial")
Adenovirus_Sym <- glm(Adenovirus ~ BloodStool+MucoidStool+NumberDiarEpi+AbdominalPain+ThreeDaysFever,data=data4,family="binomial")
Astrovirus_Sym <- glm(Astrovirus ~ BloodStool+MucoidStool+NumberDiarEpi+AbdominalPain+ThreeDaysFever,data=data4,family="binomial")
summary(Rotavirus_Sym)
summary(Norovirus_Sym)
summary(Sapovirus_Sym)
summary(Aichivirus_Sym)
summary(Adenovirus_Sym)
summary(Astrovirus_Sym)
plot(Rotavirus_Sym)
plot(Norovirus_Sym)
plot(Sapovirus_Sym)
plot(Aichivirus_Sym)
plot(Adenovirus_Sym)
plot(Astrovirus_Sym)


Infected <- c("Yes","No","Total")
common_virus <- c("Rotavirus","Rotavirus","Rotavirus","Norovirus","Norovirus","Norovirus","Sapovirus","Sapovirus","Sapovirus")
Probability <- c(52/151,250/556,302/707,29/89,273/618,302/707,16/57,286/650,302/707)
cabbage_exp <- data.frame(common_virus,Probability,Infected)

ggplot(cabbage_exp, aes(x = common_virus, y = Probability, fill = Infected)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_fill_brewer(palette=8)+
  labs(title='AbdominalPain')+
  #scale_fill_brewer("",values = c("Coin(Yes)" = "firebrick1","Coin(No)" = "cyan3","Total"="yellow"))+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(hjust=0.5, vjust=.5,size=12),axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))


############ combine virus
names(data4)
Alphatorque_Betatorque <- rep(0,times=nrow(data4))
Noro_Sapo<- rep(0,times=nrow(data4))
Noro_Entero<- rep(0,times=nrow(data4))
Alphatorque_Gammatorque<- rep(0,times=nrow(data4))
Noro_Alphatorque<- rep(0,times=nrow(data4))

for(i in 1:nrow(data4)){
  if((data4[i,16]==1)&(data4[i,19]==1)){
    Alphatorque_Betatorque[i] <- 1
  }
  if((data4[i,16]==1)&(data4[i,26]==1)){
    Alphatorque_Gammatorque[i] <- 1
  }
  if((data4[i,16]==1)&(data4[i,9]==1)){
    Noro_Alphatorque[i] <- 1
  }
  if((data4[i,9]==1)&(data4[i,10]==1)){
    Noro_Sapo[i] <- 1
  }
  if((data4[i,9]==1)&(data4[i,25]==1)){
    Noro_Entero[i] <- 1
  }
}
Alphatorque_Betatorque_Sym <- glm(Alphatorque_Betatorque ~ BloodStool+MucoidStool+NumberDiarEpi+AbdominalPain+ThreeDaysFever,data=data4,family="binomial")
Noro_Sapo_Sym <- glm(Noro_Sapo ~ BloodStool+MucoidStool+NumberDiarEpi+AbdominalPain+ThreeDaysFever,data=data4,family="binomial")
Noro_Entero_Sym <- glm(Noro_Entero ~ BloodStool+MucoidStool+NumberDiarEpi+AbdominalPain+ThreeDaysFever,data=data4,family="binomial")
Alphatorque_Gammatorque_Sym <- glm(Alphatorque_Gammatorque ~ BloodStool+MucoidStool+NumberDiarEpi+AbdominalPain+ThreeDaysFever,data=data4,family="binomial")
Noro_Alphatorque_Sym <- glm(Noro_Alphatorque ~ BloodStool+MucoidStool+NumberDiarEpi+AbdominalPain+ThreeDaysFever,data=data4,family="binomial")

summary(Alphatorque_Betatorque_Sym)
summary(Noro_Sapo_Sym)
summary(Noro_Entero_Sym)
summary(Alphatorque_Gammatorque_Sym)
summary(Noro_Alphatorque_Sym)

plot(Alphatorque_Betatorque_Sym)
plot(Noro_Sapo_Sym)
plot(Noro_Entero_Sym)
plot(Alphatorque_Gammatorque_Sym)
plot(Noro_Alphatorque_Sym)



Infected <- c("Yes","No","Total")
combination <- c("Alphatorque-_Betatorque-","Alphatorque-_Betatorque-","Alphatorque-_Betatorque-","Noro-_Entero-","Noro-_Entero-","Noro-_Entero-")
Probability <- c(8/27,98/680,106/707,6/15,100/692,106/707)
cabbage_exp <- data.frame(combination,Probability,Infected)

ggplot(cabbage_exp, aes(x = combination, y = Probability, fill = Infected)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_fill_brewer(palette=8)+
  labs(title='MucoidStool')+
  #scale_fill_brewer("",values = c("Coin(Yes)" = "firebrick1","Coin(No)" = "cyan3","Total"="yellow"))+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(hjust=0.5, vjust=.5,size=12),axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))

Infected <- c("Yes","No","Total")
combination <- c("Noro-_Sapo-","Noro-_Sapo-","Noro-_Sapo-","Noro-_Entero-","Noro-_Entero-","Noro-_Entero-")
Probability <- c(4/21,298/686,302/707,2/15,300/692,302/707)
cabbage_exp <- data.frame(combination,Probability,Infected)

ggplot(cabbage_exp, aes(x = combination, y = Probability, fill = Infected)) +
  geom_bar(position = "dodge", stat = "identity")+
  scale_fill_brewer(palette=8)+
  labs(title='AbdominalPain')+
  #scale_fill_brewer("",values = c("Coin(Yes)" = "firebrick1","Coin(No)" = "cyan3","Total"="yellow"))+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(hjust=0.5, vjust=.5,size=12),axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))


#------------------------------------------------  Q4-------------------
data4=mydata[c(com_index,uncom_index)]
names(data4)


a = rep(0,times=length(names(data4)))
b <- matrix(rep(a,times=length(names(data4))),ncol=length(names(data4))) 

for(i in 1:nrow(mydata)){
  for(j in 1:length(names(data4))){
    for(k in 1:length(names(data4))){
      if(data4[i,j]==1 & data4[i,k]==1){
        b[j,k]=b[j,k]+1
      }
    }
  }
}


for(j in 1:length(names(data4))){
  for(k in 1:length(names(data4))){
    if(j==k){
      b[j,k]=0
    }
  }
}

write.csv(b,file="~/project1/Q4_combine.csv",quote=F,row.names = F)

