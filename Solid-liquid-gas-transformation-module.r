#——————————————————————————————————————————————————————————Algorithm of estimating carbon stock in prefecture-level cities————————————————————————————————————————————————————————————————————————————
#Iterative Algorithm of Landfill Gas Production in Sanitary Landfills
mydata=read.csv('dataset0725.csv',sep=",",header=TRUE)
head(mydata)
attach(mydata)
newdata1=c()
newdata2=c()
for(i in 1:352){
  for(T in 1:20){
    a<-mydata[地级市编号==i,]
    G=0
    t=T
    repeat{G=(exp((t-T)*(a$有机组分[年份编号=t]*a$食物垃圾k[年份编号=t]+
                           a$纸类[年份编号=t]*a$纸类.织物k[年份编号=t]+
                           a$织物[年份编号=t]*a$纸类.织物k[年份编号=t]+
                           a$木竹[年份编号=t]*a$竹木k[年份编号=t]))-
                exp((t-T-1)*(a$有机组分[年份编号=t]*a$食物垃圾k[年份编号=t]+
                               a$纸类[年份编号=t]*a$纸类.织物k[年份编号=t]+
                               a$织物[年份编号=t]*a$纸类.织物k[年份编号=t]+
                               a$木竹[年份编号=t]*a$竹木k[年份编号=t])))*(a$有机组分[年份编号=t]*a$食物垃圾DOC...[年份编号=t]
                                                                 +a$纸类[年份编号=t]*a$纸类DOC...[年份编号=t]
                                                                 +a$织物[年份编号=t]*a$织物DOC...[年份编号=t]
                                                                 +a$木竹[年份编号=t]*a$竹木DOC...[年份编号=t])/100*(a$有机组分[年份编号=t]*a$食物垃圾DOCf[年份编号=t]+
                                                                                                         a$纸类[年份编号=t]*a$纸张DOCf[年份编号=t]+
                                                                                                         a$织物[年份编号=t]*a$织物DOCf[年份编号=t]+
                                                                                                         a$木竹[年份编号=t]*a$木材DOCf[年份编号=t])*a$卫生填埋量[年份编号=t]*a$卫生填埋CH4修正因子.MCF.[年份编号=t]*(1-a$卫生填埋CH4氧化因子[年份编号=t])+G
    t=t-1
    if(t<1){
      newdata1=rbind(newdata1,G)
      break
    }
    }
  }
}
write.csv(newdata1,"地级市县级市卫生填埋场填埋气碳存量结果.csv")

#the carbon input of sanitary landfills
有机组分C量=卫生填埋量*有机组分*有机组分C
灰分C量=卫生填埋量*灰分*灰分C
纸类C量=卫生填埋量*纸类*纸类C
塑料C量=卫生填埋量*塑料*塑料C
织物C量=卫生填埋量*织物*织物C
竹木C量=卫生填埋量*木竹*竹木C
金属C量=卫生填埋量*金属*金属C
玻璃C量=卫生填埋量*玻璃*玻璃C
其他C量=卫生填埋量*其他*其他C
总C量=有机组分C量+灰分C量+纸类C量+塑料C量+织物C量+竹木C量+玻璃C量+金属C量+其他C量
卫生填埋场进碳量=data.frame(有机组分C量,灰分C量,纸类C量,塑料C量,织物C量,竹木C量,玻璃C量,金属C量,其他C量,总C量)
write.csv(卫生填埋场进碳量,"D:/Lenovo station/NS或NCC/可用数据/准备写文章的数据/卫生填埋场进碳量.csv")

#Carbon amount of leachate pollutants in sanitary landfills
渗滤液C量=卫生填埋量*卫生填埋垃圾渗滤液量.立方米.吨垃圾.*(卫生填埋垃圾渗滤液COD.克.立方米.+0.0582)/3/1000/1000
write.csv(渗滤液C量,"卫生填埋场渗滤液碳量.csv")

#carbon stocks in sanitary landfills
mydata=read.csv('dataset0725.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
newdata1=c()
newdata2=c()
for(i in 1:352){
  for(T in 1:20){
    a<-mydata[地级市编号==i,]
    G=a$卫生填埋量[年份编号=T]*(a$有机组分[年份编号=T]*a$有机组分C[年份编号=T]+a$灰分[年份编号=T]*a$灰分C[年份编号=T]+a$纸类[年份编号=T]*a$纸类C[年份编号=T]+a$塑料[年份编号=T]*a$塑料C[年份编号=T]+
                         a$织物[年份编号=T]*a$织物C[年份编号=T]+a$木竹[年份编号=T]*a$竹木C[年份编号=T]+a$金属[年份编号=T]*a$金属C[年份编号=T]+
                         a$玻璃[年份编号=T]*a$玻璃C[年份编号=T]+a$其他[年份编号=T]*a$其他C[年份编号=T])-
      a$卫生填埋量[年份编号=T]*a$卫生填埋垃圾渗滤液量.立方米.吨垃圾.[年份编号=T]*(a$卫生填埋垃圾渗滤液COD.克.立方米.[年份编号=T]+0.0582)/3/1000000
    t=T
    repeat{G=-(exp((t-T)*(a$有机组分[年份编号=t]*a$食物垃圾k[年份编号=t]+
                           a$纸类[年份编号=t]*a$纸类.织物k[年份编号=t]+
                           a$织物[年份编号=t]*a$纸类.织物k[年份编号=t]+
                           a$木竹[年份编号=t]*a$竹木k[年份编号=t]))-
                exp((t-T-1)*(a$有机组分[年份编号=t]*a$食物垃圾k[年份编号=t]+
                               a$纸类[年份编号=t]*a$纸类.织物k[年份编号=t]+
                               a$织物[年份编号=t]*a$纸类.织物k[年份编号=t]+
                               a$木竹[年份编号=t]*a$竹木k[年份编号=t])))*(a$有机组分[年份编号=t]*a$食物垃圾DOC...[年份编号=t]
                                                              +a$纸类[年份编号=t]*a$纸类DOC...[年份编号=t]
                                                              +a$织物[年份编号=t]*a$织物DOC...[年份编号=t]
                                                              +a$木竹[年份编号=t]*a$竹木DOC...[年份编号=t])/100*(a$有机组分[年份编号=t]*a$食物垃圾DOCf[年份编号=t]+
                                                                                                       a$纸类[年份编号=t]*a$纸张DOCf[年份编号=t]+
                                                                                                       a$织物[年份编号=t]*a$织物DOCf[年份编号=t]+
                                                                                                       a$木竹[年份编号=t]*a$木材DOCf[年份编号=t])*a$卫生填埋量[年份编号=t]*a$卫生填埋CH4修正因子.MCF.[年份编号=t]*(1-a$卫生填埋CH4氧化因子[年份编号=t])+G
    t=t-1
    if(t<1){
      newdata1=rbind(newdata1,G)
      break
    }
    }
  }
}
write.csv(newdata1,"地级市县级市卫生填埋场C存量结果.csv")
#Cumulative amount of carbon stocks from sanitary landfills
mydata=read.csv('地级市县级市卫生填埋场C存量结果.csv',sep=",",header=TRUE)
head(mydata)
newdata1=c()
for(i in 1:352){
  for(T in 1:20){
    a<-mydata[地级市编号==i,]
    K=0
    t=T
    repeat{K=a$V1[年份编号=t]+K
    t=t-1
    if(t<1){
      newdata1=rbind(newdata1,K)
      break
    }
    }
  }
}
write.csv(newdata1,"地级市县级市卫生填埋场累计C存量结果.csv")
#The amount of carbon input from sanitary landfills
mydata=read.csv('dataset0725.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
newdata1=c()
newdata2=c()
for(i in 1:349){
  for(T in 1:18){
    a<-mydata[地级市编号==i,]
    G=0
    t=T
    repeat{G=a$卫生填埋量[年份编号=t]*(a$有机组分[年份编号=t]*a$有机组分C[年份编号=t]+a$灰分[年份编号=t]*a$灰分C[年份编号=t]+a$纸类[年份编号=t]*a$纸类C[年份编号=t]+a$塑料[年份编号=t]*a$塑料C[年份编号=t]+
                                a$织物[年份编号=t]*a$织物C[年份编号=t]+a$木竹[年份编号=t]*a$竹木C[年份编号=t]+a$金属[年份编号=t]*a$金属C[年份编号=t]+
                                a$玻璃[年份编号=t]*a$玻璃C[年份编号=t]+a$其他[年份编号=t]*a$其他C[年份编号=t])+G
    t=t-1
    if(t<1){
      newdata1=rbind(newdata1,G)
      break
    }
    }
  }
}
write.csv(newdata1,"地级市县级市卫生填埋场输入C结果.csv")


#Iterative Algorithm of Landfill Gas Production in dumps
mydata=read.csv('dataset0725.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
newdata1=c()
newdata2=c()
for(i in 1:352){
  for(T in 1:20){
    a<-mydata[地级市编号==i,]
    G=0
    t=T
    repeat{G=(exp((t-T)*(a$有机组分[年份编号=t]*a$食物垃圾k[年份编号=t]+
                           a$纸类[年份编号=t]*a$纸类.织物k[年份编号=t]+
                           a$织物[年份编号=t]*a$纸类.织物k[年份编号=t]+
                           a$木竹[年份编号=t]*a$竹木k[年份编号=t]))-
                exp((t-T-1)*(a$有机组分[年份编号=t]*a$食物垃圾k[年份编号=t]+
                               a$纸类[年份编号=t]*a$纸类.织物k[年份编号=t]+
                               a$织物[年份编号=t]*a$纸类.织物k[年份编号=t]+
                               a$木竹[年份编号=t]*a$竹木k[年份编号=t])))*(a$有机组分[年份编号=t]*a$食物垃圾DOC...[年份编号=t]
                                                              +a$纸类[年份编号=t]*a$纸类DOC...[年份编号=t]
                                                              +a$织物[年份编号=t]*a$织物DOC...[年份编号=t]
                                                              +a$木竹[年份编号=t]*a$竹木DOC...[年份编号=t])/100*(a$有机组分[年份编号=t]*a$食物垃圾DOCf[年份编号=t]+
                                                                                                       a$纸类[年份编号=t]*a$纸张DOCf[年份编号=t]+
                                                                                                       a$织物[年份编号=t]*a$织物DOCf[年份编号=t]+
                                                                                                       a$木竹[年份编号=t]*a$木材DOCf[年份编号=t])*a$简易填埋量[年份编号=t]*a$简易填埋CH4修正因子.MCF.[年份编号=t]*(1-a$简易填埋CH4氧化因子[年份编号=t])+G
    t=t-1
    if(t<1){
      newdata1=rbind(newdata1,G)
      break
    }
    }
  }
}
write.csv(newdata1,"地级市县级市简易填埋填埋气结果.csv")

#the cumulative carbon input of sanitary landfills
mydata=read.csv('dataset0725.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
newdata1=c()
newdata2=c()
for(i in 1:352){
  for(T in 1:20){
    a<-mydata[地级市编号==i,]
    G=0
    t=T
    repeat{G=a$卫生填埋量[年份编号=t]*(a$有机组分[年份编号=t]*a$有机组分C[年份编号=t]+a$灰分[年份编号=t]*a$灰分C[年份编号=t]+a$纸类[年份编号=t]*a$纸类C[年份编号=t]+a$塑料[年份编号=t]*a$塑料C[年份编号=t]+
                                a$织物[年份编号=t]*a$织物C[年份编号=t]+a$木竹[年份编号=t]*a$竹木C[年份编号=t]+a$金属[年份编号=t]*a$金属C[年份编号=t]+
                                a$玻璃[年份编号=t]*a$玻璃C[年份编号=t]+a$其他[年份编号=t]*a$其他C[年份编号=t])+G
    t=t-1
    if(t<1){
      newdata1=rbind(newdata1,G)
      break
    }
    }
  }
}
write.csv(newdata1,"地级市县级市卫生填埋场输入C累计结果.csv")

#the cumulative carbon input of dumps
mydata=read.csv('dataset0725.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
newdata1=c()
newdata2=c()
for(i in 1:352){
  for(T in 1:20){
    a<-mydata[地级市编号==i,]
    G=0
    t=T
    repeat{G=a$简易填埋量[年份编号=t]*(a$有机组分[年份编号=t]*a$有机组分C[年份编号=t]+a$灰分[年份编号=t]*a$灰分C[年份编号=t]+a$纸类[年份编号=t]*a$纸类C[年份编号=t]+a$塑料[年份编号=t]*a$塑料C[年份编号=t]+
                                a$织物[年份编号=t]*a$织物C[年份编号=t]+a$木竹[年份编号=t]*a$竹木C[年份编号=t]+a$金属[年份编号=t]*a$金属C[年份编号=t]+
                                a$玻璃[年份编号=t]*a$玻璃C[年份编号=t]+a$其他[年份编号=t]*a$其他C[年份编号=t])+G
    t=t-1
    if(t<1){
      newdata1=rbind(newdata1,G)
      break
    }
    }
  }
}
write.csv(newdata1,"地级市县级市简易填埋场输入C累计结果.csv")

#the carbon input of dumps
mydata=read.csv('dataset0725.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
有机组分C量=简易填埋量*有机组分*有机组分C
灰分C量=简易填埋量*灰分*灰分C
纸类C量=简易填埋量*纸类*纸类C
塑料C量=简易填埋量*塑料*塑料C
织物C量=简易填埋量*织物*织物C
竹木C量=简易填埋量*木竹*竹木C
金属C量=简易填埋量*金属*金属C
玻璃C量=简易填埋量*玻璃*玻璃C
其他C量=简易填埋量*其他*其他C
总C量=有机组分C量+灰分C量+纸类C量+塑料C量+织物C量+竹木C量+玻璃C量+金属C量+其他C量
简易填埋场进碳量=data.frame(有机组分C量,灰分C量,纸类C量,塑料C量,织物C量,竹木C量,玻璃C量,金属C量,其他C量,总C量)
write.csv(简易填埋场进碳量,"简易填埋场进碳量.csv")

#Carbon amount of leachate pollutants in dumps
渗滤液C量=简易填埋量*简易填埋垃圾渗滤液量.立方米.吨垃圾.*(简易填埋垃圾渗滤液COD.克.立方米.+0.0582)/3/1000/1000
write.csv(渗滤液C量,"简易填埋场渗滤液碳量.csv")

#The amount of carbon stocks from dumps
mydata=read.csv('dataset0725.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
newdata1=c()
newdata2=c()
for(i in 1:352){
  for(T in 1:20){
    a<-mydata[地级市编号==i,]
    G=a$简易填埋量[年份编号=T]*(a$有机组分[年份编号=T]*a$有机组分C[年份编号=T]+a$灰分[年份编号=T]*a$灰分C[年份编号=T]+a$纸类[年份编号=T]*a$纸类C[年份编号=T]+a$塑料[年份编号=T]*a$塑料C[年份编号=T]+
                         a$织物[年份编号=T]*a$织物C[年份编号=T]+a$木竹[年份编号=T]*a$竹木C[年份编号=T]+a$金属[年份编号=T]*a$金属C[年份编号=T]+
                         a$玻璃[年份编号=T]*a$玻璃C[年份编号=T]+a$其他[年份编号=T]*a$其他C[年份编号=T])-
      a$简易填埋量[年份编号=T]*a$简易填埋垃圾渗滤液量.立方米.吨垃圾.[年份编号=T]*(a$简易填埋垃圾渗滤液COD.克.立方米.[年份编号=T]+0.0582)/3/1000000
    t=T
    repeat{G=-(exp((t-T)*(a$有机组分[年份编号=t]*a$食物垃圾k[年份编号=t]+
                            a$纸类[年份编号=t]*a$纸类.织物k[年份编号=t]+
                            a$织物[年份编号=t]*a$纸类.织物k[年份编号=t]+
                            a$木竹[年份编号=t]*a$竹木k[年份编号=t]))-
                 exp((t-T-1)*(a$有机组分[年份编号=t]*a$食物垃圾k[年份编号=t]+
                                a$纸类[年份编号=t]*a$纸类.织物k[年份编号=t]+
                                a$织物[年份编号=t]*a$纸类.织物k[年份编号=t]+
                                a$木竹[年份编号=t]*a$竹木k[年份编号=t])))*(a$有机组分[年份编号=t]*a$食物垃圾DOC...[年份编号=t]
                                                               +a$纸类[年份编号=t]*a$纸类DOC...[年份编号=t]
                                                               +a$织物[年份编号=t]*a$织物DOC...[年份编号=t]
                                                               +a$木竹[年份编号=t]*a$竹木DOC...[年份编号=t])/100*(a$有机组分[年份编号=t]*a$食物垃圾DOCf[年份编号=t]+
                                                                                                        a$纸类[年份编号=t]*a$纸张DOCf[年份编号=t]+
                                                                                                        a$织物[年份编号=t]*a$织物DOCf[年份编号=t]+
                                                                                                        a$木竹[年份编号=t]*a$木材DOCf[年份编号=t])*a$简易填埋量[年份编号=t]*a$简易填埋CH4修正因子.MCF.[年份编号=t]*(1-a$简易填埋CH4氧化因子[年份编号=t])+G
    t=t-1
    if(t<1){
      newdata1=rbind(newdata1,G)
      break
    }
    }
  }
}
write.csv(newdata1,"地级市县级市简易填埋场C存量结果.csv")

#Cumulative amount of carbon stocks from dumps
mydata=read.csv('地级市县级市简易填埋场C存量结果.csv',sep=",",header=TRUE)
head(mydata)
newdata1=c()
for(i in 1:352){
  for(T in 1:20){
    a<-mydata[地级市编号==i,]
    K=0
    t=T
    repeat{K=a$V1[年份编号=t]+K
    t=t-1
    if(t<1){
      newdata1=rbind(newdata1,K)
      break
    }
    }
  }
}
write.csv(newdata1,"地级市县级市简易填埋场累计C存量结果.csv")