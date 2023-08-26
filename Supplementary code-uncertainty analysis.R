################################Uncertainty of organic carbon stock in landfills############################################
library(parallel)
detectCores(logical=F)
cl<-makeCluster(4)
func<-function(x){
  mmm = sample(1:100000000000, 1)
  mydata=read.csv('Supplementary data-landfill organic carbon stock accounting database.csv',sep=",",header=TRUE)
  head(mydata)
  attach(mydata)
  Prefecture.level.city<-mydata[,2]
  Prefecture.level.city.code<-mydata[,3]
  Year<-mydata[,4]
  Year.Code<-mydata[,5]
  Sanitary.landfill..wt.<-rnorm(7040,mean=mydata[,10],sd=mydata[,10]*0.30)
  Dump..wt.<-rnorm(7040,mean=mydata[,13],sd=mydata[,13]*0.30)
  Organic.fractions<-rnorm(7040,mean=mydata[,14],sd=mydata[,14]*0.30)
  Ash.and.stone<-rnorm(7040,mean=mydata[,15],sd=mydata[,15]*0.30)
  Paper<-rnorm(7040,mean=mydata[,16],sd=mydata[,16]*0.30)
  Plastic.and.Rubber<-rnorm(7040,mean=mydata[,17],sd=mydata[,17]*0.30)
  Textile<-rnorm(7040,mean=mydata[,18],sd=mydata[,18]*0.30)
  Wood<-rnorm(7040,mean=mydata[,19],sd=mydata[,19]*0.30)
  Metal<-rnorm(7040,mean=mydata[,20],sd=mydata[,20]*0.30)
  Glass<-rnorm(7040,mean=mydata[,21],sd=mydata[,21]*0.30)
  Others<-rnorm(7040,mean=mydata[,22],sd=mydata[,22]*0.30)
  Carbon.content.of.Organic.Fractions<-rnorm(7040,mean=mydata[,23],sd=mydata[,23]*0.11)
  Carbon.content.of.Ash.and.Stone<-rnorm(7040,mean=mydata[,24],sd=mydata[,24]*0.32)
  Carbon.content.of.Paper<-rnorm(7040,mean=mydata[,25],sd=mydata[,25]*0.32)
  Carbon.content.of.Plastic.and.Rubber<-rnorm(7040,mean=mydata[,26],sd=mydata[,26]*0.32)
  Carbon.content.of.Textile<-rnorm(7040,mean=mydata[,27],sd=mydata[,27]*0.32)
  Carbon.content.of.Wood<-rnorm(7040,mean=mydata[,28],sd=mydata[,28]*0.32)
  Carbon.content.of.Glass<-rnorm(7040,mean=mydata[,29],sd=mydata[,29]*0.32)
  Carbon.content.of.Metal<-rnorm(7040,mean=mydata[,30],sd=mydata[,30]*0.32)
  Carbon.content.of.Others<-rnorm(7040,mean=mydata[,31],sd=mydata[,31]*0.01)
  Methane.oxidation.factor.of.Sanitary.landfills<-rnorm(7040,mean=mydata[,32],sd=mydata[,32]*0.30)
  Methane.oxidation.factor.of.Dump<-rnorm(7040,mean=mydata[,33],sd=mydata[,33]*0.30)
  Methane.correction.factor.of.Sanitary.landfills..MCF.<-rnorm(7040,mean=mydata[,34],sd=mydata[,34]*0.30)
  Methane.correction.factor.of.Dumps..MCF.<-rnorm(7040,mean=mydata[,35],sd=mydata[,35]*0.30)
  Methane.collection.rate.of.Sanitayr.landfills....<-rnorm(7040,mean=mydata[,36],sd=mydata[,36]*0.30)
  Methane.collection.rate.of.Dumps....<-rnorm(7040,mean=mydata[,37],sd=mydata[,37]*0.30)
  Methane.production.rate.coefficient.of.Paper.Textile..k.<-rnorm(7040,mean=mydata[,38],sd=mydata[,38]*0.30)
  Methane.production.rate.coefficient.of.Wood..k.<-rnorm(7040,mean=mydata[,39],sd=mydata[,39]*0.30)
  Methane.production.rate.coefficient.of.Garden.waste..k.<-rnorm(7040,mean=mydata[,40],sd=mydata[,40]*0.30)
  Methane.production.rate.coefficient.of.Organic.fractions..k.<-rnorm(7040,mean=mydata[,41],sd=mydata[,41]*0.30)
  Decomposable.biodegradable.organic.carbon.of.Organic.fractions..DOCf.<-rnorm(7040,mean=mydata[,42],sd=mydata[,42]*0.30)
  Decomposable.biodegradable.organic.carbon.of.Wood..DOCf.<-rnorm(7040,mean=mydata[,43],sd=mydata[,43]*0.30)
  Decomposable.biodegradable.organic.carbon.of.Paper..DOCf.<-rnorm(7040,mean=mydata[,44],sd=mydata[,44]*0.30)
  Decomposable.biodegradable.organic.carbon.of.Textile..DOCf.<-rnorm(7040,mean=mydata[,45],sd=mydata[,45]*0.30)
  Decomposable.biodegradable.organic.carbon.of.Garden.waste..DOCf.<-rnorm(7040,mean=mydata[,46],sd=mydata[,46]*0.30)
  Degradable.organic.carbon.of.Paper..DOC....<-rnorm(7040,mean=mydata[,47],sd=mydata[,47]*0.30)
  Degradable.organic.carbon.of.Wood..DOC....<-rnorm(7040,mean=mydata[,48],sd=mydata[,48]*0.30)
  Degradable.organic.carbon.of.Textile..DOC....<-rnorm(7040,mean=mydata[,49],sd=mydata[,49]*0.30)
  Degradable.organic.carbon.of.Organic.fractions..DOC....<-rnorm(7040,mean=mydata[,50],sd=mydata[,50]*0.30)
  The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.<-mydata$The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.
  The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.[which(The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.==0.07)]<-runif(length(which(The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.==0.07)),min=0,max=0.15)
  The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.[which(The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.==0.15)]<-runif(length(which(The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.==0.15)),min=0.08,max=0.25)
  The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.[which(The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.==0.30)]<-runif(length(which(The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.==0.30)),min=0.20,max=0.80)
  The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.[which(The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.==0.40)]<-runif(length(which(The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.==0.40)),min=0.25,max=1.00)
  The.generation.of.leachate.in.Dumps..m3.t.waste.<-mydata$The.generation.of.leachate.in.Dumps..m3.t.waste.
  The.generation.of.leachate.in.Dumps..m3.t.waste.[which(The.generation.of.leachate.in.Dumps..m3.t.waste.==0.05)]<-runif(length(which(The.generation.of.leachate.in.Dumps..m3.t.waste.==0.05)),min=0,max=0.15)
  The.generation.of.leachate.in.Dumps..m3.t.waste.[which(The.generation.of.leachate.in.Dumps..m3.t.waste.==0.25)]<-runif(length(which(The.generation.of.leachate.in.Dumps..m3.t.waste.==0.25)),min=0.08,max=0.45)
  The.generation.of.leachate.in.Dumps..m3.t.waste.[which(The.generation.of.leachate.in.Dumps..m3.t.waste.==0.55)]<-runif(length(which(The.generation.of.leachate.in.Dumps..m3.t.waste.==0.55)),min=0.08,max=0.80)
  The.generation.of.leachate.in.Dumps..m3.t.waste.[which(The.generation.of.leachate.in.Dumps..m3.t.waste.==0.75)]<-runif(length(which(The.generation.of.leachate.in.Dumps..m3.t.waste.==0.75)),min=0.15,max=1.25)
  COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.<-mydata$COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.
  COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.[which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==12000)]<-runif(length(which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==12000)),min=2000,max=60000)
  COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.[which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==11500)]<-runif(length(which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==11500)),min=2000,max=60000)
  COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.[which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==10500)]<-runif(length(which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==10500)),min=1500,max=50000)
  COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.[which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==10000)]<-runif(length(which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==10000)),min=1500,max=50000)
  COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.[which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==7940)]<-rnorm(length(which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==7940)),mean=7940,sd=7940*0.3)
  COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.[which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==6550)]<-rnorm(length(which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==6550)),mean=6550,sd=6550*0.3)
  COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.[which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==5900)]<-rnorm(length(which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==5900)),mean=5900,sd=5900*0.3)
  COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.[which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==5020)]<-rnorm(length(which(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.==5020)),mean=5020,sd=5020*0.3)
  COD.concentrations.of.leachate.in.Dumps..g.m3.<-mydata$COD.concentrations.of.leachate.in.Dumps..g.m3.
  COD.concentrations.of.leachate.in.Dumps..g.m3.[which(mydata$COD.concentrations.of.leachate.in.Dumps..g.m3.==8000&mydata$X2001.2008=="Arid and semi-arid")]<-runif(which(mydata$COD.concentrations.of.leachate.in.Dumps..g.m3.==8000&mydata$X2001.2008的分区=="Arid and semi-arid"),min=2000,max=15000)
  COD.concentrations.of.leachate.in.Dumps..g.m3.[which(mydata$COD.concentrations.of.leachate.in.Dumps..g.m3.==8000&mydata$X2001.2008=="Semi-wet")]<-runif(length(which(mydata$COD.concentrations.of.leachate.in.Dumps..g.m3.==8000&mydata$X2001.2008的分区=="Semi-wet")),min=1000,max=35000)
  COD.concentrations.of.leachate.in.Dumps..g.m3.[which(COD.concentrations.of.leachate.in.Dumps..g.m3.==7000)]<-runif(length(which(COD.concentrations.of.leachate.in.Dumps..g.m3.==7000)),min=700,max=35000)
  COD.concentrations.of.leachate.in.Dumps..g.m3.[which(COD.concentrations.of.leachate.in.Dumps..g.m3.==6500)]<-runif(length(which(COD.concentrations.of.leachate.in.Dumps..g.m3.==6500)),min=700,max=20000)
  COD.concentrations.of.leachate.in.Dumps..g.m3.[which(COD.concentrations.of.leachate.in.Dumps..g.m3.==6000)]<-rnorm(length(which(COD.concentrations.of.leachate.in.Dumps..g.m3.==6000)),mean=6000,sd=6000*0.3)
  COD.concentrations.of.leachate.in.Dumps..g.m3.[which(COD.concentrations.of.leachate.in.Dumps..g.m3.==5150)]<-rnorm(length(which(COD.concentrations.of.leachate.in.Dumps..g.m3.==5150)),mean=5150,sd=5150*0.3)
  COD.concentrations.of.leachate.in.Dumps..g.m3.[which(COD.concentrations.of.leachate.in.Dumps..g.m3.==4380)]<-rnorm(length(which(COD.concentrations.of.leachate.in.Dumps..g.m3.==4380)),mean=4380,sd=4380*0.3)
  COD.concentrations.of.leachate.in.Dumps..g.m3.[which(COD.concentrations.of.leachate.in.Dumps..g.m3.==3720)]<-rnorm(length(which(COD.concentrations.of.leachate.in.Dumps..g.m3.==3720)),mean=3720,sd=3720*0.3)
  a=data.frame(Prefecture.level.city,Prefecture.level.city.code,Year,Year.Code,Sanitary.landfill..wt.,Dump..wt.,Organic.fractions,Ash.and.stone,Paper,Plastic.and.Rubber,Textile,Wood,Metal,Glass,
               Others,Carbon.content.of.Organic.Fractions,Carbon.content.of.Ash.and.Stone,Carbon.content.of.Paper,Carbon.content.of.Plastic.and.Rubber,Carbon.content.of.Textile,Carbon.content.of.Wood,Carbon.content.of.Metal,Carbon.content.of.Glass,
               Carbon.content.of.Others,Methane.oxidation.factor.of.Sanitary.landfills,Methane.oxidation.factor.of.Dump,Methane.correction.factor.of.Sanitary.landfills..MCF.,
               Methane.correction.factor.of.Dumps..MCF.,Methane.collection.rate.of.Sanitayr.landfills....,Methane.collection.rate.of.Dumps....,Methane.production.rate.coefficient.of.Paper.Textile..k.,
               Methane.production.rate.coefficient.of.Wood..k.,Methane.production.rate.coefficient.of.Garden.waste..k.,Methane.production.rate.coefficient.of.Organic.fractions..k.,Decomposable.biodegradable.organic.carbon.of.Organic.fractions..DOCf.,Decomposable.biodegradable.organic.carbon.of.Wood..DOCf.,Decomposable.biodegradable.organic.carbon.of.Paper..DOCf.,Decomposable.biodegradable.organic.carbon.of.Textile..DOCf.,
               Decomposable.biodegradable.organic.carbon.of.Garden.waste..DOCf.,Degradable.organic.carbon.of.Paper..DOC....,Degradable.organic.carbon.of.Wood..DOC....,Degradable.organic.carbon.of.Textile..DOC....,Degradable.organic.carbon.of.Organic.fractions..DOC....,
               The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.,The.generation.of.leachate.in.Dumps..m3.t.waste.,
               COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.,COD.concentrations.of.leachate.in.Dumps..g.m3.)
  mydata<-a
  newdata1=c()
  for(i in 1:352){
    for(T in 1:20){
      a<-mydata[Prefecture.level.city.code==i,]
      G=a$Sanitary.landfill..wt.[Year.Code=T]*(a$Organic.fractions[Year.Code=T]*a$Carbon.content.of.Organic.Fractions[Year.Code=T]+a$Plastic.and.Rubber[Year.Code=T]*a$Carbon.content.of.Plastic.and.Rubber[Year.Code=T]+a$Paper[Year.Code=T]*a$Carbon.content.of.Paper[Year.Code=T]+
                           a$Textile[Year.Code=T]*a$Carbon.content.of.Textile[Year.Code=T]+a$Wood[Year.Code=T]*a$Carbon.content.of.Wood[Year.Code=T])-
        a$Sanitary.landfill..wt.[Year.Code=T]*a$The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.[Year.Code=T]*(a$COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.[Year.Code=T]+0.0582)/3/1000000+
        a$Dump..wt.[Year.Code=T]*(a$Organic.fractions[Year.Code=T]*a$Carbon.content.of.Organic.Fractions[Year.Code=T]+a$Plastic.and.Rubber[Year.Code=T]*a$Carbon.content.of.Plastic.and.Rubber[Year.Code=T]+a$Paper[Year.Code=T]*a$Carbon.content.of.Paper[Year.Code=T]+
                           a$Textile[Year.Code=T]*a$Carbon.content.of.Textile[Year.Code=T]+a$Wood[Year.Code=T]*a$Carbon.content.of.Wood[Year.Code=T])-
        a$Dump..wt.[Year.Code=T]*a$The.generation.of.leachate.in.Dumps..m3.t.waste.[Year.Code=T]*(a$COD.concentrations.of.leachate.in.Dumps..g.m3.[Year.Code=T]+0.0582)/3/1000000
      t=T
      repeat{G=-(exp((t-T)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                              a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                              a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                              a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t]))-
                   exp((t-T-1)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                                  a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                                  a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                                  a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t])))*(a$Organic.fractions[Year.Code=t]*a$Degradable.organic.carbon.of.Organic.fractions..DOC....[Year.Code=t]
                                                                 +a$Paper[Year.Code=t]*a$Degradable.organic.carbon.of.Paper..DOC....[Year.Code=t]
                                                                 +a$Textile[Year.Code=t]*a$Degradable.organic.carbon.of.Textile..DOC....[Year.Code=t]
                                                                 +a$Wood[Year.Code=t]*a$Degradable.organic.carbon.of.Wood..DOC....[Year.Code=t])/100*(a$Organic.fractions[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Organic.fractions..DOCf.[Year.Code=t]+
                                                                                                          a$Paper[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Paper..DOCf.[Year.Code=t]+
                                                                                                          a$Textile[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Textile..DOCf.[Year.Code=t]+
                                                                                                          a$Wood[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Wood..DOCf.[Year.Code=t])*a$Sanitary.landfill..wt.[Year.Code=t]*a$Methane.correction.factor.of.Sanitary.landfills..MCF.[Year.Code=t]*(1-a$Methane.oxidation.factor.of.Sanitary.landfills[Year.Code=t])-
        (exp((t-T)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                      a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                      a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                      a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t]))-
           exp((t-T-1)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                          a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                          a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                          a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t])))*(a$Organic.fractions[Year.Code=t]*a$Degradable.organic.carbon.of.Organic.fractions..DOC....[Year.Code=t]
                                                         +a$Paper[Year.Code=t]*a$Degradable.organic.carbon.of.Paper..DOC....[Year.Code=t]
                                                         +a$Textile[Year.Code=t]*a$Degradable.organic.carbon.of.Textile..DOC....[Year.Code=t]
                                                         +a$Wood[Year.Code=t]*a$Degradable.organic.carbon.of.Wood..DOC....[Year.Code=t])/100*(a$Organic.fractions[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Organic.fractions..DOCf.[Year.Code=t]+
                                                                                                  a$Paper[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Paper..DOCf.[Year.Code=t]+
                                                                                                  a$Textile[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Textile..DOCf.[Year.Code=t]+
                                                                                                  a$Wood[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Wood..DOCf.[Year.Code=t])*a$Dump..wt.[Year.Code=t]*a$Methane.correction.factor.of.Dumps..MCF.[Year.Code=t]*(1-a$Methane.oxidation.factor.of.Dump[Year.Code=t])+G
      t=t-1
      if(t<1){
        newdata1=rbind(newdata1,G)
        break
      }
      }
    }
  }
  
  carbonstock<-newdata1[,1]
  b<-data.frame(Prefecture.level.city,Prefecture.level.city.code,Year,Year.Code,carbonstock)
  
  newdata1=c()
  for(i in 1:352){
    for(T in 1:20){
      a<-b[Prefecture.level.city.code==i,]
      K=0
      t=T
      repeat{K=a$carbonstock[Year.Code=t]+K
      t=t-1
      if(t<1){
        newdata1=rbind(newdata1,K)
        break
      }
      }
    }
  }
  cumulativecarbonstock<-newdata1[,1]
  c<-data.frame(Prefecture.level.city,Prefecture.level.city.code,Year,Year.Code,cumulativecarbonstock)
  write.csv(c,paste("C:/Users/13094/Desktop/Nature Cities/Uncertainty Analysis/", "uncertainty of organic carbon stock", mmm, ".csv", sep = ""))
}
result<-parLapply(cl,1:1000,func)
getwd()
list.files("C:/Users/13094/Desktop/Nature Cities/Uncertainty Analysis/", pattern = "*.csv") -> all_input
#all_input
#install.packages("tidyverse")
library(tidyverse)
df=read.csv('C:/Users/13094/Desktop/Nature Cities/Uncertainty Analysis/uncertainty of organic carbon stock63828854.csv',sep=",",header=TRUE)
newdata1 = c(df[which(df$Year=='2020'),])
for (filename in all_input){
  df <- read.csv(file = paste0("C:/Users/13094/Desktop/Nature Cities/Uncertainty Analysis/", filename))
  a = df[which(df$Year=='2020'),]
  newdata1 = rbind(newdata1,a)
}
head(newdata1)
length(newdata1[,1])
write.csv(newdata1,paste('C:/Users/13094/Desktop/Nature Cities/uncertainty.csv', sep = ""))
newdata2 = c()
newdata3 = c()
for (i in 1:352) {
  k = mean(newdata1[which(newdata1$Prefecture.level.city.code==i),6])
  kk = sd(newdata1[which(newdata1$Prefecture.level.city.code==i),6])
  newdata2 = c(newdata2, k)
  newdata3 = c(newdata3, kk)
}
CV = 1.96*newdata3/newdata2
