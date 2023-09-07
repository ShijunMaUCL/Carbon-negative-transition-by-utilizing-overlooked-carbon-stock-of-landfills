############################################################################policy module############################################################################
##################################################baseline scenario#############################################################
#Input carbon in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/baseline scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
Inputcarbonoforganicfractionsindumps=Dump..wt.*Organic.fractions*Carbon.content.of.Organic.Fractions
Inputcarbonofashandstoneindumps=Dump..wt.*Ash.and.stone*Carbon.content.of.Ash.and.Stone
Inputcarbonofpaperindumps=Dump..wt.*Paper*Carbon.content.of.Paper
Inputcarbonofplasticandrubberindumps=Dump..wt.*Plastic.and.Rubber*Carbon.content.of.Plastic.and.Rubber
Inputcarbonoftextileindumps=Dump..wt.*Textile*Carbon.content.of.Textile
Inputcarbonofwoodindumps=Dump..wt.*Wood*Carbon.content.of.Wood
Inputcarbonofmetalindumps=Dump..wt.*Metal*Carbon.content.of.Metal
Inputcarbonofglassindumps=Dump..wt.*Glass*Carbon.content.of.Glass
Inputcarbonofothersindumps=Dump..wt.*Others*Carbon.content.of.Others
Totalinputcarbonindumps=Inputcarbonoforganicfractionsindumps+Inputcarbonofashandstoneindumps+Inputcarbonofpaperindumps+
  Inputcarbonofplasticandrubberindumps+Inputcarbonoftextileindumps+Inputcarbonofwoodindumps+Inputcarbonofglassindumps+Inputcarbonofmetalindumps+Inputcarbonofothersindumps
Inputcarbonindumps=data.frame(Inputcarbonoforganicfractionsindumps,Inputcarbonofashandstoneindumps,Inputcarbonofpaperindumps,Inputcarbonofplasticandrubberindumps,
                    Inputcarbonoftextileindumps,Inputcarbonofwoodindumps,Inputcarbonofglassindumps,Inputcarbonofmetalindumps,Inputcarbonofothersindumps,Totalinputcarbonindumps)
write.csv(Inputcarbonindumps,"C:/Users/13094/Desktop/Nature Cities/file/baseline scenario Input carbon in dumps.csv")

#Carbon of leachate in dumps
Carbonofleachateindumps=Dump..wt.*The.generation.of.leachate.in.Dumps..m3.t.waste.*(COD.concentrations.of.leachate.in.Dumps..g.m3.+0.0582)/3/1000/1000
write.csv(Carbonofleachateindumps,"C:/Users/13094/Desktop/Nature Cities/file/baseline scenario Carbon of leachate in dumps.csv")

#carbon of landfill gas in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/baseline scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
carbonoflandfillgasindumps=c()
newdata2=c()
for(i in 1:1){
  for(T in 1:30){
    a<-mydata[Country.Code==i,]
    G=0
    t=T
    repeat{G=(exp((t-T)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                           a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t]))-
                exp((t-T-1)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                               a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t])))*(a$Organic.fractions[Year.Code=t]*a$Degradable.organic.carbon.of.Organic.fractions..DOC....[Year.Code=t]
                                                              +a$Paper[Year.Code=t]*a$Degradable.organic.carbon.of.Paper..DOC...[Year.Code=t]
                                                              +a$Textile[Year.Code=t]*a$Degradable.organic.carbon.of.Textile..DOC....[Year.Code=t]
                                                              +a$Wood[Year.Code=t]*a$Degradable.organic.carbon.of.Wood..DOC....[Year.Code=t])/100*(a$Organic.fractions[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Organic.fractions..DOCf.[Year.Code=t]+
                                                                                                       a$Paper[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Paper..DOCf.[Year.Code=t]+
                                                                                                       a$Textile[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Textile..DOCf.[Year.Code=t]+
                                                                                                       a$Wood[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Wood..DOCf.[Year.Code=t])*a$Dump..wt.[Year.Code=t]*a$Methane.correction.factor.of.Dumps..MCF.[Year.Code=t]*(1-a$Methane.oxidation.factor.of.Dump[Year.Code=t])+G
    t=t-1
    if(t<1){
      carbonoflandfillgasindumps=rbind(carbonoflandfillgasindumps,G)
      break
    }
    }
  }
}
write.csv(carbonoflandfillgasindumps,"C:/Users/13094/Desktop/Nature Cities/file/baselinescenariocarbonoflandfillgasindumps.csv")

#Input carbon in sanitary landfill
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/baseline scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
Inputcarbonoforganicfractionsinsanitarylandfill=Sanitary.landfill..wt.*Organic.fractions*Carbon.content.of.Organic.Fractions
Inputcarbonofashandstoneinsanitarylandfill=Sanitary.landfill..wt.*Ash.and.stone*Carbon.content.of.Ash.and.Stone
Inputcarbonofpaperinsanitarylandfill=Sanitary.landfill..wt.*Paper*Carbon.content.of.Paper
Inputcarbonofplasticandrubberinsanitarylandfill=Sanitary.landfill..wt.*Plastic.and.Rubber*Carbon.content.of.Plastic.and.Rubber
Inputcarbonoftextileinsanitarylandfill=Sanitary.landfill..wt.*Textile*Carbon.content.of.Textile
Inputcarbonofwoodinsanitarylandfill=Sanitary.landfill..wt.*Wood*Carbon.content.of.Wood
Inputcarbonofmetalinsanitarylandfill=Sanitary.landfill..wt.*Metal*Carbon.content.of.Metal
Inputcarbonofglassinsanitarylandfill=Sanitary.landfill..wt.*Glass*Carbon.content.of.Glass
Inputcarbonofothersinsanitarylandfill=Sanitary.landfill..wt.*Others*Carbon.content.of.Others
Totalinputcarboninsanitarylandfill=Inputcarbonoforganicfractionsinsanitarylandfill+Inputcarbonofashandstoneinsanitarylandfill+Inputcarbonofpaperinsanitarylandfill+
  Inputcarbonofplasticandrubberinsanitarylandfill+Inputcarbonoftextileinsanitarylandfill+Inputcarbonofwoodinsanitarylandfill+Inputcarbonofglassinsanitarylandfill+Inputcarbonofmetalinsanitarylandfill+Inputcarbonofothersinsanitarylandfill
Inputcarboninsanitarylandfill=data.frame(Inputcarbonoforganicfractionsinsanitarylandfill,Inputcarbonofashandstoneinsanitarylandfill,Inputcarbonofpaperinsanitarylandfill,Inputcarbonofplasticandrubberinsanitarylandfill,
                              Inputcarbonoftextileinsanitarylandfill,Inputcarbonofwoodinsanitarylandfill,Inputcarbonofglassinsanitarylandfill,Inputcarbonofmetalinsanitarylandfill,Inputcarbonofothersinsanitarylandfill,Totalinputcarboninsanitarylandfill)
write.csv(Inputcarboninsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/baseline scenario Input carbon in sanitary landfill.csv")

#Carbon of leachate in sanitary landfill
Carbonofleachateinsanitarylandfill=Sanitary.landfill..wt.*The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.*(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.+0.0582)/3/1000/1000
write.csv(Carbonofleachateinsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/baseline scenario Carbon of leachate in sanitary landfill.csv")

#carbon of landfill gas in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/baseline scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
carbonoflandfillgasinsanitarylandfill=c()
newdata2=c()
for(i in 1:1){
  for(T in 1:30){
    a<-mydata[Country.Code==i,]
    G=0
    t=T
    repeat{G=(exp((t-T)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                           a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t]))-
                exp((t-T-1)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                               a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t])))*(a$Organic.fractions[Year.Code=t]*a$Degradable.organic.carbon.of.Organic.fractions..DOC....[Year.Code=t]
                                                                                                                      +a$Paper[Year.Code=t]*a$Degradable.organic.carbon.of.Paper..DOC...[Year.Code=t]
                                                                                                                      +a$Textile[Year.Code=t]*a$Degradable.organic.carbon.of.Textile..DOC....[Year.Code=t]
                                                                                                                      +a$Wood[Year.Code=t]*a$Degradable.organic.carbon.of.Wood..DOC....[Year.Code=t])/100*(a$Organic.fractions[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Organic.fractions..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Paper[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Paper..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Textile[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Textile..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Wood[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Wood..DOCf.[Year.Code=t])*a$Sanitary.landfill..wt.[Year.Code=t]*a$Methane.correction.factor.of.Sanitary.landfills..MCF.[Year.Code=t]*(1-a$Methane.oxidation.factor.of.Sanitary.landfills[Year.Code=t])+G
    t=t-1
    if(t<1){
      carbonoflandfillgasinsanitarylandfill=rbind(carbonoflandfillgasinsanitarylandfill,G)
      break
    }
    }
  }
}
write.csv(carbonoflandfillgasinsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/baselinescenariocarbonoflandfillgasinsanitarylandfill.csv")

# final results
Result = data.frame(Inputcarbonindumps,carbonoflandfillgasindumps,Carbonofleachateindumps, Inputcarboninsanitarylandfill,carbonoflandfillgasinsanitarylandfill,Carbonofleachateinsanitarylandfill)
head(Result)
Organiccarbonstock = Result$Inputcarbonoforganicfractionsindumps+Result$Inputcarbonofpaperindumps+Result$Inputcarbonofplasticandrubberindumps+Result$Inputcarbonoftextileindumps+
  Result$Inputcarbonofwoodindumps-Result$carbonoflandfillgasindumps-Result$Carbonofleachateindumps+Result$Inputcarbonoforganicfractionsinsanitarylandfill+Result$Inputcarbonofpaperinsanitarylandfill+
  Result$Inputcarbonofplasticandrubberinsanitarylandfill+Result$Inputcarbonoftextileinsanitarylandfill+Result$Inputcarbonofwoodinsanitarylandfill-Result$carbonoflandfillgasinsanitarylandfill-Result$Carbonofleachateinsanitarylandfill
Cumulativeorganiccarbonstock = c(Organiccarbonstock[1])
for (n in 2:30) {
  a1 = sum(Organiccarbonstock[1:n])
  Cumulativeorganiccarbonstock = c(Cumulativeorganiccarbonstock, a1)
}

Methaneemission = (Result$carbonoflandfillgasindumps+Result$carbonoflandfillgasinsanitarylandfill*0.76)*0.5/12*16
Cumulativemethaneemission = c(Methaneemission[1])
for (n in 2:30) {
  a1 = sum(Methaneemission[1:n])
  Cumulativemethaneemission = c(Cumulativemethaneemission, a1)
}
Cumulativemethaneemissioncarbondioxideequivalent = Cumulativemethaneemission*27.9
Soillikematerialcarbonstock = Result$Inputcarbonoforganicfractionsindumps+Result$Inputcarbonoforganicfractionsinsanitarylandfill+Result$Inputcarbonofpaperindumps+
  Result$Inputcarbonofpaperinsanitarylandfill-
  Result$carbonoflandfillgasindumps-Result$Carbonofleachateindumps-Result$carbonoflandfillgasinsanitarylandfill-
  Result$Carbonofleachateinsanitarylandfill
Plasticcarbonstock = Result$Inputcarbonofplasticandrubberindumps+Result$Inputcarbonofplasticandrubberinsanitarylandfill
Textilecarbonstock = Result$Inputcarbonoftextileindumps+Result$Inputcarbonoftextileinsanitarylandfill
Woodcarbonstock = Result$Inputcarbonofwoodindumps+Result$Inputcarbonofwoodinsanitarylandfill
Cumulativesoillikematerialcarbonstock = c(Soillikematerialcarbonstock[1])
for (n in 2:30) {
  a1 = sum(Soillikematerialcarbonstock[1:n])
  Cumulativesoillikematerialcarbonstock = c(Cumulativesoillikematerialcarbonstock, a1)
}
Cumulativeplasticcarbonstock = c(Plasticcarbonstock[1])
for (n in 2:30) {
  a1 = sum(Plasticcarbonstock[1:n])
  Cumulativeplasticcarbonstock = c(Cumulativeplasticcarbonstock, a1)
}
Cumulativetextilecarbonstock = c(Textilecarbonstock[1])
for (n in 2:30) {
  a1 = sum(Textilecarbonstock[1:n])
  Cumulativetextilecarbonstock = c(Cumulativetextilecarbonstock, a1)
}
Cumulativewoodcarbonstock = c(Woodcarbonstock[1])
for (n in 2:30) {
  a1 = sum(Woodcarbonstock[1:n])
  Cumulativewoodcarbonstock = c(Cumulativewoodcarbonstock, a1)
}
Finalresult = data.frame(Result,Organiccarbonstock,Cumulativeorganiccarbonstock,Methaneemission,
                         Cumulativemethaneemission,Cumulativemethaneemissioncarbondioxideequivalent,
                         Soillikematerialcarbonstock,Plasticcarbonstock,Textilecarbonstock,
                         Woodcarbonstock,Cumulativesoillikematerialcarbonstock,Cumulativeplasticcarbonstock,
                         Cumulativetextilecarbonstock,Cumulativewoodcarbonstock)
colnames(Finalresult) <- c("Input carbon of organic fractions in dumps","Input carbon of ash and stone in dumps","Input carbon of paper in dumps","Input carbon of plastic and rubber in dumps",
                      "Input carbon of textile in dumps","Input carbon of wood in dumps","Input carbon of glass in dumps","Input carbon of metal in dumps",
                      "Input carbon of others in dumps","Total input carbon in dumps","Carbon of landfill gas in dumps","Carbon of leachate in dumps",
                      "Input carbon of organic fractions in sanitary landfills","Input carbon of ash and stone in sanitary landfills","Input carbon of paper in sanitary landfills","Input carbon of plastic and rubber in sanitary landfills",
                      "Input carbon of textile in sanitary landfills","Input carbon of wood in sanitary landfills","Input carbon of glass in sanitary landfills","Input carbon of metal in sanitary landfills",
                      "Input carbon of others in sanitary landfills","Total input carbon in sanitary landfills","Carbon of landfill gas in sanitary landfills","Carbon of leachate in sanitary landfills",
                      "Organic carbon stock","Cumulative organic carbon stock","Methaneemission",
                      "Cumulative methane emission","Cumulative methane emission carbon dioxide equivalent",
                      "Soil-like material carbon stock","Plastic carbon stock","Textile carbon stock",
                      "Wood carbon stock","Cumulative soil-like material carbon stock","Cumulative plastic carbon stock",
                      "Cumulative textile carbon stock","Cumulative wood carbon stock")
write.csv(Finalresult,"C:/Users/13094/Desktop/Nature Cities/file/baselinescenarioresult.csv")




##################################################high LFG collection rate#############################################################
#Input carbon in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/high LFG collection rate.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
Inputcarbonoforganicfractionsindumps=Dump..wt.*Organic.fractions*Carbon.content.of.Organic.Fractions
Inputcarbonofashandstoneindumps=Dump..wt.*Ash.and.stone*Carbon.content.of.Ash.and.Stone
Inputcarbonofpaperindumps=Dump..wt.*Paper*Carbon.content.of.Paper
Inputcarbonofplasticandrubberindumps=Dump..wt.*Plastic.and.Rubber*Carbon.content.of.Plastic.and.Rubber
Inputcarbonoftextileindumps=Dump..wt.*Textile*Carbon.content.of.Textile
Inputcarbonofwoodindumps=Dump..wt.*Wood*Carbon.content.of.Wood
Inputcarbonofmetalindumps=Dump..wt.*Metal*Carbon.content.of.Metal
Inputcarbonofglassindumps=Dump..wt.*Glass*Carbon.content.of.Glass
Inputcarbonofothersindumps=Dump..wt.*Others*Carbon.content.of.Others
Totalinputcarbonindumps=Inputcarbonoforganicfractionsindumps+Inputcarbonofashandstoneindumps+Inputcarbonofpaperindumps+
  Inputcarbonofplasticandrubberindumps+Inputcarbonoftextileindumps+Inputcarbonofwoodindumps+Inputcarbonofglassindumps+Inputcarbonofmetalindumps+Inputcarbonofothersindumps
Inputcarbonindumps=data.frame(Inputcarbonoforganicfractionsindumps,Inputcarbonofashandstoneindumps,Inputcarbonofpaperindumps,Inputcarbonofplasticandrubberindumps,
                              Inputcarbonoftextileindumps,Inputcarbonofwoodindumps,Inputcarbonofglassindumps,Inputcarbonofmetalindumps,Inputcarbonofothersindumps,Totalinputcarbonindumps)
write.csv(Inputcarbonindumps,"C:/Users/13094/Desktop/Nature Cities/file/high LFG collection rate Input carbon in dumps.csv")

#Carbon of leachate in dumps
Carbonofleachateindumps=Dump..wt.*The.generation.of.leachate.in.Dumps..m3.t.waste.*(COD.concentrations.of.leachate.in.Dumps..g.m3.+0.0582)/3/1000/1000
write.csv(Carbonofleachateindumps,"C:/Users/13094/Desktop/Nature Cities/file/high LFG collection rate Carbon of leachate in dumps.csv")

#carbon of landfill gas in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/high LFG collection rate.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
carbonoflandfillgasindumps=c()
newdata2=c()
for(i in 1:1){
  for(T in 1:30){
    a<-mydata[Country.Code==i,]
    G=0
    t=T
    repeat{G=(exp((t-T)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                           a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t]))-
                exp((t-T-1)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                               a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t])))*(a$Organic.fractions[Year.Code=t]*a$Degradable.organic.carbon.of.Organic.fractions..DOC....[Year.Code=t]
                                                                                                                      +a$Paper[Year.Code=t]*a$Degradable.organic.carbon.of.Paper..DOC...[Year.Code=t]
                                                                                                                      +a$Textile[Year.Code=t]*a$Degradable.organic.carbon.of.Textile..DOC....[Year.Code=t]
                                                                                                                      +a$Wood[Year.Code=t]*a$Degradable.organic.carbon.of.Wood..DOC....[Year.Code=t])/100*(a$Organic.fractions[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Organic.fractions..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Paper[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Paper..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Textile[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Textile..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Wood[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Wood..DOCf.[Year.Code=t])*a$Dump..wt.[Year.Code=t]*a$Methane.correction.factor.of.Dumps..MCF.[Year.Code=t]*(1-a$Methane.oxidation.factor.of.Dump[Year.Code=t])+G
    t=t-1
    if(t<1){
      carbonoflandfillgasindumps=rbind(carbonoflandfillgasindumps,G)
      break
    }
    }
  }
}
write.csv(carbonoflandfillgasindumps,"C:/Users/13094/Desktop/Nature Cities/file/highLFGcollectionratecarbonoflandfillgasindumps.csv")

#Input carbon in sanitary landfill
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/high LFG collection rate.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
Inputcarbonoforganicfractionsinsanitarylandfill=Sanitary.landfill..wt.*Organic.fractions*Carbon.content.of.Organic.Fractions
Inputcarbonofashandstoneinsanitarylandfill=Sanitary.landfill..wt.*Ash.and.stone*Carbon.content.of.Ash.and.Stone
Inputcarbonofpaperinsanitarylandfill=Sanitary.landfill..wt.*Paper*Carbon.content.of.Paper
Inputcarbonofplasticandrubberinsanitarylandfill=Sanitary.landfill..wt.*Plastic.and.Rubber*Carbon.content.of.Plastic.and.Rubber
Inputcarbonoftextileinsanitarylandfill=Sanitary.landfill..wt.*Textile*Carbon.content.of.Textile
Inputcarbonofwoodinsanitarylandfill=Sanitary.landfill..wt.*Wood*Carbon.content.of.Wood
Inputcarbonofmetalinsanitarylandfill=Sanitary.landfill..wt.*Metal*Carbon.content.of.Metal
Inputcarbonofglassinsanitarylandfill=Sanitary.landfill..wt.*Glass*Carbon.content.of.Glass
Inputcarbonofothersinsanitarylandfill=Sanitary.landfill..wt.*Others*Carbon.content.of.Others
Totalinputcarboninsanitarylandfill=Inputcarbonoforganicfractionsinsanitarylandfill+Inputcarbonofashandstoneinsanitarylandfill+Inputcarbonofpaperinsanitarylandfill+
  Inputcarbonofplasticandrubberinsanitarylandfill+Inputcarbonoftextileinsanitarylandfill+Inputcarbonofwoodinsanitarylandfill+Inputcarbonofglassinsanitarylandfill+Inputcarbonofmetalinsanitarylandfill+Inputcarbonofothersinsanitarylandfill
Inputcarboninsanitarylandfill=data.frame(Inputcarbonoforganicfractionsinsanitarylandfill,Inputcarbonofashandstoneinsanitarylandfill,Inputcarbonofpaperinsanitarylandfill,Inputcarbonofplasticandrubberinsanitarylandfill,
                                         Inputcarbonoftextileinsanitarylandfill,Inputcarbonofwoodinsanitarylandfill,Inputcarbonofglassinsanitarylandfill,Inputcarbonofmetalinsanitarylandfill,Inputcarbonofothersinsanitarylandfill,Totalinputcarboninsanitarylandfill)
write.csv(Inputcarboninsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/high LFG collection rate Input carbon in sanitary landfill.csv")

#Carbon of leachate in sanitary landfill
Carbonofleachateinsanitarylandfill=Sanitary.landfill..wt.*The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.*(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.+0.0582)/3/1000/1000
write.csv(Carbonofleachateinsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/high LFG collection rate Carbon of leachate in sanitary landfill.csv")

#carbon of landfill gas in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/high LFG collection rate.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
carbonoflandfillgasinsanitarylandfill=c()
newdata2=c()
for(i in 1:1){
  for(T in 1:30){
    a<-mydata[Country.Code==i,]
    G=0
    t=T
    repeat{G=(exp((t-T)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                           a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t]))-
                exp((t-T-1)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                               a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t])))*(a$Organic.fractions[Year.Code=t]*a$Degradable.organic.carbon.of.Organic.fractions..DOC....[Year.Code=t]
                                                                                                                      +a$Paper[Year.Code=t]*a$Degradable.organic.carbon.of.Paper..DOC...[Year.Code=t]
                                                                                                                      +a$Textile[Year.Code=t]*a$Degradable.organic.carbon.of.Textile..DOC....[Year.Code=t]
                                                                                                                      +a$Wood[Year.Code=t]*a$Degradable.organic.carbon.of.Wood..DOC....[Year.Code=t])/100*(a$Organic.fractions[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Organic.fractions..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Paper[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Paper..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Textile[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Textile..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Wood[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Wood..DOCf.[Year.Code=t])*a$Sanitary.landfill..wt.[Year.Code=t]*a$Methane.correction.factor.of.Sanitary.landfills..MCF.[Year.Code=t]*(1-a$Methane.oxidation.factor.of.Sanitary.landfills[Year.Code=t])+G
    t=t-1
    if(t<1){
      carbonoflandfillgasinsanitarylandfill=rbind(carbonoflandfillgasinsanitarylandfill,G)
      break
    }
    }
  }
}
write.csv(carbonoflandfillgasinsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/highLFGcollectionratecarbonoflandfillgasinsanitarylandfill.csv")

# final results
Result = data.frame(Inputcarbonindumps,carbonoflandfillgasindumps,Carbonofleachateindumps, Inputcarboninsanitarylandfill,carbonoflandfillgasinsanitarylandfill,Carbonofleachateinsanitarylandfill)
head(Result)
Organiccarbonstock = Result$Inputcarbonoforganicfractionsindumps+Result$Inputcarbonofpaperindumps+Result$Inputcarbonofplasticandrubberindumps+Result$Inputcarbonoftextileindumps+
  Result$Inputcarbonofwoodindumps-Result$carbonoflandfillgasindumps-Result$Carbonofleachateindumps+Result$Inputcarbonoforganicfractionsinsanitarylandfill+Result$Inputcarbonofpaperinsanitarylandfill+
  Result$Inputcarbonofplasticandrubberinsanitarylandfill+Result$Inputcarbonoftextileinsanitarylandfill+Result$Inputcarbonofwoodinsanitarylandfill-Result$carbonoflandfillgasinsanitarylandfill-Result$Carbonofleachateinsanitarylandfill
Cumulativeorganiccarbonstock = c(Organiccarbonstock[1])
for (n in 2:30) {
  a1 = sum(Organiccarbonstock[1:n])
  Cumulativeorganiccarbonstock = c(Cumulativeorganiccarbonstock, a1)
}

Methaneemission = (Result$carbonoflandfillgasindumps+Result$carbonoflandfillgasinsanitarylandfill*(1-mydata$Methane.collection.rate.of.Sanitayr.landfills..../100))*0.5/12*16
Cumulativemethaneemission = c(Methaneemission[1])
for (n in 2:30) {
  a1 = sum(Methaneemission[1:n])
  Cumulativemethaneemission = c(Cumulativemethaneemission, a1)
}
Cumulativemethaneemissioncarbondioxideequivalent = Cumulativemethaneemission*27.9
Soillikematerialcarbonstock = Result$Inputcarbonoforganicfractionsindumps+Result$Inputcarbonoforganicfractionsinsanitarylandfill+Result$Inputcarbonofpaperindumps+
  Result$Inputcarbonofpaperinsanitarylandfill-
  Result$carbonoflandfillgasindumps-Result$Carbonofleachateindumps-Result$carbonoflandfillgasinsanitarylandfill-
  Result$Carbonofleachateinsanitarylandfill
Plasticcarbonstock = Result$Inputcarbonofplasticandrubberindumps+Result$Inputcarbonofplasticandrubberinsanitarylandfill
Textilecarbonstock = Result$Inputcarbonoftextileindumps+Result$Inputcarbonoftextileinsanitarylandfill
Woodcarbonstock = Result$Inputcarbonofwoodindumps+Result$Inputcarbonofwoodinsanitarylandfill
Cumulativesoillikematerialcarbonstock = c(Soillikematerialcarbonstock[1])
for (n in 2:30) {
  a1 = sum(Soillikematerialcarbonstock[1:n])
  Cumulativesoillikematerialcarbonstock = c(Cumulativesoillikematerialcarbonstock, a1)
}
Cumulativeplasticcarbonstock = c(Plasticcarbonstock[1])
for (n in 2:30) {
  a1 = sum(Plasticcarbonstock[1:n])
  Cumulativeplasticcarbonstock = c(Cumulativeplasticcarbonstock, a1)
}
Cumulativetextilecarbonstock = c(Textilecarbonstock[1])
for (n in 2:30) {
  a1 = sum(Textilecarbonstock[1:n])
  Cumulativetextilecarbonstock = c(Cumulativetextilecarbonstock, a1)
}
Cumulativewoodcarbonstock = c(Woodcarbonstock[1])
for (n in 2:30) {
  a1 = sum(Woodcarbonstock[1:n])
  Cumulativewoodcarbonstock = c(Cumulativewoodcarbonstock, a1)
}
Finalresult = data.frame(Result,Organiccarbonstock,Cumulativeorganiccarbonstock,Methaneemission,
                         Cumulativemethaneemission,Cumulativemethaneemissioncarbondioxideequivalent,
                         Soillikematerialcarbonstock,Plasticcarbonstock,Textilecarbonstock,
                         Woodcarbonstock,Cumulativesoillikematerialcarbonstock,Cumulativeplasticcarbonstock,
                         Cumulativetextilecarbonstock,Cumulativewoodcarbonstock)
colnames(Finalresult) <- c("Input carbon of organic fractions in dumps","Input carbon of ash and stone in dumps","Input carbon of paper in dumps","Input carbon of plastic and rubber in dumps",
                           "Input carbon of textile in dumps","Input carbon of wood in dumps","Input carbon of glass in dumps","Input carbon of metal in dumps",
                           "Input carbon of others in dumps","Total input carbon in dumps","Carbon of landfill gas in dumps","Carbon of leachate in dumps",
                           "Input carbon of organic fractions in sanitary landfills","Input carbon of ash and stone in sanitary landfills","Input carbon of paper in sanitary landfills","Input carbon of plastic and rubber in sanitary landfills",
                           "Input carbon of textile in sanitary landfills","Input carbon of wood in sanitary landfills","Input carbon of glass in sanitary landfills","Input carbon of metal in sanitary landfills",
                           "Input carbon of others in sanitary landfills","Total input carbon in sanitary landfills","Carbon of landfill gas in sanitary landfills","Carbon of leachate in sanitary landfills",
                           "Organic carbon stock","Cumulative organic carbon stock","Methaneemission",
                           "Cumulative methane emission","Cumulative methane emission carbon dioxide equivalent",
                           "Soil-like material carbon stock","Plastic carbon stock","Textile carbon stock",
                           "Wood carbon stock","Cumulative soil-like material carbon stock","Cumulative plastic carbon stock",
                           "Cumulative textile carbon stock","Cumulative wood carbon stock")
write.csv(Finalresult,"C:/Users/13094/Desktop/Nature Cities/file/highLFGcollectionrateresult.csv")


##################################################Minimizing landfill rate scenario#############################################################
#Input carbon in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/Minimizing landfill rate scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
Inputcarbonoforganicfractionsindumps=Dump..wt.*Organic.fractions*Carbon.content.of.Organic.Fractions
Inputcarbonofashandstoneindumps=Dump..wt.*Ash.and.stone*Carbon.content.of.Ash.and.Stone
Inputcarbonofpaperindumps=Dump..wt.*Paper*Carbon.content.of.Paper
Inputcarbonofplasticandrubberindumps=Dump..wt.*Plastic.and.Rubber*Carbon.content.of.Plastic.and.Rubber
Inputcarbonoftextileindumps=Dump..wt.*Textile*Carbon.content.of.Textile
Inputcarbonofwoodindumps=Dump..wt.*Wood*Carbon.content.of.Wood
Inputcarbonofmetalindumps=Dump..wt.*Metal*Carbon.content.of.Metal
Inputcarbonofglassindumps=Dump..wt.*Glass*Carbon.content.of.Glass
Inputcarbonofothersindumps=Dump..wt.*Others*Carbon.content.of.Others
Totalinputcarbonindumps=Inputcarbonoforganicfractionsindumps+Inputcarbonofashandstoneindumps+Inputcarbonofpaperindumps+
  Inputcarbonofplasticandrubberindumps+Inputcarbonoftextileindumps+Inputcarbonofwoodindumps+Inputcarbonofglassindumps+Inputcarbonofmetalindumps+Inputcarbonofothersindumps
Inputcarbonindumps=data.frame(Inputcarbonoforganicfractionsindumps,Inputcarbonofashandstoneindumps,Inputcarbonofpaperindumps,Inputcarbonofplasticandrubberindumps,
                              Inputcarbonoftextileindumps,Inputcarbonofwoodindumps,Inputcarbonofglassindumps,Inputcarbonofmetalindumps,Inputcarbonofothersindumps,Totalinputcarbonindumps)
write.csv(Inputcarbonindumps,"C:/Users/13094/Desktop/Nature Cities/file/Minimizing landfill rate scenario Input carbon in dumps.csv")

#Carbon of leachate in dumps
Carbonofleachateindumps=Dump..wt.*The.generation.of.leachate.in.Dumps..m3.t.waste.*(COD.concentrations.of.leachate.in.Dumps..g.m3.+0.0582)/3/1000/1000
write.csv(Carbonofleachateindumps,"C:/Users/13094/Desktop/Nature Cities/file/Minimizing landfill rate scenario Carbon of leachate in dumps.csv")

#carbon of landfill gas in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/Minimizing landfill rate scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
carbonoflandfillgasindumps=c()
newdata2=c()
for(i in 1:1){
  for(T in 1:30){
    a<-mydata[Country.Code==i,]
    G=0
    t=T
    repeat{G=(exp((t-T)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                           a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t]))-
                exp((t-T-1)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                               a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t])))*(a$Organic.fractions[Year.Code=t]*a$Degradable.organic.carbon.of.Organic.fractions..DOC....[Year.Code=t]
                                                                                                                      +a$Paper[Year.Code=t]*a$Degradable.organic.carbon.of.Paper..DOC...[Year.Code=t]
                                                                                                                      +a$Textile[Year.Code=t]*a$Degradable.organic.carbon.of.Textile..DOC....[Year.Code=t]
                                                                                                                      +a$Wood[Year.Code=t]*a$Degradable.organic.carbon.of.Wood..DOC....[Year.Code=t])/100*(a$Organic.fractions[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Organic.fractions..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Paper[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Paper..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Textile[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Textile..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Wood[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Wood..DOCf.[Year.Code=t])*a$Dump..wt.[Year.Code=t]*a$Methane.correction.factor.of.Dumps..MCF.[Year.Code=t]*(1-a$Methane.oxidation.factor.of.Dump[Year.Code=t])+G
    t=t-1
    if(t<1){
      carbonoflandfillgasindumps=rbind(carbonoflandfillgasindumps,G)
      break
    }
    }
  }
}
write.csv(carbonoflandfillgasindumps,"C:/Users/13094/Desktop/Nature Cities/file/Minimizinglandfillratescenariocarbonoflandfillgasindumps.csv")

#Input carbon in sanitary landfill
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/Minimizing landfill rate scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
Inputcarbonoforganicfractionsinsanitarylandfill=Sanitary.landfill..wt.*Organic.fractions*Carbon.content.of.Organic.Fractions
Inputcarbonofashandstoneinsanitarylandfill=Sanitary.landfill..wt.*Ash.and.stone*Carbon.content.of.Ash.and.Stone
Inputcarbonofpaperinsanitarylandfill=Sanitary.landfill..wt.*Paper*Carbon.content.of.Paper
Inputcarbonofplasticandrubberinsanitarylandfill=Sanitary.landfill..wt.*Plastic.and.Rubber*Carbon.content.of.Plastic.and.Rubber
Inputcarbonoftextileinsanitarylandfill=Sanitary.landfill..wt.*Textile*Carbon.content.of.Textile
Inputcarbonofwoodinsanitarylandfill=Sanitary.landfill..wt.*Wood*Carbon.content.of.Wood
Inputcarbonofmetalinsanitarylandfill=Sanitary.landfill..wt.*Metal*Carbon.content.of.Metal
Inputcarbonofglassinsanitarylandfill=Sanitary.landfill..wt.*Glass*Carbon.content.of.Glass
Inputcarbonofothersinsanitarylandfill=Sanitary.landfill..wt.*Others*Carbon.content.of.Others
Totalinputcarboninsanitarylandfill=Inputcarbonoforganicfractionsinsanitarylandfill+Inputcarbonofashandstoneinsanitarylandfill+Inputcarbonofpaperinsanitarylandfill+
  Inputcarbonofplasticandrubberinsanitarylandfill+Inputcarbonoftextileinsanitarylandfill+Inputcarbonofwoodinsanitarylandfill+Inputcarbonofglassinsanitarylandfill+Inputcarbonofmetalinsanitarylandfill+Inputcarbonofothersinsanitarylandfill
Totalinputorganiccarboninsanitarylandfill=Inputcarbonoforganicfractionsinsanitarylandfill+Inputcarbonofpaperinsanitarylandfill+
  Inputcarbonofplasticandrubberinsanitarylandfill+Inputcarbonoftextileinsanitarylandfill+Inputcarbonofwoodinsanitarylandfill
Inputcarboninsanitarylandfill=data.frame(Inputcarbonoforganicfractionsinsanitarylandfill,Inputcarbonofashandstoneinsanitarylandfill,Inputcarbonofpaperinsanitarylandfill,Inputcarbonofplasticandrubberinsanitarylandfill,
                                         Inputcarbonoftextileinsanitarylandfill,Inputcarbonofwoodinsanitarylandfill,Inputcarbonofglassinsanitarylandfill,Inputcarbonofmetalinsanitarylandfill,Inputcarbonofothersinsanitarylandfill,Totalinputcarboninsanitarylandfill)
write.csv(Inputcarboninsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/Minimizing landfill rate scenario Input carbon in sanitary landfill.csv")

#Carbon of leachate in sanitary landfill
Carbonofleachateinsanitarylandfill=Sanitary.landfill..wt.*The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.*(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.+0.0582)/3/1000/1000
write.csv(Carbonofleachateinsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/Minimizing landfill rate scenario Carbon of leachate in sanitary landfill.csv")

#carbon of landfill gas in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/Minimizing landfill rate scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
carbonoflandfillgasinsanitarylandfill=c()
newdata2=c()
for(i in 1:1){
  for(T in 1:30){
    a<-mydata[Country.Code==i,]
    G=0
    t=T
    repeat{G=(exp((t-T)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                           a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t]))-
                exp((t-T-1)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                               a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t])))*(a$Organic.fractions[Year.Code=t]*a$Degradable.organic.carbon.of.Organic.fractions..DOC....[Year.Code=t]
                                                                                                                      +a$Paper[Year.Code=t]*a$Degradable.organic.carbon.of.Paper..DOC...[Year.Code=t]
                                                                                                                      +a$Textile[Year.Code=t]*a$Degradable.organic.carbon.of.Textile..DOC....[Year.Code=t]
                                                                                                                      +a$Wood[Year.Code=t]*a$Degradable.organic.carbon.of.Wood..DOC....[Year.Code=t])/100*(a$Organic.fractions[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Organic.fractions..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Paper[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Paper..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Textile[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Textile..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Wood[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Wood..DOCf.[Year.Code=t])*a$Sanitary.landfill..wt.[Year.Code=t]*a$Methane.correction.factor.of.Sanitary.landfills..MCF.[Year.Code=t]*(1-a$Methane.oxidation.factor.of.Sanitary.landfills[Year.Code=t])+G
    t=t-1
    if(t<1){
      carbonoflandfillgasinsanitarylandfill=rbind(carbonoflandfillgasinsanitarylandfill,G)
      break
    }
    }
  }
}
write.csv(carbonoflandfillgasinsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/Minimizinglandfillratescenariocarbonoflandfillgasinsanitarylandfill.csv")

# final results
Result = data.frame(Inputcarbonindumps,carbonoflandfillgasindumps,Carbonofleachateindumps, Inputcarboninsanitarylandfill,carbonoflandfillgasinsanitarylandfill,Carbonofleachateinsanitarylandfill)
head(Result)
Organiccarbonstock = Result$Inputcarbonoforganicfractionsindumps+Result$Inputcarbonofpaperindumps+Result$Inputcarbonofplasticandrubberindumps+Result$Inputcarbonoftextileindumps+
  Result$Inputcarbonofwoodindumps-Result$carbonoflandfillgasindumps-Result$Carbonofleachateindumps+Result$Inputcarbonoforganicfractionsinsanitarylandfill+Result$Inputcarbonofpaperinsanitarylandfill+
  Result$Inputcarbonofplasticandrubberinsanitarylandfill+Result$Inputcarbonoftextileinsanitarylandfill+Result$Inputcarbonofwoodinsanitarylandfill-Result$carbonoflandfillgasinsanitarylandfill-Result$Carbonofleachateinsanitarylandfill
Cumulativeorganiccarbonstock = c(Organiccarbonstock[1])
for (n in 2:30) {
  a1 = sum(Organiccarbonstock[1:n])
  Cumulativeorganiccarbonstock = c(Cumulativeorganiccarbonstock, a1)
}

Methaneemission = (Result$carbonoflandfillgasindumps+Result$carbonoflandfillgasinsanitarylandfill*(1-mydata$Methane.collection.rate.of.Sanitayr.landfills..../100))*0.5/12*16
Cumulativemethaneemission = c(Methaneemission[1])
for (n in 2:30) {
  a1 = sum(Methaneemission[1:n])
  Cumulativemethaneemission = c(Cumulativemethaneemission, a1)
}
Cumulativemethaneemissioncarbondioxideequivalent = Cumulativemethaneemission*27.9
Soillikematerialcarbonstock = Result$Inputcarbonoforganicfractionsindumps+Result$Inputcarbonoforganicfractionsinsanitarylandfill+Result$Inputcarbonofpaperindumps+
  Result$Inputcarbonofpaperinsanitarylandfill-
  Result$carbonoflandfillgasindumps-Result$Carbonofleachateindumps-Result$carbonoflandfillgasinsanitarylandfill-
  Result$Carbonofleachateinsanitarylandfill
Plasticcarbonstock = Result$Inputcarbonofplasticandrubberindumps+Result$Inputcarbonofplasticandrubberinsanitarylandfill
Textilecarbonstock = Result$Inputcarbonoftextileindumps+Result$Inputcarbonoftextileinsanitarylandfill
Woodcarbonstock = Result$Inputcarbonofwoodindumps+Result$Inputcarbonofwoodinsanitarylandfill
Cumulativesoillikematerialcarbonstock = c(Soillikematerialcarbonstock[1])
for (n in 2:30) {
  a1 = sum(Soillikematerialcarbonstock[1:n])
  Cumulativesoillikematerialcarbonstock = c(Cumulativesoillikematerialcarbonstock, a1)
}
Cumulativeplasticcarbonstock = c(Plasticcarbonstock[1])
for (n in 2:30) {
  a1 = sum(Plasticcarbonstock[1:n])
  Cumulativeplasticcarbonstock = c(Cumulativeplasticcarbonstock, a1)
}
Cumulativetextilecarbonstock = c(Textilecarbonstock[1])
for (n in 2:30) {
  a1 = sum(Textilecarbonstock[1:n])
  Cumulativetextilecarbonstock = c(Cumulativetextilecarbonstock, a1)
}
Cumulativewoodcarbonstock = c(Woodcarbonstock[1])
for (n in 2:30) {
  a1 = sum(Woodcarbonstock[1:n])
  Cumulativewoodcarbonstock = c(Cumulativewoodcarbonstock, a1)
}
Finalresult = data.frame(Result,Organiccarbonstock,Cumulativeorganiccarbonstock,Methaneemission,
                         Cumulativemethaneemission,Cumulativemethaneemissioncarbondioxideequivalent,
                         Soillikematerialcarbonstock,Plasticcarbonstock,Textilecarbonstock,
                         Woodcarbonstock,Cumulativesoillikematerialcarbonstock,Cumulativeplasticcarbonstock,
                         Cumulativetextilecarbonstock,Cumulativewoodcarbonstock)
colnames(Finalresult) <- c("Input carbon of organic fractions in dumps","Input carbon of ash and stone in dumps","Input carbon of paper in dumps","Input carbon of plastic and rubber in dumps",
                           "Input carbon of textile in dumps","Input carbon of wood in dumps","Input carbon of glass in dumps","Input carbon of metal in dumps",
                           "Input carbon of others in dumps","Total input carbon in dumps","Carbon of landfill gas in dumps","Carbon of leachate in dumps",
                           "Input carbon of organic fractions in sanitary landfills","Input carbon of ash and stone in sanitary landfills","Input carbon of paper in sanitary landfills","Input carbon of plastic and rubber in sanitary landfills",
                           "Input carbon of textile in sanitary landfills","Input carbon of wood in sanitary landfills","Input carbon of glass in sanitary landfills","Input carbon of metal in sanitary landfills",
                           "Input carbon of others in sanitary landfills","Total input carbon in sanitary landfills","Carbon of landfill gas in sanitary landfills","Carbon of leachate in sanitary landfills",
                           "Organic carbon stock","Cumulative organic carbon stock","Methaneemission",
                           "Cumulative methane emission","Cumulative methane emission carbon dioxide equivalent",
                           "Soil-like material carbon stock","Plastic carbon stock","Textile carbon stock",
                           "Wood carbon stock","Cumulative soil-like material carbon stock","Cumulative plastic carbon stock",
                           "Cumulative textile carbon stock","Cumulative wood carbon stock")
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/Minimizing landfill rate scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
increasedincineration = Incineration..wt.- Incineration..wt..of.baseline.scenario
increasedcompost = Compost..wt. - Compost..wt..of.baseline.scenario
incinerationorganicfractions = ((Incineration..wt.+Compost..wt.)*Organic.fractions-Compost..wt.)/Incineration..wt.
incinerationashandstone = (Incineration..wt.+Compost..wt.)*Ash.and.stone/Incineration..wt.
incinerationpaper = (Incineration..wt.+Compost..wt.)*Paper/Incineration..wt.
incinerationPlasticandRubber = (Incineration..wt.+Compost..wt.)*mydata$Plastic.and.Rubber/Incineration..wt.
incinerationTextile = (Incineration..wt.+Compost..wt.)*mydata$Textile/Incineration..wt.
incinerationWood = (Incineration..wt.+Compost..wt.)*mydata$Wood/Incineration..wt.
incinerationMetal = (Incineration..wt.+Compost..wt.)*mydata$Metal/Incineration..wt.
incinerationGlass = (Incineration..wt.+Compost..wt.)*mydata$Glass/Incineration..wt.
incinerationOthers = (Incineration..wt.+Compost..wt.)*mydata$Others/Incineration..wt.
fossilcarbonofmixedwaste = incinerationPlasticandRubber*Carbon.content.of.Plastic.and.Rubber
electricityofincreasedincineration = increasedincineration*(incinerationorganicfractions*Calorific.value.of.organic.fractions+
                                                              incinerationashandstone*Calorific.value.of.ash.and.stone+
                                                              incinerationpaper*Calorific.value.of.paper+
                                                              incinerationPlasticandRubber*Calorific.value.of.plastic.and.rubber+
                                                              incinerationTextile*Calorific.value.of.textile+
                                                              incinerationWood*Calorific.value.of.wood+
                                                              incinerationMetal*Calorific.value.of.metal+
                                                              incinerationGlass*Calorific.value.of.glass+
                                                              incinerationOthers*Calorific.value.of.others)*10^9*0.21/3600
relativecarbonemissionofincreasedincineration = (increasedincineration*fossilcarbonofmixedwaste*10^9*44/12-electricityofincreasedincineration*1.345)/10^9
Cumulativerelativecarbonemissionofincreasedincineration = c(relativecarbonemissionofincreasedincineration[1])
for (n in 2:30) {
  a1 = sum(relativecarbonemissionofincreasedincineration[1:n])
  Cumulativerelativecarbonemissionofincreasedincineration = c(Cumulativerelativecarbonemissionofincreasedincineration, a1)
}
relativecarbonemissionofincreasedcompost = increasedcompost*(0.239*(0.43*273+0.12*27.9)/1000+0.761*0.0008*27.9/1000)-increasedcompost*0.0108/0.45*5.9130177
Cumulativerelativecarbonemissionofincreasedcompost = c(relativecarbonemissionofincreasedcompost[1])
for (n in 2:30) {
  a1 = sum(relativecarbonemissionofincreasedcompost[1:n])
  Cumulativerelativecarbonemissionofincreasedcompost = c(Cumulativerelativecarbonemissionofincreasedcompost, a1)
}
Totalrelativecarbonemission = Finalresult$`Cumulative methane emission carbon dioxide equivalent`+ Cumulativerelativecarbonemissionofincreasedincineration + Cumulativerelativecarbonemissionofincreasedcompost
Finalresult2 = data.frame(Finalresult,increasedincineration,increasedcompost,incinerationorganicfractions,incinerationashandstone,incinerationpaper,incinerationPlasticandRubber,incinerationTextile,
                          incinerationWood,incinerationMetal,incinerationMetal,incinerationGlass,incinerationOthers,relativecarbonemissionofincreasedincineration,Cumulativerelativecarbonemissionofincreasedincineration,
                          relativecarbonemissionofincreasedcompost,Cumulativerelativecarbonemissionofincreasedcompost,Totalrelativecarbonemission)
colnames(Finalresult2) <- c("Input carbon of organic fractions in dumps","Input carbon of ash and stone in dumps","Input carbon of paper in dumps","Input carbon of plastic and rubber in dumps",
                           "Input carbon of textile in dumps","Input carbon of wood in dumps","Input carbon of glass in dumps","Input carbon of metal in dumps",
                           "Input carbon of others in dumps","Total input carbon in dumps","Carbon of landfill gas in dumps","Carbon of leachate in dumps",
                           "Input carbon of organic fractions in sanitary landfills","Input carbon of ash and stone in sanitary landfills","Input carbon of paper in sanitary landfills","Input carbon of plastic and rubber in sanitary landfills",
                           "Input carbon of textile in sanitary landfills","Input carbon of wood in sanitary landfills","Input carbon of glass in sanitary landfills","Input carbon of metal in sanitary landfills",
                           "Input carbon of others in sanitary landfills","Total input carbon in sanitary landfills","Carbon of landfill gas in sanitary landfills","Carbon of leachate in sanitary landfills",
                           "Organic carbon stock","Cumulative organic carbon stock","Methaneemission",
                           "Cumulative methane emission","Cumulative methane emission carbon dioxide equivalent",
                           "Soil-like material carbon stock","Plastic carbon stock","Textile carbon stock",
                           "Wood carbon stock","Cumulative soil-like material carbon stock","Cumulative plastic carbon stock",
                           "Cumulative textile carbon stock","Cumulative wood carbon stock","increased incineration","increased compost","incineration organic fractions","incineration ash and stone", "incineration paper","incineration Plastic and Rubber","incineration Textile",
                           "incineration Wood","incineration Metal","incineration Metal","incineration Glass","incineration Others","relative carbon emission of increased incineration","Cumulative relative carbon emission of increased incineration",
                           "relative carbon emission of increased compost","Cumulative relative carbon emission of increased compost","Totalrelativecarbonemission")
write.csv(Finalresult2,"C:/Users/13094/Desktop/Nature Cities/file/Minimizinglandfillratescenarioresult.csv")

##################################################landfill mining and eco-remediation scenario#############################################################
######carbon stock#####
#Input carbon in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/landfill mining and eco-remediation scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
Inputcarbonoforganicfractionsindumps=Dump..wt.*Organic.fractions*Carbon.content.of.Organic.Fractions
Inputcarbonofashandstoneindumps=Dump..wt.*Ash.and.stone*Carbon.content.of.Ash.and.Stone
Inputcarbonofpaperindumps=Dump..wt.*Paper*Carbon.content.of.Paper
Inputcarbonofplasticandrubberindumps=Dump..wt.*Plastic.and.Rubber*Carbon.content.of.Plastic.and.Rubber
Inputcarbonoftextileindumps=Dump..wt.*Textile*Carbon.content.of.Textile
Inputcarbonofwoodindumps=Dump..wt.*Wood*Carbon.content.of.Wood
Inputcarbonofmetalindumps=Dump..wt.*Metal*Carbon.content.of.Metal
Inputcarbonofglassindumps=Dump..wt.*Glass*Carbon.content.of.Glass
Inputcarbonofothersindumps=Dump..wt.*Others*Carbon.content.of.Others
Totalinputcarbonindumps=Inputcarbonoforganicfractionsindumps+Inputcarbonofashandstoneindumps+Inputcarbonofpaperindumps+
  Inputcarbonofplasticandrubberindumps+Inputcarbonoftextileindumps+Inputcarbonofwoodindumps+Inputcarbonofglassindumps+Inputcarbonofmetalindumps+Inputcarbonofothersindumps
Inputcarbonindumps=data.frame(Inputcarbonoforganicfractionsindumps,Inputcarbonofashandstoneindumps,Inputcarbonofpaperindumps,Inputcarbonofplasticandrubberindumps,
                              Inputcarbonoftextileindumps,Inputcarbonofwoodindumps,Inputcarbonofglassindumps,Inputcarbonofmetalindumps,Inputcarbonofothersindumps,Totalinputcarbonindumps)
write.csv(Inputcarbonindumps,"C:/Users/13094/Desktop/Nature Cities/file/Minimizing landfill rate scenario Input carbon in dumps.csv")

#Carbon of leachate in dumps
Carbonofleachateindumps=Dump..wt.*The.generation.of.leachate.in.Dumps..m3.t.waste.*(COD.concentrations.of.leachate.in.Dumps..g.m3.+0.0582)/3/1000/1000
write.csv(Carbonofleachateindumps,"C:/Users/13094/Desktop/Nature Cities/file/landfill mining and eco-remediation scenario Carbon of leachate in dumps.csv")

#carbon of landfill gas in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/landfill mining and eco-remediation scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
carbonoflandfillgasindumps=c()
newdata2=c()
for(i in 1:1){
  for(T in 1:30){
    a<-mydata[Country.Code==i,]
    G=0
    t=T
    repeat{G=(exp((t-T)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                           a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t]))-
                exp((t-T-1)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                               a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t])))*(a$Organic.fractions[Year.Code=t]*a$Degradable.organic.carbon.of.Organic.fractions..DOC....[Year.Code=t]
                                                                                                                      +a$Paper[Year.Code=t]*a$Degradable.organic.carbon.of.Paper..DOC...[Year.Code=t]
                                                                                                                      +a$Textile[Year.Code=t]*a$Degradable.organic.carbon.of.Textile..DOC....[Year.Code=t]
                                                                                                                      +a$Wood[Year.Code=t]*a$Degradable.organic.carbon.of.Wood..DOC....[Year.Code=t])/100*(a$Organic.fractions[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Organic.fractions..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Paper[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Paper..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Textile[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Textile..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Wood[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Wood..DOCf.[Year.Code=t])*a$Dump..wt.[Year.Code=t]*a$Methane.correction.factor.of.Dumps..MCF.[Year.Code=t]*(1-a$Methane.oxidation.factor.of.Dump[Year.Code=t])+G
    t=t-1
    if(t<1){
      carbonoflandfillgasindumps=rbind(carbonoflandfillgasindumps,G)
      break
    }
    }
  }
}
write.csv(carbonoflandfillgasindumps,"C:/Users/13094/Desktop/Nature Cities/file/landfill mining and eco-remediation scenariocarbonoflandfillgasindumps.csv")

#Input carbon in sanitary landfill
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/landfill mining and eco-remediation scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
Inputcarbonoforganicfractionsinsanitarylandfill=Sanitary.landfill..wt.*Organic.fractions*Carbon.content.of.Organic.Fractions
Inputcarbonofashandstoneinsanitarylandfill=Sanitary.landfill..wt.*Ash.and.stone*Carbon.content.of.Ash.and.Stone
Inputcarbonofpaperinsanitarylandfill=Sanitary.landfill..wt.*Paper*Carbon.content.of.Paper
Inputcarbonofplasticandrubberinsanitarylandfill=Sanitary.landfill..wt.*Plastic.and.Rubber*Carbon.content.of.Plastic.and.Rubber
Inputcarbonoftextileinsanitarylandfill=Sanitary.landfill..wt.*Textile*Carbon.content.of.Textile
Inputcarbonofwoodinsanitarylandfill=Sanitary.landfill..wt.*Wood*Carbon.content.of.Wood
Inputcarbonofmetalinsanitarylandfill=Sanitary.landfill..wt.*Metal*Carbon.content.of.Metal
Inputcarbonofglassinsanitarylandfill=Sanitary.landfill..wt.*Glass*Carbon.content.of.Glass
Inputcarbonofothersinsanitarylandfill=Sanitary.landfill..wt.*Others*Carbon.content.of.Others
Totalinputcarboninsanitarylandfill=Inputcarbonoforganicfractionsinsanitarylandfill+Inputcarbonofashandstoneinsanitarylandfill+Inputcarbonofpaperinsanitarylandfill+
  Inputcarbonofplasticandrubberinsanitarylandfill+Inputcarbonoftextileinsanitarylandfill+Inputcarbonofwoodinsanitarylandfill+Inputcarbonofglassinsanitarylandfill+Inputcarbonofmetalinsanitarylandfill+Inputcarbonofothersinsanitarylandfill
Inputcarboninsanitarylandfill=data.frame(Inputcarbonoforganicfractionsinsanitarylandfill,Inputcarbonofashandstoneinsanitarylandfill,Inputcarbonofpaperinsanitarylandfill,Inputcarbonofplasticandrubberinsanitarylandfill,
                                         Inputcarbonoftextileinsanitarylandfill,Inputcarbonofwoodinsanitarylandfill,Inputcarbonofglassinsanitarylandfill,Inputcarbonofmetalinsanitarylandfill,Inputcarbonofothersinsanitarylandfill,Totalinputcarboninsanitarylandfill)
write.csv(Inputcarboninsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/landfill mining and eco-remediation scenario Input carbon in sanitary landfill.csv")

#Carbon of leachate in sanitary landfill
Carbonofleachateinsanitarylandfill=Sanitary.landfill..wt.*The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.*(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.+0.0582)/3/1000/1000
write.csv(Carbonofleachateinsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/landfill mining and eco-remediation scenario Carbon of leachate in sanitary landfill.csv")

#carbon of landfill gas in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/landfill mining and eco-remediation scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
carbonoflandfillgasinsanitarylandfill=c()
newdata2=c()
for(i in 1:1){
  for(T in 1:30){
    a<-mydata[Country.Code==i,]
    G=0
    t=T
    repeat{G=(exp((t-T)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                           a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                           a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t]))-
                exp((t-T-1)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                               a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                               a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t])))*(a$Organic.fractions[Year.Code=t]*a$Degradable.organic.carbon.of.Organic.fractions..DOC....[Year.Code=t]
                                                                                                                      +a$Paper[Year.Code=t]*a$Degradable.organic.carbon.of.Paper..DOC...[Year.Code=t]
                                                                                                                      +a$Textile[Year.Code=t]*a$Degradable.organic.carbon.of.Textile..DOC....[Year.Code=t]
                                                                                                                      +a$Wood[Year.Code=t]*a$Degradable.organic.carbon.of.Wood..DOC....[Year.Code=t])/100*(a$Organic.fractions[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Organic.fractions..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Paper[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Paper..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Textile[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Textile..DOCf.[Year.Code=t]+
                                                                                                                                                                                                             a$Wood[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Wood..DOCf.[Year.Code=t])*a$Sanitary.landfill..wt.[Year.Code=t]*a$Methane.correction.factor.of.Sanitary.landfills..MCF.[Year.Code=t]*(1-a$Methane.oxidation.factor.of.Sanitary.landfills[Year.Code=t])+G
    t=t-1
    if(t<1){
      carbonoflandfillgasinsanitarylandfill=rbind(carbonoflandfillgasinsanitarylandfill,G)
      break
    }
    }
  }
}
write.csv(carbonoflandfillgasinsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/landfill mining and eco-remediation scenariocarbonoflandfillgasinsanitarylandfill.csv")

# final results
Result = data.frame(Inputcarbonindumps,carbonoflandfillgasindumps,Carbonofleachateindumps, Inputcarboninsanitarylandfill,carbonoflandfillgasinsanitarylandfill,Carbonofleachateinsanitarylandfill)
head(Result)
Organiccarbonstock = Result$Inputcarbonoforganicfractionsindumps+Result$Inputcarbonofpaperindumps+Result$Inputcarbonofplasticandrubberindumps+Result$Inputcarbonoftextileindumps+
  Result$Inputcarbonofwoodindumps-Result$carbonoflandfillgasindumps-Result$Carbonofleachateindumps+Result$Inputcarbonoforganicfractionsinsanitarylandfill+Result$Inputcarbonofpaperinsanitarylandfill+
  Result$Inputcarbonofplasticandrubberinsanitarylandfill+Result$Inputcarbonoftextileinsanitarylandfill+Result$Inputcarbonofwoodinsanitarylandfill-Result$carbonoflandfillgasinsanitarylandfill-Result$Carbonofleachateinsanitarylandfill
Cumulativeorganiccarbonstock = c(Organiccarbonstock[1])
for (n in 2:30) {
  a1 = sum(Organiccarbonstock[1:n])
  Cumulativeorganiccarbonstock = c(Cumulativeorganiccarbonstock, a1)
}

Methaneemission = (Result$carbonoflandfillgasindumps+Result$carbonoflandfillgasinsanitarylandfill*(1-mydata$Methane.collection.rate.of.Sanitayr.landfills..../100))*0.5/12*16
Cumulativemethaneemission = c(Methaneemission[1])
for (n in 2:30) {
  a1 = sum(Methaneemission[1:n])
  Cumulativemethaneemission = c(Cumulativemethaneemission, a1)
}
Cumulativemethaneemissioncarbondioxideequivalent = Cumulativemethaneemission*27.9
Soillikematerialcarbonstock = Result$Inputcarbonoforganicfractionsindumps+Result$Inputcarbonoforganicfractionsinsanitarylandfill+Result$Inputcarbonofpaperindumps+
  Result$Inputcarbonofpaperinsanitarylandfill-
  Result$carbonoflandfillgasindumps-Result$Carbonofleachateindumps-Result$carbonoflandfillgasinsanitarylandfill-
  Result$Carbonofleachateinsanitarylandfill
Plasticcarbonstock = Result$Inputcarbonofplasticandrubberindumps+Result$Inputcarbonofplasticandrubberinsanitarylandfill
Textilecarbonstock = Result$Inputcarbonoftextileindumps+Result$Inputcarbonoftextileinsanitarylandfill
Woodcarbonstock = Result$Inputcarbonofwoodindumps+Result$Inputcarbonofwoodinsanitarylandfill
Cumulativesoillikematerialcarbonstock = c(Soillikematerialcarbonstock[1])
for (n in 2:30) {
  a1 = sum(Soillikematerialcarbonstock[1:n])
  Cumulativesoillikematerialcarbonstock = c(Cumulativesoillikematerialcarbonstock, a1)
}
Cumulativeplasticcarbonstock = c(Plasticcarbonstock[1])
for (n in 2:30) {
  a1 = sum(Plasticcarbonstock[1:n])
  Cumulativeplasticcarbonstock = c(Cumulativeplasticcarbonstock, a1)
}
Cumulativetextilecarbonstock = c(Textilecarbonstock[1])
for (n in 2:30) {
  a1 = sum(Textilecarbonstock[1:n])
  Cumulativetextilecarbonstock = c(Cumulativetextilecarbonstock, a1)
}
Cumulativewoodcarbonstock = c(Woodcarbonstock[1])
for (n in 2:30) {
  a1 = sum(Woodcarbonstock[1:n])
  Cumulativewoodcarbonstock = c(Cumulativewoodcarbonstock, a1)
}
Finalresult = data.frame(Result,Organiccarbonstock,Cumulativeorganiccarbonstock,Methaneemission,
                         Cumulativemethaneemission,Cumulativemethaneemissioncarbondioxideequivalent,
                         Soillikematerialcarbonstock,Plasticcarbonstock,Textilecarbonstock,
                         Woodcarbonstock,Cumulativesoillikematerialcarbonstock,Cumulativeplasticcarbonstock,
                         Cumulativetextilecarbonstock,Cumulativewoodcarbonstock)
colnames(Finalresult) <- c("Input carbon of organic fractions in dumps","Input carbon of ash and stone in dumps","Input carbon of paper in dumps","Input carbon of plastic and rubber in dumps",
                           "Input carbon of textile in dumps","Input carbon of wood in dumps","Input carbon of glass in dumps","Input carbon of metal in dumps",
                           "Input carbon of others in dumps","Total input carbon in dumps","Carbon of landfill gas in dumps","Carbon of leachate in dumps",
                           "Input carbon of organic fractions in sanitary landfills","Input carbon of ash and stone in sanitary landfills","Input carbon of paper in sanitary landfills","Input carbon of plastic and rubber in sanitary landfills",
                           "Input carbon of textile in sanitary landfills","Input carbon of wood in sanitary landfills","Input carbon of glass in sanitary landfills","Input carbon of metal in sanitary landfills",
                           "Input carbon of others in sanitary landfills","Total input carbon in sanitary landfills","Carbon of landfill gas in sanitary landfills","Carbon of leachate in sanitary landfills",
                           "Organic carbon stock","Cumulative organic carbon stock","Methaneemission",
                           "Cumulative methane emission","Cumulative methane emission carbon dioxide equivalent",
                           "Soil-like material carbon stock","Plastic carbon stock","Textile carbon stock",
                           "Wood carbon stock","Cumulative soil-like material carbon stock","Cumulative plastic carbon stock",
                           "Cumulative textile carbon stock","Cumulative wood carbon stock")
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/landfill mining and eco-remediation scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
increasedincineration = Incineration..wt.- Incineration..wt..of.baseline.scenario
increasedcompost = Compost..wt. - Compost..wt..of.baseline.scenario
incinerationorganicfractions = ((Incineration..wt.+Compost..wt.)*Organic.fractions-Compost..wt.)/Incineration..wt.
incinerationashandstone = (Incineration..wt.+Compost..wt.)*Ash.and.stone/Incineration..wt.
incinerationpaper = (Incineration..wt.+Compost..wt.)*Paper/Incineration..wt.
incinerationPlasticandRubber = (Incineration..wt.+Compost..wt.)*mydata$Plastic.and.Rubber/Incineration..wt.
incinerationTextile = (Incineration..wt.+Compost..wt.)*mydata$Textile/Incineration..wt.
incinerationWood = (Incineration..wt.+Compost..wt.)*mydata$Wood/Incineration..wt.
incinerationMetal = (Incineration..wt.+Compost..wt.)*mydata$Metal/Incineration..wt.
incinerationGlass = (Incineration..wt.+Compost..wt.)*mydata$Glass/Incineration..wt.
incinerationOthers = (Incineration..wt.+Compost..wt.)*mydata$Others/Incineration..wt.
fossilcarbonofmixedwaste = incinerationPlasticandRubber*Carbon.content.of.Plastic.and.Rubber
electricityofincreasedincineration = increasedincineration*(incinerationorganicfractions*Calorific.value.of.organic.fractions+
                                                              incinerationashandstone*Calorific.value.of.ash.and.stone+
                                                              incinerationpaper*Calorific.value.of.paper+
                                                              incinerationPlasticandRubber*Calorific.value.of.plastic.and.rubber+
                                                              incinerationTextile*Calorific.value.of.textile+
                                                              incinerationWood*Calorific.value.of.wood+
                                                              incinerationMetal*Calorific.value.of.metal+
                                                              incinerationGlass*Calorific.value.of.glass+
                                                              incinerationOthers*Calorific.value.of.others)*10^9*0.21/3600
relativecarbonemissionofincreasedincineration = (increasedincineration*fossilcarbonofmixedwaste*10^9*44/12-electricityofincreasedincineration*1.345)/10^9
Cumulativerelativecarbonemissionofincreasedincineration = c(relativecarbonemissionofincreasedincineration[1])
for (n in 2:30) {
  a1 = sum(relativecarbonemissionofincreasedincineration[1:n])
  Cumulativerelativecarbonemissionofincreasedincineration = c(Cumulativerelativecarbonemissionofincreasedincineration, a1)
}
relativecarbonemissionofincreasedcompost = increasedcompost*(0.239*(0.43*273+0.12*27.9)/1000+0.761*0.0008*27.9/1000)-increasedcompost*0.0108/0.45*5.9130177
Cumulativerelativecarbonemissionofincreasedcompost = c(relativecarbonemissionofincreasedcompost[1])
for (n in 2:30) {
  a1 = sum(relativecarbonemissionofincreasedcompost[1:n])
  Cumulativerelativecarbonemissionofincreasedcompost = c(Cumulativerelativecarbonemissionofincreasedcompost, a1)
}
Totalrelativecarbonemission = Finalresult$`Cumulative methane emission carbon dioxide equivalent`+ Cumulativerelativecarbonemissionofincreasedincineration + Cumulativerelativecarbonemissionofincreasedcompost
Finalresult2 = data.frame(Finalresult,increasedincineration,increasedcompost,incinerationorganicfractions,incinerationashandstone,incinerationpaper,incinerationPlasticandRubber,incinerationTextile,
                          incinerationWood,incinerationMetal,incinerationMetal,incinerationGlass,incinerationOthers,relativecarbonemissionofincreasedincineration,Cumulativerelativecarbonemissionofincreasedincineration,
                          relativecarbonemissionofincreasedcompost,Cumulativerelativecarbonemissionofincreasedcompost,Totalrelativecarbonemission)
colnames(Finalresult2) <- c("Input carbon of organic fractions in dumps","Input carbon of ash and stone in dumps","Input carbon of paper in dumps","Input carbon of plastic and rubber in dumps",
                            "Input carbon of textile in dumps","Input carbon of wood in dumps","Input carbon of glass in dumps","Input carbon of metal in dumps",
                            "Input carbon of others in dumps","Total input carbon in dumps","Carbon of landfill gas in dumps","Carbon of leachate in dumps",
                            "Input carbon of organic fractions in sanitary landfills","Input carbon of ash and stone in sanitary landfills","Input carbon of paper in sanitary landfills","Input carbon of plastic and rubber in sanitary landfills",
                            "Input carbon of textile in sanitary landfills","Input carbon of wood in sanitary landfills","Input carbon of glass in sanitary landfills","Input carbon of metal in sanitary landfills",
                            "Input carbon of others in sanitary landfills","Total input carbon in sanitary landfills","Carbon of landfill gas in sanitary landfills","Carbon of leachate in sanitary landfills",
                            "Organic carbon stock","Cumulative organic carbon stock","Methaneemission",
                            "Cumulative methane emission","Cumulative methane emission carbon dioxide equivalent",
                            "Soil-like material carbon stock","Plastic carbon stock","Textile carbon stock",
                            "Wood carbon stock","Cumulative soil-like material carbon stock","Cumulative plastic carbon stock",
                            "Cumulative textile carbon stock","Cumulative wood carbon stock","increased incineration","increased compost","incineration organic fractions","incineration ash and stone", "incineration paper","incineration Plastic and Rubber","incineration Textile",
                            "incineration Wood","incineration Metal","incineration Metal","incineration Glass","incineration Others","relative carbon emission of increased incineration","Cumulative relative carbon emission of increased incineration",
                            "relative carbon emission of increased compost","Cumulative relative carbon emission of increased compost","Totalrelativecarbonemission")
write.csv(Finalresult2,"C:/Users/13094/Desktop/Nature Cities/file/landfillminingandeco-remediationscenarioresult.csv")

# input waste
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/landfill mining and eco-remediation scenario.csv',sep=",",header=TRUE)
head(mydata)
attach(mydata)
#input carbon in sanitary landfills
Organicfractionsinlandfills=(Sanitary.landfill..wt.+Dump..wt.)*Organic.fractions
Ashandstoneinlandfills=(Sanitary.landfill..wt.+Dump..wt.)*Ash.and.stone
Paperinlandfills=(Sanitary.landfill..wt.+Dump..wt.)*Paper
PlasticandRubberinlandfills=(Sanitary.landfill..wt.+Dump..wt.)*Plastic.and.Rubber
Textileinlandfills=(Sanitary.landfill..wt.+Dump..wt.)*Textile
Woodinlandfills=(Sanitary.landfill..wt.+Dump..wt.)*Wood
Metalinlandfills=(Sanitary.landfill..wt.+Dump..wt.)*Metal
Glassinlandfills=(Sanitary.landfill..wt.+Dump..wt.)*Glass
Othersinlandfills=(Sanitary.landfill..wt.+Dump..wt.)*Others
Totalinputwasteinlandfills=Organicfractionsinlandfills+Ashandstoneinlandfills+Paperinlandfills+PlasticandRubberinlandfills+Textileinlandfills+
  Woodinlandfills+Metalinlandfills+Glassinlandfills+Othersinlandfills
  
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/landfill mining and eco-remediation scenario.csv',sep=",",header=TRUE)
head(mydata)
attach(mydata)
#input carbon in dumps
Organicfractionsinsanitarylandfills=Sanitary.landfill..wt.*Organic.fractions
Ashandstoneinsanitarylandfills=Sanitary.landfill..wt.*Ash.and.stone
Paperinsanitarylandfills=Sanitary.landfill..wt.*Paper
PlasticandRubberinsanitarylandfills=Sanitary.landfill..wt.*Plastic.and.Rubber
Textileinsanitarylandfills=Sanitary.landfill..wt.*Textile
Woodinsanitarylandfills=Sanitary.landfill..wt.*Wood
Metalinsanitarylandfills=Sanitary.landfill..wt.*Metal
Glassinsanitarylandfills=Sanitary.landfill..wt.*Glass
Othersinsanitarylandfills=Sanitary.landfill..wt.*Others
Totalinputwasteinsanitarylandfills=Organicfractionsinsanitarylandfills+Ashandstoneinsanitarylandfills+Paperinsanitarylandfills+
  PlasticandRubberinsanitarylandfills+Textileinsanitarylandfills+Woodinsanitarylandfills+Metalinsanitarylandfills+Glassinsanitarylandfills+Othersinsanitarylandfills
Organicfractionsindumps=Dump..wt.*Organic.fractions
Ashandstoneindumps=Dump..wt.*Ash.and.stone
Paperindumps=Dump..wt.*Paper
PlasticandRubberindumps=Dump..wt.*Plastic.and.Rubber
Textileindumps=Dump..wt.*Textile
Woodindumps=Dump..wt.*Wood
Metalindumps=Dump..wt.*Metal
Glassindumps=Dump..wt.*Glass
Othersindumps=Dump..wt.*Others
Totalinputwasteindumps=Organicfractionsindumps+Ashandstoneindumps+Paperindumps+PlasticandRubberindumps+Textileindumps+Woodindumps+Metalindumps+Glassindumps+Othersindumps


#Nitrogen loss
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/landfill mining and eco-remediation scenario.csv',sep=",",header=TRUE)
head(mydata)
attach(mydata)
#Nitrogen of leachate in sanitary landfills
Nitrogenofleachateinsanitarylandfills=Sanitary.landfill..wt.*The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.*Total.Nitrogen.of.leachate.in.Sanitary.landfills..g.m3./1000/1000
#Nitrogen of leachate in dumps
Nitrogenofleachateindumps=Dump..wt.*The.generation.of.leachate.in.Dumps..m3.t.waste.*Total.Nitrogen.of.leachate.in.dumps..g.m3./1000/1000

#Phosphorus loss
#Phosphorus of leachate in sanitary landfills
Phosphorusofleachateinsanitarylandfills=Sanitary.landfill..wt.*The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.*Total.phosphorus.of.leachate.in.Sanitary.landfills..g.m3./1000/1000
#Phosphorus of leachate in dumps
Phosphorusofleachateindumps=Dump..wt.*The.generation.of.leachate.in.Dumps..m3.t.waste.*Total.phosphorus.of.leachate.in.dumps..g.m3./1000/1000

#Potassium loss
#Potassium of leachate in sanitary landfills
Potassiumofleachateinsanitarylandfills=Sanitary.landfill..wt.*The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.*Total.potassium.of.leachate.in.landfills..g.m3./1000/1000
#Potassium of leachate in dumps
Potassiumofleachateindumps=Dump..wt.*The.generation.of.leachate.in.Dumps..m3.t.waste.*Total.potassium.of.leachate.in.landfills..g.m3./1000/1000

#Water loss
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/landfill mining and eco-remediation scenario.csv',sep=",",header=TRUE)
head(mydata)
attach(mydata)
Selfproducedwaterfromsanitarylandfills=mydata$Sanitary.landfill..wt.*(mydata$Organic.fractions*mydata$Water.content.of.organic.fractions+mydata$Ash.and.stone*mydata$Water.content.of.ash.and.stone+
                         mydata$Paper*mydata$Water.content.of.paper+mydata$Plastic.and.Rubber*mydata$Water.content.of.plastic.and.rubber+mydata$Textile*mydata$Water.content.of.textile+
                         mydata$Wood*mydata$Water.content.of.wood+mydata$Metal*mydata$Water.content.of.metal+mydata$Glass*mydata$Water.content.of.glass+mydata$Others*mydata$Water.content.of.others-mydata$Field.capacity)
Selfproducedwaterfromdumps=mydata$Dump..wt.*(mydata$Organic.fractions*mydata$Water.content.of.organic.fractions+mydata$Ash.and.stone*mydata$Water.content.of.ash.and.stone+
                         mydata$Paper*mydata$Water.content.of.paper+mydata$Plastic.and.Rubber*mydata$Water.content.of.plastic.and.rubber+mydata$Textile*mydata$Water.content.of.textile+
                         mydata$Wood*mydata$Water.content.of.wood+mydata$Metal*mydata$Water.content.of.metal+mydata$Glass*mydata$Water.content.of.glass+mydata$Others*mydata$Water.content.of.others-mydata$Field.capacity)


#温室气体排放分解
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/landfill mining and eco-remediation scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
newdata=c()
a<-mydata[Country.Code==1,]
for(T in 1:30){
  for(i in 1:30){
    t=i
    if(t<=T){
      G=(exp((t-T)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                      a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                      a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                      a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t]))-
           exp((t-T-1)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                          a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                          a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                          a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t])))*(a$Organic.fractions[Year.Code=t]*a$Degradable.organic.carbon.of.Organic.fractions..DOC....[Year.Code=t]
                                                                                                                 +a$Paper[Year.Code=t]*a$Degradable.organic.carbon.of.Paper..DOC...[Year.Code=t]
                                                                                                                 +a$Textile[Year.Code=t]*a$Degradable.organic.carbon.of.Textile..DOC....[Year.Code=t]
                                                                                                                 +a$Wood[Year.Code=t]*a$Degradable.organic.carbon.of.Wood..DOC....[Year.Code=t])/100*(a$Organic.fractions[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Organic.fractions..DOCf.[Year.Code=t]+
                                                                                                                                                                                                        a$Paper[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Paper..DOCf.[Year.Code=t]+
                                                                                                                                                                                                        a$Textile[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Textile..DOCf.[Year.Code=t]+
                                                                                                                                                                                                        a$Wood[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Wood..DOCf.[Year.Code=t])*a$Sanitary.landfill..wt.[Year.Code=t]*a$Methane.correction.factor.of.Sanitary.landfills..MCF.[Year.Code=t]*(1-a$Methane.oxidation.factor.of.Sanitary.landfills[Year.Code=t])
    }else{G=0}
    newdata = rbind(newdata,G)
    #a = 30-length(newdata2)
    #k = rep(0,a)
    #newdata2 = c(newdata2,k)
  }
  #carbonoflandfillgasinsanitarylandfill=cbind(carbonoflandfillgasinsanitarylandfill,newdata2)
}
length(newdata)
carbonoflandfillgasinsanitarylandfill1=c(newdata[1:30])
for (i in 1:29) {
  carbonoflandfillgasinsanitarylandfill1=cbind(carbonoflandfillgasinsanitarylandfill1,newdata[(1+30*i):(30+30*i)])
}
write.csv(carbonoflandfillgasinsanitarylandfill1,"C:/Users/13094/Desktop/Nature Cities/file/carbonoflandfillgasinsanitarylandfill1.csv")

mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/landfill mining and eco-remediation scenario.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
newdata=c()
a<-mydata[Country.Code==1,]
for(T in 1:30){
  for(i in 1:30){
    t=i
    if(t<=T){
      G=G=(exp((t-T)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                        a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                        a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                        a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t]))-
             exp((t-T-1)*(a$Organic.fractions[Year.Code=t]*a$Methane.production.rate.coefficient.of.Organic.fractions..k.[Year.Code=t]+
                            a$Paper[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                            a$Textile[Year.Code=t]*a$Methane.production.rate.coefficient.of.Paper.Textile..k.[Year.Code=t]+
                            a$Wood[Year.Code=t]*a$Methane.production.rate.coefficient.of.Wood..k.[Year.Code=t])))*(a$Organic.fractions[Year.Code=t]*a$Degradable.organic.carbon.of.Organic.fractions..DOC....[Year.Code=t]
                                                                                                                   +a$Paper[Year.Code=t]*a$Degradable.organic.carbon.of.Paper..DOC...[Year.Code=t]
                                                                                                                   +a$Textile[Year.Code=t]*a$Degradable.organic.carbon.of.Textile..DOC....[Year.Code=t]
                                                                                                                   +a$Wood[Year.Code=t]*a$Degradable.organic.carbon.of.Wood..DOC....[Year.Code=t])/100*(a$Organic.fractions[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Organic.fractions..DOCf.[Year.Code=t]+
                                                                                                                                                                                                          a$Paper[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Paper..DOCf.[Year.Code=t]+
                                                                                                                                                                                                          a$Textile[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Textile..DOCf.[Year.Code=t]+
                                                                                                                                                                                                          a$Wood[Year.Code=t]*a$Decomposable.biodegradable.organic.carbon.of.Wood..DOCf.[Year.Code=t])*a$Dump..wt.[Year.Code=t]*a$Methane.correction.factor.of.Dumps..MCF.[Year.Code=t]*(1-a$Methane.oxidation.factor.of.Dump[Year.Code=t])
    }else{G=0}
    newdata = rbind(newdata,G)
    #a = 30-length(newdata2)
    #k = rep(0,a)
    #newdata2 = c(newdata2,k)
  }
  #carbonoflandfillgasinsanitarylandfill=cbind(carbonoflandfillgasinsanitarylandfill,newdata2)
}
length(newdata)
carbonoflandfillgasindump1=c(newdata[1:30])
for (i in 1:29) {
  carbonoflandfillgasindump1=cbind(carbonoflandfillgasindump1,newdata[(1+30*i):(30+30*i)])
}
write.csv(carbonoflandfillgasindump1,"C:/Users/13094/Desktop/Nature Cities/file/carbonoflandfillgasindump1.csv")
historicalwasteinlandfills = c()
for (i in 1:20) {
  a = Totalinputwasteinlandfills[i] - Nitrogenofleachateinsanitarylandfills[i]-Phosphorusofleachateinsanitarylandfills[i]-
    Potassiumofleachateinsanitarylandfills[i]-Selfproducedwaterfromsanitarylandfills[i]-sum(carbonoflandfillgasinsanitarylandfill1[i, 1:20])-Carbonofleachateinsanitarylandfill[i]-
    Nitrogenofleachateindumps[i]-Phosphorusofleachateindumps[i]-Potassiumofleachateindumps[i]-Selfproducedwaterfromdumps[i]-
    sum(carbonoflandfillgasindump1[i, 1:20])-Carbonofleachateindumps[i]
  historicalwasteinlandfills = c(historicalwasteinlandfills,a)
}

historicalsoillikematerialinlandfills = c()
for (i in 1:20) {
  a = Organicfractionsinlandfills[i]+Paperinlandfills[i] - Nitrogenofleachateinsanitarylandfills[i]-Phosphorusofleachateinsanitarylandfills[i]-
    Potassiumofleachateinsanitarylandfills[i]-Selfproducedwaterfromsanitarylandfills[i]-sum(carbonoflandfillgasinsanitarylandfill1[i, 1:20])-Carbonofleachateinsanitarylandfill[i]-
    Nitrogenofleachateindumps[i]-Phosphorusofleachateindumps[i]-Potassiumofleachateindumps[i]-Selfproducedwaterfromdumps[i]-
    sum(carbonoflandfillgasindump1[i, 1:20])-Carbonofleachateindumps[i]
  historicalsoillikematerialinlandfills = c(historicalsoillikematerialinlandfills,a)
}

historicalwasteinsanitarylandfills = c()
for (i in 1:20) {
  a = Totalinputwasteinsanitarylandfills[i] - Nitrogenofleachateinsanitarylandfills[i]-Phosphorusofleachateinsanitarylandfills[i]-
    Potassiumofleachateinsanitarylandfills[i]-Selfproducedwaterfromsanitarylandfills[i]-sum(carbonoflandfillgasinsanitarylandfill1[i, 1:20])-Carbonofleachateinsanitarylandfill[i]
  historicalwasteinsanitarylandfills = c(historicalwasteinsanitarylandfills,a)
}

historicalsoillikematerialinsanitarylandfills = c()
for (i in 1:20) {
  a = Organicfractionsinsanitarylandfills[i]+Paperinsanitarylandfills[i] - Nitrogenofleachateinsanitarylandfills[i]-Phosphorusofleachateinsanitarylandfills[i]-
    Potassiumofleachateinsanitarylandfills[i]-Selfproducedwaterfromsanitarylandfills[i]-sum(carbonoflandfillgasinsanitarylandfill1[i, 1:20])-Carbonofleachateinsanitarylandfill[i]
  historicalsoillikematerialinsanitarylandfills = c(historicalsoillikematerialinsanitarylandfills,a)
}

historicalwasteindumps = c()
for (i in 1:20) {
  a = Totalinputwasteindumps[i]-Nitrogenofleachateindumps[i]-Phosphorusofleachateindumps[i]-Potassiumofleachateindumps[i]-Selfproducedwaterfromdumps[i]-
    sum(carbonoflandfillgasindump1[i, 1:20])-Carbonofleachateindumps[i]
  historicalwasteindumps = c(historicalwasteindumps,a)
}

historicalsoillikematerialindumps = c()
for (i in 1:20) {
  a = Organicfractionsindumps[i]+Paperindumps[i]-Nitrogenofleachateindumps[i]-Phosphorusofleachateindumps[i]-Potassiumofleachateindumps[i]-Selfproducedwaterfromdumps[i]-
    sum(carbonoflandfillgasindump1[i, 1:20])-Carbonofleachateindumps[i]
  historicalsoillikematerialindumps = c(historicalsoillikematerialindumps,a)
}
Soillikematerialcarbonstockindumps = Result$Inputcarbonoforganicfractionsindumps+Result$Inputcarbonofpaperindumps-Result$carbonoflandfillgasindumps-Result$Carbonofleachateindumps
Plasticcarbonstockindumps = Result$Inputcarbonofplasticandrubberindumps
Textilecarbonstockindumps = Result$Inputcarbonoftextileindumps
Woodcarbonstockindumps = Result$Inputcarbonofwoodindumps
Soillikematerialcarbonstockinsanitarylandfill = Result$Inputcarbonoforganicfractionsinsanitarylandfill+Result$Inputcarbonofpaperinsanitarylandfill-Result$carbonoflandfillgasinsanitarylandfill-Result$Carbonofleachateinsanitarylandfill
Plasticcarbonstockinsanitarylandfill = Result$Inputcarbonofplasticandrubberinsanitarylandfill
Textilecarbonstockinsanitarylandfill = Result$Inputcarbonoftextileinsanitarylandfill
Woodcarbonstockinsanitarylandfill = Result$Inputcarbonofwoodinsanitarylandfill
Table = data.frame(historicalwasteinlandfills, historicalsoillikematerialinlandfills,Ashandstoneinlandfills[1:20],PlasticandRubberinlandfills[1:20],Textileinlandfills[1:20],Woodinlandfills[1:20],Metalinlandfills[1:20],Glassinlandfills[1:20],
                   Othersinlandfills[1:20],historicalwasteinsanitarylandfills, historicalsoillikematerialinsanitarylandfills,Ashandstoneinsanitarylandfills[1:20],PlasticandRubberinsanitarylandfills[1:20],Textileinsanitarylandfills[1:20],Woodinsanitarylandfills[1:20],Metalinsanitarylandfills[1:20],Glassinsanitarylandfills[1:20],
                   Othersinsanitarylandfills[1:20],historicalwasteindumps, historicalsoillikematerialindumps,Ashandstoneindumps[1:20],PlasticandRubberindumps[1:20],Textileindumps[1:20],Woodindumps[1:20],Metalindumps[1:20],Glassindumps[1:20],
                   Othersindumps[1:20],Soillikematerialcarbonstockindumps[1:20],Plasticcarbonstockindumps[1:20],Textilecarbonstockindumps[1:20],Woodcarbonstockindumps[1:20],
                   Soillikematerialcarbonstockinsanitarylandfill[1:20],Plasticcarbonstockinsanitarylandfill[1:20],Textilecarbonstockinsanitarylandfill[1:20],Woodcarbonstockinsanitarylandfill[1:20])
colnames(Table) <- c("histricalstockinlandfills","historicalsoillikematerialinlandfills","Historicalashandstoneinlandfills","HistoricalplasticandRubberinlandfills",
                     "histricalTextileinlandfills","histricalWoodinlandfills","histricalMetalinlandfills","histricalglassinlandfills","histricalOthersinlandfills","histricalstockinsanitarylandfills","historicalsoillikematerialinsanitarylandfills",
                     "Historicalashandstoneinsanitarylandfills","Historicalashandstoneinsanitarylandfills","HistoricalplasticandRubberinsanitarylandfills","histricalTextileinsanitarylandfills",
                     "histricalWoodinsanitarylandfills","histricalMetalinsanitarylandfills","histricalOthersinsanitarylandfills","histricalstockindumps","historicalsoillikematerialindumps",
                     "Historicalashandstoneindumps","Historicalashandstoneindumps","HistoricalplasticandRubberindumps","histricalTextileindumps",
                     "histricalWoodindumps","histricalMetalindumps","histricalOthersindumps","Soillikematerialcarbonstockindumps","Plasticcarbonstockindumps","Textilecarbonstockindumps","Woodcarbonstockindumps",
                     "Soillikematerialcarbonstockinsanitarylandfill","Plasticcarbonstockinsanitarylandfill","Textilecarbonstockinsanitarylandfill","Woodcarbonstockinsanitarylandfill")
head(Table)
for (m in 21:30) {
    G1=0
    G2=0
    k1=1
    repeat{G1=Table$histricalstockinlandfills[k1]+G1
           G2=Table$historicalsoillikematerialinlandfills[k1]+Table$Historicalashandstoneinlandfills[k1]+G2
    k1=k1+1
    if(mydata$Remaining.capacity.of.incineration[m]<=(G1-G2*0.5)){
      if((k1-2)>0){
        y = mydata$Remaining.capacity.of.incineration[m]-(sum(Table$histricalstockinlandfills[1:(k1-2)])-sum(Table$historicalsoillikematerialinlandfills[1:(k1-2)]*0.5)-sum(Table$Historicalashandstoneinlandfills[1:(k1-2)]*0.5))
      }else{y = mydata$Remaining.capacity.of.incineration[m]}
      aa = Table$HistoricalplasticandRubberinlandfills[(k1-1)]/(Table$histricalstockinlandfills[(k1-1)]-Table$historicalsoillikematerialinlandfills[(k1-1)]*0.5-Table$Historicalashandstoneinlandfills[(k1-1)]*0.5)
      bb = Table$historicalsoillikematerialinlandfills[(k1-1)]*0.5/(Table$histricalstockinlandfills[(k1-1)]-Table$historicalsoillikematerialinlandfills[(k1-1)]*0.5-Table$Historicalashandstoneinlandfills[(k1-1)]*0.5)
      cc = Table$Historicalashandstoneinlandfills[(k1-1)]*0.5/(Table$histricalstockinlandfills[(k1-1)]-Table$historicalsoillikematerialinlandfills[(k1-1)]*0.5-Table$Historicalashandstoneinlandfills[(k1-1)]*0.5)
      dd = Table$histricalTextileinlandfills[(k1-1)]/(Table$histricalstockinlandfills[(k1-1)]-Table$historicalsoillikematerialinlandfills[(k1-1)]*0.5-Table$Historicalashandstoneinlandfills[(k1-1)]*0.5)
      ee = Table$histricalWoodinlandfills[(k1-1)]/(Table$histricalstockinlandfills[(k1-1)]-Table$historicalsoillikematerialinlandfills[(k1-1)]*0.5-Table$Historicalashandstoneinlandfills[(k1-1)]*0.5)
      ff = Table$histricalMetalinlandfills[(k1-1)]/(Table$histricalstockinlandfills[(k1-1)]-Table$historicalsoillikematerialinlandfills[(k1-1)]*0.5-Table$Historicalashandstoneinlandfills[(k1-1)]*0.5)
      gg = Table$histricalglassinlandfills[(k1-1)]/(Table$histricalstockinlandfills[(k1-1)]-Table$historicalsoillikematerialinlandfills[(k1-1)]*0.5-Table$Historicalashandstoneinlandfills[(k1-1)]*0.5)
      hh = Table$histricalOthersinlandfills[(k1-1)]/(Table$histricalstockinlandfills[(k1-1)]-Table$historicalsoillikematerialinlandfills[(k1-1)]*0.5-Table$Historicalashandstoneinlandfills[(k1-1)]*0.5)
      if((k1-2)>0){electricityofincreasedincineration1 = (sum(Table$HistoricalplasticandRubberinlandfills[1:(k1-2)])*0.5*Calorific.value.of.organic.fractions[m]+
        sum(Table$Historicalashandstoneinlandfills[1:(k1-2)])*0.5*Calorific.value.of.ash.and.stone[m]+
        sum(Table$HistoricalplasticandRubberinlandfills[1:(k1-2)])*Calorific.value.of.plastic.and.rubber[m]+
        sum(Table$histricalTextileinlandfills[1:(k1-2)])*Calorific.value.of.textile[m]+
        sum(Table$histricalWoodinlandfills[1:(k1-2)])*Calorific.value.of.wood[m]+
        sum(Table$histricalMetalinlandfills[1:(k1-2)])*Calorific.value.of.metal[m]+
        sum(Table$histricalglassinlandfills[1:(k1-2)])*Calorific.value.of.glass[m]+
        sum(Table$histricalOthersinlandfills[1:(k1-2)])*Calorific.value.of.others[m]+
          y*bb*Calorific.value.of.organic.fractions[m]+
          y*cc*Calorific.value.of.ash.and.stone[m]+
          y*aa*Calorific.value.of.plastic.and.rubber[m]+
          y*dd*Calorific.value.of.textile[m]+
          y*ee*Calorific.value.of.wood[m]+
          y*ff*Calorific.value.of.metal[m]+
          y*gg*Calorific.value.of.glass[m]+
          y*hh*Calorific.value.of.others[m])*10^9*0.21/3600}else{electricityofincreasedincineration1 = (y*bb*Calorific.value.of.organic.fractions[m]+
              y*cc*Calorific.value.of.ash.and.stone[m]+
              y*aa*Calorific.value.of.plastic.and.rubber[m]+
              y*dd*Calorific.value.of.textile[m]+
              y*ee*Calorific.value.of.wood[m]+
              y*ff*Calorific.value.of.metal[m]+
              y*gg*Calorific.value.of.glass[m]+
              y*hh*Calorific.value.of.others[m])*10^9*0.21/3600}
      if((k1-2)>0){fossilcarbonofmixedwaste1 = (sum(Table$HistoricalplasticandRubberinlandfills[1:(k1-2)])+aa*y)*Carbon.content.of.Plastic.and.Rubber[m]}else{fossilcarbonofmixedwaste1 = aa*y*Carbon.content.of.Plastic.and.Rubber[m]}
      if((k1-2)>0){increasedcompost1 = sum(Table$historicalsoillikematerialinlandfills[1:(k1-2)])*0.5+bb*y}else{increasedcompost1 = bb*y}
      if((k1-2)>0){increasedcompostex = sum(Table$historicalsoillikematerialinlandfills[1:(k1-2)])*0.5+sum(Table$Historicalashandstoneinlandfills[1:(k1-2)])*0.5+bb*y}else{increasedcompostex = bb*y}
      P = (Table$histricalstockinlandfills[(k1-1)]-y-y*(Table$historicalsoillikematerialinlandfills[(k1-1)]*0.5+Table$Historicalashandstoneinlandfills[(k1-1)]*0.5)/(Table$histricalstockinlandfills[(k1-1)]-Table$historicalsoillikematerialinlandfills[(k1-1)]*0.5-Table$Historicalashandstoneinlandfills[(k1-1)]*0.5))/Table$histricalstockinlandfills[(k1-1)]
      historicalwasteinlandfills = c()
      for (i in 1:m) {
        a = Totalinputwasteinlandfills[i] - Nitrogenofleachateinsanitarylandfills[i]-Phosphorusofleachateinsanitarylandfills[i]-
          Potassiumofleachateinsanitarylandfills[i]-Selfproducedwaterfromsanitarylandfills[i]-sum(carbonoflandfillgasinsanitarylandfill1[i, 1:m])-Carbonofleachateinsanitarylandfill[i]-
          Nitrogenofleachateindumps[i]-Phosphorusofleachateindumps[i]-Potassiumofleachateindumps[i]-Selfproducedwaterfromdumps[i]-
          sum(carbonoflandfillgasindump1[i, 1:m])-Carbonofleachateindumps[i]
        historicalwasteinlandfills = c(historicalwasteinlandfills,a)
      }
      
      historicalsoillikematerialinlandfills = c()
      for (i in 1:m) {
        a = Organicfractionsinlandfills[i]+Paperinlandfills[i] - Nitrogenofleachateinsanitarylandfills[i]-Phosphorusofleachateinsanitarylandfills[i]-
          Potassiumofleachateinsanitarylandfills[i]-Selfproducedwaterfromsanitarylandfills[i]-sum(carbonoflandfillgasinsanitarylandfill1[i, 1:m])-Carbonofleachateinsanitarylandfill[i]-
          Nitrogenofleachateindumps[i]-Phosphorusofleachateindumps[i]-Potassiumofleachateindumps[i]-Selfproducedwaterfromdumps[i]-
          sum(carbonoflandfillgasindump1[i, 1:m])-Carbonofleachateindumps[i]
        historicalsoillikematerialinlandfills = c(historicalsoillikematerialinlandfills,a)
      }
      
      historicalwasteinsanitarylandfills = c()
      for (i in 1:m) {
        a = Totalinputwasteinsanitarylandfills[i] - Nitrogenofleachateinsanitarylandfills[i]-Phosphorusofleachateinsanitarylandfills[i]-
          Potassiumofleachateinsanitarylandfills[i]-Selfproducedwaterfromsanitarylandfills[i]-sum(carbonoflandfillgasinsanitarylandfill1[i, 1:m])-Carbonofleachateinsanitarylandfill[i]
        historicalwasteinsanitarylandfills = c(historicalwasteinsanitarylandfills,a)
      }
      
      historicalsoillikematerialinsanitarylandfills = c()
      for (i in 1:m) {
        a = Organicfractionsinsanitarylandfills[i]+Paperinsanitarylandfills[i] - Nitrogenofleachateinsanitarylandfills[i]-Phosphorusofleachateinsanitarylandfills[i]-
          Potassiumofleachateinsanitarylandfills[i]-Selfproducedwaterfromsanitarylandfills[i]-sum(carbonoflandfillgasinsanitarylandfill1[i, 1:m])-Carbonofleachateinsanitarylandfill[i]
        historicalsoillikematerialinsanitarylandfills = c(historicalsoillikematerialinsanitarylandfills,a)
      }
      
      historicalwasteindumps = c()
      for (i in 1:m) {
        a = Totalinputwasteindumps[i]-Nitrogenofleachateindumps[i]-Phosphorusofleachateindumps[i]-Potassiumofleachateindumps[i]-Selfproducedwaterfromdumps[i]-
          sum(carbonoflandfillgasindump1[i, 1:m])-Carbonofleachateindumps[i]
        historicalwasteindumps = c(historicalwasteindumps,a)
      }
      
      historicalsoillikematerialindumps = c()
      for (i in 1:m) {
        a = Organicfractionsindumps[i]+Paperindumps[i]-Nitrogenofleachateindumps[i]-Phosphorusofleachateindumps[i]-Potassiumofleachateindumps[i]-Selfproducedwaterfromdumps[i]-
          sum(carbonoflandfillgasindump1[i, 1:m])-Carbonofleachateindumps[i]
        historicalsoillikematerialindumps = c(historicalsoillikematerialindumps,a)
      }
      Table = data.frame(historicalwasteinlandfills, historicalsoillikematerialinlandfills,Ashandstoneinlandfills[1:m],PlasticandRubberinlandfills[1:m],Textileinlandfills[1:m],Woodinlandfills[1:m],Metalinlandfills[1:m],Glassinlandfills[1:m],
                         Othersinlandfills[1:m],historicalwasteinsanitarylandfills, historicalsoillikematerialinsanitarylandfills,Ashandstoneinsanitarylandfills[1:m],PlasticandRubberinsanitarylandfills[1:m],Textileinsanitarylandfills[1:m],Woodinsanitarylandfills[1:m],Metalinsanitarylandfills[1:m],Glassinsanitarylandfills[1:m],
                         Othersinsanitarylandfills[1:m],historicalwasteindumps, historicalsoillikematerialindumps,Ashandstoneindumps[1:m],PlasticandRubberindumps[1:m],Textileindumps[1:m],Woodindumps[1:m],Metalindumps[1:m],Glassindumps[1:m],
                         Othersindumps[1:m],Soillikematerialcarbonstockindumps[1:m],Plasticcarbonstockindumps[1:m],Textilecarbonstockindumps[1:m],Woodcarbonstockindumps[1:m],
                         Soillikematerialcarbonstockinsanitarylandfill[1:m],Plasticcarbonstockinsanitarylandfill[1:m],Textilecarbonstockinsanitarylandfill[1:m],Woodcarbonstockinsanitarylandfill[1:m])
      colnames(Table) <- c("histricalstockinlandfills","historicalsoillikematerialinlandfills","Historicalashandstoneinlandfills","HistoricalplasticandRubberinlandfills",
                           "histricalTextileinlandfills","histricalWoodinlandfills","histricalMetalinlandfills","histricalglassinlandfills","histricalOthersinlandfills","histricalstockinsanitarylandfills","historicalsoillikematerialinsanitarylandfills",
                           "Historicalashandstoneinsanitarylandfills","Historicalashandstoneinsanitarylandfills","HistoricalplasticandRubberinsanitarylandfills","histricalTextileinsanitarylandfills",
                           "histricalWoodinsanitarylandfills","histricalMetalinsanitarylandfills","histricalOthersinsanitarylandfills","histricalstockindumps","historicalsoillikematerialindumps",
                           "Historicalashandstoneindumps","Historicalashandstoneindumps","HistoricalplasticandRubberindumps","histricalTextileindumps",
                           "histricalWoodindumps","histricalMetalindumps","histricalOthersindumps","Soillikematerialcarbonstockindumps","Plasticcarbonstockindumps","Textilecarbonstockindumps","Woodcarbonstockindumps",
                           "Soillikematerialcarbonstockinsanitarylandfill","Plasticcarbonstockinsanitarylandfill","Textilecarbonstockinsanitarylandfill","Woodcarbonstockinsanitarylandfill")
      if((k1-2)>0){Table[1:(k1-2),]=0}
      Table[(k1-1),]=Table[(k1-1),]*P
      carbonoflandfillgasinsanitarylandfill1[1:(k1-2),m]=0
      carbonoflandfillgasinsanitarylandfill1[(k1-1),m]=carbonoflandfillgasinsanitarylandfill1[(k1-1),m]*P
      carbonoflandfillgasindump1[1:(k1-2),m]=0
      carbonoflandfillgasindump1[(k1-1),m]=carbonoflandfillgasindump1[(k1-1),m]*P
      relativecarbonemissionofincreasedincinerationlandfillmining1 = (fossilcarbonofmixedwaste1*10^9*44/12-electricityofincreasedincineration1*1.345)/10^9
      relativecarbonemissionofincreasedcompostlandfillmining1 = -increasedcompost1*0.0108/0.45*5.9130177
      sanitarylandfillgascarbonemission1 = sum(carbonoflandfillgasinsanitarylandfill1[,m])*0.5/12*16*27.9
      dumpgascarbonemission1 = sum(carbonoflandfillgasindump1[,m])*0.5/12*16*27.9
      
      carbon = c(sanitarylandfillgascarbonemission1,dumpgascarbonemission1, relativecarbonemissionofincreasedincinerationlandfillmining1,relativecarbonemissionofincreasedcompostlandfillmining1,electricityofincreasedincineration1,increasedcompostex)
      write.csv(carbon, paste("C:/Users/13094/Desktop/Nature Cities/file/", "landfill mining and eco-remediation scenario carbon emisssion", m, ".csv", sep = ""))
      write.csv(Table, paste("C:/Users/13094/Desktop/Nature Cities/file/", "landfill mining and eco-remediation scenario carbon stock", m, ".csv", sep = ""))
      break
    }
    }
}

newdata1 = data.frame(Finalresult2$`Cumulative organic carbon stock`[1:20],Finalresult2$`Cumulative soil-like material carbon stock`[1:20],
                         Finalresult2$`Cumulative plastic carbon stock`[1:20],Finalresult2$`Cumulative textile carbon stock`[1:20],Finalresult2$`Cumulative wood carbon stock`[1:20])
colnames(newdata1) <- c("Cumulative organic carbon stock","Cumulative soil-like material carbon stock","Cumulative plastic carbon stock",
                        "Cumulative textile carbon stock","Cumulative wood carbon stock")
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/file/landfill mining and eco-remediation scenario carbon stock21.csv',sep=",",header=TRUE)
Cumulativeorganiccarbonstock = sum(mydata[,29:36])
Cumulativesoillikematerialcarbonstock = sum((mydata[,29]+mydata[,33]))
Cumulativeplasticcarbonstock = sum((mydata[,30]+mydata[,34]))
Cumulativetextilecarbonstock = sum((mydata[,31]+mydata[,35]))
Cumulativewoodcarbonstock = sum((mydata[,32]+mydata[,36]))
newdata2 = c(Cumulativeorganiccarbonstock,Cumulativesoillikematerialcarbonstock,Cumulativeplasticcarbonstock,Cumulativetextilecarbonstock,Cumulativewoodcarbonstock)

for (m in 22:30) {
  mydata=read.csv(paste("C:/Users/13094/Desktop/Nature Cities/file/", "landfill mining and eco-remediation scenario carbon stock", m, ".csv", sep = ""),sep=",",header=TRUE)
  Cumulativeorganiccarbonstock = sum(mydata[,29:36])
  Cumulativesoillikematerialcarbonstock = sum((mydata[,29]+mydata[,33]))
  Cumulativeplasticcarbonstock = sum((mydata[,30]+mydata[,34]))
  Cumulativetextilecarbonstock = sum((mydata[,31]+mydata[,35]))
  Cumulativewoodcarbonstock = sum((mydata[,32]+mydata[,36]))
  a = c(Cumulativeorganiccarbonstock,Cumulativesoillikematerialcarbonstock,Cumulativeplasticcarbonstock,Cumulativetextilecarbonstock,Cumulativewoodcarbonstock)
  newdata2 = rbind(newdata2,a)
}
colnames(newdata2) <- c("Cumulative organic carbon stock","Cumulative soil-like material carbon stock","Cumulative plastic carbon stock",
                        "Cumulative textile carbon stock","Cumulative wood carbon stock")
landfillminingandecoremediationscenariocarbonstock = rbind(newdata1,newdata2)
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/landfill mining and eco-remediation scenario.csv',sep=",",header=TRUE)
Sanitarylandfillmethaneemissions = Finalresult2$`Carbon of landfill gas in sanitary landfills`*(1-mydata$Methane.collection.rate.of.Sanitayr.landfills..../100)*0.5/12*16*27.9
Dumpmethaneemissions = Finalresult2$`Carbon of landfill gas in dumps`*0.5/12*16*27.9

mydata1=read.csv("C:/Users/13094/Desktop/Nature Cities/file/landfill mining and eco-remediation scenario carbon emisssion21.csv",sep=",",header=TRUE)
newdata3 = c(mydata1[,2])
for (m in 22:30) {
  mydata1=read.csv(paste("C:/Users/13094/Desktop/Nature Cities/file/", "landfill mining and eco-remediation scenario carbon emisssion", m, ".csv", sep = ""),sep=",",header=TRUE)
  newdata3 = cbind(newdata3,mydata1[,2])
}
aaa = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
bbb=aaa*0.5+1*(1-aaa)
newdata3 = t(newdata3)
sum(newdata3[,5])
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/landfill mining and eco-remediation scenario.csv',sep=",",header=TRUE)
Sanitarylandfillmethaneemissionsfinal = c(Sanitarylandfillmethaneemissions[1:20],newdata3[,1]*(1-mydata$Methane.collection.rate.of.Sanitayr.landfills....[21:30]/100)*bbb)
dumpmethaneemissionsfinal = c(Dumpmethaneemissions[1:20],newdata3[,2])
ccc = rep(0,20)
relativecarbonemissionofincreasedincinerationlandfillminingfinal = c(ccc,newdata3[,3])
relativecarbonemissionofincreasedcompostlandfillminingfinal = c(ccc,newdata3[,4])
carbonemissionsfinal = data.frame(Sanitarylandfillmethaneemissionsfinal,dumpmethaneemissionsfinal,relativecarbonemissionofincreasedincinerationlandfillminingfinal,
                                  relativecarbonemissionofincreasedcompostlandfillminingfinal,Finalresult2$`relative carbon emission of increased incineration`,
                                  Finalresult2$`relative carbon emission of increased compost`)
Final = data.frame(landfillminingandecoremediationscenariocarbonstock,carbonemissionsfinal)
colnames(Final) <- c("Cumulative organic carbon stock","Cumulative soil-like material carbon stock","Cumulative plastic carbon stock",
                     "Cumulative textile carbon stock","Cumulative wood carbon stock","Sanitary landfill methane emissions","dump methane emissions",
                     "relative carbon emission of increased incineration landfill mining","relative carbonvemission of increased compost landfill mining",
                     "relative carbon emission of increased incineration","relative carbon emission of increased compost")
write.csv(Final,"C:/Users/13094/Desktop/Nature Cities/file/landfillminingandeco-remediationscenarioresult.csv")