#Input carbon in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/Code and Data/Supplementary data-solid-liquid-gas transformation module.csv',sep=",",header=TRUE)
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
write.csv(Inputcarbonindumps,"C:/Users/13094/Desktop/Nature Cities/file/Input carbon in dumps.csv")

#Carbon of leachate in dumps
Carbonofleachateindumps=Dump..wt.*The.generation.of.leachate.in.Dumps..m3.t.waste.*(COD.concentrations.of.leachate.in.Dumps..g.m3.+0.0582)/3/1000/1000
write.csv(Carbonofleachateindumps,"C:/Users/13094/Desktop/Nature Cities/file/Carbon of leachate in dumps.csv")

#carbon of landfill gas in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/Code and Data/Supplementary data-solid-liquid-gas transformation module.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
carbonoflandfillgasindumps=c()
newdata2=c()
for(i in 1:352){
  for(T in 1:20){
    a<-mydata[Prefecture.level.city.code==i,]
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
write.csv(carbonoflandfillgasindumps,"C:/Users/13094/Desktop/Nature Cities/file/carbonoflandfillgasindumps.csv")

#Input carbon in sanitary landfill
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/Code and Data/Supplementary data-solid-liquid-gas transformation module.csv',sep=",",header=TRUE)
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
write.csv(Inputcarboninsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/Input carbon in sanitary landfill.csv")

#Carbon of leachate in sanitary landfill
Carbonofleachateinsanitarylandfill=Sanitary.landfill..wt.*The.generation.of.leachate.in.Sanitary.landfills..m3.t.waste.*(COD.concentrations.of.leachate.in.Sanitary.landfills..g.m3.+0.0582)/3/1000/1000
write.csv(Carbonofleachateinsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/Carbon of leachate in sanitary landfill.csv")

#carbon of landfill gas in dumps
mydata=read.csv('C:/Users/13094/Desktop/Nature Cities/Code and Data/Supplementary data-solid-liquid-gas transformation module.csv',sep=",",header=TRUE)
head(mydata)
library(plyr)
attach(mydata)
carbonoflandfillgasinsanitarylandfill=c()
newdata2=c()
for(i in 1:352){
  for(T in 1:20){
    a<-mydata[Prefecture.level.city.code==i,]
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
write.csv(carbonoflandfillgasinsanitarylandfill,"C:/Users/13094/Desktop/Nature Cities/file/carbonoflandfillgasinsanitarylandfill.csv")

# final results
Result = data.frame(Inputcarbonindumps,carbonoflandfillgasindumps,Carbonofleachateindumps, Inputcarboninsanitarylandfill,carbonoflandfillgasinsanitarylandfill,Carbonofleachateinsanitarylandfill)
head(Result)
Organiccarbonstock = Result$Inputcarbonoforganicfractionsindumps+Result$Inputcarbonofpaperindumps+Result$Inputcarbonofplasticandrubberindumps+Result$Inputcarbonoftextileindumps+
  Result$Inputcarbonofwoodindumps-Result$carbonoflandfillgasindumps-Result$Carbonofleachateindumps+Result$Inputcarbonoforganicfractionsinsanitarylandfill+Result$Inputcarbonofpaperinsanitarylandfill+
  Result$Inputcarbonofplasticandrubberinsanitarylandfill+Result$Inputcarbonoftextileinsanitarylandfill+Result$Inputcarbonofwoodinsanitarylandfill-Result$carbonoflandfillgasinsanitarylandfill-Result$Carbonofleachateinsanitarylandfill
Cumulativeorganiccarbonstock = c()
for (m in 1:352) {
  Cumulativeorganiccarbonstock1 = c(Organiccarbonstock[1+20*(m-1)])
  for (n in 2:20) {
    a1 = sum(Organiccarbonstock[(1+20*(m-1)):(n+20*(m-1))])
    Cumulativeorganiccarbonstock1 = c(Cumulativeorganiccarbonstock1, a1)
  }
  Cumulativeorganiccarbonstock = c(Cumulativeorganiccarbonstock,Cumulativeorganiccarbonstock1)
}


Methaneemission = (Result$carbonoflandfillgasindumps+Result$carbonoflandfillgasinsanitarylandfill*0.76)*0.5/12*16
Cumulativemethaneemission = c()
for (m in 1:352) {
  Cumulativemethaneemission1 = c(Methaneemission[1])
  for (n in 2:20) {
    a1 = sum(Methaneemission[1:n])
    Cumulativemethaneemission1 = c(Cumulativemethaneemission1, a1)
  }
  Cumulativemethaneemission = c(Cumulativemethaneemission, Cumulativemethaneemission1)
}
Cumulativemethaneemissioncarbondioxideequivalent = Cumulativemethaneemission*27.9
Soillikematerialcarbonstock = Result$Inputcarbonoforganicfractionsindumps+Result$Inputcarbonoforganicfractionsinsanitarylandfill+Result$Inputcarbonofpaperindumps+
  Result$Inputcarbonofpaperinsanitarylandfill-
  Result$carbonoflandfillgasindumps-Result$Carbonofleachateindumps-Result$carbonoflandfillgasinsanitarylandfill-
  Result$Carbonofleachateinsanitarylandfill
Plasticcarbonstock = Result$Inputcarbonofplasticandrubberindumps+Result$Inputcarbonofplasticandrubberinsanitarylandfill
Textilecarbonstock = Result$Inputcarbonoftextileindumps+Result$Inputcarbonoftextileinsanitarylandfill
Woodcarbonstock = Result$Inputcarbonofwoodindumps+Result$Inputcarbonofwoodinsanitarylandfill



Cumulativesoillikematerialcarbonstock = c()
for (m in 1:352) {
  Cumulativesoillikematerialcarbonstock1 = c(Soillikematerialcarbonstock[1+20*(m-1)])
  for (n in 2:20) {
    a1 = sum(Soillikematerialcarbonstock[(1+20*(m-1)):(n+20*(m-1))])
    Cumulativesoillikematerialcarbonstock1 = c(Cumulativesoillikematerialcarbonstock1, a1)
  }
  Cumulativesoillikematerialcarbonstock = c(Cumulativesoillikematerialcarbonstock,Cumulativesoillikematerialcarbonstock1)
}

Cumulativeplasticcarbonstock = c()
for (m in 1:352) {
  Cumulativeplasticcarbonstock1 = c(Plasticcarbonstock[1+20*(m-1)])
  for (n in 2:20) {
    a1 = sum(Plasticcarbonstock[(1+20*(m-1)):(n+20*(m-1))])
    Cumulativeplasticcarbonstock1 = c(Cumulativeplasticcarbonstock1, a1)
  }
  Cumulativeplasticcarbonstock = c(Cumulativeplasticcarbonstock,Cumulativeplasticcarbonstock1)
}

Cumulativetextilecarbonstock  = c()
for (m in 1:352) {
  Cumulativetextilecarbonstock1 = c(Textilecarbonstock[1+20*(m-1)])
  for (n in 2:20) {
    a1 = sum(Textilecarbonstock[(1+20*(m-1)):(n+20*(m-1))])
    Cumulativetextilecarbonstock1 = c(Cumulativetextilecarbonstock1, a1)
  }
  Cumulativetextilecarbonstock = c(Cumulativetextilecarbonstock,Cumulativetextilecarbonstock1)
}

Cumulativewoodcarbonstock  = c()
for (m in 1:352) {
  Cumulativewoodcarbonstock1 = c(Woodcarbonstock[1+20*(m-1)])
  for (n in 2:20) {
    a1 = sum(Woodcarbonstock[(1+20*(m-1)):(n+20*(m-1))])
    Cumulativewoodcarbonstock1 = c(Cumulativewoodcarbonstock1, a1)
  }
  Cumulativewoodcarbonstock = c(Cumulativewoodcarbonstock,Cumulativewoodcarbonstock1)
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
write.csv(Finalresult,"C:/Users/13094/Desktop/Nature Cities/file/result.csv")
