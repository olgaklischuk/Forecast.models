load(paste(getwd(),"/cpi.forecast.Covid.R",sep=""))
if(Sys.info()[4]=="MacBook-Air-Ulia.local"){comp="mac"}else{comp="asus"}
if(comp=="asus"){
  OneDrive<-"C:/Users/1/OneDrive/Computer"}else{OneDrive<-"~/OneDrive/Computer"}

Inflation <- readxl::read_excel(paste(OneDrive,"/I/Monetary policy decision in Ukraine updated.xlsx",sep=""),
                                sheet = "Covid crises")[1:157+2,]

for(i in 2:ncol(Inflation)){Inflation[,i][[1]]<-as.numeric(Inflation[,i][[1]])}
Inflation[,1][[1]]<-as.Date(Inflation[,1][[1]],origin="1904-01-01")
Inflation<-Inflation[1:157,]
vars<-colnames(Inflation)
vars[1]<-"date"
colnames(Inflation)<-vars
View(vars)
new.data<-data.frame(Inflation$ln_real.gdp.gap,Inflation$ln_pi.e)
matrix.endogen<-MatrixModels::model.Matrix(~0+Inflation$ln_real.gdp.gap[2:156]+na.omit(dplyr::lead(Inflation$ln_inflation,1)[2:157]))
matrix.endogen<-model.matrix(~0+Inflation$ln_real.gdp.gap[2:156]+na.omit(dplyr::lead(Inflation$ln_inflation[2:157],1)))
colnames(matrix.endogen)<-c("Ln_real.gdp.gap","Ln_inflation");colnames(new.data)<-colnames(matrix.endogen)
matrix.endogen.ardl<-model.matrix(~0+Inflation$ln_real.gdp.gap[2:156])
colnames(matrix.endogen.ardl)<-"ln_real.gdp.gap"
library(dLagM)
library(ggplot2)
ARdl.cpi<-dlm(Inflation$ln_inflation~Inflation$ln_real.gdp.gap,data = Inflation,x = Inflation$ln_real.gdp.gap,y=Inflation$ln_inflation,q=4)
ARdl.cpi.forecast.48.m<-dLagM::forecast(model=ARdl.cpi,x=new.data$ln_real.gdp.gap[(157-47):157],h=48,interval=TRUE,level=seq(.5,.95,by=.05)[19])
ARdl.cpi.forecast.48.m<-ARdl.cpi.forecast.48.m$forecasts
phillips.cpi<-forecast::forecast(lm(data=Inflation,formula = Inflation$inflation[2:156]~matrix.endogen),newdata=new.data[157-47:157,],h=48,se=TRUE,level=seq(11,99,by=11))
phillips.cpi.forecast.48.m<-data.frame(c(rep(NA,110-48),phillips.cpi$lower[(110-47):110]),c(rep(NA,110-48),phillips.cpi$upper[(110-47):110]));colnames(phillips.cpi.forecast.48.m)<-c("lower","upper")

plot1<-ggplot2::qplot(data=Inflation,aes(y=inflation[1:141],x=unemployment[1:141]),geom="jitter", main="Phillips curve")
plot2<-ggplot2::qplot(y=phillips.cpi$fitted[(155-109):155],x=1:110,xlab="",ylab="Inflation",main="",geom="line")+geom_ribbon(data=phillips.cpi.forecast.48.m,aes(ymin=lower,ymax=upper),show.legend=TRUE,col="red",fill=alpha("red",.1))+legend(legend = c("lower","upper"))#+annotate(x=)
shocks<-phillips.cpi$residuals
plot3<-ggplot2::qplot(aes(y=1:155,x=shocks),geom="jitter")
save.image(paste(getwd(),"/cpi.forecast.Covid.R",sep=""))

