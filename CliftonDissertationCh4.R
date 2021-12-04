setwd("~/Desktop/Research/Dissertation Code and Data/Dissertation-Chapter-4")
library(ggplot2)
library(cowplot)
library(tidyverse)
library(MuMIn)
st.err = function(x) {sd(x)/sqrt(length(x))}

## Light Levels 2020- Preserved specimens ----
Both.pres<-read.csv("PreservedSpecimens.csv")
Both.pres$Datetime<-as.POSIXct(strptime(Both.pres$Datetime, tz="MST", format("%Y-%m-%d %H:%M:%S")))
Both.pres$Species<-as.factor(Both.pres$Species)
Both.pres$Size<-as.factor(Both.pres$Size)

# Summary statistics
# Tb
mean(Both.pres$Tb)
st.err(Both.pres$Tb)
t.test(Tb~Species, data=Both.pres)
aggregate(Tb~Species, mean, data=Both.pres)
aggregate(Tb~Species, st.err, data=Both.pres)
# Lux/1000
mean(Both.pres$lux.1000)
st.err(Both.pres$lux.1000)
t.test(lux.1000~Species, data=Both.pres)
aggregate(lux.1000~Species, mean, data=Both.pres)
aggregate(lux.1000~Species, st.err, data=Both.pres)
# Substrate
mean(Both.pres$Substrate, na.rm=T)
sd(Both.pres$Substrate, na.rm=T)/sqrt(611-12)
t.test(Substrate~Species, data=Both.pres)
aggregate(Substrate~Species, mean, data=Both.pres)
aggregate(Substrate~Species, st.err, data=Both.pres)

# Correlations
# Table 1- Tb and lux
cor.test(Both.pres$lux.1000, Both.pres$Tb, method="pearson")
lm1<-lm(Tb~lux.1000, data=Both.pres)
summary(lm1)

Pres.St<-subset(Both.pres, Species=="S. tristichus")
cor.test(Pres.St$lux.1000, Pres.St$Tb, method="pearson")
lm2<-lm(Tb~lux.1000, data=Pres.St)
summary(lm2)

Pres.Ph<-subset(Both.pres, Species=="P. hernandesi")
cor.test(Pres.Ph$lux.1000, Pres.Ph$Tb, method="pearson")
lm3<-lm(Tb~lux.1000, data=Pres.Ph)
summary(lm3)
aggregate(Substrate~Size, mean, data=Pres.Ph)
t.test(Tb~Size, data=Pres.Ph)

# Table 1- Tb and substrate
cor.test(Both.pres$Tb, Both.pres$Substrate, method='pearson')
lm4<-lm(Tb~Substrate, data=Both.pres)
summary(lm4)

cor.test(Pres.St$Substrate, Pres.St$Tb, method="pearson")
lm5<-lm(Tb~Substrate, data=Pres.St)
summary(lm5)

cor.test(Pres.Ph$Substrate, Pres.Ph$Tb, method="pearson")
lm6<-lm(Tb~Substrate, data=Pres.Ph)
summary(lm6)

## Linear models
# Table 2- all preserved specimens
lm7<-lm(Tb~lux.1000*Substrate*Weather*Microhabitat, data=Both.pres)
summary(lm7)
AIC(lm7)
AICc(lm7)
lm8<-lm(Tb~lux.1000+Substrate+Weather+Microhabitat+Species, data=Both.pres)
summary(lm8)
AIC(lm8)
AICc(lm8)
lm9<-lm(Tb~lux.1000*Substrate*Weather, data=Both.pres)
summary(lm9)
AIC(lm9)
AICc(lm9)
lm10<-lm(Tb~lux.1000*Substrate*Microhabitat, data=Both.pres)
summary(lm10)
AIC(lm10)
AICc(lm10)
lm11<-lm(Tb~lux.1000*Substrate, data=Both.pres)
summary(lm11)
AIC(lm11)
AICc(lm11)
lm12<-lm(Tb~lux.1000, data=Both.pres)
summary(lm12)
AIC(lm12)
AICc(lm12)
lm13<-lm(Tb~Substrate, data=Both.pres)
summary(lm13)
AIC(lm13)
AICc(lm13)
lm14<-lm(Tb~lux.1000*Substrate*Weather*Microhabitat*Species, data=Both.pres)
summary(lm14)
AIC(lm14)
lm15<-lm(Tb~lux.1000*Substrate*Species, data=Both.pres)
summary(lm15)
AIC(lm15)
lm16<-lm(Tb~Microhabitat, data=Both.pres)
summary(lm16)
AIC(lm16)
lm17<-lm(Tb~Weather, data=Both.pres)
summary(lm17)
AIC(lm17)
lm29<-lm(Tb~Species, data=Both.pres)
summary(lm29)
AIC(lm29)

# Table 3- Just P. hernandesi to consider size
lm18<-lm(Tb~lux.1000*Substrate*Weather*Microhabitat*Size, data=Pres.Ph)
summary(lm18)
AIC(lm18)
lm19<-lm(Tb~lux.1000+Substrate+Weather+Microhabitat+Size, data=Pres.Ph)
summary(lm19)
AIC(lm19)
lm20<-lm(Tb~lux.1000*Substrate*Weather, data=Pres.Ph)
summary(lm20)
AIC(lm20)
lm21<-lm(Tb~lux.1000*Substrate*Microhabitat, data=Pres.Ph)
summary(lm21)
AIC(lm21)
lm22<-lm(Tb~lux.1000*Substrate, data=Pres.Ph)
summary(lm22)
AIC(lm22)
lm23<-lm(Tb~lux.1000, data=Pres.Ph)
summary(lm23)
AIC(lm23)
lm24<-lm(Tb~Substrate, data=Pres.Ph)
summary(lm24)
AIC(lm24)
lm25<-lm(Tb~lux.1000*Substrate*Weather*Microhabitat, data=Pres.Ph)
summary(lm25)
AIC(lm25)
lm26<-lm(Tb~lux.1000*Substrate*Size, data=Pres.Ph)
summary(lm26)
AIC(lm26)
lm27<-lm(Tb~Microhabitat, data=Pres.Ph)
summary(lm27)
AIC(lm27)
lm28<-lm(Tb~Weather, data=Pres.Ph)
summary(lm28)
AIC(lm28)
lm30<-lm(Tb~Size, data=Pres.Ph)
summary(lm30)
AIC(lm30)

pred1<-predict(lm14)
summary(lm(lm7$model$Tb~pred1))

## 2021 Live Animals ----
Live2021<-read.csv("FreeRanging.csv")
Live2021$ID<-as.factor(Live2021$ID)
Live2021$datetime<-as.POSIXct(strptime(Live2021$datetime, tz="MST", format("%m/%d/%y %H:%M")))
Live2021$Time<-format(Live2021$datetime, '%H:%M')
Live2021$Hour<-as.numeric(as.character(Live2021$datetime, format="%H"))

# Summary Statistics
# Body Temperature
mean(Live2021$Tb)
st.err(Live2021$Tb)
aggregate(Tb~ID, mean, data=Live2021)
aggregate(Tb~ID, st.err, data=Live2021)
aggregate(Tb~ID, length, data=Live2021)
t.test(Tb~ID, data=Live2021)

# Light level
mean(Live2021$lux.1000)
st.err(Live2021$lux.1000)
aggregate(lux.1000~ID, mean, data=Live2021)
aggregate(lux.1000~ID, st.err, data=Live2021)
aggregate(lux.1000~ID, length, data=Live2021)
t.test(lux.1000~ID, data=Live2021)

id.2112<-subset(Live2021, ID=="2112")
id.2115<-subset(Live2021, ID=="2115")

cor.test(Live2021$lux.1000, Live2021$Tb, method="pearson")
cor.test(as.numeric(Live2021$Hour), Live2021$Tb, method="pearson")

# Linear models
# Table 4- Free-ranging
lm31<-lm(Tb~lux.1000, data=Live2021)
summary(lm31)
AIC(lm31)
AICc(lm31)
lm32<-lm(Tb~Hour.cos+Hour.sin, data=Live2021)
summary(lm32)
AIC(lm32)
AICc(lm32)
lm33<-lm(Tb~lux.1000*Hour.sin+lux.1000*Hour.cos, data=Live2021)
summary(lm33)
AIC(lm33)
AICc(lm33)
lm34<-lm(Tb~lux.1000*Hour.cos+lux.1000*Hour.sin+ID*lux.1000+ID*Hour.cos+ID*Hour.sin, data=Live2021)
summary(lm34)
AIC(lm34)
AICc(lm34)
lm35<-lm(Tb~lux.1000+Hour.cos+Hour.sin+ID, data=Live2021)
summary(lm35)
AIC(lm35)
AICc(lm35)
lm36<-lm(Tb~ID, data=Live2021)
summary(lm36)
AIC(lm36)
AICc(lm36)

## Figures ----
# Fig 1- Correlation and lm plots
CorrelationLegend<-ggplot(data=Both.pres, aes(lux.1000, Tb, group=Species))+
  geom_point(aes(colour=Species))+
  geom_abline(intercept = lm1$coefficients[1], slope = lm1$coefficients[2], size=1, alpha=0.75)+
  geom_abline(intercept = lm2$coefficients[1], slope = lm2$coefficients[2], size=1, color="#00BFC4")+
  geom_abline(intercept = lm3$coefficients[1], slope = lm3$coefficients[2], size=1, color="#F8766D")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.title=element_text(size=16, face="bold"), legend.text=element_text(size=15, face="bold.italic"))+
  theme(legend.position="bottom")+
  xlab("Light-level (klx)")+
  ylab("Body Temperature (°C)")+
  annotate("label", x=72, y=42, label=paste("R^2 == ", round(summary(lm2)$r.squared, 2)), parse=T, fontface="bold", size=3, color="#00BFC4", fill="white")+
  annotate("label", x=72, y=38, label=paste("R^2 == ", round(summary(lm1)$r.squared, 2)), parse=T, fontface="bold", size=3, fill="white")+
  annotate("label", x=72, y=33, label=paste("R^2 == ", round(summary(lm3)$r.squared, 2)), parse=T, fontface="bold", size=3, color="#F8766D", fill="white")

p1<-ggplot(data=Both.pres, aes(lux.1000, Tb, group=Species))+
  geom_point(aes(colour=Species))+
  geom_abline(intercept = lm2$coefficients[1], slope = lm2$coefficients[2], size=1, color="#00BFC4")+
  geom_abline(intercept = lm3$coefficients[1], slope = lm3$coefficients[2], size=1, color="#F8766D")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="")+
  xlab("Light Level (klx)")+
  ylab("Body Temperature (°C)")+
  annotate("label", x=72, y=42, label=paste("R^2 == ", round(summary(lm2)$r.squared, 2)), parse=T, fontface="bold", size=4, color="#00BFC4", fill="white")+
  annotate("label", x=72, y=33, label=paste("R^2 == ", round(summary(lm3)$r.squared, 2)), parse=T, fontface="bold", size=4, color="#F8766D", fill="white")

p2<-ggplot(data=Both.pres, aes(Substrate, Tb, group=Species))+
  geom_point(aes(colour=Species))+
  geom_abline(intercept = lm5$coefficients[1], slope = lm5$coefficients[2], size=1, color="#00BFC4")+ #blue
  geom_abline(intercept = lm6$coefficients[1], slope = lm6$coefficients[2], size=1, color="#F8766D")+ #pink
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="")+
  xlab("Substrate Temperature (°C)")+
  ylab("")+
  annotate("label", x=59, y=45, label=paste("R^2 == ", round(summary(lm5)$r.squared, 2)), parse=T, fontface="bold", size=4, color="#00BFC4", fill="white")+
  annotate("label", x=45, y=43, label=paste("R^2 == ", round(summary(lm6)$r.squared, 2)), parse=T, fontface="bold", size=4, color="#F8766D", fill="white")

c.legend=get_legend(CorrelationLegend)
correlations=plot_grid(p1, p2,
                       labels = "AUTO", ncol = 2, nrow=1, align="v")
Fig1=plot_grid(correlations,c.legend, ncol = 1, rel_heights = c(2,0.1))
#ggsave("Fig1.jpeg", height=7, width=15, plot=Fig1)

#Fig 2
pred1<-predict(lm14)
Pres.Predicted<-as.data.frame(cbind(pred1, lm14$model$Tb))
colnames(Pres.Predicted)<-c("Predicted", "Tb")
plot(Tb~Predicted, data=Pres.Predicted)
abline(lm(Tb~Predicted, data=Pres.Predicted))
Pres.Pred.lm<-lm(Tb~Predicted, data=Pres.Predicted)


p3<-ggplot(data=Pres.Predicted, aes(Predicted, Tb))+
  geom_point(color="grey60")+
  geom_abline(intercept = Pres.Pred.lm$coefficients[1], slope = Pres.Pred.lm$coefficients[2], size=1)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  xlab("Predicted Body Temperature (°C)")+
  ylab("Observed Body Temperature (°C)")+
  #annotate("text", x=21, y=47, label="Lux*Substrate*CloudCover*Microhabitat*Species", fontface="bold", size=3.5)+
  annotate("label", x=45, y=39, label=paste("R^2 == ", round(summary(lm14)$adj.r.squared, 2)), parse=T, fontface="bold", size=4, fill="white")
#ggsave("PreservedPredicted.png", height=7, width=7, plot=p3)

# Fig 3
Pres.Ph1<-Pres.Ph[complete.cases(Pres.Ph),]
pred2<-predict(lm18)
Ph.Predicted<-as.data.frame(cbind(pred2, lm18$model$Size))
Pres.Ph1$predicted<-predict(lm18, data.frame(lux.1000=Pres.Ph1$lux.1000, Substrate=Pres.Ph1$Substrate,
                                         Weather=Pres.Ph1$Weather, Microhabitat=Pres.Ph1$Microhabitat,
                                         Size=Pres.Ph1$Size))
plot(Tb~predicted, data=Pres.Ph1)
abline(lm(Tb~predicted, data=Pres.Ph1))
Ph.Pred.lm<-lm(Tb~predicted, data=Pres.Ph1)

p4<-ggplot(data=Pres.Ph1, aes(predicted, Tb, group=Size))+
  geom_point(aes(shape=Size), color="grey60")+
  scale_shape_manual(values=c(1,16))+
  geom_abline(intercept = Ph.Pred.lm$coefficients[1], slope = Ph.Pred.lm$coefficients[2], size=1)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  xlab("Predicted Body Temperature (°C)")+
  ylab("Observed Body Temperature (°C)")+
  annotate("label", x=45, y=40, label=paste("R^2 == ", round(summary(Ph.Pred.lm)$r.squared, 2)), parse=T, fontface="bold", size=4, fill="white")+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank(), legend.text=element_text(size=15, face="bold"))
#ggsave("PhPredicted.png", height=7, width=7, plot=p4)

# Fig 4
p7<-ggplot(data=Live2021, aes(lux.1000, Tb))+
  geom_point(color="grey60", size=3)+
  geom_abline(intercept = lm31$coefficients[1], slope = lm31$coefficients[2], size=1, alpha=0.75)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="")+
  xlab("Light Level (klx)")+
  ylab("Body Temperature (°C)")+
  annotate("label", x=70, y=38, label=paste("R^2 == ", round(summary(lm31)$r.squared, 2)), parse=T, fontface="bold", size=4, fill="white")
#ggsave("Fig4.jpeg", height=7, width=7, plot=p7)

# Fig 5
pred3<-predict(lm33)
FR.Predicted.59<-as.data.frame(cbind(pred3, lm33$model$Tb))
colnames(FR.Predicted.59)<-c("Predicted", "Tb")
plot(Tb~Predicted, data=FR.Predicted.59)
abline(lm(Tb~Predicted, data=FR.Predicted.59))
FR.59.lm<-lm(Tb~Predicted, data=FR.Predicted.59)

p5<-ggplot(data=FR.Predicted.59, aes(Predicted, Tb))+
  geom_abline(intercept = FR.59.lm$coefficients[1], slope = FR.59.lm$coefficients[2], size=1.5)+
  geom_point(color="grey60", size=3)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  xlab("Predicted Body Temperature (°C)")+
  ylab("Observed Body Temperature (°C)")+
  #annotate("text", x=23, y=38, label="Lux*cos(Hour)+Lux*sin(Hour)", fontface="bold", size=4)+
  annotate("label", x=36, y=34, label=paste("R^2 == ", round(summary(lm33)$adj.r.squared, 2)), parse=T, fontface="bold", size=4, fill="white")
#ggsave("Fig5.png", height=7, width=7, plot=p5)

## Predict Free-ranging lizards ----
# Horned lizard 2112- Data from light logger
full.2112<-read.table("2112.txt", header=T) 
full.2112$ID<-as.factor("Lizard 1")
colnames(full.2112)<-c("Date","Time","lux","ID")
full.2112$datetime<-paste(full.2112$Date,full.2112$Time)
full.2112$datetime1<-as.POSIXct(strptime(full.2112$datetime, tz="MST", format("%d/%m/%Y %H:%M")))
full.2112<-subset(full.2112, format(datetime1, "%Y-%m-%d %H:%M")<"2021-07-28 16:06" & format(datetime1, "%Y-%m-%d %H:%M")>"2021-07-25 16:54")

# Horned lizard 2115- Data from light logger
full.2115<-read.table("2115.txt", header=T)
full.2115$ID<-as.factor("Lizard 2")
colnames(full.2115)<-c("Date","Time","lux","ID")
full.2115$datetime<-paste(full.2115$Date,full.2115$Time)
full.2115$datetime1<-as.POSIXct(strptime(full.2115$datetime, tz="MST", format("%d/%m/%Y %H:%M")))
full.2115<-subset(full.2115, format(datetime1, "%Y-%m-%d %H:%M")<"2021-07-28 16:12" & format(datetime1, "%Y-%m-%d %H:%M")>"2021-07-26 13:30")

full<-rbind(full.2112, full.2115)
full$Time<-format(full$datetime1, '%H:%M')
full$Time<-as.POSIXct(full$Time, format="%R", tz="MST")
full$Hour<-as.numeric(as.character(full$Time, format="%H"))

full$Hour.sin<-sin(2*pi*full$Hour/24)
full$Hour.cos<-cos(2*pi*full$Hour/24)
full$lux.1000<-full$lux/1000

full$predicted<-predict(lm33, data.frame(lux.1000=full$lux.1000, Hour.sin=full$Hour.sin, Hour.cos=full$Hour.cos))

plot(predicted~datetime1, full)

pred.2112<-subset(full, ID=="Lizard 1")
pred.2115<-subset(full, ID=="Lizard 2")
Live2112<-subset(Live2021, ID=="2112")
Live2115<-subset(Live2021, ID=="2115")

# Fig 6
dark=data.frame(
  start = as.POSIXct(c('25-07-2021 21:00', '26-07-2021 21:00', '27-07-2021 21:00'), format="%d-%m-%Y %H:%M"), 
  end   = as.POSIXct(c('26-07-2021 06:00', '27-07-2021 06:00', '28-07-2021 06:00'), format="%d-%m-%Y %H:%M"))
dark1=data.frame(
  start = as.POSIXct(c('26-07-2021 21:00', '27-07-2021 21:00'), format="%d-%m-%Y %H:%M"), 
  end   = as.POSIXct(c('27-07-2021 06:00', '28-07-2021 06:00'), format="%d-%m-%Y %H:%M"))

p9<-ggplot(data=pred.2112, aes(datetime1, predicted))+
  geom_rect(data = dark, aes(xmin = start , xmax = end,
                             ymin = -Inf, ymax = Inf),
            inherit.aes= F, alpha=0.3, fill = c("black"))+
  geom_point(size=3, alpha=.7)+
  geom_point(data=Live2112, aes(datetime, Tb), size=6, color="#2595FF", shape=18)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  xlab("")+
  ylab("Predicted Body Temperature (°C)")+
  ggtitle("Lizard 1")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=14))

p10<-ggplot(data=pred.2115, aes(datetime1, predicted))+
  geom_rect(data = dark1, aes(xmin = start , xmax = end,
                             ymin = -Inf, ymax = Inf),
            inherit.aes= F, alpha=0.3, fill = c("black"))+
  geom_point(size=3, alpha=.7)+
  geom_point(data=Live2115, aes(datetime, Tb), size=6, color="#2595FF", shape=18)+
  scale_x_datetime(breaks = "1 day", date_labels="%b %d")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  xlab("")+
  ylab("")+
  ggtitle("Lizard 2")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=14))

Live2021$VP<-as.factor("Point-of-Capture Measurement")
p11<-ggplot(data=Live2021, aes(datetime, Tb, group=VP))+
  geom_rect(data = dark1, aes(xmin = start , xmax = end,
                             ymin = -Inf, ymax = Inf),
            inherit.aes= F, alpha=0.3, fill = c("black"))+
  geom_point(aes(shape=VP), size=5, color="#2595FF")+
  scale_shape_manual(values=18)+
  theme_bw()+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  xlab("Date")+
  ylab("")+
  theme(legend.title=element_blank(), legend.text=element_text(size=15, face="bold"))+
  theme(legend.position="bottom")

v.legend=get_legend(p11)
Predicted=plot_grid(p9, p10,
                       labels = "", ncol = 2, nrow=1, align="v")
Predicted1=plot_grid(Predicted,v.legend, ncol = 1, rel_heights = c(2,0.1))
#ggsave("Predicted.png", height=7, width=15, plot=Predicted1)

