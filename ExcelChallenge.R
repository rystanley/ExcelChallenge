### Excel challenge ###

library(ggplot2)

#Dummy Data#
SORV_Hist=data.frame(x=runif(50,300,700),y=runif(50,65,80),Element="SORV_Hist")
SORV=data.frame(x=runif(50,350,750),y=runif(50,62,82),Element="SORV")
BRDV_Hist=data.frame(x=runif(50,600,1300),y=runif(50,67,83),Element="BRDV_Hist")
BRDV=data.frame(x=runif(50,600,1200),y=runif(50,70,77),Element="BRDV")

plotdata=rbind(SORV_Hist,SORV,BRDV_Hist,BRDV)

# I can't seem to find a way for ggplot to do logarithmic gridlines so here are the manual coordinates
Gridlines=c(seq(10,100,by=10),seq(200,1000,by=100),seq(2000,10000,by=1000))
Gridlines2=c(seq(30,90,by=10))


## Replicate plot ------------

#Frame the plot
p1=ggplot(plotdata,aes(x=x,y=y,col=Element,shape=Element,size=Element,fill=Element))+
  geom_point()

#Manually add the gridlines (I can't seem to figure out why geom_vline(aes(xintercept=Gridlines)) doesn't work)
for (i in Gridlines){
  p1=p1+geom_vline(xintercept=i,col="grey75",alpha=0.5,lty=2)
}

for (i in Gridlines2){
  p1=p1+geom_hline(yintercept=i,col="grey75",alpha=0.5,lty=2)
}

#Add the layers back on top of the grid lines
p1=p1+geom_point()+
  scale_x_log10(limits=c(10,10000))+
  scale_y_continuous(limits=c(30,90),breaks=seq(30,90,by=5))+
  annotation_logticks(sides=c("bt"))+
  theme_bw()+
  labs(x=expression(paste("Zr/Ti","O"[2]," (ppm/wt. %)",sep="")),
       y=expression(paste("Si","O"[2]," (wt. %)",sep="")),
       colour="",shape="",size="",fill="")+
  geom_hline(aes(yintercept=c(45,52)))+
  geom_hline(aes(yintercept=c(64,69)))+
  geom_hline(aes(yintercept=57))+ # not sure why it won't let me plot 3 lines in one line of code
  geom_text(aes(x=10,y=88,label="A"),col="black",cex=14)+
  geom_text(aes(x=10,y=42,label="Ultramafic"),col="black",cex=7,hjust=0,vjust=0)+
  geom_text(aes(x=10,y=53,label="Andesite"),col="black",cex=7,hjust=0,vjust=0)+
  geom_text(aes(x=10,y=65,label="Rhyodacite"),col="black",cex=7,hjust=0,vjust=0)+
  geom_text(aes(x=5000,y=46,label="Basalt"),col="black",cex=7,hjust=0,vjust=0)+
  geom_text(aes(x=5000,y=58,label="Dacite"),col="black",cex=7,hjust=0,vjust=0)+
  geom_text(aes(x=5000,y=70,label="Rhyolite"),col="black",cex=7,hjust=0,vjust=0)+
  theme(legend.position = c(0.90, 0.85),
        legend.background=element_rect(c(fill="white",color="black",linetype=1,colour="black")),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ # can't seem to get it to put a border
  scale_shape_manual(values=c(24,24,25,25))+
  scale_size_manual(values=c(2,4,2,4))+
  scale_colour_manual(values=c("black","black","black","black"))+
  scale_fill_manual(values=c("yellow","yellow","orange","orange"));p1


#Dummy Data Plot B#
SORV_Hist2=data.frame(x=runif(50,500,1080),y=runif(50,45,100),Element="SORV_Hist")
SORV2=data.frame(x=runif(50,500,1090),y=runif(50,40,80),Element="SORV")
BRDV_Hist2=data.frame(x=runif(50,220,450),y=runif(50,28,50),Element="BRDV_Hist")
BRDV2=data.frame(x=runif(50,200,500),y=runif(50,18,35),Element="BRDV")

plotdata2=rbind(SORV_Hist2,SORV2,BRDV_Hist2,BRDV2)

p2=ggplot(plotdata2,aes(x=x,y=y,col=Element,shape=Element,size=Element,fill=Element))+
  geom_point()

Gridlines2=c(seq(100,1000,by=100),seq(2000,10000,by=1000))
hGridlines2=c(seq(10,100,by=10))

for (i in Gridlines2){
  p2=p2+geom_vline(xintercept=i,col="grey75",alpha=0.5,lty=2)
}

for (i in hGridlines2){
  p2=p2+geom_hline(yintercept=i,col="grey75",lty=2)
}

p2=p2+geom_point()+
  scale_x_log10(limits=c(100,10000))+
  scale_y_log10(limits=c(10,100),breaks=c(10,100))+
  annotation_logticks(sides=c("bt"))+
  theme_bw()+
  labs(x=expression(paste("Zr/Ti","O"[2]," (ppm/wt. %)",sep="")),
       y=expression(paste("Nb/TI","O"[2]," (wt. %)",sep="")),
       colour="",shape="",size="",fill="")+
  geom_text(aes(x=110,y=95,label="B"),col="black",cex=14)+
  theme(legend.position = c(0.90, 0.85),
        legend.background=element_rect(c(fill="white",color="black",linetype=1,colour="black")),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ # can't seem to get it to put a border
  scale_shape_manual(values=c(24,24,25,25))+
  scale_size_manual(values=c(2,4,2,4))+
  scale_colour_manual(values=c("black","black","black","black"))+
  scale_fill_manual(values=c("yellow","yellow","orange","orange"));p2


#Save the file
tiff("ExcelChallenge/ComboPlot.tiff",res=300,height=14,width=9,units="in")
grid.arrange(p1,p2,nrow=2)
dev.off()


### Better option ---------

#Frame the plot
p3=ggplot(plotdata,aes(x=x,y=y,col=Element,shape=Element,size=Element,fill=Element))+
  geom_point()

#Manually add the gridlines (I can't seem to figure out why geom_vline(aes(xintercept=Gridlines)) doesn't work)
for (i in Gridlines){
  p3=p3+geom_vline(xintercept=i,col="grey75",alpha=0.5,lty=2)
}

for (i in Gridlines2){
  p3=p3+geom_hline(yintercept=i,col="grey75",alpha=0.5,lty=2)
}

#Add the layers back on top of the grid lines
p3=p3+geom_point()+
  scale_x_log10(limits=c(10,10000))+
  scale_y_continuous(limits=c(30,90),breaks=seq(30,90,by=5))+
  annotation_logticks(sides=c("bt"))+
  theme_bw()+
  labs(x=expression(paste("Zr/Ti","O"[2]," (ppm/wt. %)",sep="")),
       y=expression(paste("Si","O"[2]," (wt. %)",sep="")),
       colour="",shape="",size="",fill="")+
  geom_hline(aes(yintercept=c(45,52)))+
  geom_hline(aes(yintercept=c(64,69)))+
  geom_hline(aes(yintercept=57))+ # not sure why it won't let me plot 3 lines in one line of code
  geom_text(aes(x=10,y=88,label="A"),col="black",cex=14)+
  geom_text(aes(x=10,y=42,label="Ultramafic"),col="black",cex=7,hjust=0,vjust=0)+
  geom_text(aes(x=10,y=53,label="Andesite"),col="black",cex=7,hjust=0,vjust=0)+
  geom_text(aes(x=10,y=65,label="Rhyodacite"),col="black",cex=7,hjust=0,vjust=0)+
  geom_text(aes(x=5000,y=46,label="Basalt"),col="black",cex=7,hjust=0,vjust=0)+
  geom_text(aes(x=5000,y=58,label="Dacite"),col="black",cex=7,hjust=0,vjust=0)+
  geom_text(aes(x=5000,y=70,label="Rhyolite"),col="black",cex=7,hjust=0,vjust=0)+
  theme(legend.position = "none",
        legend.background=element_rect(c(fill="white",color="black",linetype=1,colour="black")),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ # can't seem to get it to put a border
  scale_shape_manual(values=c(24,24,25,25))+
  scale_size_manual(values=c(2,4,2,4))+
  scale_colour_manual(values=c("black","black","black","black"))+
  scale_fill_manual(values=c("yellow","yellow","orange","orange"));p3

p4=ggplot(plotdata2,aes(x=x,y=y,col=Element,shape=Element,size=Element,fill=Element))+
  geom_point()

for (i in Gridlines2){
  p4=p4+geom_vline(xintercept=i,col="grey75",alpha=0.5,lty=2)
}

for (i in hGridlines2){
  p4=p4+geom_hline(yintercept=i,col="grey75",lty=2)
}

p4=p4+geom_point()+
  scale_x_log10(limits=c(100,10000))+
  scale_y_log10(limits=c(10,100),breaks=c(10,100))+
  annotation_logticks(sides=c("bt"))+
  theme_bw()+
  labs(x=expression(paste("Zr/Ti","O"[2]," (ppm/wt. %)",sep="")),
       y=expression(paste("Nb/TI","O"[2]," (wt. %)",sep="")),
       colour="",shape="",size="",fill="")+
  geom_text(aes(x=110,y=95,label="B"),col="black",cex=14)+
  theme(legend.position = "bottom",
        legend.background=element_rect(c(fill="white",color="black",linetype=1,colour="black")),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ # can't seem to get it to put a border
  scale_shape_manual(values=c(24,24,25,25))+
  scale_size_manual(values=c(2,4,2,4))+
  scale_colour_manual(values=c("black","black","black","black"))+
  scale_fill_manual(values=c("yellow","yellow","orange","orange"));p4

#Save the file
tiff("ExcelChallenge/ComboPlot_Alt.tiff",res=300,height=14,width=9,units="in")
grid.arrange(p3,p4,nrow=2)
dev.off()
