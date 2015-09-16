#### Excel challenge using base plots ####
tiff("baseplotmakesthebestplots.tiff",res=300,width=4.72,height=6.59,units="in")

layout(matrix(c(1:3),nrow=3),heights=c(18,1,18))

par(mar=c(2.5,3,0.5,0.5),cex=0.7)
#### Dummy Data ####
SORV_Hist=data.frame(x=runif(50,300,700),y=runif(50,65,80))
SORV=data.frame(x=runif(50,350,750),y=runif(50,62,82))
BRDV_Hist=data.frame(x=runif(50,600,1300),y=runif(50,67,83))
BRDV=data.frame(x=runif(50,600,1200),y=runif(50,70,77))


#### plotting ####
#basic plot
xlim=c(10,10000)  # everything will scale to xlim and ylim automatically
ylim=c(30,90)
yintervals <- 5 # set the y interval for lines and labels here, everything will update
plot.new()
plot.window(log="x",xlim=xlim,ylim=ylim)

#### add lines ####
for(i in (log10(xlim[1])):(log10(xlim[2]))){
    for(j in 1:9){
        abline(v=j*10^i,col='lightgrey',lty=2)
    }
}
for(i in (ylim[1]/yintervals):(ylim[2]/yintervals)){
        abline(h=i*yintervals,col='lightgrey',lty=2)
}

#### add thick lines with text ####
text(x=xlim[1]+2,y=88,label="A",col="black",cex=2,font=2)
text(x=xlim[1],y=42,label="Ultramafic",col="black",cex=1,adj = c(0,0),font=2)
text(x=xlim[1],y=53,label="Andesite",col="black",cex=1,adj = c(0,0),font=2)
text(x=xlim[1],y=65,label="Rhyodacite",col="black",cex=1,adj = c(0,0),font=2)
text(x=xlim[2],y=46,label="Basalt",col="black",cex=1,adj = c(1,0),font=2)
text(x=xlim[2],y=58,label="Dacite",col="black",cex=1,adj = c(1,0),font=2)
text(x=xlim[2],y=70,label="Rhyolite",col="black",cex=1,adj = c(1,0),font=2)
abline(h=45)
abline(h=52)
abline(h=57)
abline(h=64)
abline(h=69)

#### Add points ####
types <- c("SORV_Hist","SORV","BRDV_Hist","BRDV")
pchs <- c(24,24,24,24)
bg_cols <- c("yellow","yellow","orange","orange")
cexs <- c(1.5,1,1.5,1)
for(t in seq(types)){
    points(y~x,data=get(types[t]),pch=pchs[t],col="black",bg=bg_cols[t],cex=cexs[t])
}
    

#### Add legend ####
legend("topright",inset=c(0.02,0.02),types,pch=pchs,pt.cex=cexs,pt.bg=bg_cols)

#### Add axes ####
axis(1,at=c(10^((log10(xlim[1])):(log10(xlim[2])))),padj=-1.3,tck=-0.01)
axis(2,at=c(((ylim[1]/yintervals):(ylim[2]/yintervals))*yintervals),hadj=0.3,las=1,tck=-0.01)
box()
box(which="figure",lwd=5)  # I hate this but if we're trying to replicate...

#### Add axes labels ####
title(xlab=expression(~bold(paste("Zr/Ti","O"[2]," (ppm/wt. %)",sep=""))),mgp=c(1.3,0,0),cex=1.5)
title(ylab=expression(~bold(paste("Si","O"[2]," (wt. %)",sep=""))),mgp=c(1.7,0,0),cex=1.5)

#### make weird blank spot ##
par(mar=c(0,0,0,0),cex=0.7)

plot.new()

################# Second plot ################################################################################
par(mar=c(2.5,3,0.5,0.5),cex=0.7)

#### data ####
#Dummy Data Plot B#
SORV_Hist=data.frame(x=runif(50,500,1080),y=runif(50,45,100),Element="SORV_Hist")
SORV=data.frame(x=runif(50,500,1090),y=runif(50,40,80),Element="SORV")
BRDV_Hist=data.frame(x=runif(50,220,450),y=runif(50,28,50),Element="BRDV_Hist")
BRDV=data.frame(x=runif(50,200,500),y=runif(50,18,35),Element="BRDV")

#### plotting ####
#basic plot
xlim=c(100,10000)  # everything will scale to xlim and ylim automatically
ylim=c(10,100)
plot.new()
plot.window(log="xy",xlim=xlim,ylim=ylim)

#### add lines ####
for(i in (log10(xlim[1])):(log10(xlim[2]))){
    for(j in 1:9){
        abline(v=j*10^i,col='lightgrey',lty=2)
    }
}
for(i in (log10(ylim[1])):(log10(ylim[2]))){
    for(j in 1:9){
        abline(h=j*10^i,col='lightgrey',lty=2)
    }
}


#### Add points ####
types <- c("SORV_Hist","SORV","BRDV_Hist","BRDV")
pchs <- c(24,24,24,24)
bg_cols <- c("yellow","yellow","orange","orange")
cexs <- c(1.5,1,1.5,1)
for(t in seq(types)){
    points(y~x,data=get(types[t]),pch=pchs[t],col="black",bg=bg_cols[t],cex=cexs[t])
}


#### Add legend ####
legend("bottomright",inset=c(0.02,0.02),types,pch=pchs,pt.cex=cexs,pt.bg=bg_cols)

#### Add axes ####
axis(1,at=c(10^((log10(xlim[1])):(log10(xlim[2])))),padj=-1.3,tck=-0.01)
axis(2,at=c(10^((log10(ylim[1])):(log10(ylim[2])))),padj=-1.3,tck=-0.01)
box()
box(which="figure",lwd=5)  # I hate this but if we're trying to replicate...

#### Add axes labels ####
title(xlab=expression(~bold(paste("Zr/Ti","O"[2]," (ppm/wt. %)",sep=""))),mgp=c(1.3,0,0),cex=1.5)
title(ylab=expression(~bold(paste("Nb/Ti","O"[2]," (ppm/wt. %)",sep=""))),mgp=c(1.7,0,0),cex=1.5)

dev.off()

# windowsFont()