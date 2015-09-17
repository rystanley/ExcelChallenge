### Excel challenge ###

    #load libraries
      library(ggplot2);library(gridExtra);library(dplyr);library(grid)

#Made up data ------
    #Panel A
        SORV_Hist=data.frame(x=runif(50,300,700),y=runif(50,65,80),Element="SORV_Hist")
        SORV=data.frame(x=runif(50,350,750),y=runif(50,62,82),Element="SORV")
        BRDV_Hist=data.frame(x=runif(50,600,1300),y=runif(50,67,83),Element="BRDV_Hist")
        BRDV=data.frame(x=runif(50,600,1200),y=runif(50,70,77),Element="BRDV")
    #Panel B
        SORV_Hist2=data.frame(x=runif(50,500,1080),y=runif(50,45,100),Element="SORV_Hist")
        SORV2=data.frame(x=runif(50,500,1090),y=runif(50,40,80),Element="SORV")
        BRDV_Hist2=data.frame(x=runif(50,220,450),y=runif(50,28,50),Element="BRDV_Hist")
        BRDV2=data.frame(x=runif(50,200,500),y=runif(50,18,35),Element="BRDV")

    #Compile data
        plotdata=rbind(SORV_Hist,SORV,BRDV_Hist,BRDV)
        plotdata2=rbind(SORV_Hist2,SORV2,BRDV_Hist2,BRDV2)

    #Coordinates for grid lines and baselines
        Gridlines=c(seq(10,100,by=10),seq(200,1000,by=100),seq(2000,10000,by=1000))
        hGridlines=c(seq(30,90,by=10))
        Gridlines2=c(seq(100,1000,by=100),seq(2000,10000,by=1000))
        hGridlines2=c(seq(10,100,by=10))
        hlines<-c(45,52,64,69,57)
        
## Make replica plots * note I refuse morally to wrap the plot window in a box. That is stupid.
    #Create panels
        panel_A=ggplot(plotdata)+
          geom_point(aes(x=x,y=y,col=Element,shape=Element,size=Element,fill=Element))+
          scale_x_log10(limits=c(10,10000),breaks=c(10,100,1000,10000))+
          scale_y_continuous(limits=c(30,90),breaks=seq(30,90,by=5))+
          annotation_logticks(sides=c("bt"),short=unit(0.2,"cm"),long=unit(0.4,'cm'))+
          theme_bw()+
          labs(x=expression(paste("Zr/Ti","O"[2]," (ppm/wt. %)",sep="")),
               y=expression(paste("Si","O"[2]," (wt. %)",sep="")),
               colour="",shape="",size="",fill="")+
          geom_hline(aes(yintercept=hlines))+
          geom_vline(xintercept=Gridlines,col="grey60",alpha=0.5,lty=2)+
          geom_hline(yintercept=hGridlines,col="grey60",alpha=0.5,lty=2)+
          geom_text(aes(x=10,y=90,label="A"),col="black",cex=14,hjust=0)+
          geom_text(aes(x=10,y=42,label="Ultramafic"),col="black",cex=7,hjust=0,vjust=0)+
          geom_text(aes(x=10,y=53,label="Andesite"),col="black",cex=7,hjust=0,vjust=0)+
          geom_text(aes(x=10,y=65,label="Rhyodacite"),col="black",cex=7,hjust=0,vjust=0)+
          geom_text(aes(x=5000,y=46,label="Basalt"),col="black",cex=7,hjust=0,vjust=0)+
          geom_text(aes(x=5000,y=58,label="Dacite"),col="black",cex=7,hjust=0,vjust=0)+
          geom_text(aes(x=5000,y=70,label="Rhyolite"),col="black",cex=7,hjust=0,vjust=0)+
          theme(legend.position = c(0.90, 0.85),
                legend.background=element_rect(fill="white",linetype=1,colour="black"),
                legend.title=element_blank(),
                panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                axis.text.x = element_text(size=15),
                axis.text.y = element_text(size=15),
                axis.title = element_text( face="bold", size=22,vjust=0.5),
                panel.border = element_rect(fill=NA,size=1.25,colour="black"))+ 
          scale_shape_manual(values=c(24,24,25,25))+
          scale_size_manual(values=c(2,4,2,4))+
          scale_colour_manual(values=c("black","black","black","black"))+
          scale_fill_manual(values=c("yellow","yellow","orange","orange"));panel_A
        
          panel_B=ggplot(plotdata2)+
            geom_point(aes(x=x,y=y,col=Element,shape=Element,size=Element,fill=Element))+
            scale_x_log10(limits=c(100,10000),breaks=c(100,1000,10000))+
            scale_y_log10(limits=c(10,100),breaks=seq(10,100,by=10))+
            annotation_logticks(sides=c("tb"),short=unit(0.2,"cm"),long=unit(0.4,'cm'))+
            theme_bw()+
            geom_vline(xintercept=Gridlines2,col="grey60",alpha=0.5,lty=2)+
            geom_hline(yintercept=hGridlines2,col="grey60",lty=2)+
            labs(x=expression(paste("Zr/Ti","O"[2]," (ppm/wt. %)",sep="")),
                 y=expression(paste("Nb/TI","O"[2]," (ppm/wt. %)",sep="")),
                 colour="",shape="",size="",fill="")+
            geom_text(aes(x=100,y=100,label="B"),col="black",cex=14,hjust=0)+
            theme(legend.position = c(0.90, 0.85),
                  legend.background=element_rect(fill="white",linetype=1,colour="black"),
                  legend.title=element_blank(),
                  panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                  axis.text.x = element_text(size=15),
                  axis.text.y = element_text(size=15),
                  axis.title = element_text( face="bold", size=22,vjust=0.5),
                  panel.border = element_rect(fill=NA,size=1.25,colour="black"))+ # can't seem to get it to put a border
            scale_shape_manual(values=c(24,24,25,25))+
            scale_size_manual(values=c(2,4,2,4))+
            scale_colour_manual(values=c("black","black","black","black"))+
            scale_fill_manual(values=c("yellow","yellow","orange","orange"));panel_B

        #Save file
            png("ExcelChallenge/ComboPlot.png",res=300,height=14,width=9,units="in")
            grid.arrange(panel_A,panel_B,nrow=2)
            dev.off()
            
## Alternative 1 panels at the bottom --------------
            p3=panel_A+theme(legend.position="none")
            p4=panel_B+theme(legend.position="bottom")

        #save file          
            png("ExcelChallenge/ComboPlot_Alt.png",res=300,height=14,width=9,units="in")
            grid.arrange(p3,p4,nrow=2)
            dev.off()
            
## Alternative 2 add in polygon features 

        find_hull=function(df)df[chull(df$x, df$y), ] # this finds the boundaries which encapsulate all the points
        
        # make a grouping variable which is the Element
        plotdata$group="SORV";plotdata[which(plotdata$Element %in% c("BRDV_Hist","BRDV")),"group"]="BRDV"
        plotdata2$group="SORV";plotdata2[which(plotdata2$Element %in% c("BRDV_Hist","BRDV")),"group"]="BRDV"
        groups=unique(plotdata$group) # unique levels of group
        
        hulls1=NULL;hulls2=NULL
          for (i in groups){
            temp=find_hull(filter(plotdata,group==i)) # apply our function to the data which is filtered to a given country (i)
            temp2=find_hull(filter(plotdata2,group==i))
            hulls1=rbind(hulls1,temp) #for each country step (i , n=3) we add the data together
            hulls2=rbind(hulls2,temp2)
          }

    # Create plots
        p5=panel_A+geom_polygon(data=hulls1,aes(x=x,y=y,fill=group),alpha=0.3,show_guide=FALSE)+geom_point(aes(x=x,y=y,col=Element,shape=Element,size=Element,fill=Element))
        p6=panel_B+geom_polygon(data=hulls2,aes(x=x,y=y,fill=group),alpha=0.3,show_guide=FALSE)+geom_point(aes(x=x,y=y,col=Element,shape=Element,size=Element,fill=Element))
        p7=p3+geom_polygon(data=hulls1,aes(x=x,y=y,fill=group),alpha=0.3,show_guide=FALSE)+geom_point(aes(x=x,y=y,col=Element,shape=Element,size=Element,fill=Element))
        p8=p4+geom_polygon(data=hulls2,aes(x=x,y=y,fill=group),alpha=0.3,show_guide=FALSE)+geom_point(aes(x=x,y=y,col=Element,shape=Element,size=Element,fill=Element))

    #save files
      png("ExcelChallenge/ComboPlot_Alt2.png",res=300,height=14,width=9,units="in")
      grid.arrange(p5,p6,nrow=2)
      dev.off()
      
      png("ExcelChallenge/ComboPlot_Alt3.png",res=300,height=14,width=9,units="in")
      grid.arrange(p7,p8,nrow=2)
      dev.off()