library(grid)

COLS = c('#173f62','#5b8f99','#faab5c','#bf3414','#851826','grey30')	

data= list(rnorm(n=1000,mean = 12,sd = 2),
              rnorm(n=1000,mean = 12.5,sd = 1)) 

BOXPLOT_DATA = list()  
VIOLIN_DATA = list()  

for(i in 1:length(data))
{
  BOXPLOT_DATA[[i]] = list()
  BOXPLOT_DATA[[i]]$min = min((data[[i]]),na.rm = TRUE)
  BOXPLOT_DATA[[i]]$min[BOXPLOT_DATA[[i]]$min < -1] = -1 
  BOXPLOT_DATA[[i]]$q1 = as.numeric(quantile((data[[i]]),probs = 0.25,na.rm = TRUE))
  BOXPLOT_DATA[[i]]$q2 = as.numeric(quantile((data[[i]]),probs = 0.5,na.rm = TRUE))
  BOXPLOT_DATA[[i]]$q3 = as.numeric(quantile((data[[i]]),probs = 0.75,na.rm = TRUE))
  BOXPLOT_DATA[[i]]$max = max((data[[i]]),na.rm = TRUE)
}

for(i in 1:length(data))
{
  VIOLIN_DATA[[i]] = list()
  dens = density(data[[i]])
  VIOLIN_DATA[[i]]$x = dens$x
  VIOLIN_DATA[[i]]$y = dens$y
}

# png(filename = 'violin.png',width = 12,height = 12,units = 'cm',res = 700)
if(1)
{
  YRANGE = c(-2,20)
  XRANGE = c(-2,20)
  scale = c(20,10)
  grid.newpage()  
  pushViewport(plotViewport(c(4,4,1.5,1.5),xscale=c(XRANGE),yscale=c(YRANGE)))
  grid.rect()
  grid.yaxis(gp=gpar(fontsize=8))
  grid.xaxis(gp=gpar(fontsize=8))
  
  grid.text('TITLE', y=unit(1, 'npc')+unit(1, 'lines'), gp=gpar(fontsize=10,fontface = 'bold'))
  grid.text('Y-axis label',x=unit(-3.4,'lines'),rot=90,gp=gpar(fontsize=8,fontface = 'bold'))
  grid.text('X-axis label',y=unit(-2.4,'lines'),rot=0,gp=gpar(fontsize=8,fontface = 'bold'))
  
  XAXISPOS <- c(1:4)  # Positions of the boxplots on the x-axis
  
  grid.lines(x= XRANGE,y = YRANGE,default.units = 'native',
             gp = gpar(lty='dotted'))
  grid.points(x = data[[1]],
              y = data[[2]],default.units = 'native',pch = 16,gp = gpar(alpha = 0.5,cex = 0.5))
  
  for (i in 1:length(data)) 
  {

    if(i==1) 
    {
      grid.polygon(y = unit(c(VIOLIN_DATA[[i]]$y)*scale[i],'native')+unit(-2,'native'),
                   x = unit(VIOLIN_DATA[[i]]$x,'native'),
                   gp = gpar(col = NA, fill = COLS[i],alpha = 0.2))
      
      grid.polygon(x = c(BOXPLOT_DATA[[i]]$q1, BOXPLOT_DATA[[i]]$q1, BOXPLOT_DATA[[i]]$q3, BOXPLOT_DATA[[i]]$q3), 
                   y = c(-1 - offset,-1 + offset, -1 + offset, -1 - offset),
                   default.units = 'native', 
                   gp = gpar(col = NA, fill = 'white'))
      
      grid.lines(x = c(BOXPLOT_DATA[[i]]$q2, BOXPLOT_DATA[[i]]$q2), 
                 y = c(-1 - offset,-1 + offset), 
                 default.units = 'native', 
                 gp = gpar(col =COLS[i], lwd = 2,alpha = 0.2))
      
    }
    
    if(i==2) 
    {
      grid.polygon(x = unit(c(VIOLIN_DATA[[i]]$y)*scale[i],'native')+unit(-2,'native'),
                   y = unit(VIOLIN_DATA[[i]]$x,'native'),
                   gp = gpar(col = NA, fill = COLS[i],alpha = 0.2))
      
      grid.polygon(y = c(BOXPLOT_DATA[[i]]$q1, BOXPLOT_DATA[[i]]$q1, BOXPLOT_DATA[[i]]$q3, BOXPLOT_DATA[[i]]$q3), 
                   x = c(-1 - offset,-1 + offset, -1 + offset, -1 - offset),
                   default.units = 'native', 
                   gp = gpar(col = NA, fill = 'white'))
      
      grid.lines(y = c(BOXPLOT_DATA[[i]]$q2, BOXPLOT_DATA[[i]]$q2), 
                 x = c(-1 - offset,-1 + offset), 
                 default.units = 'native', 
                 gp = gpar(col =COLS[i], lwd = 2,alpha = 0.2))
      
    }
  }
  popViewport()
  
}
# dev.off()