library(grid)

COLS = c('#173f62','#5b8f99','#faab5c','#bf3414','#851826','grey30')	

xlevels= list(rnorm(n=1000,mean = 12,sd = 2),
              rnorm(n=1000,mean = 12.5,sd = 1),
              rnorm(n=1000,mean = 5,sd = 1.5),
              rnorm(n=1000,mean = 5,sd = 1.5)) 

BOXPLOT_DATA = list()  
VIOLIN_DATA = list()  

for(i in 1:length(xlevels))
{
  BOXPLOT_DATA[[i]] = list()
  BOXPLOT_DATA[[i]]$min = min((xlevels[[i]]),na.rm = TRUE)
  BOXPLOT_DATA[[i]]$min[BOXPLOT_DATA[[i]]$min < -1] = -1 
  BOXPLOT_DATA[[i]]$q1 = as.numeric(quantile((xlevels[[i]]),probs = 0.25,na.rm = TRUE))
  BOXPLOT_DATA[[i]]$q2 = as.numeric(quantile((xlevels[[i]]),probs = 0.5,na.rm = TRUE))
  BOXPLOT_DATA[[i]]$q3 = as.numeric(quantile((xlevels[[i]]),probs = 0.75,na.rm = TRUE))
  BOXPLOT_DATA[[i]]$max = max((xlevels[[i]]),na.rm = TRUE)
}

for(i in 1:length(xlevels))
{
  VIOLIN_DATA[[i]] = list()
  dens = density(xlevels[[i]])
  VIOLIN_DATA[[i]]$x = dens$x
  VIOLIN_DATA[[i]]$y = dens$y
}

# png(filename = 'violin.png',width = 12,height = 12,units = 'cm',res = 700)
if(1)
{
  YRANGE = c(-2,20)
  XRANGE = c(0,4)
  grid.newpage()  
  pushViewport(plotViewport(c(3,3.5,1.5,1.5),xscale=XRANGE+0.5,yscale=c(YRANGE)))
  grid.rect()
  grid.xaxis(label= c("Level1", "Level2",  "Level3",'Level4'),  
             at = 1:4,gp=gpar(fontsize=8))
  grid.text(label= c("V1", "V2"),  
            x = unit(c(1.5,3.5),units = 'native'),
            y= unit(-2,units = 'lines'),
            gp=gpar(fontsize=10,fontface = 'bold'))
  grid.yaxis(gp=gpar(fontsize=8))
  
  grid.text('TITLE', y=unit(1, 'npc')+unit(1, 'lines'), gp=gpar(fontsize=10,fontface = 'bold'))
  grid.text('Y-axis label',x=unit(-3.4,'lines'),rot=90,gp=gpar(fontsize=10,fontface = 'bold'))
  
  XAXISPOS <- c(1:4)  # Positions of the boxplots on the x-axis
  
  grid.lines(x = unit(2.5,'native'),gp = gpar(lty = 'dashed'))
  for (i in 1:length(xlevels)) 
  {
    
    # Draw the vertical line from min to max (whiskers)
    grid.lines(x = c(XAXISPOS[i], XAXISPOS[i]), 
               y = c(BOXPLOT_DATA[[i]]$min, BOXPLOT_DATA[[i]]$max), 
               default.units = 'native', 
               gp = gpar(col = COLS[i], lwd = 2))
    
  
    grid.polygon(x = i+c(-VIOLIN_DATA[[i]]$y,rev(VIOLIN_DATA[[i]]$y)),
                 y = c(VIOLIN_DATA[[i]]$x,rev(VIOLIN_DATA[[i]]$x)),
                 default.units = 'native',
                 gp = gpar(col = NA, fill = COLS[i]))
    
    offset = 0.05
    # Draw the box from Q1 to Q3
    grid.polygon(x = c(XAXISPOS[i] - offset, XAXISPOS[i] + offset, XAXISPOS[i] + offset, XAXISPOS[i] - offset), 
                 y = c(BOXPLOT_DATA[[i]]$q1, BOXPLOT_DATA[[i]]$q1, BOXPLOT_DATA[[i]]$q3, BOXPLOT_DATA[[i]]$q3), 
                 default.units = 'native', 
                 gp = gpar(col = NA, fill = 'white'))
    
    # Draw the median line (inside the box)
    grid.lines(x = c(XAXISPOS[i] - offset, XAXISPOS[i] + offset), 
               y = c(BOXPLOT_DATA[[i]]$q2, BOXPLOT_DATA[[i]]$q2), 
               default.units = 'native', 
               gp = gpar(col =COLS[i], lwd = 2))
  }
  popViewport()
  
}
# dev.off()