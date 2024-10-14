library(grid)

COLS = c('#173f62','#5b8f99','#faab5c','#bf3414','#851826','grey30')	
  
xlevels= list(rlnorm(n=1000,meanlog = 2,sdlog = 2),
              rlnorm(n=1000,meanlog = 2.5,sdlog = 1),
              rlnorm(n=1000,meanlog = 2,sdlog = 1.5)) 

BOXPLOT_DATA = list()  

for(i in 1:length(xlevels))
{
  BOXPLOT_DATA[[i]] = list()
  BOXPLOT_DATA[[i]]$min = min(log10(xlevels[[i]]),na.rm = TRUE)
  BOXPLOT_DATA[[i]]$min[BOXPLOT_DATA[[i]]$min < -1] = -1 
  BOXPLOT_DATA[[i]]$q1 = as.numeric(quantile(log10(xlevels[[i]]),probs = 0.25,na.rm = TRUE))
  BOXPLOT_DATA[[i]]$q2 = as.numeric(quantile(log10(xlevels[[i]]),probs = 0.5,na.rm = TRUE))
  BOXPLOT_DATA[[i]]$q3 = as.numeric(quantile(log10(xlevels[[i]]),probs = 0.75,na.rm = TRUE))
  BOXPLOT_DATA[[i]]$max = max(log10(xlevels[[i]]),na.rm = TRUE)
}

# png(filename = 'cost_boxplot.png',width = 12,height = 12,units = 'cm',res = 700)
if(1)
{
  YRANGE = log10(c(0.1,10000))
  grid.newpage()  
  pushViewport(plotViewport(c(3,3.5,1.5,1.5),xscale=c(0.5,3.5),yscale=c(YRANGE)))
  grid.rect()
  grid.xaxis(label= c("Cost 1", "Cost 2",  "Cost 3"),  
             at = c(1,2,3),gp=gpar(fontsize=8))
  grid.yaxis(label= c(10,100,1000,10000),  
             at = log10(c(10,100,1000,10000)),
             gp=gpar(fontsize=8))
  
  grid.text('TITLE', y=unit(1, 'npc')+unit(1, 'lines'), gp=gpar(fontsize=10,fontface = 'bold'))
  grid.text('Y-axis label',x=unit(-3.4,'lines'),rot=90,gp=gpar(fontsize=10,fontface = 'bold'))
  


  XAXISPOS <- c(1:3)  # Positions of the boxplots on the x-axis
  
  for (i in 1:length(xlevels)) 
  {
    # Draw the vertical line from min to max (whiskers)
    grid.lines(x = c(XAXISPOS[i], XAXISPOS[i]), 
               y = c(BOXPLOT_DATA[[i]]$min, BOXPLOT_DATA[[i]]$max), 
               default.units = 'native', 
               gp = gpar(col = COLS[i], lwd = 2))
    
    # Draw the box from Q1 to Q3
    grid.polygon(x = c(XAXISPOS[i] - 0.3, XAXISPOS[i] + 0.3, XAXISPOS[i] + 0.3, XAXISPOS[i] - 0.3), 
                 y = c(BOXPLOT_DATA[[i]]$q1, BOXPLOT_DATA[[i]]$q1, BOXPLOT_DATA[[i]]$q3, BOXPLOT_DATA[[i]]$q3), 
                 default.units = 'native', 
                 gp = gpar(col = NA, fill = COLS[i]))
    
    # Draw the median line (inside the box)
    grid.lines(x = c(XAXISPOS[i] - 0.3, XAXISPOS[i] + 0.3), 
               y = c(BOXPLOT_DATA[[i]]$q2, BOXPLOT_DATA[[i]]$q2), 
               default.units = 'native', 
               gp = gpar(col = 'white', lwd = 2))
  }
  popViewport()
  
}
# dev.off()
