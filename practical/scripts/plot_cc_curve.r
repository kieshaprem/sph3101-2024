# choose colours for thesis
# COLS = pal_npg(palette = c("nrc"),alpha = 1)(9)
COLS = c('#173f62','#5b8f99','#faab5c','#bf3414','#851826','grey30')[c(4,2,3)]	

# generate data for CC curve
cc_data = data.frame(threshold = seq(0.01,1,0.01), n = NA, percent = NA)
for (i in 1:nrow(cc_data)) {
  cc_data$n[i] = sum(1*(adultTB_1y$total_oop >= cc_data$threshold[i]*adultTB_1y$a1_q7*2))
}
cc_data$percent = cc_data$n/nrow(adultTB_1y)*100

# plot cc curve
plot(x=cc_data$threshold*100, y=cc_data$percent, type = "l")

png('cc_curve_2.png',width = 20,height = 16,units = 'cm',res = 700)
if(1)
{
  # 1) plots a blank canvas 
  grid.newpage()  
  
  # 2) sets the plotting boundaries
  pushViewport(plotViewport(c(4,4,1,1),xscale = c(0,100),yscale = c(0,32)))   
  
  # 3) plots a rectangular border around the plotting area
  grid.rect(gp = gpar(fill=NA))
  
  # 4) plot axes of the graph
  grid.yaxis()
  grid.xaxis()
  
  # 5) plot the axes label names 
  grid.text('Threshold (%)', y = unit(-3,'lines'),gp = gpar(fontsize=12,fontface = 'plain'))
  grid.text('Percentage (%)', x = unit(-3.0,'lines'), rot = 90, gp = gpar(fontsize=12,fontface = 'plain'))
  
  # 6) plot the title of the figure
  grid.text('Percentage individuals experiencing catastrophic costs\nat different thresholds levels (%)', 
            y = unit(1, 'npc')+unit(-2,'lines'), rot = 0, gp = gpar(fontsize=13,fontface = 'bold'))
  
  # plot data
  grid.lines(x = cc_data$threshold*100,y = cc_data$percent,default.units = 'native',gp = gpar(col = 'grey40',lwd = 2))
  grid.points(x = cc_data$threshold[c(10,20,30)]*100,y = cc_data$percent[c(10,20,30)], default.units = 'native', gp = gpar(col = COLS[1:3]))
  grid.points(x = cc_data$threshold[c(10,20,30)]*100,y = cc_data$percent[c(10,20,30)], default.units = 'native',
              pch = 16, gp=gpar(cex = 0.5, col = COLS[1:3]))
  
  grid.lines(x = c(10,10),y = c(0,cc_data$percent[10]), default.units = 'native',gp=gpar(col = COLS[1],lty = 'dashed'))
  grid.lines(x = c(20,20),y = c(0,cc_data$percent[20]), default.units = 'native',gp=gpar(col = COLS[2],lty = 'dashed'))
  grid.lines(x = c(30,30),y = c(0,cc_data$percent[30]), default.units = 'native',gp=gpar(col = COLS[3],lty = 'dashed'))
  
  grid.lines(x = c(0,10),y = c(cc_data$percent[10],cc_data$percent[10]), default.units = 'native',gp=gpar(col = COLS[1],lty = 'dashed'))
  grid.lines(x = c(0,20),y = c(cc_data$percent[20],cc_data$percent[20]), default.units = 'native',gp=gpar(col = COLS[2],lty = 'dashed'))
  grid.lines(x = c(0,30),y = c(cc_data$percent[30],cc_data$percent[30]), default.units = 'native',gp=gpar(col = COLS[3],lty = 'dashed'))
  
  
  grid.text('8.7%', x = unit(-0.2,'lines'), y = unit(cc_data$percent[10], 'native'), rot = 0, just = 'right', gp = gpar(fontsize=12,fontface = 'bold',col = COLS[1]))
  grid.text('3.2%', x = unit(-0.2,'lines'), y = unit(cc_data$percent[20], 'native'), rot = 0, just = 'right',gp = gpar(fontsize=12,fontface = 'bold',col = COLS[2]))
  grid.text('2.4%', x = unit(-0.2,'lines'), y = unit(cc_data$percent[30], 'native'), rot = 0, just = 'right',gp = gpar(fontsize=12,fontface = 'bold',col = COLS[3]))
  
  
  popViewport()
}
dev.off()

