#===========================================================================
# Title: Hierarchical_Clustering_plus_Visualization.R
# Author: Matt Buckley
# Description: Using R's 'hclust' function to explore the relative
## similarities in food consumption patterns among 8 high-income
## nations plus the U.A.E.
# Liscense: MIT license: http://www.opensource.org/licenses/mit-license.php 
#===========================================================================


# Load R packages
library(DMwR)
library(RMySql)
library(DBI)
library(RColorBrewer)
library(graphics)
library(ape)

### Read country data from csv
#UAE
consumption_1975.uae=read.delim("consumption_1975.csv", header=TRUE, fileEncoding="UCS-2LE")
consumption_2005.uae=read.delim("consumption_2005.csv", header=TRUE, fileEncoding="UCS-2LE")  
x_uae_1975<-consumption_1975.uae$Food.supply..kcal.capita.day.
y_uae_1975<-(x_uae/(x_uae_[2]))
x_uae_2005<-consumption_2005.uae$Food.supply..kcal.capita.day.
y_uae_2005<-(x_uae/(x_uae_2005[2]))
uae_1975<-y_uae_1975[-2]
uae_2005<-y_uae_2005


#UK
consumption_1975.uk=read.delim("consumption_1975.csv", header=TRUE, fileEncoding="UCS-2LE")
consumption_2005.uk=read.delim("consumption_2005.csv", header=TRUE, fileEncoding="UCS-2LE")
x_uk_1975<-consumption_1975.uk$Food.supply..kcal.capita.day.
y_uk_1975<-(x_uk/(x_uk_[2]))
x_uk_2005<-consumption_2005.uk$Food.supply..kcal.capita.day.
y_uk_2005<-(x_uk/(x_uk_2005[2]))
uk_1975<-y_uk_1975[-2]
uk_2005<-y_uk_2005


#Sweden
consumption_1975.sw=read.delim("consumption_1975.csv", header=TRUE, fileEncoding="UCS-2LE")
consumption_2005.sw=read.delim("consumption_2005.csv", header=TRUE, fileEncoding="UCS-2LE")
x_sw_1975<-consumption_1975.sw$Food.supply..kcal.capita.day.
y_sw_1975<-(x_sw/(x_sw_[2]))
x_sw_2005<-consumption_2005.sw$Food.supply..kcal.capita.day.
y_sw_2005<-(x_sw/(x_sw_2005[2]))
sweden_1975<-y_sw_1975[-2]
sweden_2005<-y_sw_2005


#Norway
consumption_1975.nw=read.delim("consumption_1975.csv", header=TRUE, fileEncoding="UCS-2LE")
consumption_2005.nw=read.delim("consumption_2005.csv", header=TRUE, fileEncoding="UCS-2LE")
x_nw_1975<-consumption_1975.nw$Food.supply..kcal.capita.day.
y_nw_1975<-(x_nw/(x_nw_[2]))
x_nw_2005<-consumption_2005.nw$Food.supply..kcal.capita.day.
y_nw_2005<-(x_nw/(x_nw_2005[2]))
norway_1975<-y_nw_1975[-2]
norway_2005<-y_nw_2005


#France
consumption_1975.fr=read.delim("consumption_1975.csv", header=TRUE, fileEncoding="UCS-2LE")
consumption_2005.fr=read.delim("consumption_2005.csv", header=TRUE, fileEncoding="UCS-2LE")
x_fr_1975<-consumption_1975.fr$Food.supply..kcal.capita.day.
y_fr_1975<-(x_fr/(x_fr_[2]))
x_fr_2005<-consumption_2005.fr$Food.supply..kcal.capita.day.
y_fr_2005<-(x_fr/(x_fr_2005[2]))
france_1975<-y_fr_1975[-2]
france_2005<-y_fr_2005


#Germany
consumption_1975.gr=read.delim("consumption_1975.csv", header=TRUE, fileEncoding="UCS-2LE")
consumption_2005.gr=read.delim("consumption_2005.csv", header=TRUE, fileEncoding="UCS-2LE")
x_gr_1975<-consumption_1975.gr$Food.supply..kcal.capita.day.
y_gr_1975<-(x_gr/(x_gr_[2]))
x_gr_2005<-consumption_2005.gr$Food.supply..kcal.capita.day.
y_gr_2005<-(x_gr/(x_gr_2005[2]))
germany_1975<-y_gr_1975[-2]
germany_2005<-y_gr_2005


#Italy
consumption_1975.it=read.delim("consumption_1975.csv", header=TRUE, fileEncoding="UCS-2LE")
consumption_2005.it=read.delim("consumption_2005.csv", header=TRUE, fileEncoding="UCS-2LE")
x_it_1975<-consumption_1975.it$Food.supply..kcal.capita.day.
y_it_1975<-(x_it/(x_it_[2]))
x_it_2005<-consumption_2005.it$Food.supply..kcal.capita.day.
y_it_2005<-(x_it/(x_it_2005[2]))
italy_1975<-y_it_1975[-2]
italy_2005<-y_it_2005	


#Greece
consumption_1975.gc=read.delim("consumption_1975.csv", header=TRUE, fileEncoding="UCS-2LE")
consumption_2005.gc=read.delim("consumption_2005.csv", header=TRUE, fileEncoding="UCS-2LE")
x_gc_1975<-consumption_1975.gc$Food.supply..kcal.capita.day.
y_gc_1975<-(x_gc/(x_gc_[2]))
x_gc_2005<-consumption_2005.gc$Food.supply..kcal.capita.day.
y_gc_2005<-(x_gc/(x_gc_2005[2]))
greece_1975<-y_gc_1975[-2]
greece_2005<-y_gc_2005


#Spain
consumption_1975.sp=read.delim("consumption_1975.csv", header=TRUE, fileEncoding="UCS-2LE")
consumption_2005.sp=read.delim("consumption_2005.csv", header=TRUE, fileEncoding="UCS-2LE")
x_sp_1975<-consumption_1975.sp$Food.supply..kcal.capita.day.
y_sp_1975<-(x_sp/(x_sp_[2]))
x_sp_2005<-consumption_2005.sp$Food.supply..kcal.capita.day.
y_sp_2005<-(x_sp/(x_sp_2005[2]))
spain_1975<-y_sp_1975[-2]
spain_2005<-y_sp_2005


# ... Or alternatively we can get the data from MySQL if we have it stored there
drv <- dbDriver("MySQL")
ct <- dbConnect(drv, dbname="myDatabase", "myUsername", "myPassword")
data_from_mysql <- dbGetQuery(ct, "select*from consumption")
consumption.df <- xts(data_from_mysql, order.by=as.Date(data_from_mysql[,1]))

...

dbDisconnect(ct)
dbUnloadDriver(drv)



#Combine everything using cbind()  
t_1975<-t(cbind(uae_1975,uk_1975,france_1975,germany_1975,sweden_1975,norway_1975,italy_1975,greece_1975,spain))
t_2005<-t(cbind(uae_2005,uk_2005,france_2005,germany_2005,sweden_2005,norway_2005,italy_2005,greece_2005,spain))

d_1975<-dist(as.matrix(t_1975))
d_2005<-dist(as.matrix(t_2005))

years<-list(d_1975, d_2005)

dendrogram(hc) <- {
  #DENDROGRAM
  # background color
  op=par(bg="#DDE3CA")
  #plot dendrogram
  plot(hc, col="#487AA1", col.main="#45ADAB", col.lab="#7C8071", col.axis="#F38630", lwd=3, lty=3, sub='', hang=-1, axes=FALSE, main="Grouping of Countries Based on Dietary Structure")
  #Add axis
  axis(side=2, col="#F38630", labels=FALSE, lwd=2)
  # add text in margin
  mtext(side=2, line=1, col="#A38630", las=2)
  par(op)
  
  #Fan Dendrogram
  ##### vector of colors
  mypalette = c("#556270", "#4ECDC4", "#1B676B", "#FF6B6B")
  #### cutting dendrogram in 4 clusters
  clus4 = cutree(hc, 4)
  plot(as.phylo(hc), type='fan')
  
}


# plot the final dendrograms
lapply(years, dendrogram(years))