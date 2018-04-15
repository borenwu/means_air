datafile = read.csv('air_data.csv',header = T,fileEncoding = 'GBK')

delete_na  = datafile[-which(is.na(datafile$SUM_YR_1) | is.na(datafile$SUM_YR_2)),]
index = ((delete_na$SUM_YR_1 == 0 & delete_na$SUM_YR_2 == 0)
         * (delete_na$avg_discount != 0 )
         * (delete_na$SEG_KM_SUM > 0))

deletedata = delete_na[-which(index == 1),]
data = deletedata[,c('LOAD_TIME','FFP_DATE','LAST_TO_END','FLIGHT_COUNT','SEG_KM_SUM','avg_discount')]

library('lubridate')
interval <- interval(as.Date(data$LOAD_TIME), as.Date(data$FFP_DATE))  
L <- time_length(interval, 'month')
L <- round(L, 2)
LRFMC = data.frame(L=L,R=data$LAST_TO_END,F=data$FLIGHT_COUNT,M=data$SEG_KM_SUM,C=data$avg_discount)

zscoredfile = scale(LRFMC)
colnames(zscoredfile) = c('ZL','ZR','ZF','ZM','ZC')

write.csv(zscoredfile,'./tmp/zscoreddata.csv')

result = kmeans(zscoredfile,5)
type = result$cluster
table(type)
centervec = result$center

library(reshape)
library(ggplot2)

# 每一簇各指标的关系程度  --雷达图
# install.packages("fmsb")
library(fmsb)
max <- apply(result$centers, 2, max)
min <- apply(result$centers, 2, min)
data.radar <- data.frame(rbind(max, min, result$centers))
radarchart(data.radar, pty = 32, plty = 1, plwd = 2, vlcex = 0.7)
# 给雷达图加图例
L <- 1.2
for(i in 1:5){
  text(1.8, L, labels = paste("--class", i), col = i)
  L <- L - 0.2
}
# 查看各簇个数占比 --饼图
# install.packages("plotrix")
library(plotrix)
data.pie <- c(result$size)
prob <- paste(round(result$size / sum(result$size) * 100, 2), "%", sep = "")
lbls <- c(paste("class", 1:k, sep = "", ": ", prob))
pie3D(data.pie, labels = lbls, labelcex = 0.8, explode = 0.1,
      col = c("lightskyblue", "lightcyan", "turquoise",
              "lightskyblue3", "steelblue"))

