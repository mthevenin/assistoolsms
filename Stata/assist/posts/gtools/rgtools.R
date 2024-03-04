
library("data.table")
library("gtools")
library("tictoc")
library("dplyr")

#tic()
#gtools <- read.csv("D:/D/stata_temp/gtools.csv")
#toc()

tic()
gtools <- fread("D:/D/stata_temp/gtools.csv")
toc()



tic()
gtools$gy1  =  quantcut(gtools$y1,10) 
gtools$gy2  =  quantcut(gtools$y2,10) 
gtools$gy3  =  quantcut(gtools$y3,10) 
gtools$gy4  =  quantcut(gtools$y4,10) 
gtools$gy5  =  quantcut(gtools$y5,10) 
gtools$gy6  =  quantcut(gtools$y6,10) 
gtools$gy7  =  quantcut(gtools$y7,10) 
gtools$gy8  =  quantcut(gtools$y8,10) 
gtools$gy9  =  quantcut(gtools$y9,10) 
gtools$gy10 =  quantcut(gtools$y10,10) 
toc()

tic()
gtools <- fread("D:/D/stata_temp/gtools.csv")
toc()

tic()
gtool  =  gtools %>% mutate(gy1 = ntile(y1, 10))
gtool  =  gtools %>% mutate(gy2 = ntile(y2, 10))
gtool  =  gtools %>% mutate(gy3 = ntile(y3, 10))
gtool  =  gtools %>% mutate(gy4 = ntile(y4, 10))
gtool  =  gtools %>% mutate(gy5 = ntile(y5, 10))
gtool  =  gtools %>% mutate(gy6 = ntile(y6, 10))
gtool  =  gtools %>% mutate(gy7 = ntile(y7, 10))
gtool  =  gtools %>% mutate(gy8 = ntile(y8, 10))
gtool  =  gtools %>% mutate(gy9 = ntile(y9, 10))
gtool  =  gtools %>% mutate(gy10 = ntile(y10, 10))
toc()



####### RESHAPE

gtools <- fread("D:/D/stata_temp/gtools.csv")

tic()
long = reshape(gtools, idvar = "id", timevar="j", varying = list(2:11),
       v.names = "y", direction = "long")
long = arrange(long, id, j)
toc()


tic()
wide = reshape(long, idvar = "id",  timevar="j",
               v.names = "y", sep = "", direction = "wide")
toc()



library(dplyr)
library(tidyr)

gtools <- fread("D:/D/stata_temp/gtools.csv")

tic()
long = pivot_longer(gtools, cols = starts_with("y")) 
toc()
tic()
wide = pivot_wider(long, names_from = c("name"), values_from = c("value"))
toc()

### Collapse

gtools <- fread("D:/D/stata_temp/gtools.csv")
tic()
collapse= gtools %>%  group_by(g) %>%   summarise(across(y1:y10, ~ mean(.x, na.rm = TRUE)))
toc()


gtools <- fread("D:/D/stata_temp/gtools.csv")

var <- c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9", "y10")

tic()
gtools = gtools %>% group_by(g) %>% mutate(across(var, mean, .names = "m{col}"))
toc()






