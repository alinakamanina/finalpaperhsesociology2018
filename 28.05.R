#mca with russian dots

s = read.csv("D:/4 курс/diploma/mplus/sppalastdonttouch.csv")
prob = read.table("D:/4 курс/diploma/mplus/Aasend/sppa_save.txt")

sp = subset(s, select=c(2, 4:19))


#p[, 1:11][is.na(p[, 1:11])] <- 999

for(k in 8:17)
{
  sp[k][sp[k]=="999"]<- NA
}
col_to_check = c(8:17)

## find rows with NAs only 
tmp = sp[,col_to_check]
tmp = is.na(tmp)
tmp = sapply(1:nrow(tmp), function(x){any(!tmp[x,])})

# delete
sp = sp[tmp,]


prob = subset(prob, select=15)
sp1 = cbind(sp, prob)

colnames(sp1)[18] <- "class"
sp1=sp1[,c(18, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, 16, 17)]

sp2 = subset(sp1, select = c(1:2, 4, 6:8))

for (i in 1:6) {
  sp2[[i]] = as.factor(sp2[[i]])
}

unique(sp2[c("incomenew")])
for(k in 5)
{
  sp2[k][sp2[k]==1]<- "Меньше $30,000"
}

for(k in 5)
{
  sp2[k][sp2[k]==2]<- "30K - 59K"
}

for(k in 5)
{
  sp2[k][sp2[k]==3]<- "60K - 99K"
}

for(k in 5)
{
  sp2[k][sp2[k]==4]<- "100K - 149K"
}

for(k in 5)
{
  sp2[k][sp2[k]==5]<- "больше 150K"
}


unique(sp2[c("educnew")])

for(k in 4)
{
  sp2[k][sp2[k]==3]<- "Высшее образование"
}

for(k in 4)
{
  sp2[k][sp2[k]==1]<- "Старшая школа или меньше"
}


for(k in 4)
{
  sp2[k][sp2[k]==2]<- "Среднее образование"
}

for(k in 3)
{
  sp2[k][sp2[k]==2]<- "Женщина"
}

for(k in 3)
{
  sp2[k][sp2[k]==1]<- "Мужчина"
}



for(k in 2)
{
  sp2[k][sp2[k]==1]<- "демократы>60%"
}

for(k in 2)
{
  sp2[k][sp2[k]==2]<- "60%>демократы>10%"
}

for(k in 2)
{
  sp2[k][sp2[k]==3]<- "демократы<10%"
}


for(k in 2)
{
  sp2[k][sp2[k]==4]<- "республиканцы<10%"
}

for(k in 2)
{
  sp2[k][sp2[k]==5]<- "60%>республиканцы>10%"
}

for(k in 2)
{
  sp2[k][sp2[k]==6]<- "республиканцы>60%"
}


#race
for(k in 6)
{
  sp2[k][sp2[k]==1]<- "Белые"
}

for(k in 6)
{
  sp2[k][sp2[k]==2]<- "Афроамериканцы"
}

for(k in 6)
{
  sp2[k][sp2[k]==3]<- "Азиаты"
}

for(k in 6)
{
  sp2[k][sp2[k]==4]<- "Другие расы"
}

for(k in 1)
{
  sp2[k][sp2[k]==1]<- "Неактивные"
}

for(k in 1)
{
  sp2[k][sp2[k]==2]<- "Юниворы"
}

for(k in 1)
{
  sp2[k][sp2[k]==3]<- "Омниворы"
}

for (i in 1:6) {
  sp2[[i]] = as.factor(sp2[[i]])
}

library("FactoMineR")
library("factoextra")
res.mca <- MCA(sp2, graph = FALSE)
eig.val <- get_eigenvalue(res.mca)
head(eig.val)


fviz_mca_var(res.mca, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), col.var = "cos2",
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
#dev.off()
#dev.print(pdf, 'filename.pdf')

dev.copy(device = png,filename="mcasppaplotcos.png", width = 1000, height = 500);
dev.off ();

#now graphs for sppa descrip stat

table(sp2$voting)
table(sp2$sex)
table(sp2$educnew)
table(sp2$incomenew)
table(sp2$race)

p = read.csv("D:/4 курс/diploma/mplus/pmplus.csv")
for(k in 1:11)
{
  p[k][p[k]=="999"]<- NA
}

for(k in 1:8)
{
  p[k][p[k]==1]<- "Да"
}

for(k in 1:8)
{
  p[k][p[k]==2]<- "Нет"
}

for(k in 9)
{
  p[k][p[k]==1]<- "Демократы"
}

for(k in 9)
{
  p[k][p[k]==2]<- "Республиканцы"
}

for(k in 9)
{
  p[k][p[k]==3]<- "Либертарианская партия"
}

for(k in 9)
{
  p[k][p[k]==4]<- "Партия зеленых"
}

for(k in 9)
{
  p[k][p[k]==5]<- "Независимые"
}

unique(p[c("income")])
for(k in 10)
{
  p[k][p[k]==1]<- "Меньше $30,000"
}

for(k in 10)
{
  p[k][p[k]==2]<- "30K - 59K"
}

for(k in 10)
{
  p[k][p[k]==3]<- "60K - 99K"
}

for(k in 10)
{
  p[k][p[k]==4]<- "100K - 149K"
}

for(k in 10)
{
  p[k][p[k]==5]<- "Больше 150K"
}


unique(p[c("educ")])

for(k in 11)
{
  p[k][p[k]==3]<- "Высшее образование"
}

for(k in 11)
{
  p[k][p[k]==1]<- "Старшая школа или меньше"
}


for(k in 11)
{
  p[k][p[k]==2]<- "Среднее образование"
}



#p = apply(p, 2, function(x) nlevels(as.factor(x)))
p



p = na.omit(p)

p = p[p$party!="Либертарианская партия",]

p = subset(p, select = -1)
p = subset(p, select = -6)
library("FactoMineR")
library("factoextra")

is.factor(p)
for (i in 1:9) {
  p[[i]] = as.factor(p[[i]])
}

colnames(p)[1] <- "Классическая музыка"
colnames(p)[2] <- "Живая музыка"
colnames(p)[3] <- "Театр"
colnames(p)[4] <- "Музеи"
colnames(p)[5] <- "Книги"
colnames(p)[6] <- "Спорт"



p.active <- p[, 1:9]
head(p.active[, 1:9], 3)

# Summary of the 4 first variables
summary(p.active)[, 1:9]

#for (i in 1:8) {
#  p.active[[i]] = as.logical(p.active[[i]])
#}

for (i in 1:9) {
  plot(p.active[,i], main=colnames(p.active)[i],
       ylab = "Count", col="steelblue", las = 2)
}

res.mca <- MCA(p.active, graph = FALSE)
#res.mca3 <- MCA(p.active, graph = FALSE, ncp = 3)
print(res.mca)

summary(res.mca)

eig.val <- get_eigenvalue(res.mca)
head(eig.val)

fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))

fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

var <- get_mca_var(res.mca)
var


fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(res.mca, col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

dev.copy(device = png,filename="mcapennplotcos.png", width = 1000, height = 500);
dev.off ();
