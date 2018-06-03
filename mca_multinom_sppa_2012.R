library(dplyr)
library("FactoMineR")
library("factoextra")
library(nnet)

s = read.csv("lca_sppa_2012(1).csv") 
prob = read.table("lca_sppa_2012_classes.txt") 

sp = subset(s, select=c(2, 4:19))

sp = subset(s, select=c(2, 4:19))

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

unique(sp2[c("educnew")])
sp2$educ <- (sp2$educnew - 1)/2

unique(sp2[c("incomenew")])
sp2$income <- (sp2$incomenew - 1)/4

sp3 = sp2
#ml = sp2
for (i in 2:6) {
  sp3[[i]] = as.character(sp3[[i]])
}

sp3$class = as.factor(sp3$class)

#income
unique(sp3[c("incomenew")])
for(k in 5)
{
  sp3[k][sp3[k]==1]<- "Less than $30,000"
}

for(k in 5)
{
  sp3[k][sp3[k]==2]<- "30K - 59K"
}

for(k in 5)
{
  sp3[k][sp3[k]==3]<- "60K - 99K"
}

for(k in 5)
{
  sp3[k][sp3[k]==4]<- "100K - 149K"
}

for(k in 5)
{
  sp3[k][sp3[k]==5]<- "above 150K"
}

#education
unique(sp3[c("educnew")])

for(k in 4)
{
  sp3[k][sp3[k]==3]<- "Higher Education"
}

for(k in 4)
{
  sp3[k][sp3[k]==1]<- "High school degree or less"
}


for(k in 4)
{
  sp3[k][sp3[k]==2]<- "Secondary education"
}

#sex
for(k in 3)
{
  sp3[k][sp3[k]==2]<- "Female"
}

for(k in 3)
{
  sp3[k][sp3[k]==1]<- "Male"
}


#voting
for(k in 2)
{
  sp3[k][sp3[k]==1]<- "dem>60%"
}

for(k in 2)
{
  sp3[k][sp3[k]==2]<- "60%>dem>10%"
}

for(k in 2)
{
  sp3[k][sp3[k]==3]<- "dem<10%"
}


for(k in 2)
{
  sp3[k][sp3[k]==4]<- "rep<10%"
}

for(k in 2)
{
  sp3[k][sp3[k]==5]<- "60%>rep>10%"
}

for(k in 2)
{
  sp3[k][sp3[k]==6]<- "rep>60%"
}


#race
for(k in 6)
{
  sp3[k][sp3[k]==1]<- "white"
}

for(k in 6)
{
  sp3[k][sp3[k]==2]<- "black"
}

for(k in 6)
{
  sp3[k][sp3[k]==3]<- "Asian"
}

for(k in 6)
{
  sp3[k][sp3[k]==4]<- "Other race"
}

for (i in 1:8) {
  sp3[[i]] = as.factor(sp3[[i]])
}


#MCA
spmca = subset(sp3, select=c(1:6))
res.mca <- MCA(spmca, graph = FALSE)
eig.val <- get_eigenvalue(res.mca)
head(eig.val)


fviz_mca_var(res.mca, col.var = "cos2", alpha.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

dev.copy(device = png,filename="mcasppaplotcos.png", width = 1000, height = 500);
dev.off ();

fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)

#Multinomial regression

library(stargazer)
ml<-read_csv("sppa_for_multinom.csv")

colnames(ml)[5] <- "educ"
colnames(ml)[6] <- "income"
colnames(ml)[7] <- "race"
ml$race[ml$race == "White"]<- "AAwhite" #for the default reference


library(psych)

describeBy(ml, ml$voting)

for (i in 2:7) {
  ml[[i]] = as.factor(ml[[i]])
}

with(ml, table(voting, class, sex, race, educ, income))


ml$voting2 <- relevel(ml$voting, ref = "dem>60%")
table(ml$voting2)
ml$voting2[ml$voting2 == "rep>60%"]<- "60%>rep>10%" #not enough entires for multinomial regression
#factor(ml$voting2)
ml$voting2<- droplevels(ml$voting2)

test <- multinom(voting2 ~ class + race + sex + income + educ, data = ml)

stargazer(test, type = "text", dep.var.labels=c("60%>dem>10%","60%>rep> 10%", "dem<10%", "rep<10%"),
          covariate.labels=c("Univore","Omnivore","Asian", "Black","Other race", 
                             "Male", "30K - 59K", "60K - 99K", "100K - 149K", 
                             "above 150K", "Secondary education", "Higher education"))


z <- summary(test)$coefficients/summary(test)$standard.errors
z


# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

odds = exp(coef(test))
odds = t(odds)
odds  #vertical levels the same with levels of coefficients (line 216)

test1 <- multinom(voting2 ~ class + race+income+educ, data = ml) #without sex
head(pp <- fitted(test1))

dses <- data.frame(class = c("1", "2", "3"), income = "5", #income above 150k and higher education, white race
                   race = "AAwhite", educ = "3")
stargazer(predict(test1, newdata = dses, "probs"), type = "text")

dses <- data.frame(class = c("1", "2", "3"), income = "5", #income above 150k and higher education, black race
                   race = "Black", educ = "3")
stargazer(predict(test1, newdata = dses, "probs"), type = "text")

dses <- data.frame(class = c("1", "2", "3"), income = "5", #income above 150k and higher education, asian race
                   race = "Asian", educ = "3")
stargazer(predict(test1, newdata = dses, "probs"), type = "text")


dses <- data.frame(class = c("1", "2", "3"), income = "1", #income: less than $30000 and education: higher school or less, black race
                   race = "Black", educ = "1")
stargazer(predict(test1, newdata = dses, "probs"), type = "text")


dses <- data.frame(class = c("1", "2", "3"), income = "3", #income $60-$99 and secondary education, white race
                   race = "AAwhite", educ = "2")
stargazer(predict(test1, newdata = dses, "probs"), type = "text")


#mca for student's survey

p = read.csv("pennshort.csv")
for(k in 1:11)
{
  p[k][p[k]=="999"]<- NA
}

for(k in 1:8)
{
  p[k][p[k]==1]<- "Yes"
}

for(k in 1:8)
{
  p[k][p[k]==2]<- "No"
}

for(k in 9)
{
  p[k][p[k]==1]<- "Democratic"
}

for(k in 9)
{
  p[k][p[k]==2]<- "Republican"
}

for(k in 9)
{
  p[k][p[k]==3]<- "Libertarian party"
}

for(k in 9)
{
  p[k][p[k]==4]<- "Green Party"
}

for(k in 9)
{
  p[k][p[k]==5]<- "Independents"
}

unique(p[c("income")])
for(k in 10)
{
  p[k][p[k]==1]<- "Less than $30,000"
}

for(k in 10)
{
  p[k][p[k]==2]<- "30k - 59k"
}

for(k in 10)
{
  p[k][p[k]==3]<- "60k - 99k"
}

for(k in 10)
{
  p[k][p[k]==4]<- "100k - 149k"
}

for(k in 10)
{
  p[k][p[k]==5]<- "above 150k"
}


unique(p[c("educ")])

for(k in 11)
{
  p[k][p[k]==3]<- "Higher Education"
}

for(k in 11)
{
  p[k][p[k]==1]<- "Higher School or less"
}


for(k in 11)
{
  p[k][p[k]==2]<- "Secondary Education"
}

#p = apply(p, 2, function(x) nlevels(as.factor(x)))

p = na.omit(p)

p = p[p$party!="Libertarian party",]

p = subset(p, select = -1)
p = subset(p, select = -6)


is.factor(p)
for (i in 1:9) {
  p[[i]] = as.factor(p[[i]])
}

p.active <- p[, 1:9]
head(p.active[, 1:9], 3)

summary(p.active)[, 1:9]


for (i in 1:9) {
  plot(p.active[,i], main=colnames(p.active)[i],
       ylab = "Count", col="steelblue", las = 2)
}

res.mca <- MCA(p.active, graph = FALSE)
print(res.mca)

summary(res.mca)

eig.val <- get_eigenvalue(res.mca)
head(eig.val)

fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))

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
dev.off()

