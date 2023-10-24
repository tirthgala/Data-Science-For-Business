#Data Science for Business: Team Case 1
#Team 18:Yinan Chen,Caryl Alexis Cohen,Tirth Pravin Gala,Yuhe Jin,Chenjie (Angelina) Sun

# Cleaning up of Data
source("DataAnalyticsFunctions.R")
load("natalityNew.Rda")
summary(d)
drops <- c("id","birmon","tri.none","novisit")
names(d) %in% drops
!( names(d) %in% drops )
DATA <- d[,!(names(d) %in% drops)]
summary(DATA)
#Organizational help for Question 2 
MatrixComp <- as.matrix( cbind( DATA$boy, DATA$tri1, DATA$tri2, DATA$tri3, DATA$black, DATA$married, DATA$ed.hs, DATA$ed.smcol, DATA$ed.col, DATA$smoke ))  
LabelsTmp <- c( "boy", "tri1", "tri2", "tri3", "black", "married", "ed.hs", "ed.smcol", "ed.col","smoke")
NumCol <- ncol(MatrixComp)
pvals <- rep(0, NumCol*(NumCol-1)/2) 
ListLabels <- rep("", NumCol*(NumCol-1)/2) 
k <- 0
for (i in 1:(NumCol-1) ){
  for ( j in (i+1):NumCol ){
    k <- k+1
    m00 <- sum( (MatrixComp[,i] == 0) & (MatrixComp[,j] == 0) ) 
    m01 <- sum( (MatrixComp[,i] == 0) & (MatrixComp[,j] == 1) ) 
    m10 <- sum( (MatrixComp[,i] == 1) & (MatrixComp[,j] == 0) ) 
    m11 <- sum( (MatrixComp[,i] == 1) & (MatrixComp[,j] == 1) ) 
    ContingencyMatrix <- as.table(rbind(c(m00, m01), c(m10, m11)))
    pvals[k] <- chisq.test(ContingencyMatrix)$p.value  
    ListLabels[k] <- paste(LabelsTmp[i],LabelsTmp[j], sep=" and ")  
  }  
}

#Question 1 
library(ggplot2)
ggplot(DATA, aes(x=cigsper, y=weight))+geom_point()+geom_smooth(formula = y ~ x, method="lm", se=FALSE)+labs(
  x = "Cigarretes per day by the mother",
  y ="Infant birthweight (kg)",
  title = "Does the amount of smoking affect infant weight?")

#Question 2 
for (i in 1:45){if(pvals[i] > 0.05){print(paste(ListLabels[i]," p-values=",pvals[i]))}}
for (i in 1:45){if(pvals[i] > 0.00111){print(paste(ListLabels[i]," p-values=",pvals[i]))}}

#Question 3 
library(corrplot)
correlation_matrix<-cor(DATA)
View(correlation_matrix)
corrplot(correlation_matrix, method='color', order='alphabet', type='lower', tl.col='black', cl.ratio=0.2, tl.srt=45)

#Question 4 
multireg<-lm(weight~black+married+boy+tri1+tri2+tri3+ed.hs+ed.smcol+ed.col+mom.age+smoke+cigsper+m.wtgain+mom.age2,DATA)
summary(multireg)

