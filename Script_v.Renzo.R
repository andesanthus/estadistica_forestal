df<-datos_entidad_forestal_Eq1_Est20181110
View(df)
summary(df)

names(df)=c("Árbol", "DAP", "ABm", "HT", "Rect", "BFm")
df$Rect<-as.factor(df$Rect)
df$BFm<-as.factor(df$BFm)

RectA <-as.character(labels(df$Rect))
BFmA <-as.character(labels(df$BFm))

for(i in c(1:713)){
  if (df$Rect[i]==0) RectA[i]<-"Recto"
  if (df$Rect[i]==1) RectA[i]<-"Semi sinuoso"
  if (df$Rect[i]==2) RectA[i]<-"Sinuoso"
  if (df$Rect[i]==3) RectA[i]<-"Curvo"
}

for(i in c(1:713)){
  if (df$BFm[i]==0) BFmA[i]<-"Tablar"
  if (df$BFm[i]==1) BFmA[i]<-"Ligeramente Tablar"
  if (df$BFm[i]==2) BFmA[i]<-"Semicircular"
  if (df$BFm[i]==3) BFmA[i]<-"Circular"
}

df8 <- cbind(df[,1:5], RectA, df[,6:6], BFmA)
summary(df8)

dxy<- cbind(df[,2:4])
head(dxy)
pairs(dxy)

df8_clean <- rbind(df8[1:2,], df8[4:713,])
dxy<- cbind(df8_clean[,2:4])
head(dxy)
pairs(dxy)

df8<-df8_clean

plot(df8_clean$DAP)
plot(df8_clean$ABm)
plot(df8_clean$HT)
boxplot(df8_clean$ABm)
boxplot(df8_clean$HT)
boxplot(df8_clean$DAP)
boxplot(df8_clean$Rect)

plot(df8$DAP, df8$ABm)
plot(df8$DAP, df8$HT)

Rect<- as.numeric(df8$Rect)
bar_rect<-table(Rect)
View(bar_rect)

hist(df8$DAP, main = "Distribución de DAP", labels = TRUE)
hist(df8$HT, main = "Distribución de altura total", labels = TRUE)
hist(df8$ABm, main = "Distribución de área basal", labels = TRUE)
barplot(bar_rect, main = "Rectitud", col = "grey")
