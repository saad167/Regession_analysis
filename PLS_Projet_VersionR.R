library(data.table)
library(Hmisc)
library(caTools)


setwd("C:/Users/Original Shop/Documents/Disque local (D)/INSEASCHOOL/3éme_anné/regression_grande_dimension/project_R")


bioblanc <- fread(file.path("dataset.csv"))
head(bioblanc)

#Division des données en train_set et test_set
train_set<-bioblanc[seq(1,24)]
test_set<-bioblanc[seq(25,30)]
# Regression multiple Y=f(X1..X7)
lm<-lm(formula = Y ~ 0 + X1 + X2+ X3 + X4 + X5 +X6 + X7 + X8, data=train_set)
#print 
print(lm)
#summary
print(summary(lm))

#Noramlisation des données

DM_Matrix<-as.matrix(train_set)

#Matrice de corrélation

rcorr(DM_Matrix, type=c("pearson","spearman"))

Scale_DM<-scale(train_set)
head(Scale_DM)
colnames(Scale_DM)<- c("Y_cn","X1_cn","X2_cn","X3_cn","X4_cn","X5_cn","X6_cn","X7_cn","X8_cn")
DT_scale<- cbind(train_set,Scale_DM)
head(DT_scale)
#################Construction de T1 ###########################
DT_scale<-DT_scale[,':='(T1=(1/sqrt(0.43^2+0.69^2+0.87^2+0.89^2+0.89^2+0.89^2+0.88^2+0.89^2)) 
                         *((-0.43*X1_cn)+(0.69* X2_cn)+(0.87*X3_cn)+(0.89* X4_cn)+(0.89* X5_cn)+(0.89* X6_cn)+(0.88* X7_cn)+(0.89* X8_cn)))]
#################Construction de T2 ###########################

# Régression de y sur T1 et Xj j=1..8
#pour chercher les variables contribuant de manière significative
# à la construction de T2

lm11<-lm(formula = Y_cn ~ 0 + T1 + X1_cn, data=DT_scale) #Non
print(summary(lm11))

lm12<-lm(formula = Y_cn ~ 0 + T1 + X2_cn, data=DT_scale) #Non
print(summary(lm12))
lm13<-lm(formula = Y_cn ~ 0 + T1 + X3_cn, data=DT_scale) #Non
print(summary(lm13))
lm14<-lm(formula = Y_cn ~ 0 + T1 + X4_cn, data=DT_scale) #Non
print(summary(lm14))
lm15<-lm(formula = Y_cn ~ 0 + T1 + X5_cn, data=DT_scale) #Non
print(summary(lm15))
lm16<-lm(formula = Y_cn ~ 0 + T1 + X6_cn, data=DT_scale) #Non
print(summary(lm16))
lm17<-lm(formula = Y_cn ~ 0 + T1 + X7_cn, data=DT_scale) #Oui
print(summary(lm17))
lm18<-lm(formula = Y_cn ~ 0 + T1 + X8_cn, data=DT_scale) #Non
print(summary(lm18))

# Seule la variable X2 est significative au risque de 5%

# On calcule le résidus X12 de la régression de X7_nc sur T1 

lm_R12<-lm(formula = X2_cn ~ 0 + T1  , data=DT_scale)
print(lm_R12)

#### Extraction des résidus

X12<-resid(lm_R12)

#### Extraction des résidus

DT_scale<- cbind(DT_scale,X12)
X12n<-X12/var(X12)
DT_scale<- cbind(DT_scale,X12n)

# Puis on effectue la régression multiple 
# Y_cn sur T1 et X17n = x17/var(x17)

lm_Y12<-lm(formula = Y_cn ~ 0 + T1 + X12n  , data=DT_scale)
print(summary(lm_Y12))

#DT_scale<-DT_scale[,':='(T2=(-0.04744*X17)/0.04744)]

T2=( -0.08360*X12)/ 0.08360
DT_scale<-cbind(DT_scale,T2)
DT_scale$T2

#################Construction de T3 ###########################

# Régressions de y sur T1, T2 et Xj j=1..7
#pour chercher les variables contribuant de manière significative
# à la construction de T3

lm21<-lm(formula = Y_cn ~ 0 + T1 + T2 + X1_cn, data=DT_scale) #Non
print(summary(lm21))
lm22<-lm(formula = Y_cn ~ 0 + T1 + T2 + X2_cn, data=DT_scale) #Non
print(summary(lm22))
lm23<-lm(formula = Y_cn ~ 0 + T1 + T2 + X3_cn, data=DT_scale) #Non
print(summary(lm23))
lm24<-lm(formula = Y_cn ~ 0 + T1 + T2 + X4_cn, data=DT_scale) #Non
print(summary(lm24))
lm25<-lm(formula = Y_cn ~ 0 + T1 + T2 + X5_cn, data=DT_scale) #Non
print(summary(lm25))
lm26<-lm(formula = Y_cn ~ 0 + T1 + T2 + X6_cn, data=DT_scale) #Non
print(summary(lm26))
lm27<-lm(formula = Y_cn ~ 0 + T1 + T2 + X7_cn, data=DT_scale) #Non
print(summary(lm27))
lm28<-lm(formula = Y_cn ~ 0 + T1 + T2 + X8_cn, data=DT_scale) #Non
print(summary(lm28))

# Aucune des variables X1...X8 n'est significative au risque de 5%
# Il faut retenir que les deux composantes PLS T1 T2

##########Construction de l'équation de régression PLS #########
##########           à deux composantes               #########

# Regressions Y sur T1, T2

lm_PLS<-lm(formula = Y ~   T1 + T2, data=DT_scale)
print(summary(lm_PLS))
attach(test_set)
Y_pred = 8.33708 + 0.25575 * ( -0.185717*X1 + 0.298011*X2 + 0.375753*X3 + 0.384391*X4 +
                                0.384391*X5 + 0.384391*X6 + 0.380072*X7 + 0.384391*X8) + 0.18943*(-X2)

Y_pred = 8.33708 - 0.04749712*X1 - 0.1132137*X2 + 0.09609883*X3 + 0.098308*X4 + 0.098308*X4 +0.098308*X4 
#plot(test_set$Y,Y_pred)
#abline(a=0,b=1)
xdata <- c(1,2,3,4,5,6)
plot(xdata, test_set$Y, type="o", col="blue", pch="o", lty=1, ylim=c(7,10) )
# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(xdata, Y_pred, col="red", pch="*")
lines(xdata, Y_pred, col="red",lty=2)

#Calcul de l'erreur quadratique moyenne RMSE

attach(train_set)
fitted=6.44908162853694-0.0772729118518608*X1+0.0549065267121064*X2+0.0243873350790104*X3+0.0704180603087845*X4+0.0264429625815639*X5+0.000198714605766071*X6-0.00146358017613533*X7+0.000381584623550140*X8
attach(test_set)
prediction=6.44908162853694-0.0772729118518608*X1+0.0549065267121064*X2+0.0243873350790104*X3+0.0704180603087845*X4+0.0264429625815639*X5+0.000198714605766071*X6-0.00146358017613533*X7+0.000381584623550140*X8

RMSE_pls_fitted=sqrt(mean(train_set$Y-fitted)^2)
RMSE_pls_predict=sqrt(mean(test_set$Y-prediction)^2)

library(mctest)
library(car)
imcdiag(lm_PLS,all = TRUE)
vif(lm_PLS)

# PLS automatique

library(plsdepot)
train_set=train_set[,c(2:9,1)]
test_set=test_set[,c(2:9,1)]
head(train_set)
# Ceci confirme le choix du nombre de composantes en haut (nbre=2)

modele=plsreg1(train_set[,c(1:8)],train_set[,c(9)],crosval = TRUE)
print(modele$Q2)
print(modele$R2)

plot(train_set$Y,modele$y.pred,type='n',xlab='Original',ylab='Predicted')
abline(a=0,b=1)
text(train_set$Y,modele$y.pred,col = "blue")

### Regréssion pénalisée
#Ridge

library(MASS)

#Mise en place de la régression ridge avec 99 valeur possible entre 0.1 à 10

model_ridge=lm.ridge(Y~., data=train_set,lambda = seq(0.1,10,0.1))
print(model_ridge)

#Repérage de lambda permettant de minimiser le cross validation GCV

plot(seq(0.1,10,0.1),model_ridge$GCV,xlab = "lambda",ylab = "GCV")
model_ridge$lambda[which.min(model_ridge$GCV)]
help(model_ridge$GCV)
model_ridge=lm.ridge(Y~., data=train_set,lambda = 4.5)
print(model_ridge)

#Construction de la méthode de prédiction absente dans le package MASS

attach(train_set)
fitted=9.273352-0.1137110*X1+2.575409*10^(-2)*X2+1.936221*10^(-2)*X3+4.852966*10^(-2)*X4+1.906539*10^(-2)*X5+1.774839*10^(-4)*X6+6.428905*10^(-5)*X7+02.684253*10^(-4)*X8

attach(test_set)
prediction=9.273352-0.1137110*X1+2.575409*10^(-2)*X2+1.936221*10^(-2)*X3+4.852966*10^(-2)*X4+1.906539*10^(-2)*X5+1.774839*10^(-4)*X6+6.428905*10^(-5)*X7+02.684253*10^(-4)*X8
#Ajustement du modèle

plot(my_ridge,train_set$Y)
abline(a=0,b=1)

# L'erreur commise RMSE

RMSE_ridge_fitted=sqrt(mean(train_set$Y-fitted)^2)
RMSE_ridge_predict=sqrt(mean(test_set$Y-prediction)^2)

### La regréssion LASSO
### LASSO étant une méthode de selection de variable permettant d'éliminer certaines
# variables en leur affectant un poids nul. Elle permet ainsi de ne retenir que quelques
# variables minimisant l'erreur d'ajustement.

library("lars")
model_lasso=lars(as.matrix(train_set[,1:8]),train_set$Y,type="lasso",
                 trace=F,normalize=TRUE)
plot(model_lasso,xvar = 'df', plottype = 'coeff')
print(model_lasso$beta)

plot(model_lasso$df,summary(model_lasso)$Rss,
     xlab='Df',ylab='Rss',main='LASSO')

#Cross-validation pour déterminer le coefficient optimal

cv=cv.lars(as.matrix(train_set[,1:8]),train_set$Y,K=10)

print(model_lasso$lambda[12])
print(model_lasso$beta[12,])

#Modèle final après élimination de certaines variables conservées par LASSO par 
# manque de significativité

model_lasso_final=lm(Y~X5+X6, data=train_set)
summary(model_lasso_final)

attach(train_set)
fitted=6.09+5.514*10**(-2)*X5+4.674*10**(-4)*X6
attach(test_set)
prediction=6.09+5.514*10**(-2)*X5+4.674*10**(-4)*X6
plot(fitted,train_set$Y)
abline(a=0,b=1)

#Calcul des erreurs RMSE

RMSE_lasso_fitted=sqrt(mean((fitted-train_set$Y)^2))
RMSE_lasso_predict=sqrt(mean((prediction-test_set$Y)^2))

# Des statistiques sur la performance des modèles

compare_tools=cbind(RMSE_ridge_fitted,RMSE_lasso_fitted,RMSE_pls_fitted,RMSE_ridge_predict,RMSE_lasso_predict,RMSE_pls_predict)
compare_tools