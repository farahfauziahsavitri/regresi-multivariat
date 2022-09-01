#import data from excel
data=multifix
data
attach(data)
names(data)
y1<-data$`produktivitas padi`
y2<-data$`konsumsi beras`
z1<-data$`kec angin`
z2<-data$kelembapan
z3<-data$`Curah hujan`
z4<-data$`Hari hujan`
z5<-data$`Tekanan Udara`
z6<-data$`penyinaran matahari`
z7<-data$`Luas sawah non irigasi`
z8<-data$`Luas sawah irigasi`

#korelasi antar variabel respon
cor(y1,y2)

#model regresi dengan package
library(car)
model <- lm(cbind(y1, y2) ~ z1+z5+z8, data =data)
summary(model)

#model regresi manual
#matrix X
z0 <- matrix(1,nrow=34)
X <- cbind(z0,z1,z5,z8)
#matrix Y
Y <-cbind(y1,y2)
#transpos matrix X
XT = t(X)
#DIPEROLEH OUTPUT
#matrix XTX
XTX=XT%*%X
#matrix (XTX)^1
invXTX=solve(XTX)
#matrix XTY
XTY=XT%*%Y
#koefisien Beta
Beta=invXTX%*%XTY
Beta


#UJI SIGNIFIKANSI PARAMETER (SECARA SIMULTAN)
#HIPOTESIS
#H0: Beta(ij) = 0; secara simultan tidak terdapat parameter yang signifikan terhadap model
#H1: Beta(ij) TIDAK = 0; secara simultan terdapat parameter yang signifikan terhadap model
#TARAF SIGNIFIKANSI =5%
#STATISTIK UJI: DET(E)/DET(E+H)

#DIKETAHUI BAHWA
#MATRIKS X
X
#MATRIKS Y
Y
#MATRIKSYBAR
YBAR=colMeans(Y)
YBAR=matrix(c(YBAR),ncol=1,nrow=2)
#MATRIKS BETA
Beta
#ukuran sampel
n=34
#PERHITUNGAN
E=t(Y)%*%Y-t(Beta)%*%(t(X)%*%Y)
detE=det(E)
EH=(t(Y)%*%Y)-(n*(YBAR%*%t(YBAR)))
detEH=det(EH)
WilksLambda=detE/detEH
WilksLambda

#Kriteria Uji
#Tolak H0 jika wilks hitung <= wilks tabel alpha,m(df1,df2
#dengan df1=r, df2=n-r-1
#sehingga H0 DITOLAK artinya secara simultan terdapat parameter yang signifikan terhadap model

## UJI SUBSET##
#Beta transpose
BT=t(Beta)

######MENGUJI B2 B3#####
#Betar
#matriks yang berisi konstanta beta selain Z5 dan Z8
Betar=matrix(c(-21.55942,-4.7091,50511.02,-90.6234),2,2)
#Betar transpose
BRT=t(Betar)
#Xr
Xr<- cbind(z0,z1)
#xr transpose
XRT=t(Xr)
H=BT%*%XT%*%Y-BRT%*%XRT%*%Y
E = YT%*%Y-BT%*%XT%*%Y
EH=H+E
dE=det(E)
dH=det(H)
dEH=det(EH)
wilkslamda=dE/dEH
wilkslamda

######MENGUJI B0 B1#####
#matriks yang berisi konstanta beta selain Intercept dan Z1
#Betar
Betar=matrix(c(0.07469939,0.00003246546,-49.59312,0.004431198),2,2)
#Betar transpose
BRT=t(Betar)
#Xr
Xr<- cbind(z5,z8)
#xr transpose
XRT=t(Xr)
H=BT%*%XT%*%Y-BRT%*%XRT%*%Y
E = YT%*%Y-BT%*%XT%*%Y
EH=H+E
dE=det(E)
dH=det(H)
dEH=det(EH)
wilkslamda=dE/dEH
wilkslamda


#ANALISIS RESIDU
prediksi=predict(model)
prediksi

Y1=as.matrix(y1)
Y2=as.matrix(y2)
Ya=c(Y1,Y2)
residu=Ya-prediksi
residu = data.frame(residu)
residu

#Uji Asumsi Regresi Multivariat
#Normalitas menggunakan mardia skewness dan kurtosis
library(MVN)
hasil<-mvn(data=residu, mvnTest="mardia")
hasil

#Homogenitas varians
ry1=residu$y1
ry2=residu$y2
s1=var(ry1)
s2=var(ry2)
v1=33
v2=33
lns1=log(s1)
lns2=log(s2)
##v1=v2=33
spl=(v1*s1+v2*s2)/33
lnspl=log(spl)
lnM=(1/2*(v1*lns1+v2*lns2))-(1/2*(v1+v2)*lnspl)
boxmhitung=(-2)*lnM
boxmhitung

#independensi residual
P=cor(ry1,ry2)
chihitung=-(33-(9/6))*log(P)
chihitung