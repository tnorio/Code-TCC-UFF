####PARA ESTE SCRIPT FUNCIONAR E NECESSARIO QUE A TABELA ESTAJA EM FORMATO UTF-8 SEPARADA POR ";"(DE PREFERENCIA .CSV).
library(rgdal)
library(raster)
library(tibble)
library(dplyr)
setwd('//dellt410/dados/PUBLICO/PROJETO LAGEF/TANGERINA')  ###add  o local da pasta #trocar: \ por: /
dir()

tab<-read.table('TANGERINA HEC89-2016 TRUE.csv', header=TRUE, sep = ";", na.strings = "-", encoding= "UTF-8" ) ## nome da tabela com cabeçalho.csv

head(tab)


#Ton
###CONVERSAO IBGE  mil frutas / peso ativar somente quando utilziar dataset de produção
##tab<-column_to_rownames(tab, var="MUN")
#quaseton<-tab[,2:13] / 6.25### divido pela constante de frutos/kg ibge
#quaseton<-cbind(MUN=tab[,1],quaseton)
#quaseton2<-tab[,14:29]
#tab<-cbind(quaseton,quaseton2)

###junçao municipal
#comendador levy gasparian com areal pra formar Três rios
mat = as.matrix(tab[, 2:ncol(tab)])
row.names(mat) = tab$MUN
mat = rbind(mat, colSums(mat[c("Comendador Levy Gasparian (RJ)", "Areal (RJ)"), ], na.rm = T))
row.names(mat)[nrow(mat)] = "Tres Rios*"
mat = rbind(mat, colSums(mat[c("Tres Rios*", "Três Rios (RJ)"), ], na.rm = T))
a=row.names(mat)[nrow(mat)] = "Três Rios"

#aperibÃ© com S. antonio de padua
mat = rbind(mat, colSums(mat[c("Aperibé (RJ)", "Santo Antônio de Pádua (RJ)"), ], na.rm = T))
b=row.names(mat)[nrow(mat)] = "Santo Antônio de Pádua"

#buzios+cabo frio
mat = rbind(mat, colSums(mat[c("Armação dos Búzios (RJ)", "Cabo Frio (RJ)"), ], na.rm = T))
c=row.names(mat)[nrow(mat)] = "Cabo Frio"

###########belford roxo+japeri+mesquita+queimados+Nova IguaÃ§u############
##BF.Roxo+japeri=N.I1
mat = rbind(mat, colSums(mat[c("Belford Roxo (RJ)", "Japeri (RJ)"), ], na.rm = T))
row.names(mat)[nrow(mat)] = "N.I1"
##N.I1+mesquita=N.I2
mat = rbind(mat, colSums(mat[c("N.I1", "Mesquita (RJ)"), ], na.rm = T))
row.names(mat)[nrow(mat)] = "N.I2"
##N.I2+queimados=N.I3
mat = rbind(mat, colSums(mat[c("N.I2", "Queimados (RJ)"), ], na.rm = T))
row.names(mat)[nrow(mat)] = "N.I3"
##N.I3+Nova IguaÃ§u (RJ)=Nova IguaÃ§u** (R.FINAL)
mat = rbind(mat, colSums(mat[c("N.I3", "Nova Iguaçu (RJ)"), ], na.rm = T))
d=row.names(mat)[nrow(mat)] = "Nova Iguacu"

#macae+carapebus+quissama

mat = rbind(mat, colSums(mat[c("Macaé (RJ)", "Carapebus (RJ)"), ], na.rm = T))
e=row.names(mat)[nrow(mat)] = "Macae*"
mat = rbind(mat, colSums(mat[c("Macae*", "Quissamã (RJ)"), ], na.rm = T))
e=row.names(mat)[nrow(mat)] = "Macae"

#Cardoso M + Italva +quissama+ Campos G
mat = rbind(mat, colSums(mat[c("Cardoso Moreira (RJ)", "Italva (RJ)"), ], na.rm = T))
row.names(mat)[nrow(mat)] = "C.G1"
mat = rbind(mat, colSums(mat[c("Campos dos Goytacazes (RJ)", "C.G1"), ], na.rm = T))
f=row.names(mat)[nrow(mat)] = "Campos dos Goytacazes"

#itatiaia+Porto real = resende
mat = rbind(mat, colSums(mat[c("Itatiaia (RJ)", "Porto Real (RJ)"), ], na.rm = T))
row.names(mat)[nrow(mat)] = "R1"
mat = rbind(mat, colSums(mat[c("Resende (RJ)", "R1"), ], na.rm = T))
g=row.names(mat)[nrow(mat)] = "Resende"

##macuco+cordeiro
mat = rbind(mat, colSums(mat[c("Cordeiro (RJ)","Macuco (RJ)"), ], na.rm = T))
h=row.names(mat)[nrow(mat)]="Cordeiro"

##paty +  vassouras
mat = rbind(mat, colSums(mat[c("Paty do Alferes (RJ)","Vassouras (RJ)"), ], na.rm = T))
i=row.names(mat)[nrow(mat)]="Vassouras"

#pinheiral+pirai
mat = rbind(mat, colSums(mat[c("Pinheiral (RJ)","Piraí (RJ)"), ], na.rm = T))
j=row.names(mat)[nrow(mat)]="Piraí"

#quatis+barra mansa
mat = rbind(mat, colSums(mat[c("Quatis (RJ)","Barra Mansa (RJ)"), ], na.rm = T))
k=row.names(mat)[nrow(mat)]="Barra Mansa"

#rio da ostras+casimiro
mat = rbind(mat, colSums(mat[c("Casimiro de Abreu (RJ)","Rio das Ostras (RJ)"), ], na.rm = T))
l=row.names(mat)[nrow(mat)]="Casimiro de Abreu"

#S.F.I+S.J da barra
mat = rbind(mat, colSums(mat[c("São Francisco de Itabapoana (RJ)","São João da Barra (RJ)"), ], na.rm = T))
m=row.names(mat)[nrow(mat)]="São João da Barra"

#sao jose de uba+9cambuci
mat = rbind(mat, colSums(mat[c("Cambuci (RJ)", "São José de Ubá (RJ)"), ], na.rm = T))
n=row.names(mat)[nrow(mat)] = "Cambuci"


#tangua+itaborai
mat = rbind(mat, colSums(mat[c("Tanguá (RJ)", "Itaboraí (RJ)"), ], na.rm = T))
p=row.names(mat)[nrow(mat)] = "Itaborai"

#varre-sai+natividade
mat = rbind(mat, colSums(mat[c("Varre-Sai (RJ)", "Natividade (RJ)"), ], na.rm = T))
q=row.names(mat)[nrow(mat)] = "Natividade"

############################################
## DELETE EXTRA ROWS
row.names.remove<-c("Natividade (RJ)","Varre-Sai (RJ)","Tanguá (RJ)","Itaboraí (RJ)",
                    "Cambuci (RJ)","São José de Ubá (RJ)","São Francisco de Itabapoana (RJ)",
                    "São João da Barra (RJ)","Casimiro de Abreu (RJ)","Rio das Ostras (RJ)",
                    "Quatis (RJ)", "Barra Mansa (RJ)","Pinheiral (RJ)","Piraí (RJ)",
                    "Paty do Alferes (RJ)", "Vassouras (RJ)", "Cordeiro (RJ)","Macuco (RJ)",
                    "Comendador Levy Gasparian (RJ)","Areal (RJ)","Três Rios (RJ)", "Tres Rios*",
                    "Aperibé (RJ)","Santo Antônio de Pádua (RJ)", "Armação dos Búzios (RJ)", "Cabo Frio (RJ)",
                    "Belford Roxo (RJ)","Japeri (RJ)", "N.I1", "Mesquita (RJ)","N.I2",
                    "Queimados (RJ)", "N.I3", "Nova Iguaçu (RJ)","Macaé (RJ)", "Carapebus (RJ)",
                    "Cardoso Moreira (RJ)", "Italva (RJ)", "C.G1",
                    "Campos dos Goytacazes (RJ)","Macae*","Itatiaia (RJ)","Porto Real (RJ)","Resende (RJ)", "R1")

#tab1<-rbind(tab,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,k)

mat1=mat[!(row.names(mat) %in% row.names.remove), ]
#transformar devolta em dataframe
TABTRUE=as.data.frame(mat1)
TABTRUE=TABTRUE[order(rownames(TABTRUE)),]
#add municipios que nao aparecem na pesquisa ( neste caso arrail e s.joao de meriti)
arraial=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
sjmeiriti=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
Nilopolis=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
TABTRUE<-rbind(TABTRUE,arraial, make.row.names=TRUE)
TABTRUE<-rbind(TABTRUE,sjmeiriti, make.row.names=TRUE)
TABTRUE<-rbind(TABTRUE,Nilopolis, make.row.names=TRUE)
rownames(TABTRUE)[rownames(TABTRUE)=="68"]<-"Arraial"
rownames(TABTRUE)[rownames(TABTRUE)=="69"]<-"Sao Joao de Meriti"
rownames(TABTRUE)[rownames(TABTRUE)=="70"]<-"Nilopolis"

#organizar o dataframe em alphabetical###
TABTRUE<- TABTRUE[order(rownames(TABTRUE)),]


library(tibble)
TABTRUE1<-rownames_to_column(TABTRUE, var="MUNICIPALIDADES")
##row.names(TABTRUE1) = TABTRUE1$MUNICIPALIDADES


#CRIAR CENTROID PARA O FUTURO
Centroid<-as.data.frame(TABTRUE1)


#####CRIANDO GRUPOS#########
#############grupo 1##################

TABTRUE1$G1<-rowSums(TABTRUE1[, 2:8], na.rm = TRUE)

###########GRUPO 2############
TABTRUE1$G2<-rowSums(TABTRUE1[, 9:15], na.rm = TRUE)

#######grupo3##############
TABTRUE1$G3<-rowSums(TABTRUE1[, 16:22], na.rm = TRUE)

########grupo4#########
TABTRUE1$G4<-rowSums(TABTRUE1[, 23:29], na.rm = TRUE)


#############TX CRESCIMENTO##############
#####################AQUEEEEEE?????????1
Tx.C.A<-(TABTRUE1[,31] - TABTRUE1[,30])/TABTRUE1[,30]
Tx.C.A<-Tx.C.A*100
Tx.C.B<-(TABTRUE1[,32] - TABTRUE1[,31])/TABTRUE1[,31]
Tx.C.B<- Tx.C.B*100
Tx.C.C<-(TABTRUE1[,33] - TABTRUE1[,32])/TABTRUE1[,32]
Tx.C.C<-Tx.C.C*100

#Fixar erros por intems com 0
# uma variavel = 0 resultava em inf as duas variaveis =0 resultava em NaN
Tx.C.A[Tx.C.A==Inf]<-100
Tx.C.A[Tx.C.A=="NaN"]<-0

Tx.C.B[Tx.C.B==Inf]<-100
Tx.C.B[Tx.C.B=="NaN"]<-0

Tx.C.C[Tx.C.C==Inf]<-100
Tx.C.C[Tx.C.C=="NaN"]<-0

TABTRUE1$Tx.C.A<-Tx.C.A
TABTRUE1$Tx.C.B<-Tx.C.B
TABTRUE1$Tx.C.C<-Tx.C.C

##############Tx Aceleração##############
Tx.Ac.A<-Tx.C.B-Tx.C.A
Tx.Ac.B<-Tx.C.C-Tx.C.B

TABTRUE1$Tx.Ac.A<-Tx.Ac.A
TABTRUE1$Tx.Ac.B<-Tx.Ac.B


TABTRUE1<- TABTRUE1[order(TABTRUE1$MUNICIPALIDADES),]

codigo<-c("3300100"
          ,"3300209"
          ,"3300258"
          ,"3300308"
          ,"3300407"
          ,"3300506"
          ,"3300605"
          ,"3300704"
          ,"3300803"
          ,"3300902"
          ,"3301009"
          ,"3301108"
          ,"3301207"
          ,"3301306"
          ,"3301405"
          ,"3301504"
          ,"3301603"
          ,"3301702"
          ,"3301801"
          ,"3301850"
          ,"3301876"
          ,"3301900"
          ,"3302007"
          ,"3302106"
          ,"3302205"
          ,"3302304"
          ,"3302403"
          ,"3302502"
          ,"3302601"
          ,"3302700"
          ,"3302809"
          ,"3302908"
          ,"3303005"
          ,"3303104"
          ,"3303203"
          ,"3303302"
          ,"3303401"
          ,"3303500"
          ,"3303609"
          ,"3303708"
          ,"3303807"
          ,"3303906"
          ,"3304003"
          ,"3304102"
          ,"3304151"
          ,"3304201"
          ,"3304300"
          ,"3304409"
          ,"3304508"
          ,"3304557"
          ,"3304607"
          ,"3304706"
          ,"3304805"
          ,"3304904"
          ,"3305000"
          ,"3305109"
          ,"3305158"
          ,"3305208"
          ,"3305307"
          ,"3305406"
          ,"3305505"
          ,"3305554"
          ,"3305604"
          ,"3305703"
          ,"3305802"
          ,"3305901"
          ,"3306008"
          ,"3306107"
          ,"3306156"
          ,"3306305")

TABTRUE1$codigo<-codigo          

TABTRUE1[,2:39]<- sapply(TABTRUE1[,2:39], as.numeric)

as.numeric(round(TABTRUE1[,34],2))

write.table(TABTRUE1,"TANGERINA_FULLDATA_HM2.csv",sep = ";", row.names=F, fileEncoding= "UTF-8" )

a=TABTRUE1[,34:38]
a=cbind(a,codigo)
#localizar o shapefile
setwd("//dellt410/dados/PUBLICO/PROJETO LAGEF/RJ2000/modd/NOVO/RJ1989")
dir()
S<-readOGR("RJ1989_SIRGAS2000.shp") 

#botar os dados no shapefile
S<-merge(S,a, by="codigo")
  
setwd('//dellt410/dados/PUBLICO/Matheus Tenorio/REDO/PROJETO LAGEF2/TANGERINA')
writeOGR(S, '//dellt410/dados/PUBLICO/Matheus Tenorio/REDO/PROJETO LAGEF2/TANGERINA',"TANGERINA_.HM2", driver="ESRI Shapefile", overwrite=TRUE)

### CRIAR PONTO MEDIO
###
##criar novo arquivo para tirar centroide.
Centroid$codigo<-codigo

Centroid$LAT<-c("-22,98654301310"
                ,"-22,75567394090"
                ,"-22,92265004080"
                ,"-22,42687546210"
                ,"-22,45493940940"
                ,"-22,20091461680"
                ,"-21,12404663140"
                ,"-22,72027380040"
                ,"-22,51668335480"
                ,"-21,45834103490"
                ,"-21,70370926280"
                ,"-21,86920959160"
                ,"-21,90239693000"
                ,"-22,47336064950"
                ,"-22,14000065000"
                ,"-22,04547388960"
                ,"-22,05699767560"
                ,"-22,63186602430"
                ,"-22,51820368180"
                ,"-22,58365507140"
                ,"-22,82896581640"
                ,"-22,75834181320"
                ,"-22,84520055830"
                ,"-21,72806504730"
                ,"-21,22509493020"
                ,"-21,25107852220"
                ,"-22,27964837160"
                ,"-22,61296444900"
                ,"-22,95420835800"
                ,"-22,91601159580"
                ,"-22,53169017320"
                ,"-22,50678738050"
                ,"-21,39610318460"
                ,"-21,96302372880"
                ,"-22,82129980100"
                ,"-22,91658572640"
                ,"-22,32023075370"
                ,"-22,69882631810"
                ,"-22,62225037850"
                ,"-22,18430501680"
                ,"-23,14917029950"
                ,"-22,40246431310"
                ,"-22,62237410180"
                ,"-20,90690676550"
                ,"-22,10495904250"
                ,"-22,44128750950"
                ,"-22,73586412320"
                ,"-22,78271120020"
                ,"-22,16038305140"
                ,"-22,92425274450"
                ,"-21,97017495240"
                ,"-21,56945629640"
                ,"-21,66050824890"
                ,"-22,82604523860"
                ,"-21,52498281810"
                ,"-22,78573655730"
                ,"-22,17736939710"
                ,"-22,78643783300"
                ,"-21,88405110240"
                ,"-22,02533975290"
                ,"-22,87953834340"
                ,"-22,75978079250"
                ,"-22,56831279710"
                ,"-22,11328411600"
                ,"-22,31436962280"
                ,"-22,13066143130"
                ,"-22,13185046050"
                ,"-22,23511096150"
                ,"-20,89319938470"
                ,"-22,49138844260")


#add col LON
Centroid$LON<-c("-44,35322256410"
                ,"-42,29391287450"
                ,"-42,16985658670"
                ,"-43,91381134090"
                ,"-44,20553342570"
                ,"-42,37310659140"
                ,"-41,68394422120"
                ,"-42,04183971760"
                ,"-42,72921553070"
                ,"-41,92558049400"
                ,"-41,43057831300"
                ,"-42,33473163830"
                ,"-42,56919131250"
                ,"-42,07911309650"
                ,"-41,82891580640"
                ,"-42,31508706220"
                ,"-42,49886074420"
                ,"-43,30051956790"
                ,"-43,63906742250"
                ,"-42,96521096190"
                ,"-42,21759982320"
                ,"-42,82346267640"
                ,"-43,81728733920"
                ,"-42,08334592420"
                ,"-41,89540952130"
                ,"-42,13663010870"
                ,"-41,91825201930"
                ,"-43,11399495940"
                ,"-44,04460438230"
                ,"-42,81809141440"
                ,"-43,74854172390"
                ,"-43,46907163750"
                ,"-42,15265109880"
                ,"-43,03364986150"
                ,"-43,42979195850"
                ,"-43,05589790010"
                ,"-42,50202715550"
                ,"-43,50637179230"
                ,"-43,72607306280"
                ,"-43,30627821800"
                ,"-44,70641225240"
                ,"-43,16159827160"
                ,"-43,91739814300"
                ,"-41,96763437490"
                ,"-41,44157769780"
                ,"-44,49751294470"
                ,"-42,58931460530"
                ,"-44,07942598940"
                ,"-43,53980295140"
                ,"-43,45260489850"
                ,"-41,91351649230"
                ,"-42,18515715390"
                ,"-41,78816696430"
                ,"-42,99738096070"
                ,"-41,11941284630"
                ,"-43,36622984070"
                ,"-42,93942114880"
                ,"-42,12425105640"
                ,"-42,10313569710"
                ,"-42,82359323430"
                ,"-42,51953669930"
                ,"-43,70328307330"
                ,"-42,41370551530"
                ,"-42,66733112380"
                ,"-42,87409425600"
                ,"-42,15607957060"
                ,"-43,14449540460"
                ,"-43,85841833900"
                ,"-41,82669514350"
                ,"-44,08878083490")


library(dplyr)
CentroidLAT<-Centroid %>% mutate_at(vars(c("LAT","LON")),funs(as.numeric(gsub(",","\\.",.)))) %>%
  mutate_at(vars(starts_with("X")),funs(.*LAT))

names(CentroidLAT)<-c("MUNICIPALIDADES","LATx1989","LATx1990","LATx1991","LATx1992","LATx1993","LATx1994","LATx1995"
                      ,"LATx19896","LATx1997","LATx1998","LATx1999","LATx2000","LATx2001","LATx2002","LATx2003","LATx2004",
                      "LATx2005","LATx2006","LATx2007","LATx2008","LATx2009","LATx2010","LATx2011","LATx2012","LATx2013",
                      "LATx2014","LATx2015","LATx2016","codigo","LAT","LON")

CentroidLON<-Centroid %>% mutate_at(vars(c("LAT","LON")),funs(as.numeric(gsub(",","\\.",.)))) %>%
  mutate_at(vars(starts_with("X")),funs(.*LON))

names(CentroidLON)<-c("MUNICIPALIDADES","LONx1989","LONx1990","LON1991","LONx1992","LONx1993","LONx1994","LONx1995"
                      ,"LONx19896","LONx1997","LONx1998","LONx1999","LONx2000","LONx2001","LONx2002","LONx2003","LONx2004",
                      "LONx2005","LONx2006","LONx2007","LONx2008","LONx2009","LONx2010","LONx2011","LONx2012","LONx2013",
                      "LONx2014","LONx2015","LONx2016","codigo","LAT","LON")

CentroidFINAL<-cbind(CentroidLAT[,2:29],CentroidLON[,2:29])
CentroidFINAL$codigo<- codigo
row.names(CentroidFINAL)=Centroid$MUNICIPALIDADES

#add total prod anual e da soma dos xlat/xlon na tabela do centroid ( para dividir dps)
tprod<-colSums(Centroid[,2:29], na.rm = TRUE)
txlat.xlon<-colSums(CentroidFINAL[,1:56], na.rm = TRUE)
#juntar
CentroidFINAL1<-rbind(CentroidFINAL,tprod,txlat.xlon)
rownames(CentroidFINAL1)[rownames(CentroidFINAL1)=="71"]<-"Total.prod.anual"
rownames(CentroidFINAL1)[rownames(CentroidFINAL1)=="72"]<-"Total.xlat.xlon"
#dividir
CentroidFINAL2<- CentroidFINAL1[71:72,0:56]
#tirar coord dos dados
coord <-CentroidFINAL2[2,]/CentroidFINAL2[1,]
CentroidFINAL2<-rbind(CentroidFINAL2,coord)

coord<- sapply(coord, as.numeric)

lat<-as.vector(coord[1:28])
names(lat)<-seq(1989,2016,by=1)
long<-coord[29:56]
names(long)<-seq(1989,2016,by=1)
df<-rbind(lat,long)
df<-t(df)
names(df)<-c("ano","y","x")
df<-as.data.frame(df)

write.table(df,"Centroid_TANGERINA_HM2_FINAL.csv",sep = ";", row.names=F, fileEncoding= "UTF-8")

#criar shp do centroid com os dados
library(sp)
library(raster)
library(rgdal)
coordinates(df) <- c("long","lat")
plot(df)
shapefile(df,"centroid_TANGERINA_HM2.shp")
