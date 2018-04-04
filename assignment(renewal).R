rm(list = ls())
gc()
#object.1
x = c(17, 16, 20, 24, 22, 15, 21, 18)
print(x[x>=20])
x[x>=20]=100
y =x
y
print(y)
t = c(17, 16, 20, 24, 22, 15, 21, 18)
function(x)
{
  if(x>20)
  {
    y = 100
  }
    return(y)
}
x
function(1:10)
  #object.2
    x = matrix(c(3,-1,-1,-1,-1,-1,4,-1,-1,
               -1,-1,-1,5,-1,-1,-1,
               -1,-1,6,-1,-1,-1,-1,-1,7),5,5)
y = x[,-5]  
y[y==-1]=0             
y1=y
y1

yinfo=c(nrow(y),ncol(y))
yinfo
y
if(y[i,j]=-1)
  y[i,j] = 0


#object.3
#1)
setwd("C:/temp")
A=read.table("x.txt",header =T, sep=",",stringsAsFactors= FALSE)
rdata=data.frame(A)
rdata
#2)
#is.na()
is.na()
#3)
b=c()
for (i in (1:4))
  if (!(is.na.data.frame(rdata[i,2])) & !(is.na.data.frame(rdata[i,3])))
    b=c(b,i)
b
#4)
rdata1=rdata[b,]
rdata1
#object.4
M= matrix(rep(1,4),2,2)
S=seq(0,1,length=100)
temp = list(c(TRUE, FALSE),M,S,1,2,3,4)
temp

temp[[2]]=NULL
temp
temp[[3]]
length(temp)
#object.5
a1 <- -1:2
a2 <- 1:2
a1 + a2

a1 <- -(1:2)
a2 <- 1:2
a1 + a2
a1

a1 <- matrix(0,2,2)
a2 <- c(3,4)
a1 + a2

a1<-matrix(1:4,2,2)
a1[a1>2]=0
a1

a1 <- 1:5
a1[-1] - a1[-length(a1)]
a1[-1]
a1[-length(a1)]
#programming.1
a1=1
a2=3
for(i in 1:2)
{
  a3 = 0.9*a2 - 0.1*a1 + 1
}

a= rep(0,20)
a[1] = 1
a[2] = 3
for(n in 1:18)
{
  a[n+2] = 0.9*a[n+1]-0.1*a[n]+1  
}
a[20]
#programming.2
a= rep(0,20)
a[1] = 1
a[2] = 3
for(n in 1:18)
{
  a[n+2] = 0.9*a[n+1]-0.1*a[n]+1  
  if(a[n]>4)
  {
  break
  }
}
n
#programming.3
A = matrix(runif(100), 50, 5)
A
s_rowMean = function(x)
{
  if(class(x) != "matrix")stop()
  v = rep(nrow(x),0)
  for(i in 1:nrow(x))
  {
    v[i] = mean(x[i,])
  }
  return(v)
}
s_rowMean(A)
#programming.4
tmp = rep(0,10)
a = 10:1
idx = 1
for(j in a)
{
  if(j<5)
  {
    tmp[idx] = a[j]
    idx = idx + 1
  }
}  
#programming.5

x=matrix(runif(5000),1000,5)
sid=c(sample(1:10,1000,replace=TRUE))
head(sid)
#programming.6
m.mat=matrix(,10,5)
for (i in 1:10)
{
  m=rep(0,5)
  n=0
  for (j in 1:1000)
  {
    if (sid[j]==i)
    {
      m=m+x[j,]
      n=n+1
    }
  }
  m.mat[i,]=m/n
}
m.mat

#programming.6-2

idist=matrix(rep(0,10000),1000,10)
for(i in 1:1000)
  for(j in 1:10)
    idist[i,j]=sum((x[i,]*m.mat[j,]))/(sqrt(sum(x[i,]**2))*sqrt(sum(m.mat[j,]**2)))
head(idist)

#programming.7

ivec=rep(0,1000)
for (i in 1:1000)
  for (j in 1:10)
    if (idist[i,j]==min(idist[i,]))
      ivec[i]=j
rowsum.default((x[1,]*m.mat[2,])

               
#programming.8
#1)
set.seed(1)
a=list()
for(i in 1:1000)
{
x=rpois(1,4)+1
x=min(x,10)
a[[i]]=sample(1:10,x)
}
m=c(rep(0,9))
for(i in 1:1000)
{
for (j in 2:10)
{
if (length(a[[i]])==j)
m[j-1]=m[j-1]+1
}
}
m
               
#2)
points=c(rep(0,10))               
for(i in 1:1000)
{
for(j in 1:10)
{    if(length(a[[i]])==2|length(a[[i]])==3)
{
if (a[[i]][1]==j)
points[j]=points[j]+1
}
else if(length(a[[i]])==4|length(a[[i]])==5|length(a[[i]])==6)
{
if (a[[i]][1]==j)
points[j]=points[j]+2
else if (a[[i]][2]==j)
points[j]=points[j]+1
}
else if(length(a[[i]])>6)
{
if (a[[i]][1]==j)
points[j]=points[j]+3
else if (a[[i]][2]==j)
points[j]=points[j]+2
else if (a[[i]][3]==j)
points[j]=points[j]+1
}
                 
}
points
               
               
#programming.9
#1)
set.seed(1)
m1=10
m2=5
num=0
               
for (i in 1:4)
if(rbinom(1,1,1/2)==0)
{   m1=m1-1
}else
{   m1=m1+1}
m1
#2)
set.seed(1)
m1=10
m2=5
num=0
while (m1!=0&m2!=0)
if(rbinom(1,1,1/2)==0)
{   m1=m1-1
m2=m2+1
num=num+1
}else
{   m1=m1+1
m2=m2-1
num=num+1}
num
m1
m2
               
#3)
testfunction=function(m2)
{
x=0
m3=0
for (k in 1:200)
{
set.seed(k)
m1 = 10
m3=m2
num = 0
while (m1!=0&m3!=0)
{
if(rbinom(1,1,1/2)==0)
{
m1=m1-1
m3=m3+1
num=num+1
}else{
m1=m1+1
m3=m3-1
num=num+1}
}
if (m3==0)
x=x+1
}
return (x)
}
testfunction(m2=5)
               
               
               
#10.
               
testfunction(m2=10)/200
testfunction(m2=15)/200
testfunction(m2=20)/200
testfunction(m2=25)/200
               
               
               

