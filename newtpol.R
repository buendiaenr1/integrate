
# Enrique Buendia Lozada
# 2022
# 
# 1: si tenemos los resultados de medir la actividad y
# 2: en intervalos por ejemplo de tiempo x
# 3: obtendremos vía la integral el total de F(y)
# 4: modelando el comportamiento con un polinomio conocido y multicitado
#
#

x = c(15, 17, 19, 21, 23)                        #2
y = c(10, 13, 18, 20, 30)                       #1

pol<- function(x,y){
  n<- length(x)
  a=matrix(c(rep(0,(n-1)^2)),nrow=n-1,ncol=n-1,byrow=TRUE)
  
  for(j in 1:(n-1))
        for(i in 1:(n-j)){
              if (j==1)
                 a[i,j]=(y[i+1]-y[i])/(x[i+1]-x[i])
              else
                 a[i,j]=(a[(i+1),(j-1)]- a[i,(j-1)])/(x[i+j]-x[i])
        }
  
  # evaluar el polinomio en 22
  no= 22
  #print(a)
  suma=y[1]
  cat(" f(x)= ",y[1]," + ")
  for (i in i:(n-1))
  {
      prod=1
      for (j in 1:i){
          prod=prod*(no-x[j])
          cat("(x-",x[j],")*")
      }
      cat("(",a[1,i],") + ")
      suma=suma +prod*a[1,i]
      
  }
  cat("\n")
  #print(suma)  # ejemplo de evaluación de la funcion en no=22
  
  
}

print(" Ejemplo de uso: ")
cat("x <- c(")
for (i in 1:length(x)){
    if (i<length(x)){
       cat(x[i],",")
    }else{
       cat(x[i])
    }
}
cat(")\n")

cat("y <- c(")
for (i in 1:length(y)){
    if (i<length(x)){
       cat(y[i],",")
    }else{
       cat(y[i])
    }
}
cat(")\n")



cat("pol(x,y)\n\n\nPolinomio interpolante...\n")
pol(x,y)                             #4

# forma en como se pueden graficar los datos y la funcion
fun <- function(x) {
 10  + (x- 15 )*( 1.5 ) + (x- 15 )*(x- 17 )*( 0.25 ) + (x- 15 )*(x- 17 )*(x- 19 )*( -0.1041667 ) + (x- 15 )*(x- 17 )*(x- 19 )*(x- 21 )*( 0.04166667 )
}
plot(x,y,col="red")
curve(fun, from =10, to=30,add=TRUE)


# uso con integrales
# 3
integrate(function(x) 10  + (x- 15 )*( 1.5 ) + (x- 15 )*(x- 17 )*( 0.25 ) + (x- 15 )*(x- 17 )*(x- 19 )*( -0.1041667 ) + (x- 15 )*(x- 17 )*(x- 19 )*(x- 21 )*( 0.04166667 ),lower=15,upper=23)



