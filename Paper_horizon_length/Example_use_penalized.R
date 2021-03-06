library(penalized)
df <- data.frame("a"=1:100,"b"=(1:100)*10,"c"=1,"s"=as.factor(1:5), "d" = (1:100)/100)
head(df)
options(digits = 4)

# Exemple tonto lm(a ~ 0 + b + c,df) en format penalized()
  #resposta
  y <- df$a
  #aqui aplicar la formula que es vol aplicar al model per predir la resposta
  x <- model.matrix( ~ 0 + c + b:s +d,df)
  #construcci� del model utilitzant penalized, en aquest cas lambda1 i 2 son zero per fer una regressi� "normal" sense aplicar 
  #LASSO o feature selection, el Gerard acostuma a posar-ho tot en penalized (tot susceptible a ser eliminat) i deix unpenalized "buit"
  #positive es una matriu de TRUE/FALSE que en el cas de true for�a a que el coeficient sigui positiu
  mod <- penalized(response = y, penalized = x, unpenalized = ~ 0, positive = grepl("s2",colnames(x)), lambda1 = 0,lambda2 = 0)
  coef(mod)
  #Al ser classe S4 no es pot tractar igual, si se li vol guardar alguna cosa s'ha de fer aix�:
  mod@nuisance$tbal <- 12
  #predict a vegades fa coses rares, per aix� el que aplica �s fer la multiplicaci� vectorial de x (dades) amb els coheficients del model
  #per obtindre la predicci�
  pred_vector <- x %*% coef(mod)
  acf(y-pred_vector)

  x <- model.matrix( ~ 0 + b:s + d + c,df)
  