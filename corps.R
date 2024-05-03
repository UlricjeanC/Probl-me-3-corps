library("scales")
#install.packages("tidyverse")
library("ggplot2")
library('gganimate')
library('gifski')


distEuclidienne <- function(v1,v2){
  d = sqrt ((v1[1]-v2[1])**2 + (v1[2]-v2[2])**2 )
  return(d)
}

absvec <- function(v1){
  d = abs(v1[1]) + abs(v1[2])
  return(d)
}

absvecbis <- function(v1,v2){
  d = abs(v1) + abs(v1)
  return(d)
}

iteration <- function(A1,A2,t){
  G = 6.66*10**(-11)
  m1 = A1$masse
  m2 = A2$masse
  
  p1 = A1$position
  p2 = A2$position
  
  v1 = A1$vitesse
  v2 = A2$vitesse
  
  a = G*m1*m2/distEuclidienne(p1,p2)
  
  vec_a = ((p2-p1)/distEuclidienne(p1,p2))*a/m1
  vec_v = vec_a*t+v1
  vec_p = vec_a*(t**2)/2 + vec_v*t + p1
  
  A1 = list(vitesse = vec_v , position = vec_p , masse = m1)
  
  return(A1)
}

# A1 = list(vitesse = c(-0.01,0.01) , position = c(50,50) , masse = 1*10**3)
# A2 = list(vitesse = c(0.0,0.0) , position = c(-500,0) , masse = 4*10**6)
# A3 = list(vitesse = c(0.01,0.001) , position = c(100,500) , masse = 1*10**5)

A1 = list(vitesse = c(0.0,0.0) , position = c(50,50) , masse = 1*10**6)
A2 = list(vitesse = c(0.0,0.0) , position = c(-500,0) , masse = 1*10**6)
A3 = list(vitesse = c(0.0,0.0) , position = c(100,500) , masse = 1*10**6)

t = 0
s = 1000

# plot(x = A2$position[1],y = A2$position[2] ,col='red',xlim=c(-s,s),ylim=c(-s,s))
# points(x = A1$position[1], y =A1$position[2],col='orange')
# points(x = A3$position[1], y =A3$position[2],col='purple')

data_A1 = data.frame(list(temps = 0, vitesse_x=A1$vitesse[1],vitesse_y=A1$vitesse[2],
                          position_x = A1$position[1],position_y = A1$position[2],
                          masse = A1$masse))
data_A2 = data.frame(list(temps = 0, vitesse_x=A2$vitesse[1],vitesse_y=A2$vitesse[2],
                          position_x = A2$position[1],position_y = A2$position[2],
                          masse = A2$masse))
data_A3 = data.frame(list(temps = 0, vitesse_x=A3$vitesse[1],vitesse_y=A3$vitesse[2],
                          position_x = A3$position[1],position_y = A3$position[2],
                          masse = A3$masse))

print(data_A1)

for(t in 1:1000){
  
  # points(x = A1$position[1], y =A1$position[2],col='white',pch = 19)
  # points(x = A2$position[1], y = A2$position[2],col='white',pch = 19)
  # points(x = A3$position[1], y =A3$position[2],col='white',pch = 19)
  # 
  # points(x = A1$position[1], y = A1$position[2],col = alpha('orange',absvec(A1$vitesse)*5))
  # points(x = A2$position[1], y = A2$position[2],col = alpha('red',absvec(A2$vitesse)*5))
  # points(x = A3$position[1], y = A3$position[2],col = alpha('purple',absvec(A3$vitesse)*5))
  
  A1 = iteration(A1,A2,t)
  A1 = iteration(A1,A3,t)
  
  buff_A1 = data.frame(list(temps = t, vitesse_x=A1$vitesse[1],vitesse_y=A1$vitesse[2],
                  position_x = A1$position[1],position_y = A1$position[2],
                  masse = A1$masse))
  data_A1 = rbind(data_A1,buff_A1)
  
  A2 = iteration(A2,A1,t)
  A2 = iteration(A2,A3,t)
  
  buff_A2 = data.frame(list(temps = t, vitesse_x=A2$vitesse[1],vitesse_y=A2$vitesse[2],
                                     position_x = A2$position[1],position_y = A2$position[2],
                                     masse = A2$masse))
  data_A2 = rbind(data_A2,buff_A2)
  
  A3 = iteration(A3,A1,t)
  A3 = iteration(A3,A2,t)
  
  buff_A3 = data.frame(list(temps = t, vitesse_x=A3$vitesse[1],vitesse_y=A3$vitesse[2],
                            position_x = A3$position[1],position_y = A3$position[2],
                            masse = A3$masse))
  data_A3 = rbind(data_A3,buff_A3)
  
  print(A1)
  
  # points(x = A1$position[1], y =A1$position[2],col='orange',pch = 19)
  # points(x = A2$position[1], y = A2$position[2],col='red',pch = 19)
  # points(x = A3$position[1], y =A3$position[2],col='purple',pch = 19)
}

maxM = max(c(A1$masse[1],A2$masse[1],A3$masse[1]))

p <- ggplot() + geom_point(data=data_A1, mapping = aes(x = position_x, 
                                                  y = position_y,
                                                  colour = absvecbis(vitesse_x,vitesse_y),
                                                  group = NA, size = masse/maxM )) +
  geom_point(data=data_A2, mapping = aes(x = position_x, 
                                         y = position_y,
                                         colour = absvecbis(vitesse_x,vitesse_y),
                                         group = NA, size = masse/maxM )) +
  geom_point(data=data_A3, mapping = aes(x = position_x, 
                                         y = position_y,
                                         colour = absvecbis(vitesse_x,vitesse_y),
                                         group = NA, size = masse/maxM )) +
  ggtitle("Problème à 3 corps") +
  xlab("") + 
  ylab("") +
  xlim(-s,s) +
  ylim(-s,s) +
  labs(colour = "Vitesse", size = 'masse') +
  scale_color_gradient(low = "orange", 
                       high = "purple") +
  theme_classic() +
  coord_quickmap()

panim <- p + 
  transition_time(temps) 
animate(panim, nframes = 500, renderer = gifski_renderer())  

