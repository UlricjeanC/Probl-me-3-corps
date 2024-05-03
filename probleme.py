import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from mpl_toolkits.mplot3d import Axes3D

def distEuclidienne(v1,v2):
    dist = np.sqrt(np.sum((v1-v2)**2))
    return dist
def barycentre(p1,p2,p3):
    c1 = (p3-p2)/2
    c2 = (p1-p3)/2
    c3 = (p2-p1)/2
    return c1

class CorpsCelestes:
    def __init__(self, position, vitesse, acceleration, masse):
        self.p = position
        self.v = vitesse
        self.a = acceleration
        self.m = masse
    def iteration(self,A):
        G = 6.66*10**(-11)
        a = G*self.m*A.m/distEuclidienne(self.p,A.p)
        self.a = self.a + ((A.p-self.p)/distEuclidienne(self.p,A.p)) * (a/self.m)
    def integration(self,t):
        self.p = self.a*(t**2)/2 + self.v*t + self.p
        self.v = self.a*t + self.v
        self.a = np.array([0,0,0])


def buid3corps(tp, dt):
    l = len(np.linspace(0,tp,dt))
    Mat_A1 = np.zeros((l,3))
    Mat_A2 = np.zeros((l,3))
    Mat_A3 = np.zeros((l,3))
    t = 0
    for i in np.linspace(0,tp,dt):
        A1.iteration(A2)
        A1.iteration(A3)
        A1.integration(t)
        
        Mat_A1[t,] = A1.p
        
        A2.iteration(A1)
        A2.iteration(A3)
        A2.integration(t)
        
        Mat_A2[t,] = A2.p
        
        A3.iteration(A1)
        A3.iteration(A2)
        A3.integration(t)
        
        Mat_A3[t,] = A3.p
        
        t += 1
        
    return Mat_A1, Mat_A2, Mat_A3

A1 = CorpsCelestes(position = np.array([500,100,-100]), vitesse = np.array([0,0,0]), acceleration = np.array([0,0,0]), masse = 1*10**6)
A2 = CorpsCelestes(position = np.array([100,-100,0]), vitesse = np.array([0,0,0]), acceleration = np.array([0,0,0]), masse = 1*10**6)        
A3 = CorpsCelestes(position = np.array([-100,0,-100]), vitesse = np.array([0,0,0]), acceleration = np.array([0,0,0]), masse = 1*10**6)

tmax = 1000
dt = 1500
lim = 500
r = 80

Mat_A1, Mat_A2, Mat_A3 = buid3corps(tmax,dt)

fig = plt.figure()
ax = fig.add_subplot(projection='3d')

plt.axis('off')
ax.grid(False)

def func(t):
    ax.clear()
    plt.axis('off')
    ax.grid(False)

    #ax.set_xlim(-lim, lim)
    #ax.set_ylim(-lim, lim)
    #ax.set_zlim(-lim, lim)
    
    
    ax.scatter(Mat_A1[t,0], Mat_A1[t,1], Mat_A1[t,2], s = r, color='red')
    ax.scatter(Mat_A2[t,0], Mat_A2[t,1], Mat_A2[t,2], s = r, color='orange')
    ax.scatter(Mat_A3[t,0], Mat_A3[t,1], Mat_A3[t,2], s = r, color='magenta')
    
    m = 60
    plt.plot(Mat_A1[max(0,t-m):t,0], Mat_A1[max(0,t-m):t,1], Mat_A1[max(0,t-m):t,2], alpha = 0.5, color='red')
    plt.plot(Mat_A2[max(0,t-m):t,0], Mat_A2[max(0,t-m):t,1], Mat_A2[max(0,t-m):t,2], alpha = 0.5, color='orange')
    plt.plot(Mat_A3[max(0,t-m):t,0], Mat_A3[max(0,t-m):t,1], Mat_A3[max(0,t-m):t,2], alpha = 0.5, color='magenta')
    
    return ax 

numDataPoints = len(np.linspace(0,tmax,dt))
t = np.linspace(0,tmax,dt)

line_ani = animation.FuncAnimation(fig, func, frames=numDataPoints, 
                    fargs=(), interval=100,
                    blit=False, repeat = True)

plt.show()








