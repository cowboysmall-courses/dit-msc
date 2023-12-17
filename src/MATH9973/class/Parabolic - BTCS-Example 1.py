
# coding: utf-8

# # Heat Equation
# ## The Differential Equation
# $$ \tau \frac{\partial}{\partial t} = \frac{1}{16}\frac{\partial^2 u}{\partial x^2}$$
# ## Initial Condition
# $$ u(x,0)=2\sin(2\pi x) $$
#
# ## Boundary Condition
# $$ u(0,t)=0,  u(1,t)=0 $$
#
# ## The Difference Equation
# $$ w[k+1,i] = w[k,i] + \frac{1}{16}\frac{k}{h^2}(w[k,i+1]-2w[k,i]+w[k,i-1])$$
#



# %%

import numpy as np
import matplotlib.pyplot as plt
import warnings


warnings.filterwarnings("ignore")



# %%

N  = 5
Nt = 25
h  = 1 / 5.
ht = 1 / Nt

time = np.arange(0, 1.0001, h)
x    = np.arange(0, 1.0001, h)
w    = np.zeros((Nt, N + 1))
A    = np.zeros((N - 1, N - 1))
c    = np.zeros(N - 1)



# %%

for i in range(1, N):
    w[0, i] = 2 * np.sin(2 * np.pi * x[i])

for i in range(0, N - 1):
    A[i, i] = 2

for i in range(0, N - 2):
    A[i + 1, i] = -1
    A[i, i + 1] = -1



# %%

A    = np.eye(N - 1) + 1/16 * ht / (h * h) * (A)
Ainv = np.linalg.inv(A)

fig = plt.figure(figsize = (8, 4))
plt.matshow(A)



# %%

for k in range(1, Nt):
    w[k, 1:(N)] = np.dot(Ainv, w[k - 1, 1:(N)])



# %%

fig = plt.figure(figsize = (8, 4))
plt.matshow(w)
