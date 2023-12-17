
import numpy as np
import math
import matplotlib.pyplot as plt


x_N = 10
dx  = 0.01
N   = int(x_N / dx)

x     = np.zeros(N)
n_sol = np.zeros(N)
a_sol = np.zeros(N)


x[0]     = 0
n_sol[0] = 1.0
a_sol[0] = 1.0


for i in range(1, N):
    x[i]     = x[i - 1] + dx
    n_sol[i] = n_sol[i - 1] + (dx * math.sin(x[i - 1]))
    a_sol[i] = 2.0 - math.cos(x[i])


fig = plt.figure(figsize = (8, 4))

ax  = fig.add_subplot(1, 3, 1)
plt.plot(x, n_sol, color = 'red')
plt.title('Numerical Solution')

ax  = fig.add_subplot(1, 3, 2)
plt.plot(x, a_sol, color = 'blue')
plt.title('Analytic Solution')

ax  = fig.add_subplot(1, 3, 3)
plt.plot(x, a_sol - n_sol, color = 'green')
plt.title('Error')

fig.suptitle('Sine Solution', fontsize = 20)
plt.tight_layout()
plt.subplots_adjust(top = 0.85)
