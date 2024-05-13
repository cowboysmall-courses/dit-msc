
# %%
import numpy as np
import matplotlib.pyplot as plt

# %%
dx = 0.1
t = -0.01

# %%
y = np.zeros(102)
y[0] = 1

x = np.zeros(102)
x[0] = 0

s = np.zeros(102)
s[0] = 1

# %%
for i in range(1, 102):
    y[i] = y[i - 1] + (dx * t * y[i - 1] * x[i - 1] * x[i - 1])
    x[i] = x[i - 1] + dx

# %%
fig = plt.figure(figsize = (8, 4))
plt.plot(x, y, label = 'Numerical Solution', color = 'red')
plt.show()
