import numpy as np

def Theta(z):
	return 0 if z < 0 else 1

def d(k, l, N, x):
	return np.log(C(k, l, N, x))/np.log(l)

def C(k, l, N, x):
	s = 0
	n0 = N - k
	L = np.array([[Theta(l-p(k, i, j, x)) for j in range(n0, N)] for i in range(n0, N)])
	return (1 / N**2) * np.sum(L)

def p(k, n1, n2, x):
	w_n1 = x[n1-k:n1]
	w_n2 = x[n2-k:n2]
	S = np.array([(w_n1[i]-w_n2[i])**2 for i in range(k)])
	return np.sqrt(np.sum(S))

def get_xy(a, b, dt, N):
	Ts = [i * dt for i in range(N)]
	x = np.array([a * np.cos(t) for t in Ts])
	y = np.array([a * np.sin(t) for t in Ts])
	return x, y

a = 4
b = 2
dt = 1e-3
N = 10000
l = 1e-3
x, y = get_xy(a, b, dt, N)
for k in range(2, 10):
	dim = d(k, l, N, x)
	print(dim)
