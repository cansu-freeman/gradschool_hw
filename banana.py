# Banana Function 

import numpy as np

def banana(xy):
    x = xy[0]
    y = xy[1]
    return 100*(y - x**2)**2 + (1 - x)**2

def grad_b(xy):
    x = xy[0]
    y = xy[1]
    dx = 400*x**3 - 400*x*y + 2*x - 2
    dy = 200*(y-x**2)
    return np.array([dx, dy])

def banana_opt_fixstep(x0, s=0.01, eps=0.0001, imax=10000):
    i = 0
    x = x0
    while norm(grad_b(x)) > eps and i < imax:
        i = i + 1
        gf = grad_b(x)
        d = -gf/norm(gf)
        x = x + s*d       
    return x, i

def banana_opt_backtrack(x0, eps=0.0001, imax=10000):
    i = 0
    x = x0
    while norm(grad_b(x)) > eps and i < imax:
        i = i + 1
        gf = grad_b(x)
        d = -gf/norm(gf)
        # Step size with backtracking
        s = 1

        while banana(x+s*d) > banana(x):
            s = s/2
        x = x + s*d
    return x, i

def J(x):
	x1 = x[0]
	x2 = x[1]
	J = [[-400*x2 + 1200*x1**2 + 2, -400*x1], [-400*x1, 200]]
	return J

def multiNewtBanana(x0, tol=0.00000000000000001)
	i = 0
	x = x0
	
	while True: 
		fx = banana(x)
		x = x - np.matmul(np.linalg.inv(J(x)), grad_b(x))
		i = i + 1

		if np.linalg.norm(banana(x)) < tol:
			break
	return x, i
