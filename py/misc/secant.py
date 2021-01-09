




def secant(f, delta):
	def until(condition, f, x):
		while True:
			res = f(x)
			if condition(res):
				return res
			else:
				x = res

	def update(arg):
		x, y = arg
		return (x  - (x-y) * f(x) / (f(x) - f(y)), x)

	def err(arg):
		x, y = arg
		return abs(x - y) < delta
		
	return until(err, update, (0,1))[0]
	

def zipWith(f, xs, ys):
	return [f(x,y) for (x,y) in zip(xs, ys)]

'''
npv :: Double -> [Double] -> Double
npv i = sum . zipWith (\t c -> c / (1 + i)**t) [0..]

irr :: [Double] -> Double
irr cashflows = secant (`npv` cashflows) (0.1**4)

'''
