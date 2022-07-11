from bidict import bidict


labels = bidict()

class Cons:
	def __init__(s, car=None, cdr=None):
		s.car = car
		s.cdr = cdr
		labels[len(labels)] = s
	def draw(s, depth=0, breadth=0, max_depth=100, max_breadth=10):
		print(f"{'  ' * breadth}[{labels.inverse[s]}]")
		if not isinstance(s.car, Cons):
			print('  ' * (breadth+1), s.car, sep='')
		else:
			s.car.draw(depth, breadth+1, max_depth, max_breadth)
		if not isinstance(s.cdr, Cons):
			print('  ' * breadth, s.cdr, sep='')
		else:
			s.cdr.draw(depth+1, breadth, max_depth, max_breadth) #refactor?

def bind(name, cons):
	#labels[name] = cons
	labels.inverse[cons] = name

def find(name):
	return labels[name]
	

bind("stack", Cons())

def push(val):
	cons = Cons(val, find("stack"))
	bind("stack", cons)
	#rebinding cons vs rebinding label

push(1)
#push(2)
#push(3)
#push(Cons(1, Cons(Cons(2, 3), 4)))
find("stack").draw()
