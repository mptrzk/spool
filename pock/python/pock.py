

tru = (None, None)
fal = None

def cons(a, b):
    return (a, b)

def head(a):
    return a[0]

def tail(a):
    return a[1]

def iden(a, b):
    return a == b

print(iden((None, (None, None)), None))
print(iden((None, (None, None)), (None, (None, None))))

def format(a):
    def format_list(a):
        return format(head(a)) + ((' ' + format_list(tail(a))) if tail(a) else '')
    return '['+format_list(a)+']'if a else '~'
        
print(format(cons(tru, cons(tru, tru))))
