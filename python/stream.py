class Stream:
    def __init__(self, iter):
        self.iter = iter
    
    def __iter__(self):
        yield from self.iter

    def __rmul__(self, other):
        yield other 
        yield from self

    def __pow__(self, other):
        yield from self
        yield from other

    def __rpow__(self, other):
        yield from other 
        yield from self


def streamFunc(func):
    return lambda *args, **kwargs: Stream(func(*args, **kwargs))

range = streamFunc(range)
enumerate = streamFunc(enumerate)
zip = streamFunc(zip)
empty = Stream([])

@streamFunc
def chain(*its):
    for it in its:
        yield from it
