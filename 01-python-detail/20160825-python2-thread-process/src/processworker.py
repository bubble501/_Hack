import random
import multiprocessing

def compute(n):
    return sum(
            [random.randint(1, 100) for i in range(1000000)])

# Satrt 8 workers
pool = multiprocessing.Pool(8)
print("Result: %s" % pool.map(compute, range(8)))
