import time
import sys

cosicas = ['|', '/', '-', '\\']
for i in range(60):
    time.sleep(0.1)
    sys.stdout.write('[%s] - [%-60s] %s %%\r' %
            (cosicas[(i/2) % 4], ('='*i)+'>', int((i / 59.0)*100)))
    sys.stdout.flush()
print ""
    
