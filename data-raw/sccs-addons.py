import pandas as pd

filepath = "SCCS-included-vars.txt"
left = "["
right = "]"
sccs = []

with open(filepath, 'r') as f:
    for line in f:
        code = line.split(left)[1].split(right)[0]
        sccs.append(code)
        
df = pd.DataFrame(sccs, columns=['codes'])
df.to_csv('SCCS-addons.csv', index=False)
