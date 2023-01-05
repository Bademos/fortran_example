import exrex


name = exrex.getone(r'[a-z]{14}[ ]')
init = exrex.getone(r'[a-z][ ][a-z][ ]')
text =""
for i in range(9500):
    name = exrex.getone(r'[a-z]{14}[ ]')
    init = exrex.getone(r'[a-z][ ][a-z][ ]')
    text += name+init +'\n'

with open("bigdata_9500.txt", "w") as file:
    file.write(text)
