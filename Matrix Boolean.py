def modifyMatrix(a):
  row = len(a)
  col = len(a[0])
  zrow = False
  zcol = False
  for i in range(0, row):
    for j in range(0,col):
      if a[0][j]==1:
        zrow =True
      if a[i][0]==1:
        zcol =True
      if a[i][j]==1:
        a[0][j]=1
        a[i][0]=1
  for i in range(1, row):
    for j in range(1,col):
      if a[0][j]==1 or a[i][0]==1:
        a[i][j]=1
  if zrow == True:
    for i in range(0, col):
      a[0][i] = 1
  if zcol == True:
    for i in range(0, row):
      a[i][0] = 1

def printMatrix(a):
  row = len(a)
  col = len(a[0])
  for i in range(0, row):
    for j in range(0, col):
      print(a[i][j],end=" ")
    print()
mat = [ [1, 0, 0, 1], 
        [0, 0, 1, 0], 
        [0, 0, 0, 0] ] 
          
print("Input Matrix :") 
printMatrix(mat) 
      
modifyMatrix(mat) 
      
print("Matrix After Modification :") 
printMatrix(mat) 
