def spiralPrint(m, n, a): 
  t = 0
  b = m-1
  l = 0
  r = n-1
  direc = 0
  while(t<=b and l<=r):
    if(direc==0):
      for i in range(l,r+1):
        print(a[t][i],end=" ")
      t += 1
    elif(direc==1):
      for i in range(t,b+1):
        print(a[i][r],end=" ")
      r -= 1
    elif(direc==2):
      for i in range(r,l-1,-1):
        print(a[b][i],end=" ")
      b -= 1
    elif(direc==3):
      for i in range(b,t-1,-1):
        print(a[i][l],end=" ")
      l += 1
    direc = (direc+1) % 4

    

a = [ [1, 2, 3, 4, 5, 6], 
      [7, 8, 9, 10, 11, 12], 
      [13, 14, 15, 16, 17, 18] ] 

R = 3; C = 6
spiralPrint(R, C, a) 