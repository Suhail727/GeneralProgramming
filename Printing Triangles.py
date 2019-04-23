def left_triangle(n): 
   # outer loop to handle number of rows 
    # n in this case 
    for i in range(0, n): 
      
        # inner loop to handle number of columns 
        # values changing acc. to outer loop 
        for j in range(0, i+1): 
          
            # printing stars 
            print("*",end="") 
       
        # ending line after each row 
        print("\r") 

def right_triangle(n):
  m = n-1
  for i in range(0,n):
    # Print Blanks Spaces (n-1) times
    for j in range(0,m):
      print(" ",end="")
    # Decrement Blank space for next iteration
    m=m-1
    # Print normal left triangle
    for k in range(0,i+1):
      print("*",end="")
    print("\r")
    
def triangle(n): 
  m = n-1
  for i in range(0,n):
    # Print Blanks Spaces (n-1) times
    for j in range(0,m):
      print(" ",end="")
    # Decrement Blank space for next iteration
    m=m-1
    # Print normal left triangle but with space after every star
    for k in range(0,i+1):
      print("* ",end="")
    print("\r")

if __name__ == '__main__':
    n = int(input())
    result = triangle(n)
    print(result)
