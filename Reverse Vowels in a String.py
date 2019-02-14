# Function to 
# reverse a string 
def isVowel(char):
  return (char=='a' or char=='e' or 
          char=='i' or char=='o' or
          char=='u')

def swap(str, i, j):
  str = list(str)
  str[i], str[j] = str[j], str[i]
  return ''.join(str)

def reverseVowels(str):
    i = 0 
    j = len(str)-1 

    #Start two indexes from two corners 
    #and move toward each other
    while i < j:
      if not isVowel(str[i]):
        i+=1
        continue
      if not isVowel(str[j]):
        j-=1
        continue
      str = swap(str,i,j)
      i += 1
      j -= 1
    print(str)   
  
# Driver Code  
def main(): 
    str = "hello world"; 
    reverseVowels(str); 
      
if __name__=="__main__": 
    main()      
      
