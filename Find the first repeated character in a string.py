def firstRepeatedChar(str): 
  
    h = {}  # Create empty set
  
    # Traverse each characters in string 
    # in lower case order 
    for ch in str: 
  
        # If character is already present 
        # in hash, return char 
        if ch in h: 
            return ch; 
  
        # Add ch to hash
        else: 
            h[ch] = 0
  
    return '\0'

if __name__ == '__main__':
    n = str(input())
    result = firstRepeatedChar(n)
    print(result)
