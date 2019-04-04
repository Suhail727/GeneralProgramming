# Check whether two strings are anagram of each other

### Using Counter
from collections import Counter 
  
def anagram(input1, input2): 
  
    # Counter() returns a dictionary data 
    # structure which contains characters  
    # of input as key and their frequencies 
    # as it's corresponding value 

    # When we compare two dictionaries then it compares three checks in order 
    # number of keys (if they don’t match, the dicts are not equal), 
    # names of keys (if they don’t match, they’re not equal) 
    # and value of each key (they have to be ‘==’, too)
    return Counter(input1) == Counter(input2) 


### Using Sorting
def anagram_sorted(input1, input2): 
  
  # If lenght of both strings is not same, then  
    # they cannot be anagram  
  if len(input1)!= len(input2):
    return False
  # Sort both strings 
  str1 = sorted(input1)
  str2 = sorted(input2)
  # Compare sorted strings
  for i in range (0,len(str1)):
    if str1[i] != str2[i]:
      return False
  return True


### Count characters
#This method assumes that the set of possible characters in both strings is small. 
# In the following implementation, it is assumed that the 
# characters are stored using 8 bit and there can be 256 possible characters.
def anagram_count(str1, str2): 

  NO_OF_CHARS = 256
  # Create two count arrays and initialize all values as 0 
  count1 = [0] * NO_OF_CHARS 
  count2 = [0] * NO_OF_CHARS 

  # For each character in input strings, increment count 
  # in the corresponding count array 
  for i in str1: 
      count1[ord(i)]+= 1

  for i in str2: 
      count2[ord(i)]+= 1

  # If both strings are of different length. Removing this 
  # condition will make the program fail for strings like 
  # "aaca" and "aca" 
  if len(str1) != len(str2): 
      return 0

  # Compare count arrays 
  for i in xrange(NO_OF_CHARS): 
      if count1[i] != count2[i]: 
          return 0

# Driver function 
if __name__ == "__main__": 
    input1 = 'abcd'
    input2 = 'dcab'
    print (anagram_sorted(input1, input2))


