operators = set(['+','-','*','/','(',')','^'])
precedence = {'+':1,'-':1,'*':2,'/':2,'^':3}

def infix_to_postfix(expression):
  stack = []
  postfix_string = ''

  for character in expression:
    if character not in operators:
      postfix_string += character
    elif character == '(':
      stack.append('(')
    elif character == ')':
      while stack and stack[-1]!='(':
        postfix_string += stack.pop()
      stack.pop()
    else:
      while stack and stack[-1] !='(' and precedence[stack[-1]] >= precedence[character]:
        postfix_string += stack.pop()
      stack.append(character)
  while stack:
    postfix_string += stack.pop()
  return postfix_string

expression = input('Enter infix expression: ')
print('infix expression: ',expression)
print('postfix expression: ',infix_to_postfix(expression))

# input: a+b*(c^d-e)^(f+g*h)-i
# output: abcd^e-fgh*+^*+i-