
class IExpression(object):
    def interpret(variables):
        "interprets the variables as {str: expression}"

class Number(IExpression):
    def __init__(self, number):
        self.number = number

    def interpret(self, variables):
        return self.number

class Binary(IExpression):
    def __init__(self, left, right):
        self.left = left
        self.right = right



class Plus(Binary):
    def interpret(self, variables):
        return self.left.interpret(variables) + self.right.interpret(variables)

class Minus(Binary):
    def interpret(self, variables):
        return self.left.interpret(variables) - self.right.interpret(variables)

class Variable(IExpression):
    def __init__(self, name):
        self.name = name
    def interpret(self, variables):
        var = variables.get(self.name)
        if not var:
            return
        else:
            return variables.get(self.name).interpret(variables)

class Evaluator(IExpression):
    def __init__(self, expression):
        self.expression = expression
        stack = []
        for token in expression.split():
            if token == '+':
                subexpression = Plus(stack.pop(), stack.pop())
                stack.append(subexpression)
            elif token == '-':
                right = stack.pop()
                left = stack.pop()
                subexpression = Minus(left, right)
                stack.append(subexpression)
            else:
                stack.append(Variable(token))
        self.syntax_tree = stack.pop()

    def interpret(self, context):
        return self.syntax_tree.interpret(context)



if __name__ == '__main__':
    e = "w x z -"
    sentence = Evaluator(e)
    
    d = {}
    d['w'] = Number(5)
    d['x'] = Number(10)
    d['z'] = Number(43)

    result = sentence.interpret(d)
    print result

