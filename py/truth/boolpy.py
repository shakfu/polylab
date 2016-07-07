"""
BoolType class and symbols


TO-DO

Implement an output strategy that results in the 'ideal output.'

    eg.     x + 1 = 1   (Theorem 1)
            Final Result: X + 1 = 1


            Z(~Z + Y) + Y = Z(~Z) + ZY + Y  (Theorem 16)
            Z(~Z) + ZY + Y = 0 + ZY + Y     (Theorem whatever)
            0 + ZY + Y = ZY + Y             (Theorem 3)
            ZY + Y = Z(Y + 1)               (Theorem 14)
            Z(Y + 1) = Z(1)                 (Theorem 2)
            Z(1) = Z                        (Theorem 5)

            Final Result: Z(~Z + Y) + Y = Z

    This will likely be done using a global flag during object creation. A global flag
    may affect the other functions, so this will need to be done carefully.

    Additionally, a keyword argument may be added to the And() and Or() functions. This
    may change some of the code, i.e. taking out z + y from the literal code.

    The output will involve storing the str representation of the object for later output.

    Any time a theorem is actually applied, the flag will be set and a number indicating which theorem
    has been applied will be stored for output.

        eg. if arg == 0
                theorem_applied = (True, 2)
                continue
    Then, the item in the first position of the tuple determines which action to take
    when the "theorems" are checked. If theorem_applied = False, just add the terms and move on. That
    way, only ONE change will be made at a time.
"""
from itertools import combinations, chain
from collections import defaultdict
import re

class BoolType(object):
    def __new__(cls):
        #print "In __new__"
        obj = object.__new__(cls)
        return obj

    def __add__(self, other):
        return Or(self, other)

    def __radd__(self, other):
        return Or(other, self)

    def __mul__(self, other):
        return And(self, other)

    def __rmul__(self, other):
        return And(other, self)

class Symbol(BoolType):
    def __new__(cls, name, negated = False):
        obj = BoolType.__new__(cls)
        obj.name = name

        # Reference to the negated version of the Symbol
        obj.neg = None

       #Assumptions
        obj.isAnd = False
        obj.isOr = False
        obj.isSingle = True
        obj.negated = negated

        return obj

    def __str__(self):
        str = self.name
        return str
    def __repr__(self):
        return str(self)
    def __gt__(self, other):
        if not other.isSingle:
            return True
        elif self.negated:
            if other.negated:
                if other.name[1] < self.name[1]:
                    return True
            else:
                if other.name < self.name[1]:
                    return True
        else:
            if other.negated:
                if other.name[1] < self.name:
                    return True
            else:
                if other.name < self.name:
                    return True

class Zero(Symbol):
    def __new__(cls):
        obj = Symbol.__new__(cls, "0")
        return obj
class One(Symbol):
    def __new__(cls):
        obj = Symbol.__new__(cls, "1")
        return obj
one = One()
zero = Zero()
one.neg = zero
zero.neg = one


###############################
#### AND, OR, and boolExpr ####
###############################

theorem_applied = (False, 0)
def get_theorem_applied():
    global theorem_applied
    return theorem_applied

def reset_theorem_applied():
    global theorem_applied
    theorem_applied = (False, 0)
    return

class boolExpr(BoolType):
    """
    __new__ should be defined here, in which a sort of "canonicalization" can occur, and
    then the appropriate __new__ method can be called
    """
    def __new__(cls, *args):
        """
        The appropriate cls.flatten() should be called for canonicalization of arguments
        """
        args = list(args)
        seq = cls.flatten(args)
        
        #either seq will contain One, Zero, or a list with one element, or a list of args to become a BoolExpr
        if isinstance(seq, Symbol):
            if seq.name == "1":
                return seq
            elif seq.name == "0":
                return seq
        elif len(seq) == 1:
            return seq[0]
        else:
            obj = BoolType.__new__(cls)

        seq.sort()
        obj.args = tuple(seq)

        #Assumptions
        if isinstance(obj, And):
            obj.isAnd = True
            obj.isOr = False
            obj.isSingle = False
        elif isinstance(obj, Or):
            obj.isAnd = False
            obj.isOr = True
            obj.isSingle = False
        elif isInstance(obj, Symbol):
            obj.isAnd = False
            obj.isOr = False
            obj.isSingle = True

        return obj


class And(boolExpr):

    @classmethod
    def flatten(cls, seq):
        """

        """
        terms = []
        global theorem_applied
        for arg in seq:
            if theorem_applied[0] == True:
                if arg.isAnd:
                    seq.extend(arg.args)
                    continue
                else:
                    terms.append(arg)
                    continue
            else:
                if isinstance(arg, One):
                    theorem_applied = (True, 2)
                    continue
                elif isinstance(arg, Zero):
                    theorem_applied = (True, 1)
                    return arg
                elif arg.isAnd:
                    seq.extend(arg.args)
                    continue
                else:
                    if arg.isSingle and arg.neg in terms:
                        theorem_applied = (True, 4)
                        return zero
                    elif arg in terms:
                        theorem_applied = (True, 3)
                        continue
                    else:
                        terms.append(arg)
                        continue


        return terms

    def __str__(self):

        reverse_args = list(self.args)
        reverse_args.reverse()
        if reverse_args[0].isOr or reverse_args[0].negated:
            resultstr = "(" + str(reverse_args[0]) + ")"
        else:
            resultstr = str(reverse_args[0])
        for arg in reverse_args[1:]:
            if arg.isOr:
                resultstr += "(" + str(arg) + ")"
            elif arg.negated:
                resultstr += "(" + str(arg) + ")"
            elif isinstance(arg, One) or isinstance(arg, Zero):
                resultstr += "(" + str(arg) + ")"
            else:
                resultstr += str(arg)
        return resultstr

    def __repr__(self):
        if self.args[0].isOr:
            resultstr = "(" +  self.args[0].__repr__() + ")"
        else:
            resultstr = self.args[0].__repr__()
        for arg in self.args[1:]:
            if arg.isOr:
                resultstr += " * (" + arg.__repr__() + ")"
            else:
                resultstr += " * " + arg.__repr__()
        return resultstr

    def __gt__(self, other):
        if other.isOr:
            return True
        elif other.isSingle:
            return False

    def __eq__(self, other):
        if not isinstance(other, BoolType):
            return False
        elif not other.isAnd:
            return False
        else:
            if len(self.args) != len(other.args):
                return False
            for arg in self.args:
                if not arg in other.args:
                    return False
        return True

class Or(boolExpr):

    @classmethod
    def flatten(cls, seq):
        """
        Will put the args in seq in an appropriate order for evaluation
        """
        terms = []
        global theorem_applied
        for arg in seq:
            if theorem_applied[0] == True:
                if arg.isOr:
                    seq.extend(arg.args)
                else:
                    terms.append(arg)
            else:
                if isinstance(arg, Zero):
                    theorem_applied = (True, 5)
                    continue
                elif isinstance(arg, One):
                    theorem_applied = (True, 6)
                    return arg
                elif arg.isOr:
                    seq.extend(arg.args)
                elif arg in terms:
                    theorem_applied = (True, 7)
                    continue
                elif arg.isSingle and arg.neg in terms:
                    theorem_applied = (True, 8)
                    return one
                else:
                    terms.append(arg)


        return terms

    def __str__(self):
        return_string = str(self.args[0])
        for arg in self.args[1:]:
            return_string += " + " + str(arg)
        return return_string

    def __repr__(self):
        return_string = self.args[0].__repr__()
        for arg in self.args[1:]:
            return_string += " + " + arg.__repr__()
        return return_string

    def __lt__(self, other):
        if not other.isOr:
            return True

def SOP(expr):
    """
    Takes an And or Or expression and puts it in Sum of Product (SOP) form

    Since expressions are not sorted, we can depend on the Or to be in the front of the args list,
    which will be useful for the SOP algorithm. 
    """
    global theorem_applied
    #if expr.isAnd
    while expr.isAnd:
        #if the first argument is an or
        new_or_terms = []
        if expr.args[0].isOr:
            theorem_applied = (True, "13b - Sum of Products")
            #multiply it by the next arg
            if expr.args[1].isOr:
                for a in expr.args[0].args:
                    for b in expr.args[1].args:
                        or_term = a*b
                        new_or_terms.append(or_term)
            else:
                b = expr.args[1]
                for a in expr.args[0].args:
                    or_term = a * b
                    new_or_terms.append(or_term)
        #Modify the original AND to be an AND with the first 2 arguments replaced by an OR of the products
            new_arg_list = tuple(new_or_terms)
            new_arg = Or(*new_arg_list) #Create an Or with the new arguments
            rest_of_args = tuple(expr.args[2:])
            expr = And(new_arg, *rest_of_args)
        else:
            break

    #if it's an OR expression
    if expr.isOr:
        args_list = []
        #iterate over arguments
        for arg in expr.args:
            #if the argument is an And, and it has an Or as its first argument, call SOP on that argument.
            if arg.isAnd:
                if arg.args[0].isOr:
                    new_arg = SOP(arg)
                    args_list.append(new_arg)
                    continue
                else:
                    args_list.append(arg)
            else:
                args_list.append(arg)
        args_list = tuple(args_list)
        expr = Or(*args_list)
    return expr

def neg(expr):
        terms = []
        global theorem_applied
        if expr.isAnd:
            theorem_applied = (True, "17 - DeMorgan's Law")
            for arg in expr.args:
                if arg.isSingle:
                    terms.append(arg.neg)
                    continue
                elif arg.isOr:
                    terms.append(neg(arg))
                    continue
                else:
                    print "Error"
            terms.sort()
            new_args = tuple(terms)
            obj = Or(*new_args)
            expr = obj
        elif expr.isOr:
            theorem_applied = (True, "16 - DeMorgan's Law")
            for arg in expr.args:
                if arg.isSingle:
                    terms.append(arg.neg)
                    continue
                elif arg.isAnd:
                    terms.append(neg(arg))
                    continue
                else:
                    print "Error"
            terms.sort
            new_args = tuple(terms)
            obj = And(*new_args)
            expr = obj
        else:
            return expr.neg
        return expr

def factor(expr):
    global theorem_applied
    if expr in [zero, one]:
        return expr
    elif expr.isSingle:
        return expr
    elif expr.isAnd:
        return expr
    set_list = []
    for arg in expr.args:
        if arg.isSingle:
            single_set = set()
            single_set.add(arg)
            set_list.append(single_set)
        else: 
            set_list.append(set(arg.args))


    # take the powerset of this list
    powset_list = list(powerset(set_list))
    # find the appropriate intersections between the sets in the powerset
    factor_length_dict = defaultdict(list)
    intersect_dict = dict()

    # put the factor, subset pairs into a dict
    # duplicates will not exist, meaning that each factor factors from all the terms that have it
    for subset in powset_list[len(set_list) + 1:]:
        subset_list = list(subset)
        intersect = tuple(set.intersection(*subset_list))
        if len(intersect) > 0:
            intersect_dict[intersect] = subset
    if len(intersect_dict) > 0 :
        theorem_applied = (True, "13a - Factoring")
    # add each (factor, terms) pair to a dict where the keys are the lengths of each factor
    for factor, terms in intersect_dict.iteritems():
        factor_length_dict[len(factor)].append((factor, terms))

    # get the list of (len, [(factor, terms),...]) pairs and sort them by length
    factor_length_list = factor_length_dict.items()
    factor_length_list.sort(reverse = True)

    if len(factor_length_list) == 0:
        return expr

    # from this list, rebuild the equation
    newexpr_args = []
    for length, terms in factor_length_list:
        if len(set_list) == 0:
            break
####################################################### The answer I'm looking for lies below. I need to find how to not add redundant things back into the expression. 
        remove_later = set()
        for factor, factor_terms in terms:
            newargs = []
            for item in factor_terms:
                remove_later.add(tuple(item)) # to be removed from set_list
                newarg = set.difference(item, set(factor))
                newarg = tuple(newarg)

                if len(newarg) > 0:
                    if len(newarg) == 1:
                        newargs.append(newarg[0]) #A Symbol in this case
                    else:
                        newargs.append(And(*newarg)) #Multiple Symbols to be multiplied together
                else:
                    newargs.append(one)

            newexpr_args.append(And(*factor)*Or(*newargs)) # Collects all of the factored terms to be ORed together

        # remove the terms from remove_later
        for item in remove_later:
            set_list.remove(set(item))

    # return the equation for evaluation
    return Or(*newexpr_args)


def powerset(iterable):
    """powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"""
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))

def format_input(expr_str):
    """
    Formats expr_str to be an eval-able statement in Python

    An exp_str may contain ~x, or ~(x + y). This function makes use
    of the re module to find and replace these things with
    syntactically correct input.

    expr_str = ~x + y
    expr_str = format_input(expr_str) >> results in: expr_str = x.neg + y

    expr_str = ~(x + y)
    expr_str = format_input(expr_str) >> results in: expr_str = neg(x + y)
    """

    #re to match a '~' followed by a variable
    neg_var_re = re.compile(r"~(?P<variable>[xyzabcd01])")
    #replace that with 'variable'.neg
    expr_str = re.sub(neg_var_re, r"\g<variable>.neg",  expr_str,)

    #replace all 0's with zero and 1's with one
    expr_str = expr_str.replace("1", "one")
    expr_str = expr_str.replace("0", "zero")

    #re to match a '~' followed by parenthesees
    neg_expr_re = re.compile(r"~\(")
    #replace the '~' with 'neg'
    expr_str = re.sub(neg_expr_re, "neg(", expr_str)



    return expr_str

def evaluate_expr(expr_string, print_str):

        while True:
            result =  print_str + " = "
            expr_string = format_input(expr_string)
            e = eval(expr_string)
            expr_string = e.__repr__()
            result += str(e)
            theorem_applied = get_theorem_applied()
            if theorem_applied[0] == True:
                result += "\t(Theorem " + str(theorem_applied[1]) + ")"
                print result
            if theorem_applied[0] == False:
                break
            reset_theorem_applied()
            print_str = str(e)
        return e

def do_SOP(e):

    result = str(e) + " = "
    e = SOP(e)
    expr_string = e.__repr__()
    theorem_applied = get_theorem_applied()
    if theorem_applied[0] == True:
        result += str(e) + "\t (Theorem " + str(theorem_applied[1]) + ")"
        print result
    reset_theorem_applied()
    return e

def do_factor(e):

    result = str(e) + " = "
    e = factor(e)
    expr_string = e.__repr__()
    theorem_applied = get_theorem_applied()
    if theorem_applied[0] == True:
        result += str(e) + "\t (Theorem " + str(theorem_applied[1]) + ")"
        print result
    reset_theorem_applied()
    return e

if __name__ == "__main__":
    x = Symbol('x')
    not_x = Symbol('~x', True)
    not_x.neg = x
    x.neg = not_x

    y = Symbol('y')
    not_y = Symbol('~y', True)
    not_y.neg = y
    y.neg = not_y

    z = Symbol('z')
    not_z = Symbol('~z', True)
    not_z.neg = z
    z.neg = not_z

    a = Symbol('a')
    not_a = Symbol('~a', True)
    not_a.neg = a
    a.neg = not_a

    b = Symbol('b')
    not_b = Symbol('~b', True)
    not_b.neg = b
    b.neg = not_b

    c = Symbol('c')
    not_c = Symbol('~c', True)
    not_c.neg = c
    c.neg = not_c

    d = Symbol('d')
    not_d = Symbol('~d', True)
    not_d.neg = d
    d.neg = not_d


    while True:
        #ask for input
        expr_string = raw_input("Enter a Boolean expression, or -1 to quit: ")

        if str(expr_string) == "-1":
            break
        #if input is 0, break
        #evaluate the input - the input will be a string. it can go straight into the first part of the evaluation
        e = evaluate_expr(expr_string, expr_string)

        #Put in SOP form
        e = do_SOP(e)

        #Evaluate
        expr_string = e.__repr__()
        print_str = str(e)
        e = evaluate_expr(expr_string, print_str)

        #Factor
        e = do_factor(e)

        #Evaluate
        expr_string = e.__repr__()
        print_str = str(e)
        e = evaluate_expr(expr_string, print_str)

        #Print Results
        print "Final Result: " + str(e) + "\n"
