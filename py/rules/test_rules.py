#~ from operator import xor
from pprint import pformat

class Rule:
    def __init__(self, cond):
        if isinstance(cond, str):
            self.cond = eval(cond)
        else:
            self.cond = cond
    def __call__(self):
        return self.cond

def rule(cond):
    if isinstance(cond, str):
        return lambda: eval(cond)
    else:
        return lambda: cond

class RuleSet:
    def __init__(self, relation=None, rules=[]):
        self.rules = []
        self.bools = []
        if relation and not rules:
            if isinstance(relation, str) and (relation not in ['and', 'or']):
                # it's probably a single solitary rule so treat as such
                self.add_rule(relation)
            if isinstance(relation, list):
                for r in relation:
                    self.add_rule(r)
        if relation and rules:
            self.add_ruleset(relation, rules)
        if rules:
            #assume 'and' relation
            self.add_ruleset(relation, rules)
            
    def value(self):
        return self.__call__()

    def add_rule(self, rule):
        if isinstance(rule, str):
            self.rules.append((None, [lambda: eval(rule)]))
        else:
            self.rules.append((None, [lambda: eval(rule)]))

    def add_ruleset(self, relation=None, rules=[]):
        self.rules.append((relation, rules))
        
    def __repr__(self):
        return 'RuleSet(%s) is: %s\n%s' % (id(self), self.value(), pformat(self.rules))
    
    def __call__(self):
        self.bools = []
        for relation, rules in self.rules:
            if relation == 'and':
                self.bools.append(lambda: all(r() for r in rules))
            elif relation == 'or':
                self.bools.append(lambda: any(r() for r in rules))
            elif relation == None:
                self.bools.append(lambda: all(r() for r in rules))
            else:
                raise Exception("default is 'and', must specify 'and' or 'or'")
        #~ print self.bools
        return bool(all(r() for r in self.bools))
        
    #~ def __xor__( self, other) 
    
    def __and__(self, other):
        if callable(other):
            return boo(self() and other())
        else:
            raise Exception('argument must be callable and return a bool value')
    
    def __or__(self, other):
        if callable(other):
            return bool(self() or other())
        else:
            raise Exception('argument must be callable and return a bool value')
            

def ruleset(relation, rules):
    if relation == 'and':
        return lambda: all(r() for r in rules)
    elif relation == 'or':
        return lambda: any(r() for r in rules)
    else:
        raise Exception("relation must be 'and' or 'or'")

lst = range(10)

r1 = rule(2 > 4)
r2 = rule('2 in lst')
r3 = rule('11 in lst') 

rs1 = ruleset('and', [ rule('3 > 2'), rule('0 < 1')])
rs2 = ruleset('or', [ rule('3 > 2'), rule('0 < 1'), ])

rs3 = RuleSet()
rs3.add_ruleset('and', [ rule('3 > 2'), rule('0 < 1')])
rs3.add_ruleset('or', [ rule('3 < 2'), rule('0 > 1')])
rs3.add_ruleset(rules=[rs1, rs2])

rs4 = RuleSet()
rs4.add_ruleset('and', [ rule('3 > 2'), rule('0 < 1')])
rs4.add_ruleset(rules=[rs3])


import yaml
rule_txt = '''
ruleset:
  - and:
    - role.approver in group.managers
    - proposal.benefits > proposal.risks
    - budget.value >= proposal.testing_cost
  - or:
    - proposal.approved_by.role == 'ggm'
'''

role = 'GGM'
class Company:
    managers = ['GGM', 'GM']
company = Company()
benefits = 10
risks = 20


rule_txt2 = '''
ruleset:
  - and:
    - role in company.managers
    - benefits > risks
'''

d = yaml.load(rule_txt2)

def get_rule_spec(dikt):
    rs = RuleSet()
    for relations in dikt['ruleset']:
        for relation in relations:
            print relation, relations[relation]
            rs.add_ruleset(relation, rules=[
                (lambda : eval(r)) for r in relations[relation]
            ])
    return rs

rs5 = get_rule_spec(d)
print rs5
risks = 5
print rs5


rs6 = RuleSet(['0>1', '1<0'])
print rs6


tests = {
    r1 : False,
    r2 : True,
    r3 : False,
    rs1 : True,
    rs2 : True,
    rs1 and rs2: True,  # Doesn't WORK!
    r1 or rs1: True,
    rs3: True,
    rs4: True,
    rs5: True,
    rs6: False,
}

def test():
    for rule in tests:
        assert rule() is tests[rule]
    print 'all tests pass -----------------------------------------> ok'


test()
