from easydict import EasyDict
import logging

logging.basicConfig(
    datefmt='%H:%M:%S',
    format='%(asctime)s - %(levelname)s - %(module)s.%(name)s - %(message)s',
    level=logging.INFO)
    
def get_log(instance):
    return logging.getLogger('{0}'.format(
        instance.__class__.__name__
    ))


class Action(object):
    def __init__(self, name):
        self.name = name
        self.log = get_log(self)
    
    def __call__(self):
        self.log.info(self)
        
    def __repr__(self):
        return "action('%s')" % self.name


class DecisionTable(object):
    def __init__(self, dtable):
        self.conditions = dtable['conditions']
        self.actions = dtable['actions']
        self.table = dtable['table']
        self.log = get_log(self)
    
    def evaluate(self, case):
        self.log.info('case: %s', case)
        conds = {}
        results = {}
        for c in self.conditions:
            conds[c] = eval(c, case)
        self.log.info('conds: %s', conds)
        
        for c in conds:
            results[self.conditions[c]] = conds[c]
        self.log.info('results: %s', results)
        for i in self.table:
            self.log.info("table[%s]['conditions']:%s", i, self.table[i]['conditions'])
            if self.table[i]['conditions'] == results:
                actions = self.table[i]['actions']
                self.log.info('actions: %s', actions)
                for a in sorted(actions.keys()):
                    if actions[a]:
                        self.actions[a]()

if __name__ == '__main__':

    dtable = dict(
        conditions = {
            'A == 0': 1,
            'B > 5' : 2,
            'A == C': 3,
        },

        actions = {
            1: Action('A is 0'),
            2: Action('B > 5'),
            3: Action('A == C'),
        },
        table = {
            0: { 
                'conditions': {
                    1: True,
                    2: True,
                    3: True,
                },
                'actions': {
                    1: True,
                    2: True,
                    3: True,
                }
            },
            1: { 
                'conditions': {
                    1: True,
                    2: None,
                    3: True,
                },
                'actions': {
                    1: True,
                    2: None,
                    3: True,
                }
            },
            2: { 
                'conditions': {
                    1: True,
                    2: None,
                    3: False,
                },
                'actions': {
                    1: True,
                    2: None,
                    3: None,
                }
            },
            3: { 
                'conditions': {
                    1: False,
                    2: True,
                    3: True,
                },
                'actions': {
                    1: None,
                    2: True,
                    3: True,
                }
            },
            4: { 
                'conditions': {
                    1: False,
                    2: True,
                    3: False,
                },
                'actions': {
                    1: None,
                    2: True,
                    3: None,
                }
            },
            5: { 
                'conditions': {
                    1: False,
                    2: False,
                    3: True,
                },
                'actions': {
                    1: None,
                    2: None,
                    3: True,
                }
            },
            6: { 
                'conditions': {
                    1: False,
                    2: False,
                    3: False,
                },
                'actions': {
                    1: None,
                    2: None,
                    3: None,
                }
            },
        }
    )

    dt = DecisionTable(dtable)    
    case1 = {'A': 0, 'B': 6, 'C': 0}
    dt.evaluate(case1)
    case2 = {'A': 0, 'B': 4, 'C': 0}
    dt.evaluate(case2)

    
