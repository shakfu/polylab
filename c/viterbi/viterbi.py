# states = ('Healthy', 'Fever')

# observations = ('normal', 'cold', 'dizzy')

# start_probability = {'Healthy': 0.6, 'Fever': 0.4}

# transition_probability = {
#    'Healthy' : {'Healthy': 0.7, 'Fever': 0.3},
#    'Fever' : {'Healthy': 0.4, 'Fever': 0.6}
#    }

# emission_probability = {
#    'Healthy' : {'normal': 0.5, 'cold': 0.4, 'dizzy': 0.1},
#    'Fever' : {'normal': 0.1, 'cold': 0.3, 'dizzy': 0.6}
#    }

states = ('Rainy', 'Sunny')

observations = ('walk', 'shop', 'clean')
 
start_probability = {'Rainy': 0.6, 'Sunny': 0.4}
 
transition_probability = {
   'Rainy' : {'Rainy': 0.7, 'Sunny': 0.3},
   'Sunny' : {'Rainy': 0.4, 'Sunny': 0.6},
   }
 
emission_probability = {
   'Rainy' : {'walk': 0.1, 'shop': 0.4, 'clean': 0.5},
   'Sunny' : {'walk': 0.6, 'shop': 0.3, 'clean': 0.1},
   }

def viterbi(obs, states, start_p, trans_p, emit_p):
    V = [{}]
    path = {}

    # Initialize base cases (t == 0)
    for y in states:
        V[0][y] = start_p[y] * emit_p[y][obs[0]]
        path[y] = [y]
    
    # print 't', 0, 'path:', path

    # Run Viterbi for t > 0
    for t in range(1, len(obs)):
        V.append({})
        newpath = {}

        for y in states:
            (prob, state) = max((V[t-1][y0] * trans_p[y0][y] * emit_p[y][obs[t]], y0) for y0 in states)
            # print '(prob, state):', (prob, state)
            V[t][y] = prob
            newpath[y] = path[state] + [y]

        # Don't need to remember the old paths
        path = newpath
        # print 't', t, 'path:', path

    n = 0           # if only one element is observed max is sought in the initialization values
    if len(obs) != 1:
        n = t
    print_dptable(V)
    (prob, state) = max((V[n][y], y) for y in states)
    # print 'final path:', path
    return (prob, path[state])

# Don't study this, it just prints a table of the steps.
def print_dptable(V):
    s = "    " + " ".join(("%7d" % i) for i in range(len(V))) + "\n"
    for y in V[0]:
        s += "%.5s: " % y
        s += " ".join("%.7s" % ("%f" % v[y]) for v in V)
        s += "\n"
    print(s)

def example():
    def dump(dikt):
        for key in dikt:
            print '\t', key, '->', dikt[key]
    from pprint import pprint as pp
    print 'states:', states
    print 'observations:', observations
    print 'start_probability:', start_probability
    print 'transition_probability:'
    dump(transition_probability)
    print 'emission_probability:'
    dump(emission_probability)
    print
    print 'viterbi:'
    print viterbi(observations,
                   states,
                   start_probability,
                   transition_probability,
                   emission_probability)
example()

