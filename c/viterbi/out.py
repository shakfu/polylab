states: ('Rainy', 'Sunny')

observations: ('walk', 'shop', 'clean')

start_probability: {'Rainy': 0.6, 'Sunny': 0.4}

transition_probability:
	Rainy -> {'Rainy': 0.7, 'Sunny': 0.3}
	Sunny -> {'Rainy': 0.4, 'Sunny': 0.6}

emission_probability:
	Rainy -> {'shop': 0.4, 'clean': 0.5, 'walk': 0.1}
	Sunny -> {'shop': 0.3, 'clean': 0.1, 'walk': 0.6}

viterbi:
t 0 path: {'Rainy': ['Rainy'], 
           'Sunny': ['Sunny']}

(prob, state): (0.0384, 'Sunny')
(prob, state): (0.0432, 'Sunny')

t 1 path: {'Rainy': ['Sunny', 'Rainy'], 
           'Sunny': ['Sunny', 'Sunny']}

(prob, state): (0.01344, 'Rainy')
(prob, state): (0.00259, 'Sunny')

t 2 path: {'Rainy': ['Sunny', 'Rainy', 'Rainy'], 
           'Sunny': ['Sunny', 'Sunny', 'Sunny']}

          0       1       2
Rainy: 0.06000 0.03840 0.01344
Sunny: 0.24000 0.04320 0.00259

final path: {'Rainy': ['Sunny', 'Rainy', 'Rainy'], 
             'Sunny': ['Sunny', 'Sunny', 'Sunny']}

(0.01344, ['Sunny', 'Rainy', 'Rainy'])
