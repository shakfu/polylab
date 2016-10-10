"""
code from: http://lethain.com/genetic-algorithms-cool-name-damn-simple/

"""

from random import randint, random
from operator import add

def individual(length, min, max):
    """Create a member of the population"""
    return [ randint(min, max) for x in range(length) ]


def population(count, length, min, max):
    """
    Create a number of individuals (i.e. a population).

    count: the number of individuals in the population
    length: the number of values per individual
    min: the min possible value in an individual's list of values
    max: the max possible value in an individual's list of values

    """
    return [ individual(length, min, max) for x in range(count) ]

def fitness(individual, target):
     """
     Determine the fitness of an individual. Lower is better.

     individual: the individual to evaluate
     target: the sum of numbers that individuals are aiming for
     """
     return abs(target - sum(individual))

def grade(pop, target):
    """Find average fitness for a population"""
    total = sum(fitness(x, target) for x in pop)
    return total / float(len(pop))

def evolve(pop, target, retain=0.2, random_select=0.05, mutate=0.01):
    graded = [ (fitness(x, target), x) for x in pop]
    graded = [ x[1] for x in sorted(graded)]
    retain_length = int(len(graded)*retain)
    parents = graded[:retain_length]

    # randomly add other individuals to promote genetic diversity
    for individual in graded[retain_length:]:
        if random_select > random():
            parents.append(individual)

    # mutate some individuals
    for individual in parents:
        if mutate > random():
            pos_to_mutate = randint(0, len(individual)-1)
            # this mutation is not ideal, because it
            # restricts the range of possible values,
            # but the function is unaware of the min/max
            # values used to create the individuals,
            individual[pos_to_mutate] = randint(
                min(individual), max(individual))

    # crossover parents to create children
    parents_length = len(parents)
    desired_length = len(pop) - parents_length
    children = []
    while len(children) < desired_length:
        male = randint(0, parents_length-1)
        female = randint(0, parents_length-1)
        if male != female:
            male = parents[male]
            female = parents[female]
            half = len(male) / 2
            child = male[:half] + female[half:]
            children.append(child)

    parents.extend(children)
    return parents


if __name__ == '__main__':
    target = 371
    p_count = 100
    i_length = 5
    i_min = 0
    i_max = 100
    p = population(p_count, i_length, i_min, i_max)
    fitness_history = [grade(p, target),]
    for i in range(100):
        p = evolve(p, target)
        fitness_history.append(grade(p, target))

    for datum in fitness_history:
        print datum
