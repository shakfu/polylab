import math
import random


class Individual:
    """
    Manages an individual
    """
    DEFAULT_GENE_LENGTH = 64

    def __init__(self):
        self.genes = []
        self.fitness = 0

    def generate(self):
        for i in range(self.DEFAULT_GENE_LENGTH):
            self.genes.append(int(round(random.random())))



class Population:
    """
    Manages all individuals of a population
    """
    individuals = []

    def __init__(self, size: int, init: bool = True):
        self.size = size
        self.init = init
