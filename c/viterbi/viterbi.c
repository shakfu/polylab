
#include "common.h"

/*

declaring 2d array:
    type arrayname[rows][cols];

Algorithm:

    For a Hidden Markov Model (HMM) with state space S,
    initial probabilities of being in state i
    and transition probabilities of transitioning
    from state i to state j. Say we observe outputs.

    The viterbi algorithm provides the most likely state sequence
    that produces the observations.

    uses:
        max
        argmax(n, array):
            returns index of maximum value in an array
        or
        argmax(func f, n, array):
            returns index of max value of f over array values



    see: http://en.wikipedia.org/wiki/Viterbi_algorithm
*/

#define DOUBLE_MIN -2147483648.0

typedef char* string;

typedef struct Model {
    // inputs
    string name;
    int n_states;
    string* states;
    int n_observations;
    string* observations;
    double* start_prob;
    double** transitions;
    double** emissions;
    // outputs
    double** result;
    int** path;

} Model;


Model* model_create(string name, int n_states, string* states,
                    int n_observations, string* observations,
                    double start_prob[n_states],
                    double transitions[][n_states],
                    double emissions[][n_observations])
{
    Model* model;
    model = malloc(sizeof(*model));

    // init name
    model->name = strdup(name);

    // init states (K)
    model->n_states = n_states;
    model->states = malloc(n_states * sizeof(string));
    foreach (i, n_states) {
        model->states[i] = strdup(states[i]);
    }

    // NOTE: need to add observations space != seq of observations
    // http://en.wikipedia.org/wiki/Viterbi_algorithm


    // init observations
    model->n_observations = n_observations;
    model->observations = malloc(n_observations * sizeof(string));
    foreach (i, n_observations) {
        model->observations[i] = strdup(observations[i]);
    }

    // init start_prob (n_states)
    model->start_prob = calloc(n_states, sizeof(double));
    foreach (i, n_states) {
        model->start_prob[i] = start_prob[i];
    }

    // init transitions (n_states * n_states)
    model->transitions = malloc(n_states * sizeof(double*));
    foreach (i, n_states) {
        model->transitions[i] = calloc(n_states, sizeof(double));
    }
    foreach (i, n_states) {
        foreach (j, n_states) {
            model->transitions[i][j] = transitions[i][j];
        }
    }

    // init emissions (n_states * n_observations)
    model->emissions = malloc(n_states * sizeof(double*));
    foreach (i, n_states) {
        model->emissions[i] = calloc(n_observations, sizeof(double));
    }
    foreach (i, n_states) {
        foreach (j, n_observations) {
            model->emissions[i][j] = emissions[i][j];
        }
    }

    // init result
    model->result = malloc(model->n_states * sizeof(double*));
    foreach (i, model->n_states) {
        model->result[i] = calloc(model->n_observations, sizeof(double));
    }

    // init path
    model->path = malloc(model->n_states * sizeof(int*));
    foreach (i, model->n_states) {
        model->path[i] = calloc(model->n_observations, sizeof(int));
        // set cell to -1 to show when it hasn't been written
        foreach (j, model->n_observations) {
            model->path[i][j] = -1;
        }
    }

    return model;
}

void model_destroy(Model* m)
{
    // name
    free(m->name);

    // free state memory
    if (m->states != NULL) {
        foreach (i, m->n_states) {
            free(m->states[i]);
        }
    }
    free(m->states);

    // free observations memory
    if (m->observations != NULL) {
        foreach (i, m->n_observations) {
            free(m->observations[i]);
        }
    }
    free(m->observations);

    // free start_prob
    if (m->start_prob != NULL) {
        free(m->start_prob);
    }

    // free transitions memory
    if (m->transitions != NULL) {
        foreach (i, m->n_states) {
            free(m->transitions[i]);
        }
    }
    free(m->transitions);

    // free emissions memory
    if (m->emissions != NULL) {
        foreach (i, m->n_states) {
            free(m->emissions[i]);
        }
    }
    free(m->emissions);

    // free result
    if (m->result != NULL) {
        foreach (i, m->n_states) {
            free(m->result[i]);
        }
    }
    free(m->result);

    // free path
    if (m->path != NULL) {
        foreach (i, m->n_states) {
            free(m->path[i]);
        }
    }
    free(m->path);

    // free model
    if (m != NULL) {
        free(m);
    }
}

void model_display(Model* m)
{
    printf("Model: %s\n", m->name);

    foreach (i, m->n_states) {
        printf("states[%i]: %s\n", i, m->states[i]);
    }

    foreach (i, m->n_observations) {
        printf("observations[%i]: %s\n", i, m->observations[i]);
    }

    foreach (i, m->n_states) {
        printf("start_prob[%i] -> %f\n", i, m->start_prob[i]);
    }

    foreach (i, m->n_states) {
        foreach (j, m->n_states) {
            printf("transitions[%i][%i] -> %f\n", i, j, m->transitions[i][j]);
        }
    }

    foreach (i, m->n_states) {
        foreach (j, m->n_observations) {
            printf("emissions[%i][%i] -> %f\n", i, j, m->emissions[i][j]);
        }
    }

    foreach (i, m->n_states) {
        foreach (j, m->n_observations) {
            printf("result[%i][%i] -> %f\n", i, j, m->result[i][j]);
        }
    }

    foreach (i, m->n_states) {
        foreach (j, m->n_observations) {
            printf("path[%i][%i] -> %i\n", i, j, m->path[i][j]);
        }
    }
}

// returns index of maximum value in an array
int argmax1(int length, double array[length])
{
    double max = DOUBLE_MIN;
    int index = 0;
    foreach (i, length) {
        if (array[i] > max) {
            max = array[i];
            index = i;
        }
    }
    return index;
}


typedef double (*score_func)(double x);

// returns index of arg for f in an array which returns max value
double argmax(int length, double array[length], score_func func)
{
    double max = DOUBLE_MIN;
    double score = 0.0;
    int index = 0;
    foreach (i, length) {
        score = func(array[i]);
        if (score > max) {
            max = score;
            index = i;
        }
    }
    return index;
}

// returns maximum value in an array
double max(int length, double array[length])
{
    double value = 0.0;
    for (int i = 0; i < length; i++) {
        if (array[i] > value) {
            value = array[i];
        }
    }
    return value;
}


// states = ('Rainy', 'Sunny')

// observations = ('walk', 'shop', 'clean')

// start_probability = {'Rainy': 0.6, 'Sunny': 0.4}

// transition_probability = {
//    'Rainy' : {'Rainy': 0.7, 'Sunny': 0.3},
//    'Sunny' : {'Rainy': 0.4, 'Sunny': 0.6},
//    }

// emission_probability = {
//    'Rainy' : {'walk': 0.1, 'shop': 0.4, 'clean': 0.5},
//    'Sunny' : {'walk': 0.6, 'shop': 0.3, 'clean': 0.1},
//    }

/*
def viterbi(obs, states, start_p, trans_p, emit_p):
    V = [{}]
    path = {}

    # Initialize base cases (t == 0)
    for y in states:
        V[0][y] = start_p[y] * emit_p[y][obs[0]]
        path[y] = [y]

    # Run Viterbi for t > 0
    for t in range(1, len(obs)):
        V.append({})
        newpath = {}

        for y in states:
            (prob, state) = max((V[t-1][y0] * trans_p[y0][y] *
emit_p[y][obs[t]], y0) for y0 in states) V[t][y] = prob newpath[y] =
path[state] + [y]

        # Don't need to remember the old paths
        path = newpath
    n = 0           # if only one element is observed max is sought in the
initialization values if len(obs)!=1: n = t print_dptable(V) (prob, state) =
max((V[n][y], y) for y in states) return (prob, path[state])
*/

void viterbi(Model* m)
{
    double max_p = 0.0;
    double prob = 0.0;
    // int newpath = 0;
    int state, t;

    // initialize base cases (t==0) with initial probabilities
    foreach (i, m->n_states) {
        m->result[i][0] = m->start_prob[i] * m->emissions[i][0];
        m->path[i][0] = i;
        // debug("t0 -> %s : [%s]", m->states[i], m->states[m->path[i][0]]);
    }


    // run viterbi for t > 0
    for (t = 1; t < m->n_observations; t++) {
        for (int y = 0; y < m->n_states; y++) {
            max_p = 0.0;
            state = 0;
            for (int y0 = 0; y0 < m->n_states; y0++) {
                prob = m->result[y0][t - 1] * m->transitions[y0][y]
                    * m->emissions[y][t];
                if (prob > max_p) {
                    max_p = prob;
                    state = y0;
                    debug("[t%i][%i][%i] -> (%f, %s)", t, y, y0, max_p,
                          m->states[state]);
                } else {
                    debug("[t%i][%i][%i] =* (%f, %s)", t, y, y0, max_p,
                          m->states[state]);
                }
            }
            m->result[y][t] = max_p;
            m->path[y][t] = state;
        }
    }

    // if only one element is observed max is sought in the initialization
    // values
    int n = 0;
    if (m->n_observations != 1) {
        n = t - 1;
    }
    // (prob, state) = max((V[n][y], y) for y in states)
    max_p = prob = 0;
    for (int i = 0; i < m->n_states; i++) {
        // debug("n: %i", n);
        prob = m->result[i][n];
        if (prob > max_p) {
            max_p = prob;
            state = i;
        }
    }
    // should be (0.0134, ['Sunny', 'Rainy', 'Rainy'])
    debug("   (0.013440, Rainy: [Sunny, Rainy, Rainy])");
    debug("=> (%f, %s: [%s, %s, %s])", max_p, m->states[state],
          m->states[m->path[state][0]], m->states[m->path[state][1]],
          m->states[m->path[state][2]]);

    // debug
    printf("\n");
    foreach (i, m->n_states) {
        foreach (j, m->n_observations) {
            debug("[%i][%i] : [%s][%i] -> (%f, %s)", i, j, m->states[i], j,
                  m->result[i][j],
                  m->path[i][j] >= 0 ? m->states[m->path[i][j]] : "NONE");
        }
    }
}

int main()
{
    debug("START");

    string model_name = "Hidden Markov Model";
    int n_states = 2, n_observations = 3;
    string states[] = { "Rainy", "Sunny" };
    string observations[] = { "Walk", "Shop", "Clean" };
    double start_prob[2] = { 0.6, 0.4 };
    double transitions[2][2] = { { 0.7, 0.3 }, { 0.4, 0.6 } };
    double emissions[2][3] = { { 0.1, 0.4, 0.5 }, { 0.6, 0.3, 0.1 } };

    Model* m = model_create(model_name, n_states, states, n_observations,
                            observations, start_prob, transitions, emissions);

    // run viterbi
    viterbi(m);

    // model_display(m);
    model_destroy(m);

    // test max
    // double val = max(2, start_prob);
    // debug("max: %f", val);

    debug("END");
}
