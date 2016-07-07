
#include "common.h" 

/*  declaring 2d array:
 *      type arrayname[rows][cols];
 */

typedef char* string;

typedef struct Model {
    // inputs
    string name;
    int n_states;
    string *states;
    int n_observations;
    string *observations;
    double *start_prob;
    double **transitions;
    double **emissions;
    // outputs
    double **result;
    int    **path;
} Model;


Model * model_create(string name,
                     int n_states, string *states,
                     int n_observations, string *observations,
                     double start_prob[n_states],
                     double transitions[][n_states],
                     double emissions[][n_observations])
{
    Model *model; 
    model = malloc(sizeof(*model));

    // init name
    model->name = strdup(name);

    // init states
    model->n_states = n_states;
    model->states = malloc(n_states * sizeof(string));
    foreach(i, n_states) {
        model->states[i] = strdup(states[i]);
    }

    // init observations
    model->n_observations = n_observations;
    model->observations = malloc(n_observations * sizeof(string));
    foreach(i, n_observations) {
        model->observations[i] = strdup(observations[i]);
    }

    // init start_prob
    model->start_prob = calloc(n_states, sizeof(double));
    foreach(i, n_states) {
        model->start_prob[i] = start_prob[i];
    }

    // init transitions
    ARRAY_ALLOC(model->transitions, double, n_states, n_states);
    foreach(i, n_states) {
        foreach(j, n_states) {
            model->transitions[i][j] = transitions[i][j];
        }
    }

    // init emissions
    ARRAY_ALLOC(model->emissions, double, n_states, n_observations);
    foreach(i, n_states) {
        foreach(j, n_observations) {
            model->emissions[i][j] = emissions[i][j];
        }
    }

    // init result
    ARRAY_ALLOC(model->result, double, n_states, n_observations);

    // init path
    ARRAY_ALLOC(model->path, double, n_states, n_observations);

    return model;
}
 
void model_destroy(Model *m)
{
    FREE(m->name);
    ARRAY_FREE(m->states, m->n_states);
    ARRAY_FREE(m->observations, m->n_observations);
    FREE(m->start_prob);
    ARRAY_FREE(m->transitions, m->n_states);
    ARRAY_FREE(m->emissions, m->n_states);
    ARRAY_FREE(m->result, m->n_states);
    ARRAY_FREE(m->path, m->n_states);
    FREE(m);
}

void model_display(Model *m)
{
    printf("Model: %s\n", m->name);
    ARRAY_C_SHOW(m->states, m->n_states);
    ARRAY_C_SHOW(m->observations, m->n_observations);
    ARRAY_D_SHOW(m->start_prob, m->n_states);
    MATRIX_D_SHOW(m->transitions, m->n_states, m->n_states);
    MATRIX_D_SHOW(m->emissions, m->n_states, m->n_observations);
    MATRIX_D_SHOW(m->result, m->n_states, m->n_observations);
    MATRIX_I_SHOW(m->path, m->n_states, m->n_observations);
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
            (prob, state) = max((V[t-1][y0] * trans_p[y0][y] * emit_p[y][obs[t]], y0) for y0 in states)
            V[t][y] = prob
            newpath[y] = path[state] + [y]
 
        # Don't need to remember the old paths
        path = newpath
    n = 0           # if only one element is observed max is sought in the initialization values
    if len(obs)!=1:
        n = t
    print_dptable(V)
    (prob, state) = max((V[n][y], y) for y in states)
    return (prob, path[state])
*/

void viterbi(Model *m)
{
    double max_p = 0.0;
    double prob = 0.0;
    // int newpath = 0;
    int state;

    // initialize base cases (t==0)
    foreach(i, m->n_states) {
        m->result[i][0] = m->start_prob[i] * m->emissions[i][0];
        m->path[i][0] = i;
    }

    // run viterbi for t > 0
    for (int t=1; t < m->n_observations; t++) {
        for (int y=0; y < m->n_states; y++) {
            max_p = 0.0;
            state = 0;
            for (int y0=0; y0 < m->n_states; y0++) {
                prob = m->result[y0][t-1] * m->transitions[y0][y] * m->emissions[y][t];
                if (prob > max_p) {
                    max_p = prob;
                    state = y0;
                }
            }
            m->result[y][t] = max_p;
            m->path[y][t] = state;
        }
    }

    // debug
    foreach(i, m->n_states) {
        foreach(j, m->n_observations) {
            debug("result[%i][%i] -> %f", i, j, m->result[i][j]);
        }
    }

    foreach(i, m->n_states) {
        foreach(j, m->n_observations) {
            debug("path[%i][%i] -> %i (%s:%s)", i, j, m->path[i][j],
                m->states[i], m->states[m->path[i][j]]);
        }
    }
}

int main()
{
    debug("START");

    string model_name = "Hidden Markov Model";
    int n_states = 2, n_observations = 3;
    string states[] = {"Rainy", "Sunny"};
    string observations[] = {"Walk", "Shop", "Clean"};
    double start_prob[2] = {0.6, 0.4};
    double transitions[2][2] = {
        { 0.7, 0.3 },
        { 0.4, 0.6 }
    };
    double emissions[2][3] = {
        { 0.1, 0.4, 0.5 },
        { 0.6, 0.3, 0.1 }
    };

    Model *m = model_create(
        model_name,
        n_states, states,
        n_observations, observations,
        start_prob,
        transitions,
        emissions);

    // run viterbi
    viterbi(m);

    //model_display(m);
    model_destroy(m);
    
    debug("END");
}