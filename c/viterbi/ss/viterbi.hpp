#ifndef __VITERBI_HPP
#define __VITERBI_HPP

using namespace __shedskin__;
namespace __viterbi__ {

extern str *const_0, *const_1, *const_10, *const_11, *const_12, *const_13, *const_14, *const_15, *const_16, *const_17, *const_18, *const_19, *const_2, *const_3, *const_4, *const_5, *const_6, *const_7, *const_8, *const_9;



extern str *__name__;
extern tuple<str *> *observations, *states;
extern dict<str *, __ss_float> *start_probability;
extern dict<str *, dict<str *, __ss_float> *> *emission_probability, *transition_probability;


tuple2<__ss_float, list<str *> *> *viterbi(tuple<str *> *obs, tuple<str *> *states, dict<str *, __ss_float> *start_p, dict<str *, dict<str *, __ss_float> *> *trans_p, dict<str *, dict<str *, __ss_float> *> *emit_p);
void *print_dptable(list<dict<str *, __ss_float> *> *V);
void *dump(dict<str *, dict<str *, __ss_float> *> *dikt);
void *example();

} // module namespace
#endif
