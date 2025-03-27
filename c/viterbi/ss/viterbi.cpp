#include "builtin.hpp"
#include "viterbi.hpp"

namespace __viterbi__ {

str *const_0, *const_1, *const_10, *const_11, *const_12, *const_13, *const_14, *const_15, *const_16, *const_17, *const_18, *const_19, *const_2, *const_3, *const_4, *const_5, *const_6, *const_7, *const_8, *const_9;


str *__name__;
tuple<str *> *observations, *states;
dict<str *, __ss_float> *start_probability;
dict<str *, dict<str *, __ss_float> *> *emission_probability, *transition_probability;


class list_comp_0 : public __iter<tuple2<__ss_float, str *> *> {
public:
    str *y0;
    tuple<str *> *__11;
    __iter<str *> *__12;
    __ss_int __13;
    tuple<str *>::for_in_loop __14;

    list<dict<str *, __ss_float> *> *V;
    dict<str *, dict<str *, __ss_float> *> *emit_p;
    str *y;
    dict<str *, dict<str *, __ss_float> *> *trans_p;
    tuple<str *> *states;
    tuple<str *> *obs;
    __ss_int t;
    int __last_yield;

    list_comp_0(list<dict<str *, __ss_float> *> *V, dict<str *, dict<str *, __ss_float> *> *emit_p, str *y, dict<str *, dict<str *, __ss_float> *> *trans_p, tuple<str *> *states, tuple<str *> *obs, __ss_int t);
    tuple2<__ss_float, str *> * __get_next();
};

class list_comp_1 : public __iter<tuple2<__ss_float, str *> *> {
public:
    str *y;
    tuple<str *> *__17;
    __iter<str *> *__18;
    __ss_int __19;
    tuple<str *>::for_in_loop __20;

    __ss_int n;
    tuple<str *> *states;
    list<dict<str *, __ss_float> *> *V;
    int __last_yield;

    list_comp_1(__ss_int n, tuple<str *> *states, list<dict<str *, __ss_float> *> *V);
    tuple2<__ss_float, str *> * __get_next();
};

class list_comp_2 : public __iter<str *> {
public:
    __ss_int __22, __23, i;

    list<dict<str *, __ss_float> *> *V;
    int __last_yield;

    list_comp_2(list<dict<str *, __ss_float> *> *V);
    str * __get_next();
};

class list_comp_3 : public __iter<str *> {
public:
    dict<str *, __ss_float> *v;
    list<dict<str *, __ss_float> *> *__28;
    __iter<dict<str *, __ss_float> *> *__29;
    __ss_int __30;
    list<dict<str *, __ss_float> *>::for_in_loop __31;

    str *y;
    list<dict<str *, __ss_float> *> *V;
    int __last_yield;

    list_comp_3(str *y, list<dict<str *, __ss_float> *> *V);
    str * __get_next();
};


list_comp_0::list_comp_0(list<dict<str *, __ss_float> *> *V, dict<str *, dict<str *, __ss_float> *> *emit_p, str *y, dict<str *, dict<str *, __ss_float> *> *trans_p, tuple<str *> *states, tuple<str *> *obs, __ss_int t) {
    this->V = V;
    this->emit_p = emit_p;
    this->y = y;
    this->trans_p = trans_p;
    this->states = states;
    this->obs = obs;
    this->t = t;
    __last_yield = -1;
}

tuple2<__ss_float, str *> * list_comp_0::__get_next() {
    if(!__last_yield) goto __after_yield_0;
    __last_yield = 0;

    FOR_IN(y0,states,11,13,14)
        __result = (new tuple2<__ss_float, str *>(2,(((V->__getfast__((t-__ss_int(1))))->__getitem__(y0)*(trans_p->__getitem__(y0))->__getitem__(y))*(emit_p->__getitem__(y))->__getitem__(obs->__getfast__(t))),y0));
        return __result;
        __after_yield_0:;
    END_FOR

    __stop_iteration = true;
    return __zero<tuple2<__ss_float, str *> *>();
}

list_comp_1::list_comp_1(__ss_int n, tuple<str *> *states, list<dict<str *, __ss_float> *> *V) {
    this->n = n;
    this->states = states;
    this->V = V;
    __last_yield = -1;
}

tuple2<__ss_float, str *> * list_comp_1::__get_next() {
    if(!__last_yield) goto __after_yield_0;
    __last_yield = 0;

    FOR_IN(y,states,17,19,20)
        __result = (new tuple2<__ss_float, str *>(2,(V->__getfast__(n))->__getitem__(y),y));
        return __result;
        __after_yield_0:;
    END_FOR

    __stop_iteration = true;
    return __zero<tuple2<__ss_float, str *> *>();
}

list_comp_2::list_comp_2(list<dict<str *, __ss_float> *> *V) {
    this->V = V;
    __last_yield = -1;
}

str * list_comp_2::__get_next() {
    if(!__last_yield) goto __after_yield_0;
    __last_yield = 0;

    FAST_FOR(i,0,len(V),1,22,23)
        __result = __mod6(const_0, 1, i);
        return __result;
        __after_yield_0:;
    END_FOR

    __stop_iteration = true;
    return __zero<str *>();
}

list_comp_3::list_comp_3(str *y, list<dict<str *, __ss_float> *> *V) {
    this->y = y;
    this->V = V;
    __last_yield = -1;
}

str * list_comp_3::__get_next() {
    if(!__last_yield) goto __after_yield_0;
    __last_yield = 0;

    FOR_IN(v,V,28,30,31)
        __result = __mod6(const_1, 1, __mod6(const_2, 1, v->__getitem__(y)));
        return __result;
        __after_yield_0:;
    END_FOR

    __stop_iteration = true;
    return __zero<str *>();
}

tuple2<__ss_float, list<str *> *> *viterbi(tuple<str *> *obs, tuple<str *> *states, dict<str *, __ss_float> *start_p, dict<str *, dict<str *, __ss_float> *> *trans_p, dict<str *, dict<str *, __ss_float> *> *emit_p) {
    list<dict<str *, __ss_float> *> *V;
    dict<str *, list<str *> *> *newpath, *path;
    str *state, *y;
    __ss_int __2, __5, __6, __9, n, t;
    __ss_float prob;
    void *y0;
    tuple<str *> *__0, *__7;
    __iter<str *> *__1, *__8;
    tuple<str *>::for_in_loop __10, __3;
    dict<str *, __ss_float> *__16, *__4;
    tuple2<__ss_float, str *> *__15, *__21;

    V = (new list<dict<str *, __ss_float> *>(1,(new dict<str *, __ss_float>())));
    path = (new dict<str *, list<str *> *>());

    FOR_IN(y,states,0,2,3)
        V->__getfast__(__ss_int(0))->__setitem__(y, (start_p->__getitem__(y)*(emit_p->__getitem__(y))->__getitem__(obs->__getfast__(__ss_int(0)))));
        path->__setitem__(y, (new list<str *>(1,y)));
    END_FOR


    FAST_FOR(t,__ss_int(1),len(obs),1,5,6)
        V->append((new dict<str *, __ss_float>()));
        newpath = (new dict<str *, list<str *> *>());

        FOR_IN(y,states,7,9,10)
            __15 = ___max(1, __ss_int(0), new list_comp_0(V, emit_p, y, trans_p, states, obs, t));
            __unpack_check(__15, 2);
            prob = __15->__getfirst__();
            state = __15->__getsecond__();
            V->__getfast__(t)->__setitem__(y, prob);
            newpath->__setitem__(y, __add_list_elt(path->__getitem__(state), y));
        END_FOR

        path = newpath;
    END_FOR

    n = __ss_int(0);
    if ((len(obs)!=__ss_int(1))) {
        n = t;
    }
    print_dptable(V);
    __21 = ___max(1, __ss_int(0), new list_comp_1(n, states, V));
    __unpack_check(__21, 2);
    prob = __21->__getfirst__();
    state = __21->__getsecond__();
    return (new tuple2<__ss_float, list<str *> *>(2,prob,path->__getitem__(state)));
}

void *print_dptable(list<dict<str *, __ss_float> *> *V) {
    str *s, *y;
    void *i, *v;
    dict<str *, __ss_float> *__24;
    __iter<str *> *__25;
    __ss_int __26;
    dict<str *, __ss_float>::for_in_loop __27;

    s = __add_strs(3, const_3, (const_4)->join(new list_comp_2(V)), const_5);

    FOR_IN(y,V->__getfast__(__ss_int(0)),24,26,27)
        s = (s)->__iadd__(__mod6(const_6, 1, y));
        s = (s)->__iadd__((const_4)->join(new list_comp_3(y, V)));
        s = (s)->__iadd__(const_5);
    END_FOR

    print(s);
    return NULL;
}

void *dump(dict<str *, dict<str *, __ss_float> *> *dikt) {
    str *key;
    dict<str *, dict<str *, __ss_float> *> *__32;
    __iter<str *> *__33;
    __ss_int __34;
    dict<str *, dict<str *, __ss_float> *>::for_in_loop __35;


    FOR_IN(key,dikt,32,34,35)
        print(const_7, key, const_8, dikt->__getitem__(key));
    END_FOR

    return NULL;
}

void *example() {
    print(const_9, __viterbi__::states);
    print(const_10, __viterbi__::observations);
    print(const_11, __viterbi__::start_probability);
    print(const_12);
    dump(__viterbi__::transition_probability);
    print(const_13);
    dump(__viterbi__::emission_probability);
    print();
    print(const_14);
    print(viterbi(__viterbi__::observations, __viterbi__::states, __viterbi__::start_probability, __viterbi__::transition_probability, __viterbi__::emission_probability));
    return NULL;
}

void __init() {
    const_0 = new str("%7d");
    const_1 = new str("%.7s");
    const_2 = new str("%f");
    const_3 = new str("    ");
    const_4 = __char_cache[32];
    const_5 = __char_cache[10];
    const_6 = new str("%.5s: ");
    const_7 = __char_cache[9];
    const_8 = new str("->");
    const_9 = new str("states:");
    const_10 = new str("observations:");
    const_11 = new str("start_probability:");
    const_12 = new str("transition_probability:");
    const_13 = new str("emission_probability:");
    const_14 = new str("viterbi:");
    const_15 = new str("Rainy");
    const_16 = new str("Sunny");
    const_17 = new str("walk");
    const_18 = new str("shop");
    const_19 = new str("clean");

    __name__ = new str("__main__");

    states = (new tuple<str *>(2,const_15,const_16));
    observations = (new tuple<str *>(3,const_17,const_18,const_19));
    start_probability = (new dict<str *, __ss_float>(2, (new tuple2<str *, __ss_float >(2,const_15,__ss_float(0.6))),(new tuple2<str *, __ss_float >(2,const_16,__ss_float(0.4)))));
    transition_probability = (new dict<str *, dict<str *, __ss_float> *>(2, (new tuple2<str *, dict<str *, __ss_float> *>(2,const_15,(new dict<str *, __ss_float>(2, (new tuple2<str *, __ss_float >(2,const_15,__ss_float(0.7))),(new tuple2<str *, __ss_float >(2,const_16,__ss_float(0.3))))))),(new tuple2<str *, dict<str *, __ss_float> *>(2,const_16,(new dict<str *, __ss_float>(2, (new tuple2<str *, __ss_float >(2,const_15,__ss_float(0.4))),(new tuple2<str *, __ss_float >(2,const_16,__ss_float(0.6)))))))));
    emission_probability = (new dict<str *, dict<str *, __ss_float> *>(2, (new tuple2<str *, dict<str *, __ss_float> *>(2,const_15,(new dict<str *, __ss_float>(3, (new tuple2<str *, __ss_float >(2,const_17,__ss_float(0.1))),(new tuple2<str *, __ss_float >(2,const_18,__ss_float(0.4))),(new tuple2<str *, __ss_float >(2,const_19,__ss_float(0.5))))))),(new tuple2<str *, dict<str *, __ss_float> *>(2,const_16,(new dict<str *, __ss_float>(3, (new tuple2<str *, __ss_float >(2,const_17,__ss_float(0.6))),(new tuple2<str *, __ss_float >(2,const_18,__ss_float(0.3))),(new tuple2<str *, __ss_float >(2,const_19,__ss_float(0.1)))))))));
    example();
}

} // module namespace

int main(int, char **) {
    __shedskin__::__init();
    __shedskin__::__start(__viterbi__::__init);
}
