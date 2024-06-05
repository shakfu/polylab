#ifndef __COUNTER_H__
#define __COUNTER_H__
 
struct counter;
typedef struct counter counter_t;
 
counter_t *counter_create(int start);
void counter_destroy(counter_t *c);
 
void counter_add(counter_t *c, int amount);
void counter_subtract(counter_t *c, int amount);
 
void counter_increment(counter_t *c);
void counter_decrement(counter_t *c);
 
int counter_getval(counter_t *c);
void counter_show(counter_t *c);

#endif /* __COUNTER_H__ */
