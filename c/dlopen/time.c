/* Plugin other example. query() returns the current time */

#include <time.h>

int state;

void init() { /* Do nothing */ }

int query() { return (int)time(NULL); }
