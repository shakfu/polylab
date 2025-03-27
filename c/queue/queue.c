/*initialize queue pointer*/
void init(int* head, int* tail) { *head = *tail = 0; }
/* enqueue an element
   precondition: the queue is not full
*/
void enqueue(int* q, int* tail, int element) { q[(*tail)++] = element; }
/* dequeue an element
   precondition: queue is not empty*/
int dequeue(int* q, int* head) { return q[(*head)++]; }
/* report queue is full nor not
   return 1 if queue is full, otherwise return 0*/
int full(int tail, const int size) { return tail == size ? 1 : 0; }

/* report queue is empty or not
  return 1 if the queue is empty, otherwise return 0*/
int empty(int head, int tail) { return head == tail ? 1 : 0; }