
#ifndef queue_h
#define queue_h

typedef struct QueueNode {
    void *val;
    struct QueueNode *next;
    struct QueueNode *prev;
} QueueNode;

typedef struct Queue {
    QueueNode *head;
    QueueNode *tail;
    size_t size;
    Arena arena;
} Queue;

#endif
