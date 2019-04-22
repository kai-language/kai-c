#pragma once

typedef struct QueueNode QueueNode;
struct QueueNode {
    void *val;
    struct QueueNode *next;
    struct QueueNode *prev;
};

typedef struct Queue Queue;
struct Queue {
    QueueNode *head;
    QueueNode *tail;
    size_t size;
};

void *queue_pop_back(Queue *q);
void *queue_pop_front(Queue *q);
void queue_push_back(Queue *q, void *val);
void queue_push_front(Queue *q, void *val);
