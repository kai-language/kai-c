
#include "all.h"
#include "queue.h"
#include "arena.h"
#include "ast.h"
#include "package.h"
#include "compiler.h"

#if INTERFACE
struct QueueNode {
    void *val;
    struct QueueNode *next;
    struct QueueNode *prev;
};

struct Queue {
    QueueNode *head;
    QueueNode *tail;
    size_t size;
};
#endif

void queue_push_front(Queue *q, void *val) {
    QueueNode *node = arena_alloc(&compiler.arena, sizeof *node);
    *node = (QueueNode) {val, q->head, NULL};
    if (q->head) q->head->prev = node;
    if (!q->tail) q->tail = node;
    q->head = node;
    q->size += 1;
}

void queue_push_back(Queue *q, void *val) {
    QueueNode *node = arena_alloc(&compiler.arena, sizeof *node);
    *node = (QueueNode) {val, NULL, q->tail};
    if (q->tail) q->tail->next = node;

    if (!q->head) q->head = node;
    q->tail = node;
    q->size += 1;
}

void *queue_pop_front(Queue *q) {
    ASSERT(q);
    if (!q->head) return NULL;
    QueueNode *node = q->head;
    q->size -= 1;
    q->head = node->next;
    if (!q->size) q->tail = NULL;
    return node->val;
}

void *queue_pop_back(Queue *q) {
    ASSERT(q);
    if (!q->tail) return NULL;
    QueueNode *node = q->tail;
    q->size -= 1;
    q->tail = node->prev;
    if (!q->size) q->head = NULL;
    return node->val;
}

#if TEST
void test_queue() {
    Queue queue = {0};
    for (intptr_t i = 0; i <= 4; i++) queue_push_back(&queue, (void*) i);
    for (intptr_t i = 0; i <= 4; i++) {
        intptr_t val = (intptr_t) queue_pop_front(&queue);
        ASSERT(val == i);
    }
    ASSERT(queue.size == 0);

    for (intptr_t i = 0; i <= 4; i++) queue_push_back(&queue, (void*) i);
    for (intptr_t i = 4; i >= 0; i--) {
        intptr_t val = (intptr_t) queue_pop_back(&queue);
        ASSERT(val == i);
    }
    ASSERT(queue.size == 0);

    for (intptr_t i = 0; i <= 4; i++) queue_push_front(&queue, (void*) i);
    for (intptr_t i = 0; i <= 4; i++) {
        intptr_t val = (intptr_t) queue_pop_back(&queue);
        ASSERT(val == i);
    }
    ASSERT(queue.size == 0);

    for (intptr_t i = 0; i <= 4; i++) queue_push_front(&queue, (void*) i);
    for (intptr_t i = 4; i >= 0; i--) {
        intptr_t val = (intptr_t) queue_pop_front(&queue);
        ASSERT(val == i);
    }
    ASSERT(queue.size == 0);
}
#endif

