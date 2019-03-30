
typedef struct QueueNode QueueNode;
struct QueueNode {
    void *val;
    QueueNode *next;
    QueueNode *prev;
};

typedef struct Queue Queue;
struct Queue {
    QueueNode *head;
    QueueNode *tail;
    size_t size;
    Arena arena;
};

void QueuePushFront(Queue *q, void *val) {
    QueueNode *node = ArenaAlloc(&q->arena, sizeof(QueueNode));
    *node = (QueueNode) {val, q->head, NULL};
    if (q->head) q->head->prev = node;

    if (!q->tail) q->tail = node;
    q->head = node;
    q->size += 1;
}

void QueuePushBack(Queue *q, void *val) {
    QueueNode *node = ArenaAlloc(&q->arena, sizeof *node);
    *node = (QueueNode) {val, NULL, q->tail};
    if (q->tail) q->tail->next = node;

    if (!q->head) q->head = node;
    q->tail = node;
    q->size += 1;
}

void *QueuePopFront(Queue *q) {
    ASSERT(q);
    if (!q->head) return NULL;

    QueueNode *node = q->head;
    q->size -= 1;
    q->head = node->next;
    if (!q->size) q->tail = NULL;

    return node->val;
}

void *QueuePopBack(Queue *q) {
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
    for (intptr_t i = 0; i <= 4; i++) QueuePushBack(&queue, (void*) i);
    for (intptr_t i = 0; i <= 4; i++) {
        intptr_t val = (intptr_t) QueuePopFront(&queue);
        ASSERT(val == i);
    }
    ASSERT(queue.size == 0);

    for (intptr_t i = 0; i <= 4; i++) QueuePushBack(&queue, (void*) i);
    for (intptr_t i = 4; i >= 0; i--) {
        intptr_t val = (intptr_t) QueuePopBack(&queue);
        ASSERT(val == i);
    }
    ASSERT(queue.size == 0);

    for (intptr_t i = 0; i <= 4; i++) QueuePushFront(&queue, (void*) i);
    for (intptr_t i = 0; i <= 4; i++) {
        intptr_t val = (intptr_t) QueuePopBack(&queue);
        ASSERT(val == i);
    }
    ASSERT(queue.size == 0);

    for (intptr_t i = 0; i <= 4; i++) QueuePushFront(&queue, (void*) i);
    for (intptr_t i = 4; i >= 0; i--) {
        intptr_t val = (intptr_t) QueuePopFront(&queue);
        ASSERT(val == i);
    }
    ASSERT(queue.size == 0);

    ArenaFree(&queue.arena);
}
#endif
