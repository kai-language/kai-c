
typedef struct QueueNode QueueNode;
struct QueueNode {
    void *val;
    QueueNode *next;
};

typedef struct Queue Queue;
struct Queue {
    QueueNode *head;
    QueueNode *tail;
    size_t size;
    Arena arena;
};

void QueueEnqueue(Queue *q, void *val) {
    QueueNode *node = ArenaAlloc(&q->arena, sizeof(QueueNode));
    node->val = val;
    node->next = q->tail;
    *node = (QueueNode) {val, .next = q->tail};
    if (!q->head) q->head = node;

    q->tail = node;
    q->size += 1;
}

void *QueueDequeue(Queue *q) {
    ASSERT(q);
    if (!q->tail) return NULL;

    QueueNode *node = q->tail;
    q->size -= 1;
    if (q->size == 0) {
        q->head = NULL;
        q->tail = NULL;
        return node->val;
    }

    q->tail = node->next;

    return node->val;
}

#if TEST
void test_queue() {
    Queue queue = {0};

    for (intptr_t i = 0; i < 100; i++) QueueEnqueue(&queue, (void*) i);
    for (intptr_t i = 99; i >= 0; i--) {
        intptr_t val = (intptr_t) QueueDequeue(&queue);
        TEST_ASSERT(val == i);
    }
}
#endif
