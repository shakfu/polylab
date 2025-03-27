// original source from Michael Tyson
//
// modified by adding additional functions to produce and consume without
// checking for buffer underflow for delay lines tz 11/2011
//
//  cbuf.c
//  Circular buffer implementation
//
//  Created by Michael Tyson on 20/03/2011.
//  Copyright 2011 A Tasty Pixel. All rights reserved.
//

#include "tp_cbuf.h"
#include <string.h>

static inline int min(int a, int b) { return (a > b ? b : a); }

inline void cbuf_init(cbuf_record* record, int length)
{
    record->head = record->tail = record->fill_count = 0;
    record->length = length;
}

inline int cbuf_fillcount(cbuf_record* record) { return record->fill_count; }

inline int cbuf_fillcount_contiguous(cbuf_record* record)
{
    return min(record->fill_count, record->length - record->tail);
}

inline int cbuf_space(cbuf_record* record)
{
    return record->length - record->fill_count;
}

inline int cbuf_space_contiguous(cbuf_record* record)
{
    return min(record->length - record->fill_count,
               record->length - record->head);
}

inline int cbuf_head(cbuf_record* record) { return record->head; }

inline int cbuf_tail(cbuf_record* record) { return record->tail; }

inline void cbuf_produce(cbuf_record* record, int amount)
{
    record->head = (record->head + amount) % record->length;
#ifdef ORIGINAL
    OSAtomicAdd32Barrier(amount, &record->fill_count);
#else
    atomic_fetch_add(&record->fill_count, amount);
#endif
}

inline void cbuf_produce_singlethread(cbuf_record* record, int amount)
{
    record->head = (record->head + amount) % record->length;
    record->fill_count += amount;
}

inline int cbuf_produce_bytes(cbuf_record* record, void* dst, const void* src,
                              int count, int len)
{
    int copied = 0;
    while (count > 0) {
        int space = cbuf_space_contiguous(record);
        if (space == 0) {
            return copied;
        }

        int to_copy = min(count, space);
        int bytes_to_copy = to_copy * len;
        memcpy(dst + (len * cbuf_head(record)), src, bytes_to_copy);

        src += bytes_to_copy;
        count -= to_copy;
        copied += bytes_to_copy / len;
        cbuf_produce(record, to_copy);
    }
    return copied;
}

inline void cbuf_consume(cbuf_record* record, int amount)
{
    record->tail = (record->tail + amount) % record->length;
#ifdef ORIGINAL
    OSAtomicAdd32Barrier(-amount, &record->fill_count);
#else
    atomic_fetch_add(&record->fill_count, -amount);
#endif
}

inline void cbuf_consume_singlethread(cbuf_record* record, int amount)
{
    record->tail = (record->tail + amount) % record->length;
    record->fill_count -= amount;
}

inline void cbuf_clear(cbuf_record* record)
{
    record->tail = record->head;
    record->fill_count = 0;
}

// tz addition to allow resetting the tail

inline void cbuf_set_tail(cbuf_record* record, int32_t position, int32_t count)
{
    record->tail = position;
    record->fill_count = count;
}

// tz - return buffer length

inline int cbuf_set_length(cbuf_record* record) { return record->length; }

inline void cbuf_produce_anywhere(cbuf_record* record, int amount)
{
    record->head = (record->head + amount) % record->length;
    // record->fill_count += amount;
}

inline void cbuf_consume_anywhere(cbuf_record* record, int amount)
{
    record->tail = (record->tail + amount) % record->length;
    // record->fill_count -= amount;
}

inline void cbuf_set_tail_anywhere(cbuf_record* record, int32_t position)
{
    record->tail = position;
    //    record->fill_count = count;
}
