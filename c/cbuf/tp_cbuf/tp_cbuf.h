// original source from Michael Tyson
//
// modified by adding additional functions to produce and consume without checking for buffer underflow
// for delay lines
// tz 11/2011

//
//  cbuf.h
//  Circular buffer implementation
//
//  Created by Michael Tyson on 19/03/2011.
//  Copyright 2011 A Tasty Pixel. All rights reserved.
//
// made it more readable and fix deprecation warning (convert to stdatomic)
// uncomment below for original 'deprecated' atomic  part
// #define ORIGINAL

#ifdef ORIGINAL
#include <libkern/OSAtomic.h>
#else
#include <stdatomic.h>
#endif

typedef struct { 
    int32_t head;
    int32_t tail;
#ifdef ORIGINAL
    volatile int32_t fill_count;
#else
    atomic_int fill_count;
#endif
    int32_t length;
} cbuf_record;

void cbuf_init(cbuf_record *record, int length);
void cbuf_clear(cbuf_record *record);

int cbuf_fillcount(cbuf_record *record);
int cbuf_fillcount_contiguous(cbuf_record *record);
int cbuf_space(cbuf_record *record);
int cbuf_space_contiguous(cbuf_record *record);

// Reading (consuming)
int  cbuf_tail(cbuf_record *record);
void cbuf_consume(cbuf_record *record, int amount);
void cbuf_consume_singlethread(cbuf_record *record, int amount);

// Writing (producing)
int  cbuf_head(cbuf_record *record);
void cbuf_produce(cbuf_record *record, int amount);
void cbuf_produce_singlethread(cbuf_record *record, int amount);
int  cbuf_produce_bytes(cbuf_record *record, void* dst, const void* src, int count, int len);

// tz for resetting tail

void cbuf_set_tail(cbuf_record *record, int32_t position, int32_t count);

int cbuf_length(cbuf_record *record );
void cbuf_produce_anywhere(cbuf_record *record, int amount);
void cbuf_consume_anywhere(cbuf_record *record, int amount);
void cbuf_set_tail_anywhere(cbuf_record *record, int32_t position );

