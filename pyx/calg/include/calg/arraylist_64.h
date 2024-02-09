/*

Copyright (c) 2005-2008, Simon Howard

Permission to use, copy, modify, and/or distribute this software
for any purpose with or without fee is hereby granted, provided
that the above copyright notice and this permission notice appear
in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

 */

/**
 * @file arraylist64_64.h
 *
 * @brief Automatically resizing array, using a fixed 64-bit cell
 *
 * ArrayList64s64 are arrays of pointers which automatically increase in
 * size. They are specalisations of ArrayList64s, using a fixed width
 * cell instead of a pointer to a generic data structure. This eliminates
 * memory allocations needed past the growth of the array itself.
 *
 * To create an ArrayList64, use @ref arraylist64_new.
 * To destroy an ArrayList64, use @ref arraylist64_free.
 *
 * To add a value to an ArrayList64, use @ref arraylist64_prepend,
 * @ref arraylist64_append, or @ref arraylist64_insert.
 *
 * To remove a value from an ArrayList64, use @ref arraylist64_remove
 * or @ref arraylist64_remove_range.
 *
 * \author Simon Howard
 * \author David Love
 *
 * \date December 2011
 */

#ifndef ALGORITHM_ARRAYLIST_H
#define ALGORITHM_ARRAYLIST_H

/* Link to the global configure header */
#include "config.h"

/* We need access to the exact integer types: abort if this are not present */
#if HAVE_STDINT_H
#include <stdint.h>
#else
#error "The standard integer header file cannot be found"
#endif

#ifdef __cplusplus
extern "C" {
#endif

  /**
   * A value to be stored in an @ref ArrayList64. Unlike the generic ArrayList this
   * value is pre-defined to be exactly 64 bits, and is *not* a pointer: the value is
   * instead assumed to be store in the array itself.
   */

  typedef uint64_t ArrayList64Value;

  /**
   * An ArrayList64 structure.  New ArrayList64s can be created using the
   * arraylist64_new function.
   *
   * @see arraylist64_new
   */

  typedef struct _ArrayList64 ArrayList64;

  /**
   * Definition of an @ref ArrayList64.
   */

  struct _ArrayList64 {

    /** Entries in the array */

    ArrayList64Value* data;

    /** Length of the array */

    unsigned int length;

    /** Private data and should not be accessed */

    unsigned int _alloced;
    };

  /**
   * Compare two values in an arraylist to determine if they are equal.
   *
   * @return Non-zero if the values are equal, zero if they are not equal.
   */

  typedef int (*ArrayList64EqualFunc) (ArrayList64Value value1, ArrayList64Value value2);

  /**
   * Compare two values in an arraylist.  Used by @ref arraylist64_sort
   * when sorting values.
   *
   * @param value1              The first value.
   * @param value2              The second value.
   * @return                    A negative number if value1 should be sorted
   *                            before value2, a positive number if value2 should
   *                            be sorted before value1, zero if the two values
   *                            are equal.
   */

  typedef int (*ArrayList64CompareFunc) (ArrayList64Value value1,
                                         ArrayList64Value value2);

  /**
   * Allocate a new ArrayList64 for use.
   *
   * @param length         Hint to the initialise function as to the amount
   *                       of memory to allocate initially to the ArrayList64.
   *                       If a value of zero is given, a sensible default
   *                       size is used.
   * @return               A new arraylist, or NULL if it was not possible
   *                       to allocate the memory.
   * @see arraylist64_free
   */

  ArrayList64* arraylist64_new (unsigned int length);

  /**
   * Destroy an ArrayList64 and free back the memory it uses.
   *
   * @param arraylist      The ArrayList64 to free.
   */

  void arraylist64_free (ArrayList64* arraylist);

  /**
   * Append a value to the end of an ArrayList64.
   *
   * @param arraylist      The ArrayList64.
   * @param data           The value to append.
   * @return               Non-zero if the request was successful, zero
   *                       if it was not possible to allocate more memory
   *                       for the new entry.
   */

  int arraylist64_append (ArrayList64* arraylist, ArrayList64Value data);

  /**
   * Prepend a value to the beginning of an ArrayList64.
   *
   * @param arraylist      The ArrayList64.
   * @param data           The value to prepend.
   * @return               Non-zero if the request was successful, zero
   *                       if it was not possible to allocate more memory
   *                       for the new entry.
   */

  int arraylist64_prepend (ArrayList64* arraylist, ArrayList64Value data);

  /**
   * Remove the entry at the specified location in an ArrayList64.
   *
   * @param arraylist      The ArrayList64.
   * @param index          The index of the entry to remove.
   */

  void arraylist64_remove (ArrayList64* arraylist, unsigned int index);

  /**
   * Remove a range of entries at the specified location in an ArrayList64.
   *
   * @param arraylist      The ArrayList64.
   * @param index          The index of the start of the range to remove.
   * @param length         The length of the range to remove.
   */

  void arraylist64_remove_range (ArrayList64* arraylist, unsigned int index,
                                 unsigned int length);

  /**
   * Insert a value at the specified index in an ArrayList64.
   * The index where the new value can be inserted is limited by the
   * size of the ArrayList64.
   *
   * @param arraylist      The ArrayList64.
   * @param index          The index at which to insert the value.
   * @param data           The value.
   * @return               Returns zero if unsuccessful, else non-zero
   *                       if successful (due to an invalid index or
   *                       if it was impossible to allocate more memory).
   */

  int arraylist64_insert (ArrayList64* arraylist, unsigned int index,
                          ArrayList64Value data);

  /**
   * Find the index of a particular value in an ArrayList64.
   *
   * @param arraylist      The ArrayList64 to search.
   * @param callback       Callback function to be invoked to compare
   *                       values in the list with the value to be
   *                       searched for.
   * @param data           The value to search for.
   * @return               The index of the value if found, or -1 if not found.
   */

  int arraylist64_index_of (ArrayList64* arraylist,
                            ArrayList64EqualFunc callback,
                            ArrayList64Value data);

  /**
   * Remove all entries from an ArrayList64.
   *
   * @param arraylist      The ArrayList64.
   */

  void arraylist64_clear (ArrayList64* arraylist);

  /**
   * Sort the values in an ArrayList64.
   *
   * @param arraylist      The ArrayList64.
   * @param compare_func   Function used to compare values in sorting.
   */

  void arraylist64_sort (ArrayList64* arraylist, ArrayList64CompareFunc compare_func);

#ifdef __cplusplus
  }
#endif

#endif /* #ifndef ALGORITHM_ARRAYLIST_H */

