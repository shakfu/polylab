# Smart Pointers


## Unique Pointers

A unique_ptr does not share its pointer. It cannot be copied to another unique_ptr, passed by value to a function, or used in any C++ Standard Library algorithm that requires copies to be made. A unique_ptr can only be moved. This means that the ownership of the memory resource is transferred to another unique_ptr and the original unique_ptr no longer owns it. We recommend that you restrict an object to one owner, because multiple ownership adds complexity to the program logic. Therefore, when you need a smart pointer for a plain C++ object, use unique_ptr, and when you construct a unique_ptr, use the make_unique helper function.

- <https://iamsorush.com/posts/unique-pointers-cpp>


## Shared Pointers

The shared_ptr type is a smart pointer in the C++ standard library that is designed for scenarios in which more than one owner might have to manage the lifetime of the object in memory. After you initialize a shared_ptr you can copy it, pass it by value in function arguments, and assign it to other shared_ptr instances. All the instances point to the same object, and share access to one "control block" that increments and decrements the reference count whenever a new shared_ptr is added, goes out of scope, or is reset. When the reference count reaches zero, the control block deletes the memory resource and itself.

