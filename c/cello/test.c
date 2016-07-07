/*
** Example libCello Program
*/

#include "Cello.h"

int main(int argc, char** argv) {

    println("testing array");
    var x = new(Array, Int);
    push(x, $(Int, 32));
    push(x, $(Int, 10));
    show(x);
    println("");
    delete(x);
    
    println("testing list");
    var l = new(List);
    push(l, new(String, $(String, "Hello")));
    show(l);
    println("");
    delete(l);

    
    println("testing table");
    var prices = new(Table, String, Int);
    put(prices, $(String, "Apple"),  $(Int, 12)); 
    put(prices, $(String, "Banana"), $(Int,  6)); 
    put(prices, $(String, "Pear"),   $(Int, 55)); 

    var pear_price = get(prices, $(String, "Pear"));
    print("Price of a 'Pear' is %$\n", pear_price);
    
    foreach(key in prices) {
        var price = get(prices, key);
        print("Price of %$ is %$\n", key, price);
    }

    delete(prices);
}

