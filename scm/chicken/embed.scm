#>
    extern int add(int x, int y);
<#

(define add (foreign-lambda int "add" int int))
(display (add 10 21))
(newline)
