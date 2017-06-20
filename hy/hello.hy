
(defmacro unless_ [condition body]
  `(if (not ~condition) ~body))

(defmacro class [name superclasses]
  `(defclass ~name ~superclasses))

(defclass Cat []
  [age None
   colour "white"]

  (defn speak [self] (print "Meow")))

; (disassemble '(class person [object]) true)

(defclass Person []
  [name None
   age  None]

  (defn speak [self]
    (print "my name is: " name " age is " age)))

 
