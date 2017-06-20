(import os sys)
(import [sqlalchemy [Column ForeignKey Integer String]])
(import [sqlalchemy.ext.declarative [declarative_base]])
(import [sqlalchemy.orm [relationship sessionmaker]])
(import [sqlalchemy [create_engine]])

(def Base (declarative_base))

(defclass Person [Base]
  [--tablename-- "person"
   id (Column Integer :primary-key True)
   name (Column (String 250) :nullable False)]

  (defn --repr-- [self]
    (+ "<Person" self.name ">")))


(defn itemize [xs]
  (setv it (iter xs))
  (list (zip it it)))

(defmacro model [name &rest attrs]
  (setv class-name (.title name))
  `(defclass ~class-name [Base]
    [--tablename-- ~name
        id (Column Integer :primary-key True)]
    (defn --repr-- [self]
      (+ "<" ~class-name " " (str self.id) ">"))))


(model "car" 
  [name (Column (String 250) :nullable False)])

(def engine (create_engine "sqlite:///demo.db"))

((. Base metadata create_all) engine)

(def (. Base metadata bind) engine)
(def DBSession (sessionmaker :bind engine))
(def session (DBSession))

(defn test-db []
  (for [i (range 10)]
    (do
      (setv name (gensym))
      (setv p (Person :name name))
      (.add session p)))
  (.commit session))

(test-db)

(defn display-people []
  "displays all people"
  (setv q (.query session Person))
  (.all q))

(defn get-all [db-class]
  (.all (.query session db-class)))

(display-people)

