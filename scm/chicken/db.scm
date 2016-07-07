(use postgresql)

(define dburi "dbname=sa user=sa password=sa")

(define (sql q)
    (let ((conn (connect dburi)))
  (row-values (query conn q))))

(display (sql "select * from users"))

