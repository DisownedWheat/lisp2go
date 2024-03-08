(define add (x y) (+ x y))

(print (add 1 2))

(define ::TestStruct {
    :name ::string
    :age ::int
})

(define main (w ::http.ResponseWriter r ::&http.Reqeuest)
    (define x '(1 2 3 4))
    (define y {
        :test :true
    })
    (. w Write (toBytes x))
)