# servant-generic-custom-monad

A minimal example on how to use servant's generic api together with a custom monad.

Build with:

```
stack build
```

Run with:

```
stack run -- run
```

Example:

```
$ curl -i -H "Content-Type: application/json" -XPUT --data $'11' localhost:8000
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Fri, 19 Oct 2018 10:00:18 GMT
Server: Warp/3.2.25
Content-Type: application/json;charset=utf-8

true%
$ curl -i localhost:8000/123
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Fri, 19 Oct 2018 10:01:05 GMT
Server: Warp/3.2.25
Content-Type: application/json;charset=utf-8

"123"%
```
