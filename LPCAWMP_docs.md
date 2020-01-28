# Documentation of List Prolog Interpreter CAWMP Commands

* `[[n,[not,Operator]],[Variable1,Variable2]]` where `Operator=is` or `Operator="="` e.g. `[[n,[not,is]],[[v,a],1]]` returns true where [v,a] must be instantiated (not empty).

* `[[n,[not,Operator]],[Variable1,Variable2]]` where `Operator=">"`, `Operator=">="`, `Operator="<"`, `Operator="=<"` or `Operator="=\="` e.g. `[[n,[not,>]],[3,2]]` returns false.

* `[[n,[not,member]],[Variable1,Variable2]]` e.g. `[[n,[not,member]],[["a","b","c"],"d"]]` returns true.

* `[[n,[not,head]],[Variable1,Variable2]]` e.g. `[[n,[not,head]],[["a","b","c"],"a"]]` returns false.

* `[[n,[not,tail]],[Variable1,Variable2]]` e.g. `[[n,[not,tail]],[["a","b","c"],["b","c"]]]` returns false.

* `[[n,[not,append]],[Variable1,Variable2,Variable3]]` e.g. `[[n,[not,append]],[["a"],["b","c"],["a","b","c"]]]` returns false.

* `[[n,[]],[Variable1]]` e.g. `[[n,[]],[[v,a]]]` returns `[v,a]=[]`.

* `[[n,""],[Variable1]]` e.g. `[[n,""],[[v,a]]]` returns `[v,a]=""`.

* `[[n,"_"],[Variable1]]` behaves similarly to _A in Prolog e.g. `[[n,"_"],[[v,a]]]` returns true.

