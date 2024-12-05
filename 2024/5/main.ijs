i=:1!:1<'input.txt'
nl=:10{a.
s=:>(nl,nl)splitstring i
ps=:>{{|.(0&".)"1 >'|'splitstring y}}"1>nl splitstring 0{s
ts=:}: >{{(0&".) "1 > ',' splitstring y}}"1>nl splitstring 1{s
cs=:{{>(1&{@{&ps)"1>(I. y = ({."1 ps))}}
sat =: dyad define
  pcs=.(cs((0~:y)i:1){y)(e.#[)x
  (#pcs)={.+/>(e.&y) "1 pcs
)
val =:{{(#y)=+/(y&sat) \ y}}
mid=:{{(<.((0~:y) i: 1)%2){y}}
echo+/(val"1 ts)*mid"1 ts
inv=:(1&~:@val) "1 ts
p=:{{((>(#@(e.#[)&(y-.x)@cs@>)"1 y-.x)i.0){y-.x}}
echo+/inv*(mid@{{(0$0)]F..{{y,y p((>x)-.y)}}(1+(0~:y)i:1)$<y}})"1 ts