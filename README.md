# LCPI: an untyped Lambda Calculus Parser and Interpreter

[![Build Status](https://travis-ci.org/kevinvandervlist/lcpi.svg?branch=master)](https://travis-ci.org/kevinvandervlist/lcpi)
[![Coverage Status](https://coveralls.io/repos/github/kevinvandervlist/lcpi/badge.svg)](https://coveralls.io/github/kevinvandervlist/lcpi)

This provides a parser and interpreter and REPL for the untyped lambda calculus. 
The REPL has tracing capabilities to show all intermediate reduction steps.
Among others, the following features are supported:

* Lambda is either a `λ` or a `\\ `, so `\\x.x` and `λx.x` are identical.
* There is a shorthand available for nesting, so `λx y z.x y z` is equal to `λx.λy.λz.x y z`.
* The parser is fully left-associative, so `x y z` is `(x y) z`.
* The REPL can toggle between a normal and a *tracing* mode, showing all intermediate reduction steps.
* Although not supported in pure λ-calculus, one can use the REPL to assign expressions to variables. These will be substituted before running the α-reduction.
* A few well-known functions are added by default for convenience (e.g. I, Y, TRUE etc).
* Additional files containing extra λs can be (re)loaded from within the REPL. See the loading and reloading demo.
* You can `load` files from disk, and `reload` them if you changed them.
* Any valid λ-expression can be rendered as [De Bruijn Index notation](https://en.wikipedia.org/wiki/De_Bruijn_index) via the `dbi` command.
* Run `help` in the REPL for additional information.

## running the REPL:
* You can download [the latest release](https://github.com/kevinvandervlist/lcpi/releases/latest) ([changelog](https://github.com/kevinvandervlist/lcpi/blob/master/CHANGELOG.md)).
* ...or clone the repo and `sbt run` it.

## Building (and testing) the REPL
* `sbt test`
* `sbt assembly`
* `java -jar repl/target/scala-2.12/repl.jar`

## Demo: derive I from S and K

```
╭─kevin@Asus15  ~/src/lcpi  ‹master*›
╰─$ sbt run
[...]

lcpi λ> show
[...]
S := (λx.(λy.(λz.((x z) (y z)))))
K := (λx.(λy.x))
lcpi λ> trace
Trace mode is now on.
lcpi λ> S K K
S => ((S K) K)
S => (((λx.(λy.(λz.((x z) (y z))))) K) K)
S => (((λx.(λy.(λz.((x z) (y z))))) (λx.(λy.x))) (λx.(λy.x)))
α => (((λx.(λy.(λz.((x z) (y z))))) (λa.(λb.a))) (λc.(λd.c)))
β => ((λy.(λz.(((λa.(λb.a)) z) (y z)))) (λc.(λd.c)))
β => (λz.(((λa.(λb.a)) z) ((λc.(λd.c)) z)))
β => (λz.((λb.z) (λd.z)))
β => (λz.z)
η => (λz.z)
```

## Demo: successor

```
lcpi λ> ZERO := λf.λx.x
(λf.(λx.x))
lcpi λ> SUCCESSOR := λn.λf.λx.f (n f x)
(λn.(λf.(λx.(f ((n f) x)))))
lcpi λ> SUCCESSOR ZERO
(λf.(λx.(f x)))
lcpi λ> SUCCESSOR (SUCCESSOR ZERO)
(λf.(λx.(f (f x))))
lcpi λ> trace
Trace mode is now on.
lcpi λ> SUCCESSOR (SUCCESSOR ZERO)
S => (SUCCESSOR (SUCCESSOR ZERO))
S => ((λn.(λf.(λx.(f ((n f) x))))) ((λn.(λf.(λx.(f ((n f) x))))) ZERO))
S => ((λn.(λf.(λx.(f ((n f) x))))) ((λn.(λf.(λx.(f ((n f) x))))) (λf.(λx.x))))
α => ((λn.(λf.(λx.(f ((n f) x))))) ((λa.(λb.(λc.(b ((a b) c))))) (λd.(λe.e))))
β => (λf.(λx.(f ((((λa.(λb.(λc.(b ((a b) c))))) (λd.(λe.e))) f) x))))
β => (λf.(λx.(f (((λb.(λc.(b (((λd.(λe.e)) b) c)))) f) x))))
β => (λf.(λx.(f ((λc.(f (((λd.(λe.e)) f) c))) x))))
β => (λf.(λx.(f (f (((λd.(λe.e)) f) x)))))
β => (λf.(λx.(f (f ((λe.e) x)))))
β => (λf.(λx.(f (f x))))
η => (λf.(λx.(f (f x))))
```

## Demo: summation of positive natural numbers
```
lcpi λ> ZERO := λf.λx.x
(λf.(λx.x))
lcpi λ> SUCCESSOR := λn.λf.λx.f (n f x)
(λn.(λf.(λx.(f ((n f) x)))))
lcpi λ> ONE := SUCCESSOR ZERO
(λf.(λx.(f x)))
lcpi λ> TWO := SUCCESSOR ONE
(λf.(λx.(f (f x))))
lcpi λ> THREE := SUCCESSOR TWO
(λf.(λx.(f (f (f x)))))
lcpi λ> ISZERO := λn.n (λx.FALSE) TRUE
(λn.((n (λx.(λa.(λy.y)))) (λb.(λc.b))))
lcpi λ> PLUS := λm.λn.m SUCCESSOR n
(λm.(λn.((m (λa.(λf.(λx.(f ((a f) x)))))) n)))
lcpi λ> PREDECESSOR := λn.n (λg.λk.ISZERO (g ONE) k (PLUS (g k) ONE)) (λv.ZERO) ZERO
(λn.(((n (λg.(λk.(((((g (λf.(λh.(f h)))) (λx.(λb.(λy.y)))) (λc.(λd.c))) k) (((g k) (λo.(λp.(λq.(p ((o p) q)))))) (λs.(λt.(s t)))))))) (λw.(λz.(λx0.x0)))) (λf0.(λx1.x1))))
lcpi λ> PARTIALSUMMATION := (λf.λn.(ISZERO n) ZERO (PLUS n (f (PREDECESSOR n))))
(λf.(λn.((((n (λx.(λb.(λy.y)))) (λc.(λd.c))) (λe.(λg.g))) ((n (λi.(λj.(λk.(j ((i j) k)))))) (f (((n (λo.(λp.(((((o (λz.(λx0.(z x0)))) (λr.(λs.(λt.t)))) (λu.(λv.u))) p) (((o p) (λn1.(λf1.(λx2.(f1 ((n1 f1) x2)))))) (λf2.(λx3.(f2 x3)))))))) (λv0.(λf4.(λx5.x5)))) (λf5.(λx6.x6))))))))
lcpi λ> SUMMATION := Y PARTIALSUMMATION 
[...] // really, really long λ-expression
lcpi λ> SUMMATION THREE
(λo.(λp.(o (o (o (o (o (o p))))))))
```

## Demo: loading and reloading
```
╭─kevin@Asus15  ~/src/lcpi  ‹master› 
╰─$ cat extra.lcpi 
# This is a comment
# The following line will assign `FOO`
FOO := λx.x
# This will silently be discarded
astnuasohu a +]4Clru[
# This will be loaded as well
BAR := FOO y
╭─kevin@Asus15  ~/src/lcpi  ‹master› 
╰─$ sbt run       
[...]

A λ-calculus interpreter. Type `help` for usage information.
    
lcpi λ> show
[...]
lcpi λ> load extra.lcpi
Successfully loaded file `extra.lcpi`
lcpi λ> show
[...]
BAR := (FOO y)
FOO := (λx.x)
lcpi λ>
// In another terminal
╭─kevin@Asus15  ~/src/lcpi  ‹master› 
╰─$ echo "QUUK := z" >> extra.lcpi 

// Back to the REPL again
lcpi λ> reload
Successfully reloaded file `extra.lcpi`
lcpi λ> show
[...]
BAR := (FOO y)
QUUK := z
FOO := (λx.x)
```
