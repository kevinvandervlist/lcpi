# Simple lambda calculus parser and interpreter

* Lambda is either a `λ` or a `\ `, so `\x.x` and `λx.x` are identical
* `sbt test`
* `sbt assembly`
* `java -jar interpreter/target/scala-2.12/interpreter.jar`

# Demo

```
╭─kevin@Asus15  ~/src/lcp  ‹master*›
╰─$ sbt run
[...]

A λ-calculus interpreter. Type `help` for usage information.
    
λ ZERO := λf.λx.x
(λf.(λx.x))
λ SUCCESSOR := λn.λf.λx.f (n f x)
(λn.(λf.(λx.(f ((n f) x)))))
λ SUCCESSOR ZERO
(λf.(λx.(f x)))
λ SUCCESSOR (SUCCESSOR ZERO)
(λf.(λx.(f (f x))))
λ trace
Trace mode is now on.
λ SUCCESSOR (SUCCESSOR ZERO)
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
λ I x
S => (I x)
S => ((λx.x) x)
α => ((λx.x) x)
β => x
η => x
```