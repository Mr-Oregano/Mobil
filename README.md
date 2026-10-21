# Mobil
### An implementation of the calculus from my thesis: *Coeffects for Code Mobility*. 

[Code mobility](https://en.wikipedia.org/wiki/Code_mobility) refers to the marshaling of *code* rather than *data* &mdash; a concept that has been in circulation for decades with the initial hope that it would find use in [mobile agents](https://en.wikipedia.org/wiki/Mobile_agent) but eventually failed for that purpose due to complexity and security concerns. Ultimately, coarse-grained code mobility became ubiquitous in the WWW in many forms (e.g. JavaScript, WASM/WASI, Edge-computing), "coarse-grained" meaning that code is packaged and transferred in entire, standalone (sandboxed) modules/programs rather than in bits and pieces. Fine-grained code mobility has seen prominence in data science and analytics. Engines like [Apache Spark](https://spark.apache.org/) allow programmers to execute custom code on distributed worker nodes that are more performant and closer to the data. Code mobility has certainly found its place in various industry-wide settings. But its accompanying problem of requirements tracking is often glossed-over. 

It's not always feasible to transfer the entire runtime environment marshaled code requires to execute (e.g. Docker Images). This can be due to the environment being too large, impractical to send, or containing confidential information. This means that certain resources must be provided by a receiving end before the code can be executed. The aforementioned instances of code mobility often lack flexibility in the approaches to this: WASI modules, being coarse-grained, impose whole-program requirements even if only part of the program will be executing; code for Spark is written in languages that lack sufficient static analyses for detecting implicit resource captures in lambdas, leading to the dreaded (runtime) `NotSerializableException`. The Mobil language developed in my thesis work proposes an alternative choice for modeling code mobility using a recent innovation in type systems research: coeffects.

Coeffects grade modal types and type-contexts with an algebraic structure (e.g. semiring) tracking requirements for terms &mdash; seemingly a natural fit for the problem of code mobility but has surprisingly seen limited investigation. The Mobil language is a simply-typed lambda calculus extended with coeffect-graded modal types (inspired from modal logic) that demonstrates what a type system like this might look like and what it might be capable of. The language type system can track the requirements a term needs to execute throughout its marshaling and sending process and ensure at compile time that a receiving end can provide those requirements (avoiding the `NotSerializableException`); it can also provide a runtime framework or enforcement for graceful handling when static checks are not possible; additionally, it can provide the ability for a receiver to sandbox the resources it provides. These are all first-class features offered by Mobil.

## Overview
The OCaml implementation of `Mobil` in this repository is a simple tree-walk interpreter that is not meant to be used as a real programming language but merely to service as a toy example for how type-safe tracking of fine-grained code mobility can be achieved with the latest research on coeffects.

The coeffect system used in this language is 'flat', meaning that it tracks information for the entire variable context $\Gamma$ rather than for each variable. The `marshal ...` construct allows the introduction of terms that are marshaled and meant to be transferred. References to variables that are not also marshaled within this code are tracked as the 'coeffectful' requirements for subsequent `unmarshal [ ... ] ...` terms.

Here is a simple working sample:

```py
let m = 
    let foo = \(n: num) -> n + n in
    let bar = marshal \(n: num) -> (foo n) + (foo n) in

    # 'bar' is serializable -- no tracking needed
    # 'bar' references 'foo' (unserializable) -- need to track 'foo'
    # 'bar 69' is NOT executed here, will execute in 'unmarshal'
    marshal (bar 69) in 
in

# ... 
# Potentially send 'm' via network communication
# ...

# 'foo' was tracked in the marshaled code, this needs to be provided
unmarshal [ foo: \(n: num) -> n ] m
```

In this example, some code is marshaled which references a function `bar`. However, `bar` was marked as marshaled previously so `bar` does not need to be tracked (it will be packaged along with the code when received by an `unmarshal`).

But, `bar` does reference `foo` which is *not* marshaled (meaning this is an unserializable function). So `foo` does need to be tracked as a dependency of `bar`. The resulting type of `m` will be `marsh [ foo: num -> num ] num` (here the latent coeffect in `[ ... ]` indicating that `foo` will be needed).

The `unmarshal` block then provides its own instance of `foo`. Note that this does not need to be the same instance as before, this allows for sandboxing or 'injection' of the needed resources by the receiving end.

---
More examples can be found in [the supplemental folder](supplemental/examples/). A full grammar BNF can be found [here](lib/frontend/parser.mly).

## Development

### With Nix Flakes and Package Manager
This project uses a [Nix flake](https://wiki.nixos.org/wiki/Flakes) for managing the development environment; the [Nix](https://nixos.org/) package manager will be needed with [flakes enabled](https://wiki.nixos.org/wiki/Flakes#Enabling_flakes_permanently). For quick usage, run the command: `nix develop` in the root of this project to enter the nix shell containing the required dependencies or `nix build` to create a final executable. Shell hooks like [`nix-direnv`](https://github.com/nix-community/nix-direnv) and relevant editor extensions are recommended for handling this automatically.

Once in the Nix shell, simply run `dune build` to build he project executable or run `dune exec Mobil -- <ARGS>` to execute the project. You can view usage with just `dune exec Mobil`.

### Other
This project can be built without Nix if this is not desired, *but the necessary environment and dependencies will need to be manually provided as dune is not being used to fully track dependencies here.*

You will need:
 - OCaml compiler version 5.3.0
 - Dune version 3.21.1
 - Packages: [`menhir`](https://ocaml.org/p/menhir/latest), [`alcotest`](https://ocaml.org/p/alcotest/latest)
