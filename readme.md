## Evaluation of the modified Sum-Product Loop Language (SPLL)
This evaluation framework is written in Haskell. 

To use Haskell it is recommended to have installed [GHC, Cabal and Stack](https://www.haskell.org/get-started/).

## Instructions
The evaluation data of distribution plots, quantitative comparison data, and query result can be replicated with a single command:
```
cabal run
```
Alternatively, depending on preference: 
```
stack run
```

### Features
This implementation features a functional [Parser](src/Parser/Program.hs) capable of converting strings into SPLL programs, which can subsequently be inferred using EVI, MAR, CON, MAP, and MMAP queries or used for [sample generation](src/Sample.hs).

### The implementation of the semantics rules
| Semantics  | File Location|
| ------------- | ------------- |
| Probability Semantics  | [src/Infer.hs](src/Infer.hs)  |
| Query Semantics | [src/Query.hs](src/Query.hs)  | 
| MAP Semantics | [src/MaximumAPosteriori.hs](src/MaximumAPosteriori.hs)  | 
