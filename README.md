# prop-logic
A module representing propositional logic. Implemented in Haskell.

To use: run the appropriate executable and follow the prompts.

Be nice with the syntax.  
Wrap every ambiguity in parentheses. Actually, in the below example, there isn't any ambiguity in the second `-` in 

`(-p)||(-q)`

but wrap it up in parentheses anyways.  
Use `&&` for and, `||` for or, `=>` for implies, and `~` or `-` for not. Variable names can be single letters.

Example run:
```
How many statements would you like to assume?
1
Reading in 1.
-(p&&q)
What statement would you like to check?
(-p)||(-q)
Provable true.
```
