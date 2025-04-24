# Scoped Values

implementation of scoped values inspired by JEP487

current problems:
- unwinds the stack for every get, caching would be nice perhaps
- threading support is hilariously inefficient