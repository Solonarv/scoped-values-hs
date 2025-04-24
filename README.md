# Scoped Values

implementation of scoped values inspired by JEP487

current problems:
- does not work across threads
- unwinds the stack for every get, caching would be nice perhaps