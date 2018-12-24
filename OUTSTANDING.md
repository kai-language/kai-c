# Improve Errors

Update the error formatting desired may be to do something like the following

```kai
    libc.printf("argc = %d\n", argc)
    ^~~~
ERROR: Undeclared identifier 'libc'
    libc.printf("argv[0] = %s\n", argv[0])
    ^~~~
```

