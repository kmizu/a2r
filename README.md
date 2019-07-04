# a2r: A Command-line Tool Translating Regular Expression ASTs to Regular Expression Strings

[![Build Status](https://travis-ci.org/kmizu/a2r.png?branch=master)](https://travis-ci.org/kmizu/a2r)

a2r provides a way to write regexes as ASTs. Currently, only experimental parser is implemented.

Here is the example of my regex ASTs (character classes is not supported yet):

```
"assertThat" 
  "(" 
    `[a-zA-Z_]`*
    "," 
    `[a-zA-Z_]`*
  ")"
}
```

## Syntax

### identifier

```
a
```

### string literal

```
"a"
```

### implicit sequence

```
"a" "b"
```

### explicit sequence

```
["a" "b"]
```

### alternation

```
"a" | "b"
```

### grouping

```
("a" | "b")+ | c
```
