## 0.4.0
+ Change trait to support types containing references (such as &str)
+ Allow override of regular expression of primitive via attribute
+ Add benchmark to prove that it is zero-cost abstraction

### 0.3.1
+ Fix failing test

### 0.3.0
+ Add compiler error for incorrect regex
+ Add compiler error for attempts to use named capture groups
+ Add compiler error for cases then format string does not match struct/enum declaration.

### 0.2.3
+ Add slack mode
+ Implement Reformation for char
+ Enum support
+ Tuple struct support

### 0.2.2
+ Auto replacement of capturing groups with non-capturing
+ Allow to omit items implementing default trait from format string
+ Add no_regex mode

### 0.2.1
+ Now able to use derived struct as build brick for other structs
+ Fixed macro naming for correct import

## 0.2.0
+ Changed Reformation trait signature
+ Add derive macro

## 0.1.0
+ Add simplest macro capable of some parsings. No derive yet
