## 0.6.0
+ Allow enums to have variant which cannot be parsed into.
+ Make regex override work for types implementing `ParseOverride` instead of `ReformationPrimitive`,
    removing latter, as its no longer needed.
+ Fixed buf with default not handled correctly

## 0.5.0
+ Remove old enum syntax.
+ Remove need for magical lifetime name and 
    add support for multiple lifetimes for 
    in place parsing.
+ Remove lazy_static public dependancy.
+ Add base for generics support.

### 0.4.1
+ Fix bug with enums not being put into group
+ Hide clippy lint on evaluation order

## 0.4.0
+ Change trait to support types containing references (such as &str)
+ Allow override of regular expression of primitive via attribute
+ Add benchmark to prove that it is zero-cost abstraction
+ Add per-variant regex declaration for enum

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
