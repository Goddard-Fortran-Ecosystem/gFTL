## template parameters

### mandatory parameters

	| parameter | definition|
	|-----------|-----------|
	| Key       | the type of keys |
	| T         | the type of values |
 
### optional parameters 
 
	| parameter | definition | default  |
	|-----------|------------|----------|
	| map       | container type | Map |
	| iterator  | iterator type  | MapIterator |
	| pair      | iterator type  | Pair |
	!-----------|------------|---------|
 


## component types

	| Member type      | Definition             | Default  |
	|------------------|------------------------|----------|
	| container        | Map                    | Map            |
	| iterator         | bidirectional iterator | MapIterator            |
	| value_type       | T                      | n/a               |
	| size_type        | integer type-kind      | `integer(kind=selected_int_kind(18))` |
	| difference_type  | integer type-kind      | `integer(kind=selected_int_kind(18))` |
	

## Constructor overloads

1. Empty map
``` f90
function map() result(m)
   type(map) :: m
end function map
```

## Type-bound procedures

### Element access

| procedure |  action |
|-|-|
[of](#of) | access specified element |
[at](#at) | access specified element with bounds checking |
[set](#set) | inserts or modifies value at key if it already exists

#### of

``` f90
function of(m, key)
   <T>, pointer :: of
   class(Map), target, intent(in) :: m
   <Key>, intent(in) :: key
end function of
```

#### at

``` f90
function at(m, key, rc)
   <T>, pointer :: of
   class(Map), target, intent(in) :: m
   <Key>, intent(in) :: key
   integer, intent(out) :: rc
end function at
```

#### set

``` f90
subroutine set(m, key, value)
   class(Map), target, intent(inout) :: m
   <Key>, intent(in) :: key
   <T>, intent(in) :: value
end function set
```


### Capacity
| procedure |  action |
|-|-|
|[empty](#empty) | checks if container is empty |
|[size](#size)   | returns number of elements   |
|[max_size](#max_size) | returns the maximum possible number of elements |


### Modifiers

| procedure       | action |
|- |-   |
| [clear](#clear)         | clears the contents                   |
| [insert](#insert)       | inserts elements                      |
| [erase](#erase)         | erases elements                       |
| [swap](#swap)           | swaps contents with another container |
| [extract](#extract)     | unimplemented                         |
| [merge](#merge)         | unimplemented                         |


#### clear
```f90
subroutine clear(m)
   class(map), intent(inout) :: m
end subroutine clear
```

Examples:

#### insert
1. insert key and value
```f90
subroutine insert(m, key, value)
   <class(map), intent(inout) :: m
   <Key>, intent(in) :: key
   <T>, intent(in) :: value
end subroutine insert
```
2. insert pair
```f90
subroutine insert(m, p)
   <class(map), intent(inout) :: m
   <pair>, intent(in) :: p
end subroutine insert
```

#### erase
1. Erase one element
```f90
function erase(m, key) result(iter)
   type(iterator) :: iter
   class(map), intent(inout) :: m
   <Key>, intent(in) : key
end function erase
```

2. Remove elements in range [first, last)
```f90
function erase(m, first, last) result(iter)
   class(map), intent(inout) :: m
   type(iterator), intent(in) : first
   type(iterator), intent(in) : last
   type(iterator) :: iter
end function erase
```


#### swap

1. Swap elements with another container

``` f90
subroutine swap(m, x)
   class(map), intent(inout) :: m
   type(map), intent(inout) :: x
end subroutine swap
```




### Lookup

| procedure       | action |
|- |-   |
| [count](#count)         | returns number of elements matching specific key |
| [find](#find)         | finds element with specific key |
| [contains](#contains)         | checks if container contains element with specific key |



## Non type-bound procedures

### Relational operators

[operator(==)](#operator(==))
`operator(/=)`
`operator(<)`
`operator(<=)`
`operator(>)`
`operator(>=)`

#### operator(==)

``` f90
logical function equal_to(a, b)
   type(map), intent(in) :: a
   type(map), intent(in) :: b
end function equal_to
```

#### operator(/=)

``` f90
logical function not_equal_to(a, b)
   type(map), intent(in) :: a
   type(map), intent(in) :: b
end function not_equal_to
```

#### operator(<)

``` f90
logical function less_than(a, b)
   type(map), intent(in) :: a
   type(map), intent(in) :: b
end function less_than
```

#### operator(<=)

``` f90
logical function less_than_or_equal_to(a, b)
   type(map), intent(in) :: a
   type(map), intent(in) :: b
end function less_than_or_equal_to
```

#### operator(>)

``` f90
logical function greater_than(a, b)
   type(map), intent(in) :: a
   type(map), intent(in) :: b
end function greater_than
```

#### operator(>=)

``` f90
logical function greater_than_or_equal_to(a, b)
   type(map), intent(in) :: a
   type(map), intent(in) :: b
end function greater_than_or_equal_to
```

### Others
`swap`
`erase`
`erase_if` (unimplemented)
 
