https://en.cppreference.com/w/cpp/container/vector

### Primary type parameters (fpp)

| parameter | definition|
|-----------|-----------|
| T | the type of elements|

Note that each type parameter has numerous optional modifiers.

### Optional parameters (fpp)
 
| parameter | definition | default  |
|-----------|------------|----------|
| vector    | container type | Vector |
| iterator  | iterator type  | VectorIterator |
| riterator | reverse iterator type | VectorRIterator |
 
### Optional type modifiers

| decorator           | use       |
|---------------------|-----------|
| T_polymorphic       | elements are CLASS(T) (implies deferred) |
| T_rank              | elements are T arrays of rank T_rank (implies deferred) |
| T_shape             | elements are T arrays of specified shape (not deferred) |
| T_KINDLEN(context)  | use if T has kind/len parameters |
| T_kindlen_dummy     | override kind/len parameters for dummy arguments |
| T_kindlen_component | override kind/len parameters for declaring component |
| T_LT                | | 
| T_EQ                | |
| T_deferred          | indicates elements of T require ALLOCATABLE attribute |
| T_MOVE(lhs,rhs)     | move storage of element from rhs to lhs |
| T_COPY(lhs,rhs)     | copy element from rhs to lhs |
| T_FREE(x)           | free element |

 
## Container Types

| Member type      | Definition             | Default  |
|------------------|------------------------|----------|
| container        | vector                 | vector            |
| iterator         | random access iterator | VectorIterator    |
| reverse_iterator | random access iterator | VectorRIterator   |
| value_type       | T                      | n/a               |
| size_type        | integer type-kind      | `integer(kind=selected_int_kind(18))` |
| difference_type  | integer type-kind      | `integer(kind=selected_int_kind(18))` |
	
## Constructor overloads

1. Empty vector
``` f90
function vector() result(v)
   type(vector) :: v
end function vector
```

2. Constructs vector with count copies of elements.  Default value is
   used unless optional value is provided.
``` f90
function vector(count, unused, value, rc) result(v)
   type(vector) :: v
   integer(kind=size_kind), intent(in) :: count
   class(KeywordEnforcer), optional, intent(in) :: value

   type(T), optional, intent(in) :: value ! non polymorphic T
   ! or
   class(T), optional, intent(in) :: value ! polymorphic T
   
   integer, optional, intent(out) :: rc
end function vector
```

3. Same as (2), except count is of default integer kind.
``` f90
function vector(count, unused, value, rc) result(v)
   type(vector) :: v
   integer, intent(in) :: count
   class(KeywordEnforcer), optional, intent(in) :: value
   type(T), optional, intent(in) :: value
   integer, optional, intent(out) :: rc
end function vector
```

4. Construct vector with elements provided by contents in range `[last, first)`
``` f90
function vector(first, last) result(v)
   type(vector) :: v
   type(inputIt), intent(in) :: first
   type(inputIt), intent(in) :: last
end function vector
```

5. Construct vector from array of elements.   Requires that T be non-deferred.
``` f90
function vector(il) result(v)
   type(vector) :: v
   <T>, intent(in) :: il(:)
end function
```


## Type-bound procedures

### Element access

| procedure |  action |
|-|-|
[at](#at) | access specified element |
[front](#front) | access first element |
[back](#back) | access alst element |


#### at

1. No bounds checking
``` f90
function at(this, i)
   <T>, pointer :: at
   class(Vector), target, intent(in) :: this
   integer(kind=size_kind), intent(in) :: i
end function at
```

2. No bounds checking, default integer
``` f90
function at(this, i)
   <T>, pointer :: at
   class(Vector), target, intent(in) :: this
   integer, intent(in) :: i
end function at
```

3. Bounds checking
``` f90
function at(v, i, rc)
   <T>, pointer :: at
   class(Vector), target, intent(in) :: this
   integer(kind=size_kind), intent(in) :: i
   integer, intent(out) :: rc
end function at
```

4. Bounds checking, default integer
``` f90
function at(v, i, rc) 
   <T>, pointer :: at
   class(Vector), target, intent(in) :: this
   integer, intent(in) :: i
   integer, intent(out) :: rc
end function at
```

#### front

``` f90
function front(v)
   <T>, pointer :: front
   class(vector), target, intent(in) :: v
end function front
```

#### back

``` f90
function back(v)
   <T>, pointer :: back
   class(vector), target, intent(in) :: v
end function back
```


### Iterators

| procedure |  action |
|-|-|
[begin](#begin) | returns iterator to the beginning |
[end](#end) | returns iterator to end |
[rbegin](#rbegin) | returns reverse iterator to the beginning
[rend](#rend) | returns iterator to end |

#### begin

``` f90
function begin(v)
   type(iterator) :: begin
   class(vector), target, intent(in) :: v
end function begin
```

#### end

``` f90
function end(v)
   type(iterator) :: end
   class(vector), target, intent(in) :: v
end function end
```

#### rbegin

``` f90
function rbegin(v)
   type(iterator) :: rbegin
   class(vector), target, intent(in) :: v
end function rbegin
```

#### rend

``` f90
function rend(v)
   type(iterator) :: rend
   class(vector), target, intent(in) :: v
end function rend
```

### Capacity
| procedure |  action |
|-|-|
|[empty](#empty) | checks if container is empty |
|[size](#size)   | returns number of elements   |
|[max_size](#max_size) | returns the maximum possible number of elements |
|[reserve](#reserve) | reserves storage |
|[capacity](#capacity) | returns number of elements that can be held in currently allocated storage |

### Modifiers

| procedure       | action |
|- |-   |
| [clear](#clear)         | clears the contents                   |
| [insert](#insert)       | inserts elements                      |
| [erase](#erase)         | erases elements                       |
| [push_back](#push_back) | add an element to the end             |
| [pop_back](#pop_back)   | removes last element                  |
| [resize](#resize)       | changes the number of elements stored |
| [swap](#swap)           | swaps contents with another container |
		

#### clear
```f90
subroutine clear(this)
   class(vector), intent(inout) :: this
end subroutine clear
```

Examples:

#### insert
```f90
subroutine insert(this, pos, value)
   <class(vector), intent(inout) :: this
   type(iterator), intent(in) : pos
   <T>, intent(in) :: value
end subroutine insert
```

#### erase
1. Erase one element
```f90
function erase(this, pos) result(iter)
   type(iterator) :: iter
   <class(vector), intent(inout) :: this
   type(iterator), intent(in) : pos
end function erase
```

2. Remove elements in range [first, last)
```f90
function erase(this, first, last) result(iter)
   <class(vector), intent(inout) :: this
   type(iterator), intent(in) : first
   type(iterator), intent(in) : last
   type(iterator) :: iter
end function erase
```


#### push_back

``` f90
subroutine push_back(this, value)
   class(vector), intent(inout) :: this
   <T>, intent(in) :: value
end subroutine push_back
```

#### pop_back

``` f90
subroutine pop_back(this)
   class(vector), intent(inout) :: this
end subroutine pop_back
```

#### resize

1. Resize to count elements, optional value; count is kind=size_kind

``` f90
subroutine resize(this, count, unused, value, rc)
   class(vector), intent(inout) :: this
   integer(kind=size_kind), intent(in) :: count
   class(KeywordEnforcer), optional, intent(in) :: unused
   <T>, optional, intent(in) :: value
   integer, optional, intent(out) :: rc
end subroutine resise
```

2. Resize to count elements, optional value.  count is default integer.

``` f90
subroutine resize(this, count, unused, value, rc)
   class(vector), intent(inout) :: this
   integer, intent(in) :: count
   class(KeywordEnforcer), optional, intent(in) :: unused
   <T>, optional, intent(in) :: value
   integer, optional, intent(out) :: rc
end subroutine resise
```

#### swap

1. Swap elements with another container

``` f90
subroutine swap(this, x)
   class(vector), intent(inout) :: this
   type(vector), intent(inout) :: x
end subroutine swap
```



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
logical funciton equal_to(a, b)
   type(vector), intent(in) :: a
   type(vector), intent(in) :: b
end function equal_to
```

#### operator(==)

``` f90
logical funciton not_equal_to(a, b)
   type(vector), intent(in) :: a
   type(vector), intent(in) :: b
end function not_equal_to
```

#### operator(<)

``` f90
logical funciton less_than(a, b)
   type(vector), intent(in) :: a
   type(vector), intent(in) :: b
end function less_than
```

#### operator(<=)

``` f90
logical funciton less_than_or_equal_to(a, b)
   type(vector), intent(in) :: a
   type(vector), intent(in) :: b
end function less_than_or_equal_to
```

#### operator(>)

``` f90
logical funciton greater_than(a, b)
   type(vector), intent(in) :: a
   type(vector), intent(in) :: b
end function greater_than
```

#### operator(>=)

``` f90
logical funciton greater_than_or_equal_to(a, b)
   type(vector), intent(in) :: a
   type(vector), intent(in) :: b
end function greater_than_or_equal_to
```

### Others
`swap`
`erase`
`erase_if`

