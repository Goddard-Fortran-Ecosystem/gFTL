## template parameters

### mandatory parameters

	| parameter | definition|
	|-----------|-----------|
	| Key       | the type of elements|
	| T         | the type of elements|
 
### optional parameters 
 
	| parameter | definition | default  |
	|-----------|------------|----------|
	| vector    | container type | Vector |
	| iterator  | iterator type  | VectorIterator |
	| riterator | reverse iterator type | VectorRIterator |
	!-----------|------------|---------|
 


## component types types

	| Member type      | Definition             | Default  |
	|------------------|------------------------|----------|
	| container        | Vector                 | Vector            |
	| iterator         | random access iterator | VectorIterator    |
	| reverse_iterator | random access iterator | VectorRIterator   |
	| value_type       | T                      | n/a               |
	| size_type        | integer type-kind      | `integer(kind=selected_int_kind(18))` |
	| difference_type  | integer type-kind      | `integer(kind=selected_int_kind(18))` |
	
