# Project 1a: Ruby Warmup

Due: February 5, 2023 at 11:59 PM (late February 6, *10% penalty*). **Note the deadline: just over one week**

Points: 80 public, 20 semipublic

**This is an individual assignment. You must work on this project alone.**

## Before You Start

**If you have not yet completed [project 0](https://classroom.github.com/a/uXj0y7qf), you should do so before starting this project.**  At the very least, you should have everything related to Ruby installed correctly.  If you have any trouble with installation, check Piazza (or create a post if you don't find your problem there), or come to office hours to get help from a TA.

## Introduction
This project aims to give you some experience with basic Ruby functionality, including using basic data types (integers, strings), collections (arrays and hashes), and classes. You will also become familiar with Ruby's basic control constructs for looping and conditional execution, and how to run Ruby unit tests.

## Testing
During project 0 you installed a ruby gem called `minitest`. This is a testing framework for testing ruby code. To run the public tests, you can run

```bash
ruby /path/to/tests/public/public.rb
```

You can also write your own tests in this file. Just make sure that tests methods start with `test_`. You can also look through this file to see examples of how to use `assert_equal`. I would also recommend checking out the documentation [here](https://github.com/minitest/minitest). 

## Submitting
You will submit this the same way you submitted project 0: by running `gradescope-submit` in the `project1a` folder.  If you are unable to get this to work, you can just submit both files in the src/ directory manually to the assignment on Gradescope.

## A Note on Types
Ruby has no built-in way to restrict the types passed to methods. As such, all method types specified in this document are the only ones you need to handle, or a `Generic` type is given. You may assume that no arguments will be passed outside of the types we specify, and your program may do anything in cases where improperly typed arguments are passed. This is undefined behavior for this program and **will not be tested**.

The expected types will be represented in the following format at the beginning of each section:

```ruby
(String) -> Array or nil
```

The left-hand side of the arrow specifies the parameter types, and the right-hand side specifies the return type. This example describes a method that takes a single `String` as an argument and either returns an `Array` or `nil`. When implementing a method with this signature, you may assume that a `String` will be passed in and you are responsible for ensuring that *only* a `Array` or `nil` is returned. (Since Ruby is object oriented, the signature also means that a subclass of `String` could be passed in, and that a subclass of `Array` could be returned.)

**Note**: Some shorthand is used to avoid verbosity in type signatures; namely:
- `Integer` is used to refer to either `Fixnum` or `Bignum` (i.e., we can think of `Integer` as a superclass of these two).
- `Bool` is used to refer to the either `TrueClass` or `FalseClass`.
- `Generic` is used to refer to some arbitrary value. The type is not fixed. 
- `nil` is used to refer to `NilClass`.

Each of the methods you must implement are described below. We provide you with the signature of each method and a description of its required behavior. For some methods, we state assumptions that you can make about the input. In these cases, it doesn't matter what your code does if the assumption isn't met, since we will never run a test case that contradicts the assumption.

You may add helper methods if you need them.

# Part 1 (Basic Data Types)

#### `fib(n)`
- **Description**: Returns the first `n` [fibonacci numbers](https://www.mathsisfun.com/numbers/fibonacci-sequence.html#:~:text=Here%20is%20a%20longer%20list,196418%2C%20317811%2C%20...).
- **Type**: `(Integer) -> Array`
- **Assumptions**: `n` is non-negative.
- **Examples**:
  ```ruby
  fib(0) == []
  fib(1) == [0]
  fib(2) == [0, 1]
  fib(3) == [0, 1, 1]
  fib(10) == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
  ```

#### `isPalindrome(n)`
- **Description**: Returns `true` if `n` is a palindrome and `false` otherwise. A palindrome is the same read forward and backward.
- **Type**: `(Integer) -> Bool`
- **Assumptions**: `n` is non-negative; `n` will not be provided with any leading 0s.
- **Hint**: It may be easier to do this after converting the provided integer to a String.
- **Examples**:
  ```ruby
  isPalindrome(0) == true
  isPalindrome(1) == true
  isPalindrome(10) == false
  isPalindrome(101) == true
  isPalindrome(120210) == false
  ```

#### `nthmax(n, a)`
- **Description**: Returns the `n`th largest element in the array `a` or `nil` if it does not exist. The largest element is specified using n = 0. Treat duplicated values seperately.
- **Type**: `(Integer, Array) -> Integer or nil`
- **Assumptions**: `n` is non-negative.
- **Examples**:
  ```ruby
  nthmax(0, [1,2,3,0]) == 3
  nthmax(1, [3,2,1,0]) == 2
  nthmax(2, [7,3,4,5]) == 4
  nthmax(5, [1,2,3]) == nil
  ```

#### `freq(s)`
- **Description**: Returns a one-character string containing the character that occurs with the highest frequency within 's'. If `s` has no characters, it should return the empty string.
- **Type**: `(String) -> String`
- **Assumptions**: Only one character will have the highest frequency (i.e. there will be no "ties").
- **Examples**:
  ```ruby
  freq("") == ""
  freq("aaabb") == "a"
  freq("bbaaa") == "a"
  freq("ssabcd") == "s"
  freq("a12xxxxxyyyxyxyxy") == "x"
  ```

# Part 2 (Basic Data Structures)
#### `zipHash(arr1, arr2)`
- **Description**: Returns a hash that maps corresponding elements in `arr1` and `arr2`, i.e., `arr1[i]` maps to `arr2[i]`, for all i. If the two arrays are not the same length, return `nil`.
- **Type**: `(Array, Array) -> Hash or nil`
- **Examples**:
  ```ruby
  zipHash([], []) == {}
  zipHash([1], [2]) == {1 => 2}
  zipHash([1, 5], [2, 4]) == {1 => 2, 5 => 4}
  zipHash([1], [2, 3]) == nil
  zipHash(["Mamat", "Hicks", "Vinnie"], ["prof", "prof", "TA"]) == {"Mamat" => "prof", "Hicks" => "prof", "Vinnie" => "TA"}
  ```

#### `hashToArray(hash)`
- **Description**: Returns an array of arrays; each element of the returned array is a two-element array where the first item is a key from the hash and the second item is its corresponding value. The entries in the returned array must be sorted in the same order as they appear in `hash.keys`.
- **Type**: `(Hash) -> Array`
- **Examples**:
  ```ruby
  hashToArray({}) == []
  hashToArray({"a" => "b"}) == [["a", "b"]]
  hashToArray({"a" => "b", 1 => 2}) == [["a", "b"], [1, 2]]
  hashToArray({"x" => "v", "y" => "w", "z" => "u"}) == [["x", "v"], ["y", "w"], ["z", "u"]]
  ```

# Part 3 (Code Control)

#### `MaxProcChain(init, procs)`
- **Description**: Takes a list of procs and decides to either apply each proc
or not to maximize the final or not. For example, if I have a list of procs:
`[procA, procB, procC]` and an initial value `x`, then I take the maximum value
of
   + `x`
   + `procA(x)`
   + `procB(procA(x))`
   + `procC(procB(procA(x)))`
   + `procC(procA(x))`
   + `ProcB(x)`
   + `ProcC(ProcB(x))`
   + `ProcC(x)`
You may assume each proc has type `Integer -> Integer`.
- **Type**: `(Integer, Array) -> Integer`
- **Examples**:
  ```ruby
  MaxProcChain(2,[Proc.new{|x| x + 6}]) == 8
  MaxProcChain(2,[Proc.new{|x| x + 4},Proc.new{|x| x * 4]}) == 24
  MaxProcChain(-4,[Proc.new{|x| x * 4},Proc.new{|x| x + 3}]) == -1
  ```

# Part 4 (Object Oriented Programming)

For this part, edit the `roster.rb`. You will be making a `roster` class that
keeps track of a list of `Person`s. There are 2 types of `Persons`: `Staff` and
`Student`s. The following describes the 4 classes you need to make along with 
any mandatory associated methods. You may add other classes and methods if you
need them.

## Person
 This is the superclass of `Staff` and `Student`. Every person has a `name`, 
 and `age` attribute. Every `Person` should also have the following 
 methods
 
#### `initialize(name,age)`
- **Description**: creates a Person with `name` and `age`. 
- **Type**: `(String, Integer)-> self`
- **Examples**:
  ```ruby
  Person.new('Cliff',84)
  ```

#### `getAge`
- **Description**: Returns the age of the person
- **Type**: `nil-> Integer`
- **Examples**:
  ```ruby
  clyff = Person.new('Cliff', 84)
  clyff.getAge == 84
  ```

#### `setAge(x)`
- **Description**: changes the age of the person. You may assume that any age 
is valid. Returns `self`
- **Type**: `Integer -> self`
- **Examples**:
  ```ruby
  clyff = Person.new('Cliff', 84)
  clyff.setAge(42)
  clyff.getAge == 42
  ```

## Student

This is a subclass of a `Person`. Each student has a `grade` attribute.
Every `Student` should also have the following methods

#### `initialize(name,age, grade)`
- **Description**: creates a Student with `name`, `age` and `grade` 
- **Type**: `(String, Integer, Float)-> self`
- **Examples**:
  ```ruby
  Student.new('Cliff',16,72.5)
  ```

#### `getGrade`
- **Description**: Returns the grade of the student 
- **Type**: `nil-> Float`
- **Examples**:
  ```ruby
  clyff = Student.new('Cliff',16,72.5)
  clyff.getGrade == 72.5
  ```

#### `changeGrade(x)`
- **Description**: changes the grade of the student. You may assume that any 
grade is valid. Returns `self`
- **Type**: `Float-> self`
- **Examples**:
  ```ruby
  clyff = Student.new('Cliff', 84, 50.0)
  clyff.changeGrade(42.0)
  clyff.getGrade == 42.0
  ```

## Staff

This is a subclass of a `Person`. Each staff member has a `position` attribute
Every `Staff` member should also have the following methods

#### `initialize(name,age, position)`
- **Description**: creates a Student with `name`, `age` and `position` 
- **Type**: `(String, Integer, String)-> self`
- **Examples**:
  ```ruby
  Staff.new('Cliff',16,'Professsor')
  ```

#### `getPosition`
- **Description**: Returns the position of the staff member 
- **Type**: `nil-> String`
- **Examples**:
  ```ruby
  clyff = Staff.new('Cliff',16,"TA")
  clyff.getPosition== "TA"
  ```

#### `changePosition(newPosition)`
- **Description**: changes the position of the student. You may assume that 
`newPosition` is always valid. Returns `self`
- **Type**: `String -> self`
- **Examples**:
  ```ruby
  clyff = Staff.new('Cliff', 84, "TA")
  clyff.changePosition("Head TA")
  clyff.getPosition == "Head TA"
  ```

## Roster

This will hold all the `Person`s. You should make your own `initialize` method..

#### `add(person)`
- **Description**: Adds the person to the roster. 
- **Type**: `Person -> nil`
- **Examples**:
  ```ruby
  roster = Roster.new
  roster.add(Staff.new('Cliff', 84, 'Professor'))
  ```

#### `size`
- **Description**: Returns how many people are in the roster 
- **Type**: `nil -> Integer`
- **Examples**:
  ```ruby
  roster = Roster.new
  roster.size == 0
  roster.add('Cliff', 84, 'Professor')
  roster.size == 1
  ```

#### `remove(Person)`
- **Description**: remove the person from the roster. You may assume everyone 
in the roster is unique. If the person is not in the roster, do nothing.
- **Type**: `Person -> nil`
- **Examples**:
  ```ruby
  roster = Roster.new
  clyff = Person.new('Cliff', 84)
  roster.add(clyff)
  roster.remove(clyff)
  roster.size == 0
  ```

#### `getPerson(name)`
- **Description**: get the person with `name` in the roster. You may assume that
everyone in the roster has a unique name. If the person is not in the roster, 
return nil.
- **Type**: `String-> Person`
- **Examples**:
  ```ruby
  roster = Roster.new
  cliff = Person.new('Cliff', 84)
  roster.add(cliff)
  cliff == roster.getPerson('Cliff')
  ```

#### `map`
- **Description**: Takes in a `Person -> Person` codeblock and applies it to 
every person in the roster. If no codeblock is given, do nothing.
- **Type**: `nil`
- **Examples**:
  ```ruby
  roster = Roster.new
  roster.add(Person.new('Cliff', 84))
  roster.add(Person.new('Clyff', 42))
  roster.map{|person| person.changeAge(52)}
  roster.getPerson('Cliff').getAge == 52
  roster.getPerson('Clyff').getAge == 52
  ```
