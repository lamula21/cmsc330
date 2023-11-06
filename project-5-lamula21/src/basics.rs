use std::result;

/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    if n < 0 {
        return -1;
    }
    let n_1:i32 = n+1;
    (n * n_1)/2
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut count: i32 = 0; // allow count to mutate

    // each is a reference
    for each in ls.iter(){
        if *each >= s && *each <= e { // (*) derefence a reference, 
            count += 1;
        }       
    }
    count
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    for each in target {
        if !set.contains(&each) {
            return false;
        }
    }
    true
}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    if ls.is_empty() { return None; }
    let mean: f64 = ls.iter().fold(0.0, |acc, x| acc + x);
    let result: f64 = mean / (ls.len() as f64); 
    // `as f64` specifies that result type of this function is f64 since by default is usize
    Some(result)
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    let mut i = ls.len() as i32;
    let result: i32 = ls.iter().fold(0, |acc , x| {
        i -= 1;
        acc + (x * 2_i32.pow(i as u32))
    });
    return result;
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut mut_n: u32 = n;
    let mut factors: Vec<u32> = Vec::new();
    let mut factor: u32 = 2;
    while factor <= mut_n {
        if mut_n % factor == 0{
            factors.push(factor);
            mut_n /= factor;
        } else {
            factor += 1;
        }
    }
    factors   
}

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    // `::with_capacity`  pre-allocate the exact amount of memory needed for the output vector
    let mut rotated = Vec::with_capacity(lst.len());
    for i in 1..=lst.len() {
        rotated.push(lst[i % lst.len()]);
    }
    rotated    
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {

    // rustacean    Nope
    // acean
    
    // rustacean   Nope
    //  acean       
      
    // rustacean   Nope
    //   acean
      
    // rustacean   nope
    //    acean
      
    // rustacean
    //     acean     yep

    if s.len() == 0 {return true;}
    
    let mut initial_index = 0;
    let mut target_last_index = target.len();

    // loop from last target's index to last string's index inclusive
    while target_last_index <= s.len() {
        if &s[initial_index..target_last_index] == target { return true; }

        initial_index += 1;
        target_last_index += 1;
    }
    
    false
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    if s.is_empty() {
        return None;
    }
    let mut start:usize = 0;
    let mut end:usize = 0;
    let mut max_start:usize = 0;
    let mut max_end :usize = 0;

    while end < s.len() {
        if s.chars().nth(start) == s.chars().nth(end) {
            end += 1;
        } else {
            if end - start > max_end - max_start {
                max_start = start;
                max_end = end;
            }
            start = end;
        }
    }

    if end - start > max_end - max_start {
        max_start = start;
        max_end = end;
    }

    Some(&s[max_start..max_end])    
}
