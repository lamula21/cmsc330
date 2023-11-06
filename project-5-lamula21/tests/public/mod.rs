extern crate stark_suit_repair;

use std::collections::HashMap;

use stark_suit_repair::basics::{gauss, in_range, subset, mean, to_decimal, factorize, rotate, substr, longest_sequence};
use stark_suit_repair::locator::{PriorityQueue, distance, target_locator};


//Basic Functions
#[test]
fn public_test_gauss() {
    assert_eq!(190, gauss(19)); //prime
    assert_eq!(1, gauss(1));
    assert_eq!(54615, gauss(330));   //composite
    assert_eq!(-1, gauss(-400));
}

#[test]
fn public_test_in_range() {
    let xs = [1,3,5];
    assert_eq!(1, in_range(&xs, 2, 4));
    let xs = [1,2,3,5,6];
    assert_eq!(2, in_range(&xs, 2, 4));
    let xs = [-4,-3,-2,-1,0,1,2,3,4];
    assert_eq!(9, in_range(&xs, -5, 5));
}

#[test]
fn public_test_subset() {
    let ys = [1,2,3,4,5];
    let xs = [3,2,1];
    let zs = [6];

    assert_eq!(true, subset(&ys, &xs));
    assert_eq!(false, subset(&ys, &zs));
    assert_eq!(true, subset(&ys, &ys));
}

#[test]
fn public_test_mean() {
    let xs = [1 as f64,2 as f64,3 as f64,4 as f64];
    assert_eq!(Some(2.5), mean(&xs));
    let xs = [-4 as f64, -5 as f64, -6 as f64];
    assert_eq!(Some(-5.), mean(&xs));
    let xs = [-400 as f64, 1 as f64, 2 as f64, 2 as f64, 400 as f64];
    assert_eq!(Some(1.), mean(&xs));
}

#[test]
fn public_test_decimal() {
    let xs = [1,0,1,0,0,1,0,1,0];
    assert_eq!(330, to_decimal(&xs));

    let ys = [0];
    assert_eq!(0, to_decimal(&ys));

    let zs = [1,0,1,0,1,1,1,1,1];
    assert_eq!(351, to_decimal(&zs));
}

#[test]
fn public_test_factorize() {
    let mut vec = Vec::new();
    vec.push(2);
    vec.push(2);
    assert_eq!(vec, factorize(4));
    vec.remove(0);
    vec.remove(0);
    vec.push(19);
    assert_eq!(vec, factorize(19));
    vec.remove(0);
    vec.push(2);
    vec.push(3);
    vec.push(5);
    vec.push(11);
    assert_eq!(vec, factorize(330));
}

#[test]
fn public_test_rotate() {
    let mut vec = Vec::new();
    vec.push(3);
    vec.push(3);
    vec.push(0);

    let xs = [0,3,3];
    assert_eq!(vec, rotate(&xs));

    vec.remove(0); vec.remove(0); vec.remove(0);

    let xs = [1];
    vec.push(1);
    assert_eq!(vec, rotate(&xs));
    vec.remove(0);

    let xs = [1, 1, 2, 1];
    vec.push(1); vec.push(2); vec.push(1); vec.push(1);
    assert_eq!(vec, rotate(&xs));
}

#[test]
fn public_test_substr() {
    assert_eq!(true, substr(&"CMSC 330 is the best CS class".to_string(), &"CMSC 330".to_string()));
    assert_eq!(false, substr(&"CMSC 330 is the best CS class".to_string(), &"CMSC 351 is the best CS class".to_string()));
    assert_eq!(true, substr(&"I love CMSC330".to_string(), &"I love CMSC330".to_string()));
}

#[test]
fn public_test_longseq() {
    assert_eq!(Some("aa"), longest_sequence(&"aabbaa".to_string()));
    assert_eq!(Some("bb"), longest_sequence(&"a abba a".to_string()));
    assert_eq!(Some("bbbbbb"), longest_sequence(&"babaabbbaaaabbbbbbaaaa"));
}

//Locator Functions
#[test]
fn public_test_priority_queue() {
    let mut q = Vec::new();

    q.enqueue(5);
    q.enqueue(1);
    q.enqueue(3);
    q.enqueue(4);

    assert_eq!(1, q[0]);
    assert_eq!(4, q[1]);

    assert_eq!(Some(&1), q.peek());
    assert_eq!(Some(1), q.dequeue());
    assert_eq!(Some(&3), q.peek());
    assert_eq!(Some(3), q.dequeue());
}

#[test]
fn public_test_distance() {
    let c1 = (5,5);
    let c2 = (3,2);

    assert_eq!(5, distance(c1, c2));
}

#[test]
fn public_test_locator() {
    let mut allies: HashMap<&String, (i32, i32)> = HashMap::new();
    let stark: String = "Stark".to_string();
    let hulk: String = "Hulk".to_string();
    allies.insert(&stark, (1 as i32,1 as i32));
    allies.insert(&hulk, (2 as i32,3 as i32));

    let mut enemies: HashMap<&String, (i32, i32)> = HashMap::new();
    let thanos: String = "Thanos".to_string();
    let ebony: String = "Ebony Maw".to_string();
    enemies.insert(&thanos, (3 as i32,2 as i32));
    enemies.insert(&ebony, (4 as i32,4 as i32));

    assert_eq!(("Ebony Maw", 4, 4), target_locator(&allies, &enemies));
}