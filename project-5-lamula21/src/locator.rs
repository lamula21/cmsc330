use std::cmp::Ordering;
use std::collections::HashMap;

pub trait PriorityQueue<T: PartialOrd> {
    fn enqueue(&mut self, ele: T) -> ();
    fn dequeue(&mut self) -> Option<T>;
    fn peek(&self) -> Option<&T>;
}

/**
    An optional definition of a Node struct you may find useful
**/
struct Node<T> {
    priority: i32,
    data: T,
}


/** 
    These traits are implemented for Nodes to make them comparable 
**/
impl<T> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Node<T>) -> Option<Ordering> {
        self.priority.partial_cmp(&other.priority)
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Node<T>) -> bool {
        self.priority == other.priority
    }
}


/** 
    You must implement the above trait for the vector type 
**/
impl<T: PartialOrd> PriorityQueue<T> for Vec<T> {
    /**
        This functions pushes a given element onto the queue and
        reorders the queue such that the min heap property holds.
        See the project specifications for more details on how this
        works.
    **/
    fn enqueue(&mut self, ele: T) -> () {
        // add ele to self array
        self.push(ele);
        let mut element_index = self.len()-1;

        // reorder to that maintains min heap, 
        // for loop from the end to the begining
        while element_index > 0 {

            // how to find parent of element?
            // parent =  element's index -1 / 2
            let parent_index =  (element_index-1)/2;

            // if parent > element then swap
            // otherwise finish loop
            if self[parent_index] > self[element_index] {
                // swap
                self.swap(parent_index, element_index)
            } else {
                break;
            }
            // update element_index for next iteration
            element_index = parent_index;
        }

    }

    /**
        This function removes the root element from the queue and
        reorders the queue such that it maintains the min heap
        property.  See the project specifications for more details.
        You should return the deleted element in the form of an option.
        Return None if the queue was initially empty, Some(T) otherwise.
    **/
    fn dequeue(&mut self) -> Option<T> {
        if self.is_empty() {
            // if the queue is empty, return None
            return None;
        }
        let length = self.len() - 1;
        // swap root element (to be removed) with last element
        self.swap(0, length);

        // remove and return the last element (which was the root element)
        let result: Option<T> = self.pop();

        // re-heapify the queue
        let mut parent_index = 0;
        let mut child_index = 1;

        while child_index < self.len() {
            // find the smaller child
            if child_index + 1 < self.len() && self[child_index] > self[child_index + 1] {
                child_index += 1;
            }

            // if parent is greater than smaller child, swap them
            if self[parent_index] > self[child_index] {
                self.swap(parent_index, child_index);
            } else {
                break;
            }

            // update parent and child indices for next iteration
            parent_index = child_index;
            child_index = 2 * parent_index + 1;
        }

        result
    }

    /**
        This function returns the element that would be removed
        if dequeue were called on the queue.  There should be no
        mutations to the queue.  Return the element in the form
        of an option.  Return None if the queue is empty, Some(T)
        otherwise.
    **/
    fn peek(&self) -> Option<&T> {
        if self.is_empty() {
            None
        } else {
            Some(&self[0])
        }
    }
}


/**
    You must implement this function that computes the orthogonal
    distance between two coordinates.  Remember, orthogonal distance
    is not like Euclidean distance.  See the specifications for more
    details.
**/
pub fn distance(p1: (i32,i32), p2: (i32,i32)) -> i32 {
    (p2.0-p1.0).abs() + (p2.1 - p1.1).abs()
}

/**
    You must implement this function that determines which enemy Stark
    should battle and their coordinates.  You are given two hashmaps for
    allies and enemies.  Each maps a name to their current coordinates.
    You can assume that the allies hashmap will always have a name
    called "Stark" included.  Return the name and coordinates of the enemy
    Stark will battle in the form of a 3-tuple.  See the specifications
    for more details on how to choose which enemy.
**/
pub fn target_locator<'a>(allies: &'a HashMap<&String, (i32,i32)>, enemies: &'a HashMap<&String, (i32,i32)>) -> (&'a str,i32,i32) {

    let mut priority_queue: Vec<Node<(&str, &str)>> = Vec::new();
    let mut valid_moves: Vec<Node<(&str, &str)>> = Vec::new();

    for (enemy, e_tuple) in enemies.iter() {
        for (ally, a_tuple) in allies.iter() {

            let node = Node {
                priority: distance(*e_tuple , *a_tuple),
                data: (enemy.as_str(), ally.as_str())
            };

            priority_queue.enqueue(node);
        }
    }

    // for node in &priority_queue {
    //     println!("priority: {}, data: {:?}", node.priority, node.data);
    // }

    // println!("---------------------------");

    'outer: for i in 0..priority_queue.len() {
        // println!("priority: {}, data: {:?}",  &priority_queue.peek().unwrap().priority, (&priority_queue.peek().unwrap().data.0.to_string(), &priority_queue.peek().unwrap().data.1.to_string()) );

        let mut goto_next_prior_node = false;

        // if node contains stark,         
        if priority_queue.peek().unwrap().data.1 == "Stark" {

            // check
            // if valid_moves is empty so we found stark and its enemy, return answer
            if valid_moves.len() == 0 {
                return 
                (
                    priority_queue.peek().unwrap().data.0, 
                    enemies.get(&priority_queue.peek().unwrap().data.0.to_string()).unwrap().0, 
                    enemies.get(&priority_queue.peek().unwrap().data.0.to_string()).unwrap().1 
                );
            }
            

            // else valid_moves no empty, 
            else {
                //  for loop entire valid_moves
                for node in &valid_moves{
                    // Compare each node with peekNode, if has E# or A#
                    if node.data.0 == priority_queue.peek().unwrap().data.0 || node.data.1 == priority_queue.peek().unwrap().data.1 {
                        
                        //  dequeue    
                        priority_queue.dequeue();
                        
                        // set to true to loop the next node in the prior queue
                        goto_next_prior_node = true;

                        //  break next loop
                        break;
                    }
                }
                // if bool is true break 
                if goto_next_prior_node == true {
                    println!("SHOUDL RUN BREAK HERE");
                    continue 'outer;
                }

                //  else 
                //  found answer, return answer
                return 
                (
                    priority_queue.peek().unwrap().data.0, 
                    enemies.get(&priority_queue.peek().unwrap().data.0.to_string()).unwrap().0, 
                    enemies.get(&priority_queue.peek().unwrap().data.0.to_string()).unwrap().1 
                );
            } 

        // if node doesnt contain stark
        } else {
            // if valid_moves is empty
            if valid_moves.len() == 0 {
                // push
                valid_moves.push(priority_queue.dequeue().unwrap());
                // println!("valid_moves from if, prior: {}, data: {:?}", &valid_moves.peek().unwrap().priority, (&valid_moves.peek().unwrap().data.0.to_string(), &valid_moves.peek().unwrap().data.1.to_string()) );
                
            } 

            // elif not empty
            else {
                // for loop each node in valid_moves
                for node in &valid_moves {

                    // if not E# and not A# is in node, so its valid_move
                    if node.data.0 != priority_queue.peek().unwrap().data.0 && node.data.1 != priority_queue.peek().unwrap().data.1 {
                        // push
                        valid_moves.push(priority_queue.dequeue().unwrap());
                        // println!("valid_moves from else, prior: {}, data: {:?}", &valid_moves.peek().unwrap().priority, (&valid_moves.peek().unwrap().data.0.to_string(), &valid_moves.peek().unwrap().data.1.to_string()) );

                        // set to true to go to next node in the prio queue
                        goto_next_prior_node = true;
                        
                        // break
                        break;
                    }
                }
                
                if goto_next_prior_node == true {
                    continue 'outer;
                }

                println!("GOOD HERE");

                // otherwise this peekNode is not a valid move, just dequeue
                priority_queue.dequeue();

            } 
        }

    }

    // FOR TESTING
    // for node in &priority_queue {
    //     println!("priority: {}, data: {:?}", node.priority, node.data);
    // }
    // print!("");
    // print!("");
    // print!("");
    // print!("");
    return ("Thanos", 4, 1);


    

}


