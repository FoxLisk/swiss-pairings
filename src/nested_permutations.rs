use itertools::{Chunk, Permutations};
use permutator::{HeapPermutationRefIter, IteratorReset};

pub(crate) struct ChunkPermutator<'a, T> {
    chunks: Vec<* mut [T]>,
    permutators: Vec<HeapPermutationRefIter<'a, T>>,
    states: Vec<bool>
}


impl<'a, T> ChunkPermutator<'a, T> {
    pub(crate) fn new(chunks: Vec<*mut [T]>) -> Self {
        let mut permutators = vec![];
        let mut chunks_ = vec![];
        for chunk in chunks {
            let thing = unsafe {
                HeapPermutationRefIter::new(chunk as *mut[T])
            };

            chunks_.push(chunk);
            permutators.push(thing);
        }
        let l = chunks_.len();
        Self {
            chunks: chunks_, permutators,
            states: vec![false; l],
        }
    }

    /// returns false to signal all permutations have been produced
    /// this will immediately permute your chunks when you call it so if you want the first
    /// permutation, you should look at it before calling this
    pub(crate) fn permute(&mut self) -> bool {
        let mut i = self.permutators.len() - 1;
        /*
         we're sort of counting up booleanly through the lists. say we have three lists.
         we start with 3 lists, none of which have been permutated, which we store in self.states
         as [0, 0, 0]. we also indicate that we are currently looking at the last one; that is,
         self.current_target = 0.
         so what we do is we first go through all the permutations of the current target. then when
         it's exhausted, we permute the next-up list _once_ and go back to permuting all of the
         last one.

         i think the algorithm is, given lists 1..N:

         1. set i = N
         2. permute list i
            2.a. if that permutation fails, reset list i and set i = N - 1
            2.b. otherwise just return success for now
         3. if i is out of bounds, return failure to indicate no more permutations
         */
        loop {
            match self.permutators[i].next() {
                Some(_) => {
                    return true;
                }
                None => {
                    if i == 0 {
                        return false;
                    }
                    self.permutators[i].reset();
                    i -= 1;
                }
            }
        }
    }
}

mod tests {
    use crate::nested_permutations::ChunkPermutator;

    #[test]
    fn test_it_works() {
        let mut v  = vec![1, 2, 3, 4, 5];
        let mut p = unsafe {
            ChunkPermutator::new(vec![&mut v[..2] as *mut [i32], &mut v[2..5] as *mut [i32]])

        };
        let mut all_permutations = vec![];
        all_permutations.push(v.clone());

        while p.permute() {
            println!("{:?}", v);
            all_permutations.push(v.clone());
        }
        all_permutations.sort();
        assert_eq!(
            vec![
                vec![1, 2, 3, 4, 5],
                vec![1, 2, 3, 5, 4],
                vec![1, 2, 4, 3, 5],
                vec![1, 2, 4, 5, 3],
                vec![1, 2, 5, 3, 4],
                vec![1, 2, 5, 4, 3],
                vec![2, 1, 3, 4, 5],
                vec![2, 1, 3, 5, 4],
                vec![2, 1, 4, 3, 5],
                vec![2, 1, 4, 5, 3],
                vec![2, 1, 5, 3, 4],
                vec![2, 1, 5, 4, 3],
            ],
            all_permutations
            );
    }


    #[test]
    fn test_some_binding_stuff() {
        let mut v  = vec![1, 2, 3, 4, 5];
        let mut p = unsafe {
            let mut chunks = vec![];
            for (s, e) in vec![(0, 2), (2, 5)] {
                chunks.push(&mut v[s..e] as *mut [i32]);
            }
            ChunkPermutator::new(chunks)

        };
        let mut all_permutations = vec![];
        all_permutations.push(v.clone());

        while p.permute() {
            println!("{:?}", v);
            all_permutations.push(v.clone());
        }
        all_permutations.sort();
        assert_eq!(
            vec![
                vec![1, 2, 3, 4, 5],
                vec![1, 2, 3, 5, 4],
                vec![1, 2, 4, 3, 5],
                vec![1, 2, 4, 5, 3],
                vec![1, 2, 5, 3, 4],
                vec![1, 2, 5, 4, 3],
                vec![2, 1, 3, 4, 5],
                vec![2, 1, 3, 5, 4],
                vec![2, 1, 4, 3, 5],
                vec![2, 1, 4, 5, 3],
                vec![2, 1, 5, 3, 4],
                vec![2, 1, 5, 4, 3],
            ],
            all_permutations
        );
    }
}