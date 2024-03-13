use std::collections::VecDeque;

/// given a list of elements, return a list of all partitions of that list into sets of 2
/// (specifically sets; return types are tuples but the algorithm treats order as irrelevant)
///
/// i.e. pair_partitions([1, 2, 3, 4]) => [
///     [(1, 2), (3, 4)],
///     [(1, 3), (2, 4)],
///     [(1, 4), (2, 3)],
/// ]
pub(crate) fn pair_partitions<'a, T>(
    things: Vec<&'a T>,
) -> Result<Vec<Vec<(&'a T, &'a T)>>, String> {
    if things.is_empty() {
        return Ok(vec![]);
    }
    if things.len() % 2 != 0 {
        return Err("Input vector must be of even length".to_string());
    }
    let collection = things.into_iter().collect::<VecDeque<_>>();

    let partitions = actually_do_the_thing(collection);
    let blargh = partitions
        .into_iter()
        .map(|p| p.into_iter().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    Ok(blargh)
}

fn actually_do_the_thing<'a, T>(mut things: VecDeque<&'a T>) -> VecDeque<VecDeque<(&'a T, &'a T)>> {
    if things.is_empty() {
        let mut out = VecDeque::with_capacity(1);
        out.push_back(VecDeque::new());
        return out;
    }
    let mut out = VecDeque::with_capacity(bangbang(things.len() - 1));

    let my_thing = things.pop_front().unwrap();
    let mut i = 0;
    while i < things.len() {
        let other_thing = things.pop_front().unwrap();
        let this_pair = (my_thing, other_thing);

        let things_copy = things.iter().map(|r| *r).collect::<VecDeque<_>>();
        let partitions_of_rest = actually_do_the_thing(things_copy);

        for mut partition in partitions_of_rest {
            partition.push_front(this_pair);
            out.push_back(partition);
        }

        things.push_back(other_thing);
        i += 1;
    }

    out
}

fn bangbang(mut n: usize) -> usize {
    let mut out = 1;
    while n > 1 {
        out *= n;
        n -= 2;
    }
    out
}

#[cfg(test)]
mod tests {
    use crate::permutation_utils::{bangbang, pair_partitions};

    type Thing = i32;

    #[test]
    fn test_double_factorial() {
        assert_eq!(1, bangbang(1));
        assert_eq!(2, bangbang(2));
        assert_eq!(3, bangbang(3));
        assert_eq!(8, bangbang(4));
        assert_eq!(15, bangbang(5));
        assert_eq!(48, bangbang(6));
        assert_eq!(105, bangbang(7));
    }

    #[test]
    fn test_pairs_empty() {
        assert_eq!(
            Ok(Vec::<Vec<(&Thing, &Thing)>>::new()),
            pair_partitions::<Thing>(vec![])
        );
    }

    #[test]
    fn test_pairs_one_element() {
        assert!(pair_partitions::<Thing>(vec![&1]).is_err());
    }

    #[test]
    fn test_pairs_two_elements() {
        assert_eq!(Ok(vec![vec![(&1, &2)]]), pair_partitions(vec![&1, &2]))
    }

    #[test]
    fn test_pairs_four_elements() {
        assert_eq!(
            Ok(vec![
                vec![(&1, &2), (&3, &4)],
                vec![(&1, &3), (&4, &2)],
                vec![(&1, &4), (&2, &3)],
            ]),
            pair_partitions(vec![&1, &2, &3, &4])
        );
    }

}
