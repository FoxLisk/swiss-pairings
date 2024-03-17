extern crate itertools;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

use itertools::Itertools;
use permutation_utils::pair_partitions;

use rand::seq::SliceRandom;
use rand::thread_rng;

mod permutation_utils;

// player order matters for determining initial pairing
// in principle it also matters in chess for white/black stuff, although that's not implemented here
// probably this should not have the Player trait bound, for good citizenship reasons, i believe
// it is generally considered better to add trait bounds on functions instead of structs
#[derive(Debug)]
pub enum MatchResult<'a, P: Player> {
    Player1Win { p1: &'a P, p2: &'a P },
    Player2Win { p1: &'a P, p2: &'a P },
    Draw { p1: &'a P, p2: &'a P },
}

pub fn format_round<P: Player>(round: &Round<P>) -> String {
    if round.len() == 0 {
        return "".to_string();
    }

    let mut mus = Vec::with_capacity(round.len());
    for mr in round {
        let formatted = match mr {
            MatchResult::Player1Win { p1, p2 } => {
                format!("{} > {}", p1.id(), p2.id())
            }
            MatchResult::Player2Win { p1, p2 } => {
                format!("{} < {}", p1.id(), p2.id())
            }
            MatchResult::Draw { p1, p2 } => {
                format!("{} = {}", p1.id(), p2.id())
            }
        };
        mus.push(formatted);
    }
    mus.join("\n")
}

pub fn format_bracket<P: Player>(rounds: &Vec<Round<P>>) -> String {
    let mut rounds_out = vec![];
    let mut rn = 1;
    for round in rounds {
        rounds_out.push(format!("Round {}:", rn));
        rounds_out.push("".to_string());
        rn += 1;
        rounds_out.push(format_round(round));
        rounds_out.push("".to_string());
    }
    rounds_out.join("\n")
}

pub trait Player: Eq + Hash + Debug {
    /// must return an ID that is unique within the player set
    fn id(&self) -> String;
}

impl<T: Eq + Hash + Debug + ToString> Player for T {
    fn id(&self) -> String {
        self.to_string()
    }
}

pub type Round<'a, P> = Vec<MatchResult<'a, P>>;

#[derive(Debug, PartialEq)]
pub enum PairingError {
    /// No first round given (you do the first round yourself with whatever seeding you want)
    MissingInitialRound,
    /// A player was involved in multiple matches in the same round
    DuplicatePlayer {
        id: String,
    },
    /// A player was added after the first round (which is not currently handled)
    UnexpectedPlayerAdded {
        id: String,
    },
    /// The pairing algorithm can be configured to error on repeated pairings in the history
    RepeatedPairing {
        p1: String,
        p2: String,
    },
    /// you cannot pair zero players
    NoPlayers,
    Other {
        err: String,
    },
}

impl Display for PairingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PairingError::MissingInitialRound => {
                write!(f, "Missing initial round")
            }
            PairingError::DuplicatePlayer { id } => {
                write!(f, "Duplicate player with id {}", id)
            }
            PairingError::UnexpectedPlayerAdded { id } => {
                write!(f, "Player unexpectedly added after first round: {}", id)
            }
            PairingError::RepeatedPairing { p1, p2 } => {
                write!(f, "Illegal repeated pairing found: {} vs {}", p1, p2)
            }
            PairingError::NoPlayers => {
                write!(f, "No players given?")
            }
            PairingError::Other { err } => {
                write!(f, "Other error: {}", err)
            }
        }
    }
}

impl Error for PairingError {}

/// Scores are integers for ease of implementation. if you want 1/0/0.5, use 2/0/1 and divide
pub type Score = i32;

pub struct TourneyConfig {
    pub points_per_win: Score,
    pub points_per_loss: Score,
    pub points_per_draw: Score,
    pub error_on_repeated_opponent: bool,
}

fn scores<'a, P: Player>(
    mr: &MatchResult<'a, P>,
    config: &TourneyConfig,
) -> ((&'a P, Score), (&'a P, Score)) {
    match mr {
        MatchResult::Player1Win { p1, p2 } => {
            ((p1, config.points_per_win), (p2, config.points_per_loss))
        }
        MatchResult::Player2Win { p1, p2 } => {
            ((p1, config.points_per_loss), (p2, config.points_per_win))
        }
        MatchResult::Draw { p1, p2 } => {
            ((p1, config.points_per_draw), (p2, config.points_per_draw))
        }
    }
}

/// mind your own first round pairings
pub fn swiss_pairings<'a, P: Player, F>(
    rounds: &[Round<'a, P>],
    config: &TourneyConfig,
    pairings_result: F,
) -> Result<(Vec<(&'a P, &'a P)>, Vec<(&'a P, Score)>), PairingError>
where
    F: FnOnce(
        &[&'a P],
        &HashMap<&'a P, Score>,
        &HashMap<&'a P, HashSet<&'a P>>,
    ) -> Result<Vec<(&'a P, &'a P)>, PairingError>,
{
    let nrounds = rounds.len();
    if nrounds == 0 {
        return Err(PairingError::MissingInitialRound);
    }
    let nplayers = rounds[0].len() * 2;

    let mut player_scores: HashMap<&P, Score> = HashMap::with_capacity(nplayers);
    // i feel like there's a way to create a big matrix of players and use that to test
    // has-played-ness (like a 2x2 where M[i, j] is set if p_i has played p_j)
    // but this is ez
    let mut opponents: HashMap<&P, HashSet<&P>> = HashMap::with_capacity(nplayers);
    let mut initial_seeding: Vec<&P> = Vec::with_capacity(nplayers);

    for res in &rounds[0] {
        let ((p1, points1), (p2, points2)) = scores(res, config);
        if let Some(_) = player_scores.insert(p1, points1) {
            return Err(PairingError::DuplicatePlayer { id: p1.id() });
        }
        if let Some(_) = player_scores.insert(p2, points2) {
            return Err(PairingError::DuplicatePlayer { id: p2.id() });
        }

        let mut p1_opps = HashSet::with_capacity(nrounds);
        p1_opps.insert(p2);
        opponents.insert(p1, p1_opps);
        let mut p2_opps = HashSet::with_capacity(nrounds);
        p2_opps.insert(p1);
        opponents.insert(p2, p2_opps);

        initial_seeding.push(p1);
        initial_seeding.push(p2);
    }

    for round in rounds.iter().skip(1) {
        for res in round {
            let ((p1, points1), (p2, points2)) = scores(res, config);
            let s1 = player_scores
                .get_mut(p1)
                .ok_or(PairingError::UnexpectedPlayerAdded { id: p1.id() })?;
            *s1 += points1;
            let s2 = player_scores
                .get_mut(p2)
                .ok_or(PairingError::UnexpectedPlayerAdded { id: p2.id() })?;
            *s2 += points2;
            let p1_opps = opponents
                .get_mut(p1)
                .ok_or(PairingError::UnexpectedPlayerAdded { id: p1.id() })?;
            if !p1_opps.insert(p2) && config.error_on_repeated_opponent {
                return Err(PairingError::RepeatedPairing {
                    p1: p1.id(),
                    p2: p2.id(),
                });
            }
            let p2_opps = opponents
                .get_mut(p2)
                .ok_or(PairingError::UnexpectedPlayerAdded { id: p1.id() })?;

            if !p2_opps.insert(p1) && config.error_on_repeated_opponent {
                return Err(PairingError::RepeatedPairing {
                    p1: p1.id(),
                    p2: p2.id(),
                });
            }
        }
    }

    let pairings = pairings_result(&initial_seeding, &player_scores, &opponents)?;

    let initial_seeds: HashMap<&P, usize> = initial_seeding
        .into_iter()
        .enumerate()
        .map(|(k, v)| (v, k))
        .collect();
    let mut standings = player_scores.into_iter().collect::<Vec<(&P, Score)>>();

    standings.sort_by_key(|(p, s)| (-s, initial_seeds.get(p).unwrap_or(&999)));

    Ok((pairings, standings))
}

/// the idea on this one is to go through the score groups, and in each score group, try
/// permutations until you find one that results in a valid pairing
pub fn random_by_scoregroup<'a, P: Player>(
    initial_seeding: &[&'a P],
    scores: &HashMap<&'a P, Score>,
    opponents: &HashMap<&'a P, HashSet<&'a P>>,
) -> Result<Vec<(&'a P, &'a P)>, PairingError> {
    if scores.is_empty() {
        return Err(PairingError::MissingInitialRound);
    }
    let mut score_groups_builder = HashMap::with_capacity(initial_seeding.len());
    for (p, s) in scores.iter() {
        score_groups_builder.entry(*s).or_insert(vec![]).push(*p);
    }
    // [(score, [players_with_that_score])]
    let mut score_groups = score_groups_builder.into_iter().collect::<Vec<_>>();
    score_groups.sort_unstable_by(|a, b| a.0.cmp(&b.0));

    // println!("Calling rec_pair");
    match rec_pair(
        score_groups.into_iter().map(|(_, p)| p).collect::<_>(),
        opponents,
    ) {
        Some(p) => Ok(p),
        None => Err(PairingError::Other {
            err: "Unable to find a valid pairing".to_string(),
        }),
    }
}

/// pairs all players using a recursive implementation of the following algorithm:
///
/// 1. take the highest score group left
/// 2. if it's got an even number of players, try to pair it
///   2a. if pairing succeeds, try to recursively pair remaining players
///   2b. if this also succeeds, return this result
/// 3. if it's an odd number of players or 2a. fails:
///   3a. for each player in the *next* score group
fn rec_pair<'a, P: Player>(
    mut score_groups: Vec<Vec<&'a P>>,
    opponents: &HashMap<&'a P, HashSet<&'a P>>,
) -> Option<Vec<(&'a P, &'a P)>> {
    let our_score_group = match score_groups.pop() {
        Some(p) => p,
        None => {
            return Some(vec![]);
        }
    };

    if our_score_group.is_empty() {
        // sometimes we get called with an empty score group because everyone from it has been
        // "borrowed" to make the next-highest score group pair successfully
        return rec_pair(score_groups, opponents);
    }

    // TODO benchmark this and see if doing this faster is relevant
    let is_valid_pairing = |p: &Vec<(&P, &P)>| {
        for (p1, p2) in p {
            if opponents.get(p1).map(|os| os.contains(p2)).unwrap_or(false) {
                return false;
            }
        }
        true
    };

    // println!(
    //     "rec_pair: trying to pair score group {}\n    remaining score groups: {}",
    //     our_score_group.iter().map(|p| p.id()).join(" "),
    //     score_groups
    //         .iter()
    //         .map(|s| s.iter().map(|p| p.id()).join(" "))
    //         .join("; ")
    // );

    if our_score_group.len() % 2 == 0 {
        let mut pairings = pair_partitions(our_score_group.copy_references()).ok()?;
        let mut rng = thread_rng();
        pairings.shuffle(&mut rng);
        for pairing in pairings {
            let blargh = score_groups
                .iter()
                .map(|v| v.copy_references())
                .collect::<_>();
            if is_valid_pairing(&pairing) {
                if let Some(mut rest_paired) = rec_pair(blargh, opponents) {
                    rest_paired.extend(pairing);
                    return Some(rest_paired);
                }
            }
        }
    }
    let mut next_group = score_groups.pop()?;
    let mut rng = thread_rng();
    next_group.shuffle(&mut rng);
    let mut i = 0;
    while i < next_group.len() {
        let mut our_group_copy = our_score_group.copy_references();
        let mut next_group_copy = next_group.copy_references();
        let mut score_groups_copy = score_groups.iter().cloned().collect::<Vec<_>>();
        let trial_person = next_group_copy.swap_remove(i);
        our_group_copy.push(trial_person);
        score_groups_copy.push(next_group_copy);
        score_groups_copy.push(our_group_copy);
        if let Some(pairings) = rec_pair(score_groups_copy, opponents) {
            return Some(pairings);
        }
        i += 1;
    }

    None
}

trait CopyReferences {
    fn copy_references(&self) -> Self;
}

impl<T> CopyReferences for Vec<&T> {
    fn copy_references(&self) -> Self {
        self.iter().map(|e| *e).collect()
    }
}

/// i.e. 1v2, 3v4, etc
pub fn monrad_pairings<'a, P: Player>(
    initial_seeding: &[&'a P],
    scores: &HashMap<&'a P, Score>,
    opponents: &HashMap<&'a P, HashSet<&'a P>>,
) -> Result<Vec<(&'a P, &'a P)>, PairingError> {
    let initial_seeds: HashMap<&P, usize> = initial_seeding
        .into_iter()
        .enumerate()
        .map(|(k, v)| (*v, k))
        .collect();
    let mut standings = scores
        .into_iter()
        .map(|(k, v)| (*k, *v))
        .collect::<Vec<(&P, Score)>>();

    standings.sort_by_key(|(p, s)| (-s, initial_seeds.get(p).unwrap_or(&999)));

    let mut pairings: Vec<(&P, &P)> = Vec::with_capacity(standings.len());
    let mut unpaired = standings.clone();
    // top players at the end so we can pop()
    unpaired.reverse();

    while unpaired.len() > 1 {
        let p1 = unpaired.pop().unwrap().0;
        let opps = opponents.get(p1).unwrap();
        for i in (0..unpaired.len()).rev() {
            if !opps.contains(unpaired[i].0) {
                let p2 = unpaired.remove(i).0;
                pairings.push((p1, p2));
                break;
            }
        }
    }
    Ok(pairings)
}

#[derive(PartialEq, Eq, Hash, Debug)]
struct MyPlayer {
    id: String,
}

impl Player for MyPlayer {
    fn id(&self) -> String {
        self.id.clone()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use crate::{monrad_pairings, swiss_pairings, MatchResult, Player, TourneyConfig};

    #[derive(PartialEq, Eq, Hash, Debug)]
    struct TestPlayer {
        id: String,
    }

    impl Player for TestPlayer {
        fn id(&self) -> String {
            self.id.clone()
        }
    }

    #[test]
    fn test_r1_standings() {
        let mut players = vec![];
        for i in 0..8 {
            players.push(TestPlayer {
                id: format!("p{}", i),
            });
        }

        let r1 = MatchResult::Player1Win {
            p1: &players[0],
            p2: &players[1],
        };

        let r2 = MatchResult::Player1Win {
            p1: &players[2],
            p2: &players[3],
        };

        let r3 = MatchResult::Draw {
            p1: &players[4],
            p2: &players[5],
        };
        let r4 = MatchResult::Player2Win {
            p1: &players[6],
            p2: &players[7],
        };

        let config = TourneyConfig {
            points_per_win: 2,
            points_per_loss: 0,
            points_per_draw: 1,
            error_on_repeated_opponent: true,
        };
        let standings =
            swiss_pairings(&vec![vec![r1, r2, r3, r4]], &config, monrad_pairings).unwrap();
        assert_eq![
            vec![
                (&players[0], 2),
                (&players[2], 2),
                (&players[7], 2),
                (&players[4], 1),
                (&players[5], 1),
                (&players[1], 0),
                (&players[3], 0),
                (&players[6], 0)
            ],
            standings.1
        ];
        assert_eq![
            vec![
                (&players[0], &players[2]),
                (&players[7], &players[4]),
                (&players[5], &players[1]),
                (&players[3], &players[6]),
            ],
            standings.0,
        ];
    }

    #[test]
    fn test_r2_standings() {
        let mut players = vec![];
        for i in 0..8 {
            players.push(TestPlayer {
                id: format!("p{}", i),
            });
        }

        let r1 = vec![
            MatchResult::Player1Win {
                p1: &players[0],
                p2: &players[1],
            },
            MatchResult::Player1Win {
                p1: &players[2],
                p2: &players[3],
            },
            MatchResult::Draw {
                p1: &players[4],
                p2: &players[5],
            },
            MatchResult::Player2Win {
                p1: &players[6],
                p2: &players[7],
            },
        ];

        let r2 = vec![
            MatchResult::Player1Win {
                p1: &players[0],
                p2: &players[2],
            },
            MatchResult::Player2Win {
                p1: &players[7],
                p2: &players[4],
            },
            MatchResult::Player1Win {
                p1: &players[5],
                p2: &players[1],
            },
            MatchResult::Player2Win {
                p1: &players[3],
                p2: &players[6],
            },
        ];

        let config = TourneyConfig {
            points_per_win: 2,
            points_per_loss: 0,
            points_per_draw: 1,
            error_on_repeated_opponent: true,
        };
        let (pairings, standings) =
            swiss_pairings(&vec![r1, r2], &config, monrad_pairings).unwrap();
        assert_eq![
            vec![
                (&players[0], 4),
                (&players[4], 3),
                (&players[5], 3),
                (&players[2], 2),
                (&players[6], 2),
                (&players[7], 2),
                (&players[1], 0),
                (&players[3], 0),
            ],
            standings
        ];

        assert_eq![
            vec![
                (&players[0], &players[4]),
                (&players[5], &players[2]),
                // 6 vs 7 already happened, so 6 gets paired with 1, and 7 with 3
                (&players[6], &players[1]),
                (&players[7], &players[3]),
            ],
            pairings
        ];
    }
}
