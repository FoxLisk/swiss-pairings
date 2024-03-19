use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::time::{Duration, Instant};

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

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum PairingError {
    #[error(
        "No first round given (you do the first round yourself with whatever seeding you want)"
    )]
    MissingInitialRound,
    #[error("A player was involved in multiple matches in the same round: {id}")]
    DuplicatePlayer { id: String },
    #[error("A player ({id}) was added after the first round (which is not currently handled)")]
    UnexpectedPlayerAdded { id: String },
    #[error("Repeated pairing: {p1} vs {p2}")]
    RepeatedPairing { p1: String, p2: String },
    #[error("You cannot pair zero players")]
    NoPlayers,
    #[error("No valid pairing found")]
    NoValidPairing,
    #[error("Timed out generating pairings")]
    Timeout,
    #[error("Other error: {0}")]
    Other(String),
}

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

struct TimedOutChecker {
    no_timeout: bool,
    start: Instant,
    duration: Duration,
}

impl TimedOutChecker {
    fn new(dur: Option<Duration>) -> Self {
        match dur {
            Some(d) => Self {
                no_timeout: false,
                start: Instant::now(),
                duration: d,
            },
            None => Self {
                no_timeout: true,
                start: Instant::now(),
                duration: Duration::from_millis(1),
            },
        }
    }

    fn timed_out(&self) -> bool {
        if self.no_timeout {
            false
        } else {
            let passed = Instant::now().duration_since(self.start);
            if passed > self.duration {
                true
            } else {
                // println!(
                //     "Only {passed:?} has passed, which is less than {:?}",
                //     self.duration
                // );
                false
            }
        }
    }
}

/// mind your own first round pairings
pub fn swiss_pairings<'a, P: Player>(
    rounds: &[Round<'a, P>],
    config: &TourneyConfig,
    timeout: Option<Duration>,
) -> Result<(Vec<(&'a P, &'a P)>, Vec<(&'a P, Score)>), PairingError> {
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

    let timer = TimedOutChecker::new(timeout);

    let pairings = random_by_scoregroup(&initial_seeding, &player_scores, &opponents, &timer)?;

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
fn random_by_scoregroup<'a, P: Player>(
    initial_seeding: &[&'a P],
    scores: &HashMap<&'a P, Score>,
    opponents: &HashMap<&'a P, HashSet<&'a P>>,
    timer: &TimedOutChecker,
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

    rec_pair(
        score_groups.into_iter().map(|(_, p)| p).collect::<_>(),
        opponents,
        timer,
    )
}

/// pairs all players using a recursive implementation of the following algorithm:
///
/// 1. take the highest score group left
/// 2. if it's got an even number of players, try to pair it
///   2a. if pairing succeeds, try to recursively pair remaining players
///   2b. if this also succeeds, return this result
/// 3. if it's an odd number of players or 2a. fails:
///   3a. for each player in the *next* score group:
///     3ai. try adding them to this score group and recursing
fn rec_pair<'a, P: Player>(
    mut score_groups: Vec<Vec<&'a P>>,
    opponents: &HashMap<&'a P, HashSet<&'a P>>,
    time_checker: &TimedOutChecker,
) -> Result<Vec<(&'a P, &'a P)>, PairingError> {
    if time_checker.timed_out() {
        return Err(PairingError::Timeout);
    }
    let our_score_group = match score_groups.pop() {
        Some(p) => p,
        None => {
            return Ok(vec![]);
        }
    };

    if our_score_group.is_empty() {
        // sometimes we get called with an empty score group because everyone from it has been
        // "borrowed" to make the next-highest score group pair successfully
        return rec_pair(score_groups, opponents, time_checker);
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
        let mut pairings = pair_partitions(our_score_group.copy_references())
            .map_err(|e| PairingError::Other(e))?;
        let mut rng = thread_rng();
        pairings.shuffle(&mut rng);
        for pairing in pairings {
            if is_valid_pairing(&pairing) {
                let score_groups_copied = score_groups
                    .iter()
                    .map(|v| v.copy_references())
                    .collect::<_>();
                match rec_pair(score_groups_copied, opponents, time_checker) {
                    Ok(mut rest_paired) => {
                        rest_paired.extend(pairing);
                        return Ok(rest_paired);
                    }
                    Err(PairingError::Timeout) => {
                        return Err(PairingError::Timeout);
                    }
                    Err(_) => {}
                }
            }
        }
    }
    let mut next_group = score_groups.pop().ok_or(PairingError::NoValidPairing)?;
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
        match rec_pair(score_groups_copy, opponents, time_checker) {
            Ok(pairings) => {
                return Ok(pairings);
            }
            Err(PairingError::Timeout) => {
                return Err(PairingError::Timeout);
            }
            Err(_) => {}
        }
        i += 1;
    }

    Err(PairingError::NoValidPairing)
}

trait CopyReferences {
    fn copy_references(&self) -> Self;
}

impl<T> CopyReferences for Vec<&T> {
    fn copy_references(&self) -> Self {
        self.iter().map(|e| *e).collect()
    }
}
