use std::cmp::{min};
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use rand::prelude::SliceRandom;
use rand::thread_rng;
use crate::subgroups::SubgroupIterator;

// player order matters for determining initial pairing
// in principle it also matters in chess for white/black stuff, although that's not implemented here
pub enum MatchResult<'a, P: Player> {
    Player1Win { p1: &'a P, p2: &'a P },
    Player2Win { p1: &'a P, p2: &'a P },
    Draw { p1: &'a P, p2: &'a P },
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
    DuplicatePlayer { id: String },
    /// A player was added after the first round (which is not currently handled)
    UnexpectedPlayerAdded { id: String },
    /// The pairing algorithm can be configured to error on repeated pairings in the history
    RepeatedPairing { p1: String, p2: String },
    /// you cannot pair zero players
    NoPlayers,
    Other { err: String },
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
                .get_mut(p1)
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


/// i.e. 1v2, 3v4, etc
pub fn monrad_pairings_shuffle_between<'a, P: Player>(
    initial_seeding: &[&'a P],
    scores: &HashMap<&'a P, Score>,
    opponents: &HashMap<&'a P, HashSet<&'a P>>,
) -> Result<Vec<(&'a P, &'a P)>, PairingError> {

    let mut rng = thread_rng();
    let mut seeds = initial_seeding.iter().map(|p| *p).collect::<Vec<&'a P>>();
    seeds.shuffle(&mut rng);
    monrad_pairings(
        &seeds,
        scores,
        opponents
    )

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

mod subgroups {
    use std::cmp::min;
    use crate::{form_subgroups, Player};

    enum SubgroupIteratorState {
        Fresh,
    }

    pub(crate) struct SubgroupIterator<'a, P: Player> {
        s1: Vec<&'a P>,
        s2: Vec<&'a P>,
        limbo: Vec<&'a P>,
        state: SubgroupIteratorState,
        m1: usize,
        is_heterogeneous: bool,
    }


    impl<'a, P: Player> SubgroupIterator<'a, P> {
        /// (Self, max_pairs)
        pub(crate) fn new(mdps: Vec<&'a P>, resident_players: Vec<&'a P>) -> (Self, usize) {
            // B.1 parameters
            let m0 = mdps.len();
            // unclear if this is the correct calculation; see B.1.b
            let max_pairs = min((resident_players.len() + m0) / 2, resident_players.len());
            // unclear if this is the correct calculation; see B.1.c
            let m1 = min(mdps.len(), resident_players.len());
            let is_heterogeneous = m0 > 0;
            let n1 = if is_heterogeneous {
                // heterogeneous bracket
                m1
            } else {
                // homogenous bracket
                max_pairs
            };


            /*
        To make the pairing, each bracket[0] will be usually divided into two subgroups, called S1 and S2.

        S1 initially contains the highest N1 players (sorted according to A.2), where N1 is either
            M1 (in a heterogeneous bracket) or MaxPairs (otherwise).
        S2 initially contains all the remaining resident players.
        When M1 is less than M0, some MDPs are not included in S1. The excluded MDPs
            (in number of M0 - M1), who are neither in S1 nor in S2, are said to be in a Limbo.

        [0] A (pairing) bracket is a group of players to be paired. It is composed of players coming
            from one same scoregroup (called resident players) and of players who remained unpaired
            after the pairing of the previous bracket.

        implementation note: manually constructing S1 and S2 seems exhausting. basically
            S1 = mdps + resident_players[:n1-m0]
            S2 = resident_players[n1-m0:]

         */

            // n1 should maybe be calculated in the func
            let (s1, s2, limbo) = {
                form_subgroups(mdps, resident_players, n1)
            };

            (Self {
                s1,
                s2,
                limbo,
                state: SubgroupIteratorState::Fresh,
                m1,
                is_heterogeneous
            }, max_pairs)
        }

        pub(crate) fn get_s1(&self) -> &Vec<&P> {
            &self.s1
        }
        pub(crate) fn get_s2(&self) -> &Vec<&'a P> {
            &self.s2
        }
        pub(crate) fn get_limbo(&self) -> &Vec<&'a P> {
            &self.limbo
        }
        pub(crate) fn is_heterogeneous(&self) -> bool {
            self.is_heterogeneous
        }
    }
}



// this is so complicated
// https://handbook.fide.com/chapter/C0403

pub fn fide_dutch_pairings<'a, P: Player>(
    initial_seeding: &[&'a P],
    scores: &HashMap<&'a P, Score>,
    opponents: &HashMap<&'a P, HashSet<&'a P>>,
) -> Result<Vec<(&'a P, &'a P)>, PairingError> {
    let initial_seeds: HashMap<&P, usize> = initial_seeding
        .into_iter()
        .enumerate()
        .map(|(k, v)| (*v, k))
        .collect();

    let mut score_groups: HashMap<Score, Vec<&P>> = Default::default();
    let mut score_group_scores: BinaryHeap<Score> = Default::default();
    for (p, s) in scores.into_iter() {
        score_groups.entry(*s).or_insert(vec![]).push(*p);
        score_group_scores.push(*s);
    }

    if score_group_scores.is_empty() {
        return Err(PairingError::NoPlayers);
    }

    // Moved-Down Players (MDPs)
    let mut mdps: Vec<&P> = vec![];

    loop {
        let this_score = score_group_scores.pop().unwrap();
        // penultimate pairing bracket; it is the "last paired bracket" (see A.9)
        // (it's penultimate because there is sometimes a "collapsed last bracket" afterward)
        let is_ppb = score_group_scores.is_empty();
        let mut resident_players = score_groups.remove(&this_score).ok_or(
            PairingError::Other { err: format!("missing {} from scores", this_score)}
        )?;
        resident_players.sort_by_key(|p| initial_seeds.get(*p).unwrap());




        let m0 = mdps.len();
        let max_pairs = min((resident_players.len() + m0) / 2, resident_players.len());
        let m1 = min(mdps.len(), resident_players.len());
        let is_heterogeneous = m0 > 0;
        let n1 = if is_heterogeneous {
            // heterogeneous bracket
            m1
        } else {
            // homogenous bracket
            max_pairs
        };
        let more_references_to_mdps = mdps.iter().map(|p| *p).collect::<Vec<&P>>();


        // n1 should maybe be calculated in the func
        let (s1, s2, limbo) = {
            form_subgroups(more_references_to_mdps, resident_players, n1)
        };

        let mut best_score = QualityCriteria::min();
        let mut best_scoring_candidate = vec![];

        if is_heterogeneous {
            let mut mdp_pairing = vec![];
            for i in 0..s1.len() {
                mdp_pairing.push((s1[i], s2[i]));
            }
            let remainder: Vec<&P> = s2.iter().skip(s1.len()).map(|p| *p).collect();
            let n1r = remainder.len() / 2;
            let (s1r, s2r, limbor) = form_subgroups(
                vec![], remainder, n1r
            );
            let mut remainder_candidate = vec![];
            for i in 0..s1r.len() {
                remainder_candidate.push((s1r[i], s2r[i]));
            }
            let candidate = mdp_pairing.iter().chain(&remainder_candidate)
                .map(|(a, b)| (*a, *b))
                .collect::<Vec<(&P, &P)>>();
            let quality_criteria = EvaluationCriteria {
                max_pairs
            };
            let eval = evaluate_candidate::<P>(
                &candidate,
                &opponents,
                is_ppb,
                &quality_criteria
            );
            println!("eval: {:?}", eval);

            match eval {
                Evaluation::Evaluation(qc) => {
                    /*
                    B.8 Actions when no perfect candidate exists

                    Choose the best available candidate. In order to do so, consider that a
                    candidate is better than another if it better satisfies a quality criterion
                    (C5-C19) of higher priority; or, all quality criteria being equally satisfied,
                    it is generated earlier than the other one in the sequence of the candidates
                    (see B.6 or B.7).

                    this implies that we only need to track the best candidate so far. equally-good
                    later candidates are discarded.
                     */
                    if qc > best_score {
                        best_score = qc;
                        best_scoring_candidate = candidate;
                    }
                }
                Evaluation::AbsoluteFailure => {}
                Evaluation::Perfect => {
                    return Ok(candidate);
                }
            }
        } else {
            println!("s1: {:?}, s2: {:?}", s1, s2);
            let mut candidate = vec![];
            for i in 0..s1.len() {
                candidate.push((s1[i], s2[i]));
            }
            let quality_criteria = EvaluationCriteria {
                max_pairs
            };
            let eval = evaluate_candidate::<P>(
                &candidate,
                &opponents,
                is_ppb,
                &quality_criteria
            );
            println!("eval: {:?}", eval);

            match eval {
                Evaluation::Evaluation(qc) => {
                    /*
                    B.8 Actions when no perfect candidate exists

                    Choose the best available candidate. In order to do so, consider that a
                    candidate is better than another if it better satisfies a quality criterion
                    (C5-C19) of higher priority; or, all quality criteria being equally satisfied,
                    it is generated earlier than the other one in the sequence of the candidates
                    (see B.6 or B.7).

                    this implies that we only need to track the best candidate so far. equally-good
                    later candidates are discarded.
                     */
                    if qc > best_score {
                        best_score = qc;
                        best_scoring_candidate = candidate;
                    }
                }
                Evaluation::AbsoluteFailure => {}
                Evaluation::Perfect => {
                    return Ok(candidate);
                }
            }
        }



    }

    let mut pairings: Vec<(&P, &P)> = Vec::with_capacity(10);

    Ok(pairings)
}

fn form_subgroups<'a, P: Player>(
    mdps: Vec<&'a P>,
    resident_players: Vec<&'a P>,
    n1: usize,
) -> (Vec<&'a P>, Vec<&'a P>, Vec<&'a P>) {
    let mut s1 = vec![];
    let mut s2 = vec![];
    let mut limbo = vec![];

    let mut s1_count = n1;
    for i in 0..s1_count {
        let p = match mdps.get(i) {
            Some(p) => p,
            None => {
                break;
            }
        };
        s1.push(*p);
        s1_count -= 1;
    }

    if s1.len() < mdps.len() {
        for i in s1.len()..mdps.len() {
            limbo.push(*&mdps[i]);
        }
    }

    for i in 0..s1_count {
        // i'm pretty sure this can't fail
        let p = &resident_players[i];
        s1.push(*p);
    }

    for i in s1_count..resident_players.len() {
        s2.push(*&resident_players[i]);
    }

    (s1, s2, limbo)
}


/// the stuff in C0403 from C.5 to C.19
#[derive(PartialEq, PartialOrd, Debug)]
struct QualityCriteria {
    num_pairs: usize,
}

impl QualityCriteria {
    fn min() -> Self {
        Self {
            num_pairs: 0
        }
    }
}


#[derive(Debug)]
enum Evaluation {
    Evaluation(QualityCriteria),
    Perfect,
    AbsoluteFailure,
}

struct EvaluationCriteria {
    max_pairs: usize,
}

/// see fide rules C.04.03.C (pairing criteria)
fn evaluate_candidate<P: Player>(
    candidate_pairings: &[(&P, &P)],
    opponents: &HashMap<&P, HashSet<&P>>,
    is_ppb: bool,
    crit: &EvaluationCriteria,
) -> Evaluation {
    // absolute criteria first:
    // C.1. no repeat matchups
    for (p1, p2) in candidate_pairings {
        if let Some(opps) = opponents.get(*p1) {
            if opps.contains(*p2) {
                return Evaluation::AbsoluteFailure;
            }
        }
    }
    // C.2. no repeat bye
    // handled (i hope!) via having the "bye" just be a member of the player list

    // C.3. chess-specific constraints about colour requirements that I am opting to ignore

    // C.4. says
    //    if the current bracket is the PPB (see A.9): choose the set of downfloaters in order to
    //    complete the round-pairing.
    // I do not know what that means. this is supposed to be an evaluation step, not a "choose"
    // step. I am going to ignore this because it doesn't make sense.


    // C.5. maximize the number of pairs (equivalent to: minimize the number of downfloaters).
    // The document indicates that this is something that can be "fulfilled" rather than a scalar,
    // which I suppose means something like "num_pairs = floor(bracket_size/2)"?
    let n_pairs = candidate_pairings.len();
    if n_pairs == crit.max_pairs {
        Evaluation::Perfect
    } else {
        Evaluation::Evaluation(
            QualityCriteria {
                num_pairs: n_pairs
            }
        )
    }
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
    use crate::{form_subgroups, monrad_pairings, swiss_pairings, MatchResult, Player, TourneyConfig, fide_dutch_pairings};

    impl Player for i32 {
        fn id(&self) -> String {
            self.to_string()
        }
    }

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

    #[test]
    fn test_form_pairings() {
        let mdps = vec![];
        let residents = vec![&1, &2, &3, &4, &5, &6];
        let (s1, s2, limbo) = form_subgroups(mdps, residents, 3);
        assert_eq!(vec![&1, &2, &3], s1);
        assert_eq!(vec![&4, &5, &6], s2);
        assert!(limbo.is_empty());
    }

    #[test]
    fn test_form_pairings_leftover() {
        let mdps = vec![];
        let residents = vec![&1, &2, &3, &4, &5, &6, &7];
        let (s1, s2, limbo) = form_subgroups(mdps, residents, 3);
        assert_eq!(vec![&1, &2, &3], s1);
        assert_eq!(vec![&4, &5, &6, &7], s2);
        assert!(limbo.is_empty());
    }

    #[test]
    fn test_form_pairings_mdps() {
        let mdps = vec![&1, &2];
        let residents = vec![&3, &4, &5, &6, &7];
        let (s1, s2, limbo) = form_subgroups(mdps, residents, 3);
        assert_eq!(vec![&1, &2, &3], s1);
        assert_eq!(vec![&4, &5, &6, &7], s2);
        assert!(limbo.is_empty());
    }

    #[test]
    fn test_form_pairings_limbo() {
        let mdps = vec![&1, &2, &3, &4];
        let residents = vec![&5, &6, &7];
        let (s1, s2, limbo) = form_subgroups(mdps, residents, 3);
        assert_eq!(vec![&1, &2, &3], s1);
        assert_eq!(vec![&5, &6, &7], s2);
        assert_eq!(vec![&4], limbo);
    }

    #[test]
    fn test_fide_pairings_simple() {
        let players = vec![&1, &2, &3, &4];
        let scores = HashMap::from([(&1, 0), (&2, 0), (&3, 0), (&4, 0)]);
        let opps = HashMap::from([
            (&1, HashSet::new()),
            (&2, HashSet::new()),
            (&3, HashSet::new()),
            (&4, HashSet::new())
        ]);
        let pairs = fide_dutch_pairings(&players, &scores, &opps);
        assert_eq!(Ok(vec![(&1, &3), (&2, &4)]), pairs);
    }
}
