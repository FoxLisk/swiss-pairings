use std::collections::{HashMap, HashSet};
use std::hash::Hash;

// player order matters for some stuff
pub enum MatchResult<'a, P: Player> {
    Player1Win { p1: &'a P, p2: &'a P },
    Player2Win { p1: &'a P, p2: &'a P },
    Draw { p1: &'a P, p2: &'a P },
}

pub trait Player: Eq + Hash {
    fn bye() -> Self;
    /// must return an ID that is unique within the player set
    fn id(&self) -> String;
}

type Round<'a, P> = Vec<MatchResult<'a, P>>;

#[derive(Debug)]
pub enum PairingError {
    /// No first round given (you do the first round yourself with whatever seeding you want)
    MissingInitialRound,
    /// A player was involved in multiple matches in the same round
    DuplicatePlayer { id: String },
    /// A player was added after the first round (which is not currently handled)
    UnexpectedPlayerAdded { id: String },
    /// The pairing algorithm can be configured to error on repeated pairings in the history
    RepeatedPairing { p1: String, p2: String },
}

/// Scores are integers for ease of implementation. if you want 1/0/0.5, use 2/0/1 and divide
type Score = i32;

pub enum PairingsRule {
    MonradSystem,
}

pub struct TourneyConfig {
    points_per_win: Score,
    points_per_loss: Score,
    points_per_draw: Score,
    error_on_repeated_opponent: bool,
    pairings_rule: PairingsRule,
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

pub fn swiss_pairings<'a, P: Player>(
    rounds: &[Round<'a, P>],
    config: &TourneyConfig,
) -> Result<(Vec<(&'a P, &'a P)>, Vec<(&'a P, Score)>), PairingError> {
    let nrounds = rounds.len();
    if nrounds == 0 {
        return Err(PairingError::MissingInitialRound);
    }
    let nplayers = rounds[0].len() * 2;

    let mut player_scores: HashMap<&P, Score> = HashMap::with_capacity(nplayers);
    // i feel like there's a way to create a big matrix of players and use that to test has-played-ness
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

    let initial_seeds: HashMap<&P, usize> = initial_seeding
        .into_iter()
        .enumerate()
        .map(|(k, v)| (v, k))
        .collect();

    let mut standings = player_scores.into_iter().collect::<Vec<(&P, Score)>>();

    standings.sort_by_key(|(p, s)| (-s, initial_seeds.get(p).unwrap_or(&999)));

    let mut pairings: Vec<(&P, &P)> = Vec::with_capacity(nplayers / 2);
    let mut unpaired = standings.clone();
    // top players at the end so we
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

    Ok((pairings, standings))
}

#[derive(PartialEq, Eq, Hash)]
struct MyPlayer {
    id: String,
}

impl Player for MyPlayer {
    fn bye() -> Self {
        Self {
            id: "Bye".to_string(),
        }
    }
    fn id(&self) -> String {
        self.id.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::{swiss_pairings, MatchResult, Player, TourneyConfig, PairingsRule};

    #[derive(PartialEq, Eq, Hash, Debug)]
    struct TestPlayer {
        id: String,
    }

    impl Player for TestPlayer {
        fn bye() -> Self {
            Self {
                id: "Bye".to_string(),
            }
        }
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
            pairings_rule: PairingsRule::MonradSystem
        };
        let standings = swiss_pairings(&vec![vec![r1, r2, r3, r4]], &config).unwrap();
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
            pairings_rule: PairingsRule::MonradSystem
        };
        let (pairings, standings) = swiss_pairings(&vec![r1, r2], &config).unwrap();
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

    /*
    what do we want api to look like

    # first round:
        just establish pairings yourself
    # subsequent round:
        pairings are determined by:

        * player list
        * pairings history
        * pairing strategy

    so you do something like

    ... do whatever for round 1, pair it yourself ...

    let (standings, pairings): (Vec<&P, i32>, Vec<(&P, &P)>)  = swiss_pairings(
        vec![
            // round 1
            vec![
                Result::Win { winner: &p1, loser: &p2 },
                ...
            ]
            // round 2

            vec![
                Result::Win { winner: &p2, loser: &p3 },
                ...
            ]
            ...
        ],
        PairingStrategy::Monrad,
    )


    # then, maybe we dont necessarily report pairings one by one
     */
}
