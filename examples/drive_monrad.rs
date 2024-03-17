//! This file is less of an example and more of a place that I use to test things at this point
//! but i dont feel like rearranging it :\
extern crate core;
extern crate rand;
extern crate swiss_pairings;

use anyhow::anyhow;
use dotenv::var;
use rand::rngs::ThreadRng;
use rand::{thread_rng, Rng};
use std::cmp::max;
use std::collections::HashMap;
use std::io::Write;
use std::time::Instant;
use std::{env, io};
use swiss_pairings::MatchResult::{Player1Win, Player2Win};
use swiss_pairings::{format_bracket, MatchResult, Player, Score, TourneyConfig};

macro_rules! matchup {
    ($p1:ident > $p2:ident) => {
        swiss_pairings::MatchResult::Player1Win {
            p1: &stringify!($p1),
            p2: &stringify!($p2),
        }
    };
    ($p1:ident = $p2:ident) => {
        swiss_pairings::MatchResult::Draw {
            p1: &stringify!($p1),
            p2: &stringify!($p2),
        }
    };
    ($p1:ident < $p2:ident) => {
        swiss_pairings::MatchResult::Player2Win {
            p1: &stringify!($p1),
            p2: &stringify!($p2),
        }
    };
}

macro_rules! matchups {
    ( $( $p1:ident $op:tt $p2:ident ),* $(,)?) => {
        vec![
            $( matchup!($p1 $op $p2) ),*
        ]
    }
}

fn a_specific_6p_bracket() {
    let config = TourneyConfig {
        points_per_win: 2,
        points_per_loss: 0,
        points_per_draw: 1,
        error_on_repeated_opponent: true,
    };
    let rounds = vec![
        matchups![A > B, C < D, E > F,],
        matchups![D > E, A < C, B < F],
    ];

    println!("{}", format_bracket(&rounds));

    let (pairings, _standings) =
        swiss_pairings::swiss_pairings(&rounds, &config, swiss_pairings::random_by_scoregroup)
            .unwrap();
    println!("{pairings:?}");
}

fn another_specific_6p_bracket() {
    let config = TourneyConfig {
        points_per_win: 2,
        points_per_loss: 0,
        points_per_draw: 1,
        error_on_repeated_opponent: true,
    };
    let rounds = vec![
        matchups![A < B, C > D, E > F],
        matchups![F < A, B < C, E > D],
    ];

    println!("{}", format_bracket(&rounds));

    let (pairings, _standings) =
        swiss_pairings::swiss_pairings(&rounds, &config, swiss_pairings::random_by_scoregroup)
            .unwrap();
    println!("{pairings:?}");
}

fn a_6p_with_draws() {
    let config = TourneyConfig {
        points_per_win: 2,
        points_per_loss: 0,
        points_per_draw: 1,
        error_on_repeated_opponent: true,
    };
    // let mut initial_pairings = vec![(&p0, &p1), (&p2, &p3), (&p4, &p5)];
    let rounds = vec![
        matchups![A = B, C = D, E > F],
        matchups![B < D, F > A, E = C],
    ];

    println!("{}", format_bracket(&rounds));

    let (pairings, _standings) =
        swiss_pairings::swiss_pairings(&rounds, &config, swiss_pairings::random_by_scoregroup)
            .unwrap();
    println!("{pairings:?}");
}

fn weird_16() {
    let config = TourneyConfig {
        points_per_win: 2,
        points_per_loss: 0,
        points_per_draw: 1,
        error_on_repeated_opponent: true,
    };
    let rounds = vec![
        matchups![A = B, C > D, E = F, G < H, I < J, K = L, M = N, O = P],
        matchups![I > G, N = P, O = B, M = A, F = K, E < D, H > C, J = L],
    ];

    println!("{}", format_bracket(&rounds));

    let (pairings, _standings) =
        swiss_pairings::swiss_pairings(&rounds, &config, swiss_pairings::random_by_scoregroup)
            .unwrap();
    println!("{pairings:?}");
}

// this 16-man bracket hit an exponential time blowup thingy
fn degenerate_16() {
    let config = TourneyConfig {
        points_per_win: 2,
        points_per_loss: 0,
        points_per_draw: 1,
        error_on_repeated_opponent: true,
    };
    let rounds = vec![
        matchups![A = B, C > D, E < F, G = H, I = J, K > L, M < N, O < P,],
        matchups![E < D, L = M, A < G, B < I, O < J, K > N, C < H, P = F,],
        matchups![E > O, M = A, C = B, D < L, F = G, J > H, I = N, K = P,],
    ];

    println!("{}", format_bracket(&rounds));

    let (pairings, _standings) =
        swiss_pairings::swiss_pairings(&rounds, &config, swiss_pairings::random_by_scoregroup)
            .unwrap();
    println!("{pairings:?}");
}

fn main() -> anyhow::Result<()> {
    a_specific_6p_bracket();
    another_specific_6p_bracket();
    a_6p_with_draws();
    weird_16();
    degenerate_16();
    // return Ok(());
    dotenv::dotenv().ok();
    let iterations = var("ITERATIONS")
        .map(|s| s.parse::<i32>().unwrap())
        .unwrap();
    let nplayers = var("NPLAYERS").map(|s| s.parse::<u32>().unwrap()).unwrap();
    assert_eq!(
        0,
        nplayers % 2,
        "Even number of players required (mind your own byes)"
    );

    let mut wins = HashMap::<char, u32>::new();
    let mut gaps = HashMap::<i32, u32>::new();
    let players = (0..nplayers)
        .into_iter()
        .map(|i| char::from_u32(i + 65).unwrap())
        .collect::<Vec<_>>();
    assert_eq!(nplayers as usize, players.len());
    let mut scores_by_player: HashMap<char, HashMap<Score, u32>> = Default::default();
    for p in &players {
        scores_by_player.insert(p.clone(), HashMap::new());
    }

    let start = Instant::now();
    for i in 1..=iterations {
        if i % 100 == 0 {
            print!(".");
            io::stdout().flush()?;
            if i % 1000 == 0 {
                println!();
            }
        }
        let (gap, standings) = match do_a_bracket(players.clone()) {
            Ok(whatever) => whatever,
            Err(e) => {
                println!("{e}");
                return Err(anyhow!(""));
            }
        };
        let e = gaps.entry(gap).or_insert(0);
        *e += 1;

        let (winner, _) = standings.first().unwrap();
        let e = wins.entry(*winner).or_insert(0);
        *e += 1;

        for (p, s) in standings {
            let thing = scores_by_player.get_mut(&p).unwrap();
            let e = thing.entry(s).or_insert(0);
            *e += 1;
        }
    }

    println!("Gaps: {:?}", gaps);
    // println!("Wins: {:?} (total unique winners: {})", wins, wins.len());
    println!("Scores by player: ");
    for p in players {
        println!("  {}: {:?}", p, scores_by_player.remove(&p).unwrap());
    }
    println!(
        "Ran {} iterations over {} players in {:?}",
        iterations,
        nplayers,
        Instant::now() - start
    );
    Ok(())
}

/// what this actually does is:
/// 1. establish an initial bracket with monrad pairings (1v2, 3v4, 5v6...)
/// 2. run n-1 rounds, where n is what you expect (ceil(log2(number of players))
/// 3. calculate the final pairings & final standings
/// 4. returns (greatest downpairing, final standings)
///     where downpairing = the discrepancy between 2 players scores during a pairing, and
///           standings are a (sorted) vector of (player, score) tuples
///    note that this is not returning a winner, since we don't actually run the final round.
fn do_a_bracket(players: Vec<char>) -> Result<(i32, Vec<(char, i32)>), String> {
    let nrounds = (players.len() as f64).log2().ceil() as i32;
    assert!(2_usize.pow(nrounds as u32) >= players.len());
    assert!(2_usize.pow((nrounds - 1) as u32) < players.len());

    let config = TourneyConfig {
        points_per_win: 2,
        points_per_loss: 0,
        points_per_draw: 1,
        error_on_repeated_opponent: true,
    };
    let mut initial_pairings = vec![];

    for i in 0..(players.len() / 2) {
        initial_pairings.push((&players[2 * i], &players[2 * i + 1]));
    }
    assert_eq!(players.len() / 2, initial_pairings.len());
    let mut decider = thread_rng();
    let first_round = decide_matchups(&initial_pairings, &mut decider);
    let mut rounds: Vec<Vec<MatchResult<char>>> = vec![first_round];

    let mut greatest_downpairing = 0;

    let f = swiss_pairings::random_by_scoregroup;

    // round 1 was handled separately above because of poor design decisions
    // final round is handled separately below
    for _round_num in 2..nrounds {
        let (pairings, standings) = swiss_pairings::swiss_pairings(&rounds, &config, f).unwrap();
        let points: HashMap<&char, Score> = HashMap::from_iter(standings);
        for e in &pairings {
            let (p1, p2): &(&char, &char) = e;
            let p1_score = points.get(p1).unwrap();
            let p2_score = points.get(p2).unwrap();
            greatest_downpairing = max(greatest_downpairing, (p2_score - p1_score).abs());
        }

        // println!("round {} pairings: {:?}", round_num, pairings);
        let round_result = decide_matchups(&pairings, &mut decider);
        rounds.push(round_result);
    }

    let (_final_pairings, final_standings) =
        match swiss_pairings::swiss_pairings(&rounds, &config, f) {
            Ok(stuff) => stuff,
            Err(e) => {
                return Err(format!(
                    "Error pairing the final round! {}
                 Previous rounds: {}",
                    e,
                    format_bracket(&rounds)
                ));
            }
        };

    if greatest_downpairing > 0 || env::var("DEBUG").is_ok() {
        println!("{}", format_bracket(&rounds));
        println!("{:?}", final_standings);
    }

    Ok((
        greatest_downpairing,
        final_standings.iter().map(|(c, s)| (**c, *s)).collect(),
    ))
}

fn decide_matchups<'a, P: Player, D: DecideWinner<P>>(
    pairings: &Vec<(&'a P, &'a P)>,
    d: &mut D,
) -> Vec<MatchResult<'a, P>> {
    let mut results = vec![];
    for pair in pairings {
        results.push(d.decide_winner(*pair));
    }
    results
}

trait DecideWinner<P: Player> {
    fn decide_winner<'a>(&mut self, players: (&'a P, &'a P)) -> MatchResult<'a, P>;
}

impl<P: Player> DecideWinner<P> for ThreadRng {
    fn decide_winner<'a>(&mut self, players: (&'a P, &'a P)) -> MatchResult<'a, P> {
        let (p1, p2) = players;
        if self.gen_bool(0.3) {
            Player1Win { p1, p2 }
        } else if self.gen_bool(0.5) {
            Player2Win { p1, p2 }
        } else {
            MatchResult::Draw { p1: p1, p2: p2 }
        }
    }
}

enum Decider {
    P1Win,
    P2Win,
    Random(ThreadRng),
}

impl<P: Player> DecideWinner<P> for Decider {
    fn decide_winner<'a>(&mut self, players: (&'a P, &'a P)) -> MatchResult<'a, P> {
        let (p1, p2) = players;
        match self {
            Decider::P1Win => Player1Win { p1, p2 },
            Decider::P2Win => Player2Win { p1, p2 },
            Decider::Random(rng) => {
                if rng.gen_bool(0.5) {
                    Player1Win { p1, p2 }
                } else {
                    Player2Win { p1, p2 }
                }
            }
        }
    }
}
