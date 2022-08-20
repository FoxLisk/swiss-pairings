extern crate swiss_pairings;
extern crate core;
extern crate rand;

use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::{env, io};
use std::io::Write;
use std::time::Instant;
use dotenv::var;
use rand::{Rng, thread_rng};
use rand::rngs::ThreadRng;
use swiss_pairings::{format_bracket, MatchResult, Player, Score, TourneyConfig};
use swiss_pairings::MatchResult::{Player1Win, Player2Win};

type ToyPlayer = i32;

fn main() -> Result<(), String> {
    dotenv::dotenv().ok();
    let iterations = var("ITERATIONS").map(|s| s.parse::<i32>().unwrap()).unwrap();
    let nplayers = var("NPLAYERS").map(|s| s.parse::<i32>().unwrap()).unwrap();
    assert_eq!(0, nplayers % 2, "Even number of players required (mind your own byes)");

    let mut wins = HashMap::<ToyPlayer, u32>::new();
    let mut gaps = HashMap::<i32, u32>::new();
    let players = (0..nplayers).into_iter().collect::<Vec<_>>();
    assert_eq!(nplayers as usize, players.len());
    let mut scores_by_player: HashMap<ToyPlayer, HashMap<Score, u32>> = Default::default();
    for p in &players {
        scores_by_player.insert(p.clone(), HashMap::new());
    }

    let start = Instant::now();
    for i in 1..=iterations {
        if i % 100 == 0 {
            print!(".");
            io::stdout().flush().map_err(|e| e.to_string())?;
            if i % 1000 == 0 {
                println!();
            }
        }
        let (gap, standings) = do_a_bracket(players.clone())?;
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
    println!("Ran {} iterations over {} players in {:?}", iterations, nplayers, Instant::now() - start);
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
fn do_a_bracket(players: Vec<ToyPlayer>) -> Result<(i32, Vec<(ToyPlayer, i32)>), String> {
    let nrounds = (players.len() as f64).log2().ceil() as i32;
    assert!(2_usize.pow(nrounds as u32) >= players.len());
    assert!(2_usize.pow((nrounds-1) as u32) < players.len());


    let config = TourneyConfig {
        points_per_win: 2,
        points_per_loss: 0,
        points_per_draw: 1,
        error_on_repeated_opponent: false
    };
    let mut initial_pairings = vec![];

    for i in 0..(players.len() / 2) {
        initial_pairings.push((&players[2*i], &players[2*i+1]));
    }
    assert_eq!(players.len() / 2, initial_pairings.len());
    // let mut decider = Decider::Random(thread_rng());
    let mut decider = thread_rng();
    let first_round = decide_matchups(&initial_pairings, &mut decider);
    let mut rounds: Vec<Vec<MatchResult<ToyPlayer>>> = vec![first_round];

    let mut greatest_downpairing = 0;

    let f = swiss_pairings::random_by_scoregroup;

    // round 1 was handled separately above because of poor design decisions
    // final round is handled separately below
    for round_num in 2..nrounds {
        let (pairings, standings) = swiss_pairings::swiss_pairings(
            &rounds,
            &config,
            f
        ).unwrap();
        let points: HashMap<&ToyPlayer, Score> = HashMap::from_iter(standings);
        for e in &pairings {
            let (p1, p2): &(&ToyPlayer, &ToyPlayer) = e;
            let p1_score = points.get(p1).unwrap();
            let p2_score = points.get(p2).unwrap();
            greatest_downpairing = max(greatest_downpairing, (p2_score - p1_score).abs());
        }

        // println!("round {} pairings: {:?}", round_num, pairings);
        let round_result = decide_matchups(&pairings, &mut decider);
        rounds.push(round_result);
    }

    let (final_pairings, final_standings) = match swiss_pairings::swiss_pairings(
        &rounds,
        &config,
        f
    ) {
        Ok(stuff) => { stuff}
        Err(e) => {

            return Err(format!(
                "Error pairing the final round! {}
                 Previous rounds: {}", e, format_bracket(&rounds)
            ));
        }
    };


    if greatest_downpairing > 0 || env::var("DEBUG").is_ok() {
        println!("{}", format_bracket(&rounds));
        println!("{:?}", final_standings);
    }

    Ok((greatest_downpairing, final_standings.iter().map(|(c, s)| (**c, *s)).collect()))

}

type decision_func<'a, P> = dyn Fn((&P, &P)) -> MatchResult<'a, P>;

fn decide_matchups<'a, P: Player, D: DecideWinner<P>>(pairings: &Vec<(&'a P, &'a P)>, d: &mut D) -> Vec<MatchResult<'a, P>>

{
    let mut results = vec![];
    for pair in pairings {
        results.push(d.decide_winner(*pair));
    }
    results
}

fn random_winner<'a, P: Player>(players: (&'a P, &'a P)) -> MatchResult<'a, P> {
    let (p1, p2) = players;
    let mut rng = thread_rng();
    if rng.gen_bool(0.5) {
        MatchResult::Player1Win {
            p1,
            p2
        }
    } else {
        MatchResult::Player2Win {
            p1,
            p2
        }
    }
}

trait DecideWinner<P: Player> {
    fn decide_winner<'a>(&mut self, players: (&'a P, &'a P)) -> MatchResult<'a, P>;
}

impl<P: Player> DecideWinner<P> for ThreadRng {
    fn decide_winner<'a>(&mut self, players: (&'a P, &'a P)) -> MatchResult<'a, P> {
        let (p1, p2) = players;
        if self.gen_bool(0.5) {
            Player1Win {
                p1,
                p2
            }
        } else {
            Player2Win {
                p1,
                p2
            }
        }
    }
}

enum Decider {
    P1Win,
    P2Win,
    Random(ThreadRng)
}

impl<P: Player> DecideWinner<P> for Decider {
    fn decide_winner<'a>(&mut self, players: (&'a P, &'a P)) -> MatchResult<'a, P> {
        let (p1, p2) = players;
        match self {
            Decider::P1Win => { Player1Win {p1, p2}}
            Decider::P2Win => { Player2Win {p1, p2}}
            Decider::Random(rng) => {
                if rng.gen_bool(0.5) {
                    Player1Win {
                        p1,
                        p2
                    }
                } else {
                    Player2Win {
                        p1,
                        p2
                    }
                }
            }
        }
    }
}