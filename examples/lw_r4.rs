extern crate core;
extern crate rand;
extern crate swiss_pairings;

use swiss_pairings::{MatchResult, Player, TourneyConfig};

fn p1_win<'a, P: Player>(p1: &'a P, p2: &'a P) -> MatchResult<'a, P> {
    MatchResult::Player1Win { p1, p2 }
}

fn p2_win<'a, P: Player>(p1: &'a P, p2: &'a P) -> MatchResult<'a, P> {
    MatchResult::Player2Win { p1, p2 }
}

fn main() -> Result<(), String> {
    let trinexx = "trinexx".to_string();
    let spleebie = "spleebie".to_string();
    let flipheal = "flipheal".to_string();
    let rob = "rob".to_string();
    let monkey = "monkey".to_string();
    let shady = "shadyforce".to_string();
    let aaron = "aaron".to_string();
    let vex = "vex".to_string();
    let tam = "tam".to_string();
    let cheamo = "cheamo".to_string();
    let yeroc = "yeroc".to_string();
    let rei = "rei".to_string();
    let danty = "daaanty".to_string();
    let parisian = "parisian".to_string();
    let coxla = "coxla".to_string();
    let john = "john snuu".to_string();
    let config = TourneyConfig {
        points_per_win: 2,
        points_per_loss: 0,
        points_per_draw: 1,
        error_on_repeated_opponent: false,
    };

    let rounds = vec![
        vec![
            MatchResult::Player2Win {
                p1: &trinexx,
                p2: &spleebie,
            },
            MatchResult::Player1Win {
                p1: &flipheal,
                p2: &rob,
            },
            MatchResult::Player2Win {
                p1: &monkey,
                p2: &shady,
            },
            MatchResult::Player2Win {
                p1: &aaron,
                p2: &vex,
            },
            MatchResult::Player2Win {
                p1: &tam,
                p2: &cheamo,
            },
            MatchResult::Player2Win {
                p1: &yeroc,
                p2: &rei,
            },
            MatchResult::Player2Win {
                p1: &danty,
                p2: &parisian,
            },
            MatchResult::Player2Win {
                p1: &coxla,
                p2: &john,
            },
        ],
        vec![
            MatchResult::Player1Win {
                p1: &yeroc,
                p2: &rob,
            },
            MatchResult::Player1Win {
                p1: &tam,
                p2: &aaron,
            },
            MatchResult::Player1Win {
                p1: &monkey,
                p2: &coxla,
            },
            MatchResult::Player1Win {
                p1: &trinexx,
                p2: &danty,
            },
            MatchResult::Player1Win {
                p1: &cheamo,
                p2: &vex,
            },
            MatchResult::Player1Win {
                p1: &shady,
                p2: &john,
            },
            MatchResult::Player1Win {
                p1: &spleebie,
                p2: &parisian,
            },
            MatchResult::Player1Win {
                p1: &flipheal,
                p2: &rei,
            },
        ],
        vec![
            p1_win(&tam, &yeroc),
            p2_win(&monkey, &vex),
            p2_win(&trinexx, &john),
            p1_win(&rei, &parisian),
            p2_win(&aaron, &danty),
            p1_win(&rob, &coxla),
            p1_win(&spleebie, &cheamo),
            p2_win(&flipheal, &shady),
        ],
    ];

    // round 1 was handled separately above because of poor design decisions
    let (pairings, standings) = swiss_pairings::swiss_pairings(&rounds, &config, None).unwrap();
    println!("pairings:  {:?}", pairings);
    println!("   standings: {:?}", standings);

    Ok(())
}
