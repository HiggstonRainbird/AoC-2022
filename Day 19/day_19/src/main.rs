// Advent of Code 2022 Day 19
// Created December 19th, 2022.

use std::time::{Instant};
//use std::collections::HashSet;
use pareto_front::{Dominate, ParetoFront};

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Conf {
	resources: [u16; 4],
	robots: [u16; 4]
}

impl Dominate for Conf {
	fn dominate(&self, x: &Self) -> bool {
		for i in 0..4 {
			if self.resources[i] < x.resources[i] {
				return false
			}
			if self.robots[i] < x.robots[i] {
				return false
			}
		}

		return true
	}
}

fn add_a(a: &[u16; 4], b: &[u16; 4]) -> [u16; 4] {
	return [a[0] + b[0], a[1] + b[1], a[2] + b[2], a[3] + b[3]]
}

fn sub_a(a: &[u16; 4], b: &[u16; 4]) -> [u16; 4] {
	return [a[0] - b[0], a[1] - b[1], a[2] - b[2], a[3] - b[3]]
}

fn mul_s(a: &[u16; 4], s: u16) -> [u16; 4] {
	return [a[0] * s, a[1] * s, a[2] * s, a[3] * s]
}

// #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]

fn timestep(conf: Conf, time_left: u16, blueprint: &[[u16; 4]; 4]) -> Vec<Conf> {
	let resources = conf.resources;
	let robots = conf.robots;

	let mut index = [[0; 4]; 4];
	'outer: for i in 0..4 {
		for r in 0..4 {
			if resources[r] < blueprint[i][r] {
				continue 'outer
			}
		}
		index[i][i] = 1;
	}

	let mut to_return = vec![];
	for i in 0..4 {
		if index[i][i] == 0 {
			continue
		}

		let buy_every = mul_s(&robots, time_left);
		

		let mut tmp_resources = add_a(&resources, &robots);
		tmp_resources = sub_a(&tmp_resources, &blueprint[i]);
		let possibility = Conf {
			resources: tmp_resources,
			robots: add_a(&robots, &index[i])
			};
		to_return.push(possibility);
	}
	let possibility = Conf {
		resources: add_a(&resources, &robots), 
		robots: robots.clone()};
	to_return.push(possibility);
	return to_return
}

fn main() {
	// let file = "../Day16TrimmedSample.txt";
	// let file = "../Day19TrimmedInput.txt";
	// let input_raw: String = fs::read_to_string(file).unwrap();
	
	let start = Instant::now();

	let max = 32;
	
	//let blueprint = [[4, 0, 0, 0], [2, 0, 0, 0], [3, 14, 0, 0], [2, 0, 7, 0]];
	let blueprint = [[2, 0, 0, 0], [3, 0, 0, 0], [3, 8, 0, 0], [3, 0, 12, 0]];
	let mut possibilities = vec![];
	let start_conf = Conf {resources: [0, 0, 0, 0], robots: [1, 0, 0, 0]};
	possibilities.push(start_conf);

	for minute in 1..=max {
		let time_left = max - minute;
		let cumulative = time_left * (time_left - 1) / 2;

		let mut new_possibilities = ParetoFront::new();
		for p in possibilities {
			let result = timestep(p, time_left, &blueprint);
			for r in result {
				new_possibilities.push(r);
			}
		}
		possibilities = new_possibilities.iter().map(|c| c.clone()).collect();

		let best = possibilities
					.iter()
					.map(|c| c.resources[3] + time_left * c.robots[3])
					.fold(std::u16::MIN, |a,b| a.max(b));
		possibilities = possibilities
			.iter()
			.filter(|&c| c.resources[3] + time_left * c.robots[3] + cumulative >= best)
			.map(|c| c.clone())
			.collect::<Vec<Conf>>();

		println!("Minute {} completed with {} possibilities.", minute, possibilities.len());
	}

	let mut most_geodes = 0;
	for p in possibilities {
		if p.resources[3] > most_geodes {
			most_geodes = p.resources[3];
			println!("{:?}", p);
		}
	}
	

	let end = start.elapsed().as_micros();
    println!("Time: {} μs", end);
}

// Time: 78274216 μs