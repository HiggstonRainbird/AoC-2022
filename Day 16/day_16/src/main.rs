// Advent of Code 2022 Day 16
// Created December 16th, 2022.

use std::collections::{HashMap};
use std::fs;
use std::time::{Instant};

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Path {
	hist: u64,
	curr: (u64, u64),
	opening: (bool, bool),
	flow: usize
}

fn flows(n: u64, max_nonzero: u64, flow_hash: &HashMap<u64, usize>) -> usize {
	let mut pow = 1;
	let mut total = 0;
	let new_n = n & max_nonzero;
	while pow <= new_n {
		if new_n & pow != 0 {
			total += flow_hash[&pow];
		}
		pow <<= 1;
	}

	return total
}

fn stay1stay2(path: Path, max_nonzero: u64, _connections: &HashMap<u64, Vec<u64>>, flow_hash: &HashMap<u64, usize>) -> Vec<Path> {
	let mut to_return = vec![];
	
	let hist = path.hist | path.curr.0 | path.curr.1;
	let curr = path.curr;
	let opening = (true, true);
	let flow = path.flow + flows(path.hist, max_nonzero, flow_hash);

	let new_path = Path{hist, curr, opening, flow};
	to_return.push(new_path);

	return to_return;
}

fn stay1go2(path: Path, max_nonzero: u64, connections: &HashMap<u64, Vec<u64>>, flow_hash: &HashMap<u64, usize>) -> Vec<Path> {
	let mut to_return = vec![];
	let new_connections = &connections[&path.curr.1];
	
	for v in new_connections {
		let hist = path.hist | path.curr.0;
		let curr = (path.curr.0, *v);
		let opening = (true, false);
		let flow = path.flow + flows(path.hist, max_nonzero, flow_hash);

		let new_path = Path{hist, curr, opening, flow};
		to_return.push(new_path);
	}

	return to_return;
}

fn go1stay2(path: Path, max_nonzero: u64, connections: &HashMap<u64, Vec<u64>>, flow_hash: &HashMap<u64, usize>) -> Vec<Path> {
	let mut to_return = vec![];
	let new_connections = &connections[&path.curr.0];
	
	for v in new_connections {
		let hist = path.hist | path.curr.1;
		let curr = (*v, path.curr.1);
		let opening = (false, true);
		let flow = path.flow + flows(path.hist, max_nonzero, flow_hash);

		let new_path = Path{hist, curr, opening, flow};
		to_return.push(new_path);
	}

	return to_return;
}

fn go1go2(path: Path, max_nonzero: u64, connections: &HashMap<u64, Vec<u64>>, flow_hash: &HashMap<u64, usize>) -> Vec<Path> {
	let mut to_return = vec![];
	let new_connections_0 = &connections[&path.curr.1];
	let new_connections_1 = &connections[&path.curr.0];
	
	for v1 in new_connections_0 {
		for v2 in new_connections_1 {
			let hist = path.hist;
			let curr = (*v1, *v2);
			let opening = (false, false);
			let flow = path.flow + flows(path.hist, max_nonzero, flow_hash);

			let new_path = Path{hist, curr, opening, flow};
			to_return.push(new_path);
		}
	}

	return to_return;
}

fn nothing(path: Path, max_nonzero: u64, _connections: &HashMap<u64, Vec<u64>>, flow_hash: &HashMap<u64, usize>) -> Vec<Path> {
	let mut to_return = vec![];

	let hist = path.hist;
	let curr = (1, 1);
	let opening = (false, false);
	let flow = path.flow + flows(path.hist, max_nonzero, flow_hash);

	let new_path = Path{hist, curr, opening, flow};
	to_return.push(new_path);

	return to_return
}

fn next_path(path: Path, max_nonzero: u64, connections: &HashMap<u64, Vec<u64>>, flow_hash: &HashMap<u64, usize>) -> Vec<Path> {
	let first_stay = path.hist & path.curr.0 > 0 || flow_hash[&path.curr.0] == 0;
	let second_stay = path.hist & path.curr.1 > 0 || flow_hash[&path.curr.1] == 0;

	if path.hist == max_nonzero {
		return nothing(path, max_nonzero, connections, flow_hash)
	} else if path.opening == (true, true) {
		return go1go2(path, max_nonzero, connections, flow_hash)
	} else if path.opening == (false, true) {
		if first_stay {
			return go1go2(path, max_nonzero, connections, flow_hash)
		} else {
			let mut to_return = stay1go2(path.clone(), max_nonzero, connections, flow_hash);
			let mut tmp = go1go2(path, max_nonzero, connections, flow_hash);
			to_return.append(&mut tmp);
			return to_return
		}
	} else if path.opening == (true,false) {
		if second_stay {
			return go1go2(path, max_nonzero, connections, flow_hash)
		} else {
			let mut to_return = go1stay2(path.clone(), max_nonzero, connections, flow_hash);
			let mut tmp = go1go2(path, max_nonzero, connections, flow_hash);
			to_return.append(&mut tmp);
			return to_return
		}
	} else if path.opening == (false, false) {
		if first_stay && second_stay {
			return go1go2(path, max_nonzero, connections, flow_hash)
		} else if first_stay {
			let mut to_return = go1stay2(path.clone(), max_nonzero, connections, flow_hash);
			let mut tmp = go1go2(path, max_nonzero, connections, flow_hash);
			to_return.append(&mut tmp);
			return to_return
		} else if second_stay {
			let mut to_return = stay1go2(path.clone(), max_nonzero, connections, flow_hash);
			let mut tmp = go1go2(path, max_nonzero, connections, flow_hash);
			to_return.append(&mut tmp);
			return to_return
		} else {
			let mut to_return = stay1stay2(path.clone(), max_nonzero, connections, flow_hash);
			let mut tmp = go1go2(path, max_nonzero, connections, flow_hash);
			to_return.append(&mut tmp);
			return to_return
		}
	}

	return vec![]
}

fn max_gather(paths: Vec<Path>) -> Vec<Path> {
	let mut tmp_hash: HashMap<(u64, (u64, u64), (bool, bool)), Path> = HashMap::new();
	for p in paths {
		if !tmp_hash.contains_key(&(p.hist, p.curr, p.opening)) || tmp_hash.get(&(p.hist, p.curr, p.opening)).unwrap().flow < p.flow {

			let key = (p.hist.clone(), p.curr.clone(), p.opening.clone());
			tmp_hash.insert(key, p);
		}
	}

	let mut to_return = vec![];
	for (_, v) in tmp_hash {
		to_return.push(v);
	}

	return to_return
}

fn hist_gather(paths: Vec<Path>, max_nonzero: u64) -> Vec<Path> {
	let mut best_hist = HashMap::new();
	let mut sorted_paths = paths;
	sorted_paths.sort_by_key(|p| p.hist.count_ones());
	sorted_paths.reverse();
	let mut to_return = vec![];

	'outer: for p in sorted_paths {
		if p.hist == max_nonzero {
			to_return.push(p);
		} else {
			if !best_hist.contains_key(&(p.curr, p.opening)) {
				best_hist.insert((p.curr, p.opening), vec![p.hist]);
				to_return.push(p);
			} else {
				let mut best = (best_hist.get(&(p.curr, p.opening)).unwrap()).clone();
				for b in &best {
					if b & p.hist == p.hist && p.hist < *b {
						continue 'outer;
					}
				}

				best.push(p.hist);
				best_hist.insert((p.curr, p.opening), best);
				to_return.push(p);
			}
		}
	}


	return to_return
}

fn main() {
	//let file = "../Day16TrimmedSample.txt";
	let file = "../Day16TrimmedInput.txt";
	let input_raw: String = fs::read_to_string(file).unwrap();
	
	let start = Instant::now();
	
	let mut tmp_valves: Vec<(&str, usize, Vec<&str>)> = Vec::new();
	
	for line in input_raw.lines() {
		let mut tokens = line.split(|c| c == ' ' || c == ',');

		let valve: &str = tokens.next().unwrap();
		let flow: usize = tokens.next().unwrap().parse::<usize>().unwrap();
		let mut connections = vec![];
		while let Some(token) = tokens.next() {
			connections.push(token);
		}

		tmp_valves.push((valve, flow, connections));
	}
	let mut all_valves: Vec<&str> = 
		tmp_valves
			.iter()
			.map(|(v, _, _)| *v)
			.collect();
	all_valves.sort();

	let mut all_nonzeroes: Vec<&str> = 
		tmp_valves
			.iter()
			.filter(|(_, f, _)| *f != 0)
			.map(|(v, _, _)| *v)
			.collect();
	all_nonzeroes.sort();

	let mut masks = HashMap::new();
	for (i, v) in all_valves.iter().enumerate() {
		masks.insert(*v, 2u64.pow(i as u32));
	}

	let mut max_nonzero = 0;
	for v in all_nonzeroes {
		max_nonzero += masks[v]
	};
	
	let mut flow_hash = HashMap::new();
	let mut connections = HashMap::new();
	for (v, f, cs) in &tmp_valves {
		flow_hash.insert(masks[*v], *f);

		let mut cs_masks = vec![];
		for &c in cs {
			cs_masks.push(masks[c]);
		}
		connections.insert(masks[*v], cs_masks);
	}
	


	let mut paths = vec![Path{hist: 0, curr: (1, 1), opening: (false, false), flow: 0}];

	for round in 0..26 {
		let mut new = vec![];
		for p in paths {
			let next = next_path(p, max_nonzero, &connections, &flow_hash);
			for n in next {
				new.push(n);
			}
		}
		new = max_gather(new);
		new = hist_gather(new, max_nonzero);
		new.sort();
		//new.dedup();

		let count = new.len();
		let mut best = 0;
		for p in &new {
			if p.flow > best {
				best = p.flow
			}
		};

		paths = new;
		let end = start.elapsed().as_micros();
		println!("{}, {}, {} in {} μs.", round + 1, count, best, end);
	}

	// for p in paths {
	// 	println!("{:?}", p);
	// }
	// println!("");

	// let tmp = Path { hist: 2, curr: (1, 2), opening: (false, true), flow: 0 };
	// for p in next_path(tmp, max_nonzero, &connections, &flow_hash) {
	// 	println!("{:?}", p);
	// }

	let end = start.elapsed().as_micros();
    println!("Time: {} μs", end);
}

// 26, 2818170, 2213 in 227476596 μs.