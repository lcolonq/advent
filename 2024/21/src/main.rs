use std::collections::HashMap;

use petgraph::Graph;
use petgraph::prelude::*;
use petgraph::algo::simple_paths::all_simple_paths;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Digit { Digit(usize), A }
impl Default for Digit { fn default() -> Self { Self::A } }
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Dir { N, S, W, E, A }
impl Default for Dir { fn default() -> Self { Self::A } }
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Atom { // "round-trip" ending with A
    path: Vec<Dir>,
}

fn digit_atoms(prev: &Digit, cur: &Digit) -> Vec<Atom> {
    let mut graph = Graph::<_, Dir, Directed>::new();
    let da = graph.add_node(Digit::A);
    let mut d = [<Graph::<Digit, Dir, Directed> as petgraph::visit::GraphBase>::NodeId::default(); 10];
    for i in 0..=9 { d[i] = graph.add_node(Digit::Digit(i)); }
    graph.extend_with_edges(&[
        (d[7], d[8], Dir::E), (d[7], d[4], Dir::S),
        (d[8], d[7], Dir::W), (d[8], d[9], Dir::E), (d[8], d[5], Dir::S),
        (d[9], d[8], Dir::W), (d[9], d[6], Dir::S),
        (d[4], d[5], Dir::E), (d[4], d[1], Dir::S), (d[4], d[7], Dir::N),
        (d[5], d[4], Dir::W), (d[5], d[6], Dir::E), (d[5], d[2], Dir::S), (d[5], d[8], Dir::N),
        (d[6], d[5], Dir::W), (d[6], d[3], Dir::S), (d[6], d[9], Dir::N),
        (d[1], d[2], Dir::E), (d[1], d[4], Dir::N),
        (d[2], d[1], Dir::W), (d[2], d[3], Dir::E), (d[2], d[0], Dir::S), (d[2], d[5], Dir::N),
        (d[3], d[2], Dir::W), (d[3],   da, Dir::S), (d[3], d[6], Dir::N),
        (d[0], da, Dir::E), (d[0], d[2], Dir::N),
        (da, d[0], Dir::W), (da, d[3], Dir::N),
    ]);
    let pi = match prev { Digit::A => da, Digit::Digit(i) => d[*i] };
    let ci = match cur { Digit::A => da, Digit::Digit(i) => d[*i] };
    let mut res = Vec::new();
    let mut minl = usize::MAX;
    for p in all_simple_paths::<Vec<_>, _>(&graph, pi, ci, 0, None) {
        let mut nodes = p.iter();
        let mut path = Vec::new();
        if let Some(mut last) = nodes.next() {
            for n in nodes {
                let e = graph.find_edge(*last, *n).expect("no edge");
                path.push(graph.edge_weight(e).expect("no edge").clone());
                last = n;
            }
        }
        path.push(Dir::A);
        if path.len() < minl { minl = path.len(); }
        res.push(Atom { path });
    }
    res.into_iter().filter(|p| p.path.len() == minl).collect()
}

fn dir_atoms(prev: &Dir, cur: &Dir) -> Vec<Atom> {
    let mut graph = Graph::<_, Dir, Directed>::new();
    let da = graph.add_node(Dir::A);
    let dn = graph.add_node(Dir::N); let ds = graph.add_node(Dir::S);
    let dw = graph.add_node(Dir::W); let de = graph.add_node(Dir::E);
    graph.extend_with_edges(&[
        (dn, da, Dir::E), (dn, ds, Dir::S),
        (da, dn, Dir::W), (da, de, Dir::S),
        (dw, ds, Dir::E),
        (ds, dn, Dir::N), (ds, dw, Dir::W), (ds, de, Dir::E),
        (de, da, Dir::N), (de, ds, Dir::W),
    ]);
    let pi = match prev { Dir::A => da, Dir::N => dn, Dir::S => ds, Dir::W => dw, Dir::E => de };
    let ci = match cur { Dir::A => da, Dir::N => dn, Dir::S => ds, Dir::W => dw, Dir::E => de };
    if pi == ci { return vec![Atom { path: vec![Dir::A] }]; }
    let mut res = Vec::new();
    let mut minl = usize::MAX;
    for p in all_simple_paths::<Vec<_>, _>(&graph, pi, ci, 0, None) {
        let mut nodes = p.iter();
        let mut path = Vec::new();
        if let Some(mut last) = nodes.next() {
            for n in nodes {
                let e = graph.find_edge(*last, *n).expect("no edge");
                path.push(graph.edge_weight(e).expect("no edge").clone());
                last = n;
            }
        }
        path.push(Dir::A);
        if path.len() < minl { minl = path.len(); }
        res.push(Atom { path });
    }
    res.into_iter().filter(|p| p.path.len() == minl).collect()
}

struct Memo {
    possible_atoms: HashMap<(Dir, Dir), Vec<Atom>>,
    shortest_expansions: HashMap<(Atom, usize), usize>,
}

impl Memo {
    fn new() -> Self {
        Self {
            possible_atoms: HashMap::new(),
            shortest_expansions: HashMap::new(),
        }
    }
    fn possibilities(&mut self, prev: &Dir, next: &Dir) -> &Vec<Atom> {
        self.possible_atoms.entry((prev.clone(), next.clone()))
            .or_insert(dir_atoms(prev, next))
    }
    fn expanded_len(&mut self, a: &Atom, n: usize) -> usize {
        if let Some(r) = self.shortest_expansions.get(&(a.clone(), n)) {
            return *r;
        } else {
            let ret = self.base_expanded_len(a, n);
            self.shortest_expansions.insert((a.clone(), n), ret);
            ret
        }
    }
    fn base_expanded_len(&mut self, a: &Atom, n: usize) -> usize {
        if n == 0 { return a.path.len(); }
        let mut prev = Dir::A;
        let mut prefixes = vec![0];
        for cur in a.path.iter() {
            let suffixes = self.possibilities(&prev, &cur).clone();
            let mut new = Vec::new();
            for pfx in prefixes {
                for sfx in suffixes.iter() {
                    new.push(pfx + self.expanded_len(sfx, n - 1));
                }
            }
            prefixes = new;
            prev = cur.clone();
        }
        prefixes.into_iter().min().unwrap()
    }
}


fn main() {
    let mut m = Memo::new();
    let mut sum = 0;
    for l in include_str!("input.txt").to_string().split("\n").filter(|l| l.len() != 0) {
        let num: usize = l.to_string().replace("A", "").parse().unwrap();
        let digits = l.chars().map(|c| match c {
            'A' => Digit::A, _ => Digit::Digit(c as usize - '0' as usize),
        }).collect::<Vec<_>>();
        let mut total = 0;
        let mut prev = Digit::A;
        for cur in digits.iter() {
            let atoms = digit_atoms(&prev, &cur);
            total += atoms.into_iter().map(|a| m.expanded_len(&a, 25)).min().unwrap();
            prev = cur.clone();
        }
        println!("{}: {}", l, total);
        sum += num * total;
    }
    println!("total: {}", sum);
}
