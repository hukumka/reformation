// Comparison of original kirby (https://github.com/rubytogether/kirby) user agent parsing
// and it reimplementation using reformation.
// Copyright notice: https://github.com/rubytogether/kirby/blob/main/LICENSE

#[macro_use]
extern crate criterion;

use reformation::{Reformation, Captures};
use criterion::{Criterion, Fun};
use regex::Regex;
use lazy_static::lazy_static;


#[derive(Reformation)]
#[reformation("\
    bundler/{bundler} rubygems/{rubygems} ruby/{ruby} \\({platform}\\) command/{command}\
    ( jruby/{jruby})?( truffleruby/{truffleruby})?( options/{options})?( ci/{ci})? \
    [a-f0-9]{{16}}( Gemstash/{gemstash})?\
")]
struct UserAgent2<'input>{ // 'input - special lifetime name corresponding to lifetime of input string
    bundler: Version<'input>,
    rubygems: Version<'input>,
    ruby: Version<'input>,

    #[reformation(r"([^)]*)")]
    platform: &'input str,

    command: &'input str,
    jruby: Option<Version<'input>>,
    truffleruby: Option<Version<'input>>,
    options: Option<&'input str>,
    ci: Option<&'input str>,
    gemstash: Option<Version<'input>>,
}

#[derive(Reformation)]
#[reformation("{}")]
struct Version<'input>(
    #[reformation(r"([0-9a-zA-Z.\-]+)")]
    &'input str
);

// Original

#[derive(PartialEq, Debug)]
pub struct UserAgent<'a> {
    pub bundler: Option<&'a str>,
    pub rubygems: &'a str,
    pub ruby: Option<&'a str>,
    pub platform: Option<&'a str>,
    pub command: Option<&'a str>,
    pub options: Option<&'a str>,
    pub jruby: Option<&'a str>,
    pub truffleruby: Option<&'a str>,
    pub ci: Option<&'a str>,
    pub gemstash: Option<&'a str>,
}

pub fn parse(a: &str) -> Option<UserAgent> {
    lazy_static! {
    // Here is the named regex. The regex created below does not include names, because that interface has borrowing issues 😬
    // \Abundler/(?<bundler>[0-9a-zA-Z.\-]+) rubygems/(?<rubygems>[0-9a-zA-Z.\-]+) ruby/(?<ruby>[0-9a-zA-Z.\-]+) \((?<platform>.*)\) command/(.*?)(?: jruby/(?<jruby>[0-9a-zA-Z.\-]+))?(?: truffleruby/(?<truffleruby>[0-9a-zA-Z.\-]+))?(?: options/(?<options>.*?))?(?: ci/(?<ci>.*?))? ([a-f0-9]{16})(?: Gemstash/(?<gemstash>[0-9a-zA-Z.\-]+))?\z
    static ref br: Regex = Regex::new(r"\Abundler/([0-9a-zA-Z.\-]+) rubygems/([0-9a-zA-Z.\-]+) ruby/([0-9a-zA-Z.\-]+) \(([^)]*)\) command/(.*?)(?: jruby/([0-9a-zA-Z.\-]+))?(?: truffleruby/([0-9a-zA-Z.\-]+))?(?: options/(.*?))?(?: ci/(.*?))? [a-f0-9]{16}(?: Gemstash/([0-9a-zA-Z.\-]+))?\z").unwrap();
    static ref rr: Regex = Regex::new(r"\A(?:Ruby, )?RubyGems/([0-9a-z.\-]+) (.*) Ruby/([0-9a-z.\-]+) \(.*?\)(?: jruby| truffleruby| rbx)?(?: Gemstash/([0-9a-z.\-]+))?\z").unwrap();
    static ref gr: Regex = Regex::new(r"\ARuby, Gems ([0-9a-z.\-]+)\z").unwrap();
    }

    let mut bl = br.capture_locations();
    let mut rl = rr.capture_locations();
    let mut gl = gr.capture_locations();

    if let Some(_) = br.captures_read(&mut bl, a) {
        return Some(UserAgent {
            bundler: match bl.get(1) {
                Some(loc) => Some(&a[loc.0..loc.1]),
                _ => None,
            },
            rubygems: match bl.get(2) {
                Some(loc) => &a[loc.0..loc.1],
                _ => panic!("parse failed on {:?}", a),
            },
            ruby: match bl.get(3) {
                Some(loc) => Some(&a[loc.0..loc.1]),
                _ => None,
            },
            platform: match bl.get(4) {
                Some(loc) => Some(&a[loc.0..loc.1]),
                _ => None,
            },
            command: match bl.get(5) {
                Some(loc) => Some(&a[loc.0..loc.1]),
                _ => None,
            },
            jruby: match bl.get(6) {
                Some(loc) => Some(&a[loc.0..loc.1]),
                _ => None,
            },
            truffleruby: match bl.get(7) {
                Some(loc) => Some(&a[loc.0..loc.1]),
                _ => None,
            },
            options: match bl.get(8) {
                Some(loc) => Some(&a[loc.0..loc.1]),
                _ => None,
            },
            ci: match bl.get(9) {
                Some(loc) => Some(&a[loc.0..loc.1]),
                _ => None,
            },
            gemstash: match bl.get(11) {
                Some(loc) => Some(&a[loc.0..loc.1]),
                _ => None,
            },
        });
    } else if let Some(_) = rr.captures_read(&mut rl, a) {
        return Some(UserAgent {
            bundler: None,
            rubygems: match rl.get(1) {
                Some(loc) => &a[loc.0..loc.1],
                _ => panic!("parse failed on {:?}", a),
            },
            ruby: match rl.get(3) {
                Some(loc) => Some(&a[loc.0..loc.1]),
                _ => None,
            },
            platform: match rl.get(2) {
                Some(loc) => Some(&a[loc.0..loc.1]),
                _ => None,
            },
            command: None,
            jruby: None,
            truffleruby: None,
            options: None,
            ci: None,
            gemstash: match rl.get(4) {
                Some(loc) => Some(&a[loc.0..loc.1]),
                _ => None,
            },
        });
    } else if let Some(_) = gr.captures_read(&mut gl, a) {
        return Some(UserAgent {
            bundler: None,
            rubygems: match gl.get(1) {
                Some(loc) => &a[loc.0..loc.1],
                _ => panic!("parse failed on {:?}", a),
            },
            ruby: None,
            platform: None,
            command: None,
            jruby: None,
            truffleruby: None,
            options: None,
            ci: None,
            gemstash: None,
        });
    } else {
        return None;
    }
}

fn reformation_parse(inputs: &Vec<&str>){
    for i in inputs{
        let r = UserAgent2::parse(i).unwrap();
    }
}

fn kirby_parse(inputs: &Vec<&str>){
    for i in inputs{
        let r = parse(i).unwrap();
    }
}

fn compare(c: &mut Criterion){
    let inputs = vec![
        "bundler/1.16.1 rubygems/2.6.11 ruby/2.4.1 (x86_64-pc-linux-gnu) command/install options/no_install,mirror.https://rubygems.org/,mirror.https://rubygems.org/.fallback_timeout/,path 59dbf8e99fa09c0a",
    ];
    let re = Fun::new("Reformation", |b, i| b.iter(|| reformation_parse(i)));
    let kr = Fun::new("Kirby", |b, i| b.iter(|| kirby_parse(i)));
    c.bench_functions("reformation parse", vec![re, kr], inputs);
}

criterion_group!(benches, compare);
criterion_main!(benches);