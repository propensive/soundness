use std::env;

fn completions(line: &str, cursor: usize) -> Vec<(String, String)> {
    let commands = vec![
        ("build", "Compile the project"),
        ("clean", "Remove build artifacts"),
        ("test", "Run the test suite"),
        ("run", "Run the application"),
        ("help", "Show usage information"),
    ];
    let effective = &line[..line.len().min(cursor)];
    let last_word = if cursor > line.len() || effective.ends_with(' ') {
        ""
    } else {
        effective.split_whitespace().last().unwrap_or("")
    };
    commands
        .into_iter()
        .filter(|(c, _)| c.starts_with(last_word))
        .map(|(c, d)| (String::from(c), String::from(d)))
        .collect()
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() >= 3 && args[1] == "--completions" {
        let line = &args[2];
        let cursor: usize = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(line.len());
        for (name, desc) in completions(line, cursor) {
            println!("{}\t{}", name, desc);
        }
        return;
    }

    println!("Arguments:");
    for (i, arg) in args.iter().enumerate() {
        println!("  [{}] {}", i, arg);
    }

    println!();
    println!("Environment:");
    for (key, value) in env::vars() {
        println!("  {}={}", key, value);
    }
}
