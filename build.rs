extern crate lalrpop;

fn main() {
    println!("cargo:rerun-if-changed=grammar.lalrpop");
    lalrpop::Configuration::new()
        .use_cargo_dir_conventions()
        .process_file("src/syntax/grammar.lalrpop")
        .unwrap();
}
