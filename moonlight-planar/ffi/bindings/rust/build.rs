use std::env;

fn main() {
    println!("cargo:rerun-if-env-changed=MOONLIGHT_TRIANGULATION_LIB_DIR");
    if let Some(directory) = env::var_os("MOONLIGHT_TRIANGULATION_LIB_DIR") {
        println!(
            "cargo:rustc-link-search=native={}",
            directory.to_string_lossy()
        );
    }
}
