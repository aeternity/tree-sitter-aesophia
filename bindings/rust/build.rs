fn track_file(c_config: &mut cc::Build, path: &std::path::PathBuf) {
    c_config.file(path);
    println!("cargo:rerun-if-changed={}", path.to_str().unwrap());
}

fn main() {
    let src_dir = std::path::Path::new("src");

    let mut c_config = cc::Build::new();
    c_config.include(src_dir);
    c_config
        .flag_if_supported("-Weverything")
        .flag_if_supported("-Wno-padded")
        .flag_if_supported("-Wno-declaration-after-statement")
        .flag_if_supported("-Wno-old-style-declaration")
        .flag_if_supported("-Werror");
    #[cfg(target_env = "msvc")]
    c_config.flag("-utf-8");

    println!("cargo:rerun-if-changed={}", src_dir.join("preamble.h").to_str().unwrap());
    println!("cargo:rerun-if-changed={}", src_dir.join("state.h").to_str().unwrap());
    println!("cargo:rerun-if-changed={}", src_dir.join("indent_vec.h").to_str().unwrap());
    println!("cargo:rerun-if-changed={}", src_dir.join("scanner.h").to_str().unwrap());

    track_file(&mut c_config, &src_dir.join("scanner.c"));

    let parser_path = src_dir.join("parser.c");

    track_file(&mut c_config, &parser_path);
    c_config.compile("parser");

}
