fn main() {
    println!("cargo:rerun-if-changed=tests/lua/snapshots");
    println!("cargo:rerun-if-changed=tests/markdown/snapshots");
}
