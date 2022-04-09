use lvenc::test_for;
use anyhow::Result;

fn main() -> Result<()> {
    for source_video in std::fs::read_dir("videos")? {
        let path = source_video?.path();
        println!("\n\ntesting video {}", path.display());
        test_for(path, "out")?;
    }
    Ok(())
}
