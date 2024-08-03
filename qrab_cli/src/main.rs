use std::fs::File;
use std::io::{BufWriter, Read};
use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, Subcommand};

use qrab_encode::{AsciiRenderer, Encoder};

#[derive(Parser)]
#[command(name = "qrab_cli")]
#[command(version)]
#[command(propagate_version = true)]
#[command(about = "Generate or decode QR codes from the command line", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    #[command(flatten_help = true)]
    Encode {
        #[arg(help = "Input file or stdin if unspecified")]
        file: Option<PathBuf>,
        #[arg(
            short,
            long,
            help = "Output file or stdout if unspecified",
            long_help = "Output file or stdout if unspecified. The output format is determined based on the extension. \
            Supported extensions are:\n\
            * text: .txt\n\
            * images: .png"
        )]
        output: Option<PathBuf>,
    },
    Decode {
        file: PathBuf,
    },
}

enum Output {
    Stdout,
    Text(PathBuf),
    Png(PathBuf),
}

fn determine_output_kind(path: Option<PathBuf>) -> Result<Output> {
    let Some(path) = path else {
        return Ok(Output::Stdout);
    };
    match path.extension().and_then(|ext| ext.to_str()) {
        Some("txt") => Ok(Output::Text(path)),
        Some("png") => Ok(Output::Png(path)),
        Some(invalid) => Err(anyhow::Error::msg(format!(
            "invalid output extension '{}'",
            invalid
        ))),
        None => Err(anyhow::Error::msg("requested output has no extension")),
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Command::Encode { file, output } => {
            // Before doing anything, make sure the requested output is valid.
            let output = determine_output_kind(output)?;
            // Read the input bytes.
            let mut bytes = Vec::new();
            match file {
                Some(path) => {
                    // Read file at `path`.
                    std::fs::File::open(path)?.read_to_end(&mut bytes)?;
                }
                None => {
                    // Read from stdin.
                    std::io::stdin().read_to_end(&mut bytes)?;
                }
            };
            let qrcode = Encoder::new().encode(bytes)?;
            match output {
                Output::Text(path) => {
                    let f = File::create(path)?;
                    let mut writer = BufWriter::new(f);
                    let renderer = AsciiRenderer::new();
                    renderer.render(&mut writer, qrcode)?;
                }
                _ => todo!("rendering"),
            }
        }
        Command::Decode { .. } => {
            todo!("decoding")
        }
    }
    Ok(())
}
