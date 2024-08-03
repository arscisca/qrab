use std::io::Write;

use qrab_core::{Canvas, Module};

pub struct AsciiRenderer {
    light_pattern: Box<str>,
    dark_pattern: Box<str>,
}

impl AsciiRenderer {
    /// Construct a new [AsciiRenderer] that uses "██" to render dark modules and "  " to print light ones.
    pub fn new() -> Self {
        Self {
            light_pattern: "  ".into(),
            dark_pattern: "██".into(),
        }
    }

    /// Set the light module `pattern` to be used when rendering.
    pub fn with_light_pattern(mut self, pattern: &str) -> Self {
        self.light_pattern = pattern.into();
        self
    }

    /// Set the dark module `pattern` to be used when rendering.
    pub fn with_dark_pattern(mut self, pattern: &str) -> Self {
        self.dark_pattern = pattern.into();
        self
    }

    /// Invert the light and dark patterns.
    pub fn inverted(mut self) -> Self {
        std::mem::swap(&mut self.light_pattern, &mut self.dark_pattern);
        self
    }

    /// Peek at the pattern used to render light modules.
    pub fn light_pattern(&self) -> &str {
        self.light_pattern.as_ref()
    }

    /// Peek at the pattern used to render dark modules.
    pub fn dark_pattern(&self) -> &str {
        self.dark_pattern.as_ref()
    }

    /// Render the `canvas` into `output`.
    pub fn render<C, W>(&self, output: &mut W, canvas: C) -> std::io::Result<()>
    where
        C: AsRef<Canvas>,
        W: Write,
    {
        let canvas = canvas.as_ref();
        for i in 0..canvas.size() {
            for j in 0..canvas.size() {
                let module = canvas.get(i, j).unwrap();
                let pattern = match module {
                    Module::Dark => self.dark_pattern(),
                    Module::Light => self.light_pattern(),
                };
                write!(output, "{}", pattern)?;
            }
            writeln!(output)?;
        }
        Ok(())
    }
}

impl Default for AsciiRenderer {
    fn default() -> Self {
        Self::new()
    }
}
