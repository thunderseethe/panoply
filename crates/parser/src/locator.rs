use std::str::CharIndices;

use base::{file::FileId, loc::Loc};

// A line of source text.
#[derive(Debug, PartialEq, Eq)]
struct Line {
  // The byte in the source text at which this line starts.
  start_byte: usize,
  // The byte indices of each character of the line, measured from the beginning of the source
  // text.
  char_indices: Vec<usize>,
  // The length of the line in bytes. Includes the newline (if any).
  length: usize,
}

impl Line {
  // Consumes and returns a new line from the given `CharIndices` iterator. Also returns whether
  // there are more lines in the source text.
  fn from_char_indices(start: usize, iter: &mut CharIndices<'_>) -> (Line, bool) {
    let mut char_indices = Vec::new();
    let mut length = 0;
    let mut more = false;
    for (i, c) in iter {
      char_indices.push(i);
      length += c.len_utf8();
      if c == '\n' {
        more = true;
        break;
      }
    }
    (
      Line {
        start_byte: start,
        char_indices,
        length,
      },
      more,
    )
  }

  // The column corresponding to the given byte in source text, if the byte occurs in this line.
  //
  // Returns `None` if the byte falls between character boundaries. Allows for the byte to refer
  // to the "one past the end" column.
  fn column(&self, byte: usize) -> Option<usize> {
    if byte == self.start_byte + self.length {
      Some(self.eol())
    } else {
      self.char_indices.binary_search(&byte).ok()
    }
  }

  // The column corresponding to the "one past the end" character.
  fn eol(&self) -> usize {
    self.char_indices.len()
  }

  // The byte index of the given column. Allows for the column to be the "one past the end"
  // column.
  fn uncolumn(&self, col: usize) -> Option<usize> {
    if col == self.char_indices.len() {
      Some(self.start_byte + self.length)
    } else {
      self.char_indices.get(col).copied()
    }
  }
}

/// Converts byte indices in a particular source text to `Loc`s.
#[derive(Debug, PartialEq, Eq)]
pub struct Locator {
  file: FileId,
  // The lines of source text. Always holds at least one value.
  lines: Vec<Line>,
  // The total length of the source text in bytes.
  length: usize,
}

impl Locator {
  /// Returns a new `Locator` for the given source text.
  pub fn new(file: FileId, text: &str) -> Locator {
    let mut ci = text.char_indices();
    let mut lines = Vec::new();
    let mut length = 0;
    while {
      let (line, more) = Line::from_char_indices(length, &mut ci);
      length += line.length;
      lines.push(line);
      more
    } {}
    Locator {
      file,
      lines,
      length,
    }
  }

  /// Converts a byte offset in the original source text to a `Loc`.
  ///
  /// Returns `None` if the byte offset falls within a character or outside the range of the
  /// original source text.
  pub fn locate(&self, byte: usize) -> Option<Loc> {
    Some(
      match self
        .lines
        .binary_search_by_key(&byte, |line| line.start_byte)
      {
        Ok(i) => Loc {
          file: self.file,
          byte,
          line: i,
          col: 0,
        },
        Err(i) => Loc {
          file: self.file,
          byte,
          line: i - 1,
          col: self.lines[i - 1].column(byte)?,
        },
      },
    )
  }

  /// The "one past the end" location in the source text.
  pub fn eoi(&self) -> Loc {
    Loc {
      file: self.file,
      byte: self.length,
      line: self.lines.len() - 1,
      col: self.lines.last().unwrap().eol(),
    }
  }

  /// Converts line and column numbers to a byte offset in the original source text.
  ///
  /// Allows for column numbers which are "one past the end" in their line, which are interpreted
  /// as referring to the beginning of the next line or the end of input.
  #[allow(dead_code)]
  pub fn unlocate(&self, line: usize, col: usize) -> Option<usize> {
    self.lines.get(line)?.uncolumn(col)
  }
}

#[cfg(test)]
mod tests {
  use base::file::FileId;
  use salsa::AsId;

  use super::Locator;

  #[test]
  fn test_locator_unicode() {
    let locator = Locator::new(FileId::from_id(salsa::Id::from_u32(0)), "y̆es");

    let l0 = locator.locate(0).unwrap();
    assert_eq!(l0.byte, 0);
    assert_eq!(l0.line, 0);
    assert_eq!(l0.col, 0);

    let l1 = locator.locate(1).unwrap();
    assert_eq!(l1.byte, 1);
    assert_eq!(l1.line, 0);
    assert_eq!(l1.col, 1);

    let l3 = locator.locate(3).unwrap();
    assert_eq!(l3.byte, 3);
    assert_eq!(l3.line, 0);
    assert_eq!(l3.col, 2);

    let l4 = locator.locate(4).unwrap();
    assert_eq!(l4.byte, 4);
    assert_eq!(l4.line, 0);
    assert_eq!(l4.col, 3);

    let eoi = locator.eoi();
    assert_eq!(eoi.byte, 5);
    assert_eq!(eoi.line, 0);
    assert_eq!(eoi.col, 4);

    assert_eq!(locator.unlocate(0, 0), Some(0));
    assert_eq!(locator.unlocate(0, 1), Some(1));
    assert_eq!(locator.unlocate(0, 2), Some(3));
    assert_eq!(locator.unlocate(0, 3), Some(4));
    assert_eq!(locator.unlocate(0, 4), Some(5)); // EOI

    assert_eq!(locator.unlocate(0, 5), None);
    assert_eq!(locator.unlocate(1, 0), None);
  }

  #[test]
  fn test_multiline() {
    let locator = Locator::new(FileId::from_id(salsa::Id::from_u32(0)), "a\nbc\nd");

    let l2 = locator.locate(2).unwrap();
    assert_eq!(l2.byte, 2);
    assert_eq!(l2.line, 1);
    assert_eq!(l2.col, 0);

    let l3 = locator.locate(3).unwrap();
    assert_eq!(l3.byte, 3);
    assert_eq!(l3.line, 1);
    assert_eq!(l3.col, 1);

    let l4 = locator.locate(4).unwrap();
    assert_eq!(l4.byte, 4);
    assert_eq!(l4.line, 1);
    assert_eq!(l4.col, 2);

    let l5 = locator.locate(5).unwrap();
    assert_eq!(l5.byte, 5);
    assert_eq!(l5.line, 2);
    assert_eq!(l5.col, 0);

    let eoi = locator.eoi();
    assert_eq!(eoi.byte, 6);
    assert_eq!(eoi.line, 2);
    assert_eq!(eoi.col, 1);

    assert_eq!(locator.unlocate(0, 2), Some(2)); // EOL
    assert_eq!(locator.unlocate(1, 0), Some(2));
    assert_eq!(locator.unlocate(1, 1), Some(3));
    assert_eq!(locator.unlocate(1, 2), Some(4));
    assert_eq!(locator.unlocate(1, 3), Some(5)); // EOL
    assert_eq!(locator.unlocate(2, 0), Some(5));
    assert_eq!(locator.unlocate(2, 1), Some(6)); // EOI

    assert_eq!(locator.unlocate(0, 3), None);
    assert_eq!(locator.unlocate(1, 4), None);
    assert_eq!(locator.unlocate(2, 2), None);
  }

  #[test]
  fn test_w3_demo_text() {
    // Sourced from https://www.w3.org/2001/06/utf-8-test/UTF-8-demo.html.
    const W3_DEMO_TEXT: &str = r#"
UTF-8 encoded sample plain-text file
‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

Markus Kuhn [ˈmaʳkʊs kuːn] <mkuhn@acm.org> — 1999-08-20


The ASCII compatible UTF-8 encoding of ISO 10646 and Unicode
plain-text files is defined in RFC 2279 and in ISO 10646-1 Annex R.


Using Unicode/UTF-8, you can write in emails and source code things such as

Mathematics and Sciences:

    ∮ E⋅da = Q,  n → ∞, ∑ f(i) = ∏ g(i), ∀x∈ℝ: ⌈x⌉ = −⌊−x⌋, α ∧ ¬β = ¬(¬α ∨ β),

    ℕ ⊆ ℕ₀ ⊂ ℤ ⊂ ℚ ⊂ ℝ ⊂ ℂ, ⊥ < a ≠ b ≡ c ≤ d ≪ ⊤ ⇒ (A ⇔ B),

    2H₂ + O₂ ⇌ 2H₂O, R = 4.7 kΩ, ⌀ 200 mm

Linguistics and dictionaries:

    ði ıntəˈnæʃənəl fəˈnɛtık əsoʊsiˈeıʃn
    Y [ˈʏpsilɔn], Yen [jɛn], Yoga [ˈjoːgɑ]

APL:

    ((V⍳V)=⍳⍴V)/V←,V    ⌷←⍳→⍴∆∇⊃‾⍎⍕⌈

Nicer typography in plain text files:

    ╔══════════════════════════════════════════╗
    ║                                          ║
    ║   • ‘single’ and “double” quotes         ║
    ║                                          ║
    ║   • Curly apostrophes: “We’ve been here” ║
    ║                                          ║
    ║   • Latin-1 apostrophe and accents: '´`  ║
    ║                                          ║
    ║   • ‚deutsche‘ „Anführungszeichen“       ║
    ║                                          ║
    ║   • †, ‡, ‰, •, 3–4, —, −5/+5, ™, …      ║
    ║                                          ║
    ║   • ASCII safety test: 1lI|, 0OD, 8B     ║
    ║                      ╭─────────╮         ║
    ║   • the euro symbol: │ 14.95 € │         ║
    ║                      ╰─────────╯         ║
    ╚══════════════════════════════════════════╝

Greek (in Polytonic):

    The Greek anthem:

    Σὲ γνωρίζω ἀπὸ τὴν κόψη
    τοῦ σπαθιοῦ τὴν τρομερή,
    σὲ γνωρίζω ἀπὸ τὴν ὄψη
    ποὺ μὲ βία μετράει τὴ γῆ.

    ᾿Απ᾿ τὰ κόκκαλα βγαλμένη
    τῶν ῾Ελλήνων τὰ ἱερά
    καὶ σὰν πρῶτα ἀνδρειωμένη
    χαῖρε, ὦ χαῖρε, ᾿Ελευθεριά!

    From a speech of Demosthenes in the 4th century BC:

    Οὐχὶ ταὐτὰ παρίσταταί μοι γιγνώσκειν, ὦ ἄνδρες ᾿Αθηναῖοι,
    ὅταν τ᾿ εἰς τὰ πράγματα ἀποβλέψω καὶ ὅταν πρὸς τοὺς
    λόγους οὓς ἀκούω· τοὺς μὲν γὰρ λόγους περὶ τοῦ
    τιμωρήσασθαι Φίλιππον ὁρῶ γιγνομένους, τὰ δὲ πράγματ᾿ 
    εἰς τοῦτο προήκοντα,  ὥσθ᾿ ὅπως μὴ πεισόμεθ᾿ αὐτοὶ
    πρότερον κακῶς σκέψασθαι δέον. οὐδέν οὖν ἄλλο μοι δοκοῦσιν
    οἱ τὰ τοιαῦτα λέγοντες ἢ τὴν ὑπόθεσιν, περὶ ἧς βουλεύεσθαι,
    οὐχὶ τὴν οὖσαν παριστάντες ὑμῖν ἁμαρτάνειν. ἐγὼ δέ, ὅτι μέν
    ποτ᾿ ἐξῆν τῇ πόλει καὶ τὰ αὑτῆς ἔχειν ἀσφαλῶς καὶ Φίλιππον
    τιμωρήσασθαι, καὶ μάλ᾿ ἀκριβῶς οἶδα· ἐπ᾿ ἐμοῦ γάρ, οὐ πάλαι
    γέγονεν ταῦτ᾿ ἀμφότερα· νῦν μέντοι πέπεισμαι τοῦθ᾿ ἱκανὸν
    προλαβεῖν ἡμῖν εἶναι τὴν πρώτην, ὅπως τοὺς συμμάχους
    σώσομεν. ἐὰν γὰρ τοῦτο βεβαίως ὑπάρξῃ, τότε καὶ περὶ τοῦ
    τίνα τιμωρήσεταί τις καὶ ὃν τρόπον ἐξέσται σκοπεῖν· πρὶν δὲ
    τὴν ἀρχὴν ὀρθῶς ὑποθέσθαι, μάταιον ἡγοῦμαι περὶ τῆς
    τελευτῆς ὁντινοῦν ποιεῖσθαι λόγον.

    Δημοσθένους, Γ´ ᾿Ολυνθιακὸς

Georgian:

    From a Unicode conference invitation:

    გთხოვთ ახლავე გაიაროთ რეგისტრაცია Unicode-ის მეათე საერთაშორისო
    კონფერენციაზე დასასწრებად, რომელიც გაიმართება 10-12 მარტს,
    ქ. მაინცში, გერმანიაში. კონფერენცია შეჰკრებს ერთად მსოფლიოს
    ექსპერტებს ისეთ დარგებში როგორიცაა ინტერნეტი და Unicode-ი,
    ინტერნაციონალიზაცია და ლოკალიზაცია, Unicode-ის გამოყენება
    ოპერაციულ სისტემებსა, და გამოყენებით პროგრამებში, შრიფტებში,
    ტექსტების დამუშავებასა და მრავალენოვან კომპიუტერულ სისტემებში.

Russian:

    From a Unicode conference invitation:

    Зарегистрируйтесь сейчас на Десятую Международную Конференцию по
    Unicode, которая состоится 10-12 марта 1997 года в Майнце в Германии.
    Конференция соберет широкий круг экспертов по  вопросам глобального
    Интернета и Unicode, локализации и интернационализации, воплощению и
    применению Unicode в различных операционных системах и программных
    приложениях, шрифтах, верстке и многоязычных компьютерных системах.

Thai (UCS Level 2):

    Excerpt from a poetry on The Romance of The Three Kingdoms (a Chinese
    classic 'San Gua'):

    [----------------------------|------------------------]
    ๏ แผ่นดินฮั่นเสื่อมโทรมแสนสังเวช  พระปกเกศกองบู๊กู้ขึ้นใหม่
    สิบสองกษัตริย์ก่อนหน้าแลถัดไป       สององค์ไซร้โง่เขลาเบาปัญญา
    ทรงนับถือขันทีเป็นที่พึ่ง           บ้านเมืองจึงวิปริตเป็นนักหนา
    โฮจิ๋นเรียกทัพทั่วหัวเมืองมา         หมายจะฆ่ามดชั่วตัวสำคัญ
    เหมือนขับไสไล่เสือจากเคหา      รับหมาป่าเข้ามาเลยอาสัญ
    ฝ่ายอ้องอุ้นยุแยกให้แตกกัน          ใช้สาวนั้นเป็นชนวนชื่นชวนใจ
    พลันลิฉุยกุยกีกลับก่อเหตุ          ช่างอาเพศจริงหนาฟ้าร้องไห้
    ต้องรบราฆ่าฟันจนบรรลัย           ฤๅหาใครค้ำชูกู้บรรลังก์ ฯ

    (The above is a two-column text. If combining characters are handled
    correctly, the lines of the second column should be aligned with the
    | character above.)

Ethiopian:

    Proverbs in the Amharic language:

    ሰማይ አይታረስ ንጉሥ አይከሰስ።
    ብላ ካለኝ እንደአባቴ በቆመጠኝ።
    ጌጥ ያለቤቱ ቁምጥና ነው።
    ደሀ በሕልሙ ቅቤ ባይጠጣ ንጣት በገደለው።
    የአፍ ወለምታ በቅቤ አይታሽም።
    አይጥ በበላ ዳዋ ተመታ።
    ሲተረጉሙ ይደረግሙ።
    ቀስ በቀስ፥ ዕንቁላል በእግሩ ይሄዳል።
    ድር ቢያብር አንበሳ ያስር።
    ሰው እንደቤቱ እንጅ እንደ ጉረቤቱ አይተዳደርም።
    እግዜር የከፈተውን ጉሮሮ ሳይዘጋው አይድርም።
    የጎረቤት ሌባ፥ ቢያዩት ይስቅ ባያዩት ያጠልቅ።
    ሥራ ከመፍታት ልጄን ላፋታት።
    ዓባይ ማደሪያ የለው፥ ግንድ ይዞ ይዞራል።
    የእስላም አገሩ መካ የአሞራ አገሩ ዋርካ።
    ተንጋሎ ቢተፉ ተመልሶ ባፉ።
    ወዳጅህ ማር ቢሆን ጨርስህ አትላሰው።
    እግርህን በፍራሽህ ልክ ዘርጋ።

Runes:

    ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ

    (Old English, which transcribed into Latin reads 'He cwaeth that he
    bude thaem lande northweardum with tha Westsae.' and means 'He said
    that he lived in the northern land near the Western Sea.')

Braille:

    ⡌⠁⠧⠑ ⠼⠁⠒  ⡍⠜⠇⠑⠹⠰⠎ ⡣⠕⠌

    ⡍⠜⠇⠑⠹ ⠺⠁⠎ ⠙⠑⠁⠙⠒ ⠞⠕ ⠃⠑⠛⠔ ⠺⠊⠹⠲ ⡹⠻⠑ ⠊⠎ ⠝⠕ ⠙⠳⠃⠞
    ⠱⠁⠞⠑⠧⠻ ⠁⠃⠳⠞ ⠹⠁⠞⠲ ⡹⠑ ⠗⠑⠛⠊⠌⠻ ⠕⠋ ⠙⠊⠎ ⠃⠥⠗⠊⠁⠇ ⠺⠁⠎
    ⠎⠊⠛⠝⠫ ⠃⠹ ⠹⠑ ⠊⠇⠻⠛⠹⠍⠁⠝⠂ ⠹⠑ ⠊⠇⠻⠅⠂ ⠹⠑ ⠥⠝⠙⠻⠞⠁⠅⠻⠂
    ⠁⠝⠙ ⠹⠑ ⠡⠊⠑⠋ ⠍⠳⠗⠝⠻⠲ ⡎⠊⠗⠕⠕⠛⠑ ⠎⠊⠛⠝⠫ ⠊⠞⠲ ⡁⠝⠙
    ⡎⠊⠗⠕⠕⠛⠑⠰⠎ ⠝⠁⠍⠑ ⠺⠁⠎ ⠛⠕⠕⠙ ⠥⠏⠕⠝ ⠰⡡⠁⠝⠛⠑⠂ ⠋⠕⠗ ⠁⠝⠹⠹⠔⠛ ⠙⠑ 
    ⠡⠕⠎⠑ ⠞⠕ ⠏⠥⠞ ⠙⠊⠎ ⠙⠁⠝⠙ ⠞⠕⠲

    ⡕⠇⠙ ⡍⠜⠇⠑⠹ ⠺⠁⠎ ⠁⠎ ⠙⠑⠁⠙ ⠁⠎ ⠁ ⠙⠕⠕⠗⠤⠝⠁⠊⠇⠲

    ⡍⠔⠙⠖ ⡊ ⠙⠕⠝⠰⠞ ⠍⠑⠁⠝ ⠞⠕ ⠎⠁⠹ ⠹⠁⠞ ⡊ ⠅⠝⠪⠂ ⠕⠋ ⠍⠹
    ⠪⠝ ⠅⠝⠪⠇⠫⠛⠑⠂ ⠱⠁⠞ ⠹⠻⠑ ⠊⠎ ⠏⠜⠞⠊⠊⠥⠇⠜⠇⠹ ⠙⠑⠁⠙ ⠁⠃⠳⠞
    ⠁ ⠙⠕⠕⠗⠤⠝⠁⠊⠇⠲ ⡊ ⠍⠊⠣⠞ ⠙⠁⠧⠑ ⠃⠑⠲ ⠔⠊⠇⠔⠫⠂ ⠍⠹⠎⠑⠇⠋⠂ ⠞⠕
    ⠗⠑⠛⠜⠙ ⠁ ⠊⠕⠋⠋⠔⠤⠝⠁⠊⠇ ⠁⠎ ⠹⠑ ⠙⠑⠁⠙⠑⠌ ⠏⠊⠑⠊⠑ ⠕⠋ ⠊⠗⠕⠝⠍⠕⠝⠛⠻⠹ 
    ⠔ ⠹⠑ ⠞⠗⠁⠙⠑⠲ ⡃⠥⠞ ⠹⠑ ⠺⠊⠎⠙⠕⠍ ⠕⠋ ⠳⠗ ⠁⠝⠊⠑⠌⠕⠗⠎ 
    ⠊⠎ ⠔ ⠹⠑ ⠎⠊⠍⠊⠇⠑⠆ ⠁⠝⠙ ⠍⠹ ⠥⠝⠙⠁⠇⠇⠪⠫ ⠙⠁⠝⠙⠎
    ⠩⠁⠇⠇ ⠝⠕⠞ ⠙⠊⠌⠥⠗⠃ ⠊⠞⠂ ⠕⠗ ⠹⠑ ⡊⠳⠝⠞⠗⠹⠰⠎ ⠙⠕⠝⠑ ⠋⠕⠗⠲ ⡹⠳
    ⠺⠊⠇⠇ ⠹⠻⠑⠋⠕⠗⠑ ⠏⠻⠍⠊⠞ ⠍⠑ ⠞⠕ ⠗⠑⠏⠑⠁⠞⠂ ⠑⠍⠏⠙⠁⠞⠊⠊⠁⠇⠇⠹⠂ ⠹⠁⠞
    ⡍⠜⠇⠑⠹ ⠺⠁⠎ ⠁⠎ ⠙⠑⠁⠙ ⠁⠎ ⠁ ⠙⠕⠕⠗⠤⠝⠁⠊⠇⠲

    (The first couple of paragraphs of "A Christmas Carol" by Dickens)

Compact font selection example text:

    ABCDEFGHIJKLMNOPQRSTUVWXYZ /0123456789
    abcdefghijklmnopqrstuvwxyz £©µÀÆÖÞßéöÿ
    –—‘“”„†•…‰™œŠŸž€ ΑΒΓΔΩαβγδω АБВГДабвгд
    ∀∂∈ℝ∧∪≡∞ ↑↗↨↻⇣ ┐┼╔╘░►☺♀ ﬁ�⑀₂ἠḂӥẄɐː⍎אԱა

Greetings in various languages:

    Hello world, Καλημέρα κόσμε, コンニチハ

Box drawing alignment tests:                                          █
                                                                        ▉
    ╔══╦══╗  ┌──┬──┐  ╭──┬──╮  ╭──┬──╮  ┏━━┳━━┓  ┎┒┏┑   ╷  ╻ ┏┯┓ ┌┰┐    ▊ ╱╲╱╲╳╳╳
    ║┌─╨─┐║  │╔═╧═╗│  │╒═╪═╕│  │╓─╁─╖│  ┃┌─╂─┐┃  ┗╃╄┙  ╶┼╴╺╋╸┠┼┨ ┝╋┥    ▋ ╲╱╲╱╳╳╳
    ║│╲ ╱│║  │║   ║│  ││ │ ││  │║ ┃ ║│  ┃│ ╿ │┃  ┍╅╆┓   ╵  ╹ ┗┷┛ └┸┘    ▌ ╱╲╱╲╳╳╳
    ╠╡ ╳ ╞╣  ├╢   ╟┤  ├┼─┼─┼┤  ├╫─╂─╫┤  ┣┿╾┼╼┿┫  ┕┛┖┚     ┌┄┄┐ ╎ ┏┅┅┓ ┋ ▍ ╲╱╲╱╳╳╳
    ║│╱ ╲│║  │║   ║│  ││ │ ││  │║ ┃ ║│  ┃│ ╽ │┃  ░░▒▒▓▓██ ┊  ┆ ╎ ╏  ┇ ┋ ▎
    ║└─╥─┘║  │╚═╤═╝│  │╘═╪═╛│  │╙─╀─╜│  ┃└─╂─┘┃  ░░▒▒▓▓██ ┊  ┆ ╎ ╏  ┇ ┋ ▏
    ╚══╩══╝  └──┴──┘  ╰──┴──╯  ╰──┴──╯  ┗━━┻━━┛           └╌╌┘ ╎ ┗╍╍┛ ┋  ▁▂▃▄▅▆▇█

"#;

    let locator = Locator::new(FileId::from_id(salsa::Id::from_u32(0)), W3_DEMO_TEXT);
    assert_eq!(locator.eoi().byte, W3_DEMO_TEXT.len());
    for (i, _) in W3_DEMO_TEXT.char_indices() {
      let loc = locator
        .locate(i)
        .unwrap_or_else(|| panic!("Expected location at byte {}", i));
      let j = locator.unlocate(loc.line, loc.col).unwrap_or_else(|| {
        panic!(
          "Expected roundtrip for byte {} (line {}, col {})",
          i, loc.line, loc.col
        )
      });
      assert_eq!(i, j);
    }
  }
}
