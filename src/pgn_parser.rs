use std::fmt::Display;

use regex::Regex;

use crate::{chessboard_datatypes::{GameError, Field}};

/// The [`PGNMove`] contains all the data its string representation encodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PGNMove {
    /// The piece which is moved. Is '\0' if it's a castling move
    pub piece: char,
    /// The field to which the piece will move. Is (255, 255) if it's a castling
    /// move.
    pub target: Field,
    /// If a piece of the opponent is captured on the field
    pub captures: bool,
    /// If the king of the opponent is in check by this move
    pub check: bool,
    /// If this moves ends the game with a check mate
    pub mate: bool,
    /// Optional column (maybe row too), if its not clear which piece should
    /// move. (Happens if two pawns could capture on the same field.)
    pub optional_position: Option<char>,
    /// Optional promotion if you have a pawn getting to the end of the field
    pub optional_promotion: Option<char>,
    /// If this move is a queens side castle
    pub is_castle: bool,
    /// If this move is a kings side castle
    pub is_short_castle: bool,
}

impl Display for PGNMove {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_castle {
            let appendix = if self.check { "+" } else if self.mate { "#"} else {""};
            if self.is_short_castle {
                write!(f, "O-O{}", appendix)
            } else {
                write!(f, "O-O-O{}", appendix)
            }
        } else {
            let captures = if self.captures { "x" } else {""};
            let appendix = if self.check { "+" } else if self.mate { "#"} else {""};
            let piece = self.piece.to_string();
            let piece = if self.piece == 'P' {""} else {piece.as_str()};
            let optional_column = self.optional_position.map(|c| c.to_string()).unwrap_or("".to_string());
            let optional_promotion = self.optional_promotion.map(|c| format!("={}", c)).unwrap_or("".to_string());
            write!(f, "{}{}{}{}{}{}", piece, optional_column, captures, self.target, optional_promotion, appendix)
        }
    }
}

impl PGNMove {
    pub fn new(
        piece: char,
        target: Field,
        captures: bool,
        check: bool,
        mate: bool,
        optional_position: Option<char>,
        optional_promotion: Option<char>,
    ) -> Self {
        Self {
            piece,
            target,
            captures,
            check,
            mate,
            optional_position,
            optional_promotion,
            is_castle: false,
            is_short_castle: false,
        }
    }

    pub fn castle(is_short_castle: bool, check: bool, mate: bool) -> Self {
        Self {
            piece: '\0',
            target: Field(255, 255),
            captures: false,
            check,
            mate,
            optional_position: None,
            optional_promotion: None,
            is_castle: true,
            is_short_castle,
        }
    }

    /// Tests if [`src`] starts with a valid pgn move via regex.
    fn is_valid_pgn(src: &str) -> bool {
        // I don't think i can explain it anymore. (or ever really..).
        // You have the two castlings ("(O-O-O|O-O|") or a normal move
        // Then you have a move which starts with a Piece ("(R|N|B|K|Q|") or
        // a column for a pawn (supports lower and upper case letters, even 
        // though there are some problems with Bishops (both beeing 'B')).
        // Then follows an optional column/row for pawn captures followed by an
        // optional x, which indicates the capture. But all of this can be
        // optional in a normal move and you have just a requiered target field.
        // Afterwards you have an optional promotion and optional check situations
        // ".*" indicates that whatever can follow after a valid PGN move.
        let re = Regex::new(
            "(O-O-O|O-O|((R|N|B|K|Q|a|b|c|d|e|f|g|h|A|B|C|D|E|F|G|H)(a|b|c|d|e|f|g|h|A|B|C|D|E|F|G|H|1|2|3|4|5|6|7|8)?(x)?)?(a|b|c|d|e|f|g|h|A|B|C|D|E|F|G|H)(1|2|3|4|5|6|7|8)(=(R|N|B|K|Q))?(#|\\+)?).*"
        ).unwrap();
        let result =re.is_match(src);
        result
    }

    pub fn parse(src: &str) -> Option<(Self, usize)> {
        if !Self::is_valid_pgn(src) {
            return None;
        }
        let mut length = 0;
        Some((
            // Castling moves are easy and just checked with an if.
            if src.starts_with("O-O-O") {
                length += 5;
                let check = src.chars().nth(length).map_or(false, |c| c == '+');
                let mate = src.chars().nth(length).map_or(false, |c| c == '#');
                if check || mate {
                    length += 1
                }
                Self::castle(true, check, mate)
            } else if src.starts_with("O-O") {
                length += 3;
                let check = src.chars().nth(length).map_or(false, |c| c == '+');
                let mate = src.chars().nth(length).map_or(false, |c| c == '#');
                if check || mate {
                    length += 1;
                }
                Self::castle(true, check, mate)
            } else {
                // And here you can see a lot of regex magic.....
                let piece_re =
                    Regex::new("^(R|N|B|K|Q)(a|b|c|d|e|f|g|h|A|C|D|E|F|G|H|1|2|3|4|5|6|7|8)?(x)?(a|b|c|d|e|f|g|h|A|B|C|D|E|F|G|H)(1|2|3|4|5|6|7|8).*")
                        .unwrap();
                let piece = if piece_re
                    .is_match_at(src, length) {
                        length += 1;
                        src.chars().nth(length - 1).unwrap_or('P')
                    } else {
                        'P' // Pawns are not specified in a PGN move, so we set it our selves
                    };
                let column_re = Regex::new("^(R|N|B|K|Q)?(a|b|c|d|e|f|g|h|A|C|D|E|F|G|H|1|2|3|4|5|6|7|8)(x)?(a|b|c|d|e|f|g|h|A|B|C|D|E|F|G|H)(1|2|3|4|5|6|7|8).*").unwrap();
                let optional_column = if column_re.is_match(src) {
                    length += 1;
                    let result = src.chars().nth(length - 1).unwrap().to_ascii_lowercase();
                    Some(result)
                }else {None};
                
                let capture_re = Regex::new("^(R|N|B|K|Q)?(a|b|c|d|e|f|g|h|A|C|D|E|F|G|H|1|2|3|4|5|6|7|8)?x(a|b|c|d|e|f|g|h|A|B|C|D|E|F|G|H)(1|2|3|4|5|6|7|8).*").unwrap();
                let captures = capture_re.is_match(src);
                if captures {length += 1;}

                let target = if let Ok(target) = Field::parse(&src[length..length + 2]) {
                    target
                } else {
                    return None;
                };
                length += 2;
                let promotion_re = Regex::new("^(R|N|B|K|Q)?(a|b|c|d|e|f|g|h|A|C|D|E|F|G|H|1|2|3|4|5|6|7|8)?(x)?(a|b|c|d|e|f|g|h|A|B|C|D|E|F|G|H)(1|2|3|4|5|6|7|8)=(R|N|B|K|Q).*").unwrap();
                let optional_promotion = if promotion_re.is_match(src) {
                    length += 2;
                    src.chars().nth(length - 1)
                } else {None};
                
                let check = src.chars().nth(length).map_or(false, |c| c == '+');
                let mate = src.chars().nth(length).map_or(false, |c| c == '#');
                if check || mate {
                    length += 1
                }
                Self::new(piece, target, captures, check, mate, optional_column, optional_promotion)
            },
            length,
        ))
    }
}

/// Increases [`index`] as long as the index'th char is whitespace.
/// returns none if there is no input left.
fn _eat_whitespace(src: &str, index: usize) -> Option<usize> {
    let mut index = index;
    while src.chars().nth(index)?.is_whitespace() {
        index += 1;
    }
    Some(index)
}

/// Parses a list of pgn moves.
pub fn parse_pgn_moves(src: &str) -> (Vec<PGNMove>, GameError) {
    let mut result = vec![];
    let mut index = 0;
    let mut error = GameError::None;
    while src.chars().nth(index).is_some() {
        index = if let Some(index) = _eat_whitespace(src, index) {
            index
        } else {
            break;
        };
        // pgn moves need turn indices. r"[0-9]+\."
        if !src.chars().nth(index).unwrap_or('\0').is_digit(10) {
            error = GameError::ParseErrorAt(index);
            eprintln!("Expected MoveIndex.");
            break;
        }
        while src.chars().nth(index).unwrap_or('\0').is_digit(10) {
            index += 1;
        }
        if src.chars().nth(index).unwrap_or('\0') == '.' {
            index += 1;
        }

        index = if let Some(index) = _eat_whitespace(src, index) {
            index
        } else {
            break;
        };
        // parse whites move.
        if let Some((mov, len)) = PGNMove::parse(&src[index..]) {
            index += len;
            result.push(mov);
        } else {
            error = GameError::ParseErrorAt(index);
            eprintln!("Expected Move. (white)");
            break;
        }
        index = if let Some(index) = _eat_whitespace(src, index) {
            index
        } else {
            break;
        };
        // parse blacks move.
        if let Some((mov, len)) = PGNMove::parse(&src[index..]) {
            index += len;
            result.push(mov);
        } else {
            error = GameError::ParseErrorAt(index);
            eprintln!("Expected Move. (black)");
            break;
        }
    }

    (result, error)
}

/// In older versions queens side castling became kings side castling. so here
/// is just a test for that.
#[test]
fn test_parses_queen_side_castling() {
    assert!(PGNMove::is_valid_pgn("O-O-O"), "is_valid_pgn failed");
    let mov = PGNMove::parse("O-O-O").expect("could not parse.");
    assert_eq!(mov.1, 5);
}

/// This uses some databases which have been cleaned up to check that the pgn
/// parser works as intended to some degree.
#[test]
fn test_pgn_parser() -> Result<(), Box<dyn std::any::Any + Send>> {
    // There are currently 3198 files in pgn_tests
    let files_to_parse = 120;
    let mut threads= vec![];
    for n in 0..4 {
        threads.push(std::thread::spawn(move || {
            for i in 1..=files_to_parse / 4 {
                let f = files_to_parse / 4 * n + i;
                let (_, error) = parse_pgn_moves(&std::fs::read_to_string(format!("./pgn_tests/{}.pgn", f)).unwrap());
                assert_eq!(error, GameError::None, "Found error {:?} on file 'pgn_tests/{}.pgn'.", error, f);
            }
        }));
    }
    for t in threads {
        t.join()?;
    }
    Ok(())
}