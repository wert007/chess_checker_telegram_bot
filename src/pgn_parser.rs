use std::fmt::Display;

use regex::Regex;

use crate::{chessboard_datatypes::{GameError, Field}};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PGNMove {
    pub piece: char,
    pub target: Field,
    pub captures: bool,
    pub check: bool,
    pub mate: bool,
    pub optional_position: Option<char>,
    pub optional_promotion: Option<char>,
    pub is_castle: bool,
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

    fn is_valid_pgn(src: &str) -> bool {
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
                let piece_re =
                    Regex::new("^(R|N|B|K|Q)(a|b|c|d|e|f|g|h|A|C|D|E|F|G|H|1|2|3|4|5|6|7|8)?(x)?(a|b|c|d|e|f|g|h|A|B|C|D|E|F|G|H)(1|2|3|4|5|6|7|8).*")
                        .unwrap();
                let piece = if piece_re
                    .is_match_at(src, length) {
                        length += 1;
                        src.chars().nth(length - 1).unwrap_or('P')
                    } else {
                        'P'
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


fn _eat_whitespace(src: &str, index: usize) -> Option<usize> {
    let mut index = index;
    while src.chars().nth(index)?.is_whitespace() {
        index += 1;
    }
    Some(index)
}

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

#[test]
fn test_parses_queen_side_castling() {
    assert!(PGNMove::is_valid_pgn("O-O-O"), "is_valid_pgn failed");
    let mov = PGNMove::parse("O-O-O").expect("could not parse.");
    assert_eq!(mov.1, 5);
}

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