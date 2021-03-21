use std::{
    cmp::{max, min},
    fmt::Display,
};

pub use Color::*;
pub use PieceKind::*;

use crate::{chessboard::ChessBoard, pgn_parser::PGNMove};

/// Things, that can be displayed on a Checkerboard. Like ChessBoard setups or
/// lines to see if a piece can capture another piece.
pub trait CheckerBoardDisplay {
    fn get_at(&self, x: u8, y: u8) -> char;
}

macro_rules! checker_display {
    ($t:ty) => {
        impl std::fmt::Display for $t {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "    a   b   c   d   e   f   g   h  \n")?;
                for y in (0..8).rev() {
                    write!(f, "  +---+---+---+---+---+---+---+---+\n{} ", y + 1)?;
                    for x in 0..8 {
                        write!(f, "| {} ", self.get_at(x, y))?
                    }
                    write!(f, "|\n")?;
                }
                write!(f, "  +---+---+---+---+---+---+---+---+\n")
            }
        }
    };
}

checker_display!(ChessBoard);
checker_display!(FieldLine);

impl CheckerBoardDisplay for FieldLine {
    fn get_at(&self, x: u8, y: u8) -> char {
        let field = Field(x, y);
        if self.0.iter().any(|f| f == &field) {
            'X'
        } else if self.1 == field {
            'A'
        } else if self.2 == field {
            'B'
        } else {
            ' '
        }
    }
}

impl Display for dyn CheckerBoardDisplay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in (0..8).rev() {
            write!(f, "+---+---+---+---+---+---+---+---+\n")?;
            for x in 0..8 {
                write!(f, "| {} ", self.get_at(x, y))?
            }
            write!(f, "|\n")?;
        }
        write!(f, "+---+---+---+---+---+---+---+---+\n")
    }
}

/// The field on a chessboard. Defined by its column and row. Each go from 0..7
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Field(pub u8, pub u8);

/// A line of fields between two points.
pub struct FieldLine(pub Vec<Field>, Field, Field);

impl Field {
    /// Parses everything between a1 to h8
    pub fn parse(text: &str) -> Result<Field, ()> {
        if text.len() != 2 {
            return Err(());
        }
        let column = text.chars().next().unwrap();
        let x = column as i16 - 'a' as i16;
        if x >= 8 || x < 0 {
            return Err(());
        }
        let x = x as u8;
        let row = text.chars().last().unwrap();
        let y = if let Some(y) = row.to_digit(10) {
            y as u8 - 1
        } else {
            return Err(());
        };

        Ok(Field(x, y))
    }

    /// Creates a line between from and to.. Lines can only be on multiple of 45°
    pub fn create_line(from: Field, to: Field) -> FieldLine {
        let mut result = FieldLine(vec![], from, to);
        let x_dif = to.0 as i8 - from.0 as i8;
        let y_dif = to.1 as i8 - from.1 as i8;
        // No line
        if x_dif == 0 && y_dif == 0 {
            return result;
            // Parallel to the y-Axis
        } else if x_dif == 0 {
            for y in 1 + min(from.1, to.1)..max(from.1, to.1) {
                result.0.push(Field(from.0, y));
            }
        // Parallel to the x-Axis
        } else if y_dif == 0 {
            for x in 1 + min(from.0, to.0)..max(from.0, to.0) {
                result.0.push(Field(x, from.1));
            }
        // Diagonal line.
        } else {
            assert_eq!(x_dif.abs(), y_dif.abs(), "{} -> {}", from, to);
            let length = x_dif.abs();
            let x_sign = length / x_dif;
            let y_sign = length / y_dif;
            for i in 1..length {
                let x = i * x_sign + from.0 as i8;
                let y = i * y_sign + from.1 as i8;
                result.0.push(Field(x as u8, y as u8));
            }
        }

        result
    }

    /// Euclidean distance squared to another field..
    pub fn distance_to(&self, other: Field) -> u8 {
        let x_dif = self.0 as i8 - other.0 as i8;
        let y_dif = self.1 as i8 - other.1 as i8;
        (x_dif * x_dif + y_dif * y_dif) as u8
    }

    /// Display name of the column
    pub fn column(&self) -> char {
        (self.0 + 'a' as u8) as char
    }

    /// Display number of the row
    pub fn row(&self) -> u8 {
        self.1 + 1
    }

    /// Color of the field.
    pub fn color(&self) -> Color {
        if self.0 + self.1 % 2 == 0 {
            Black
        } else {
            White
        }
    }

    /// Checks if you can go x and y steps from the current field without
    /// leaving the board.
    pub fn is_valid_offset(&self, x: i8, y: i8) -> bool {
        let x = self.0 as i8 + x;
        let y = self.1 as i8 + y;
        x >= 0 && x < 8 && y >= 0 && y < 8
    }

    /// Returns a Field which is x and y steps away from the current field.
    pub fn move_field(&self, x: i8, y: i8) -> Field {
        let x = self.0 as i8 + x;
        let y = self.1 as i8 + y;
        if x < 0 || x >= 8 || y < 0 || y >= 8 {
            panic!("({}, {}) is no valid Field!", x, y);
        }
        Field(x as u8, y as u8)
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.column(), self.row())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Color {
    White,
    Black,
}

impl Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            White => write!(f, "white"),
            Black => write!(f, "black"),
        }
    }
}

impl Color {
    pub fn invert(&self) -> Color {
        match self {
            White => Black,
            Black => White,
        }
    }

    pub fn _to_fen(&self) -> char {
        match self {
            White => 'w',
            Black => 'b',
        }
    }

    pub fn from_fen(c: char) -> Result<Self, char> {
        match c {
            'w' => Ok(White),
            'b' => Ok(Black),
            err => Err(err),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PieceKind {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

impl PieceKind {
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            'P' => Some(Pawn),
            'R' => Some(Rook),
            'N' => Some(Knight),
            'B' => Some(Bishop),
            'Q' => Some(Queen),
            'K' => Some(King),
            _ => None,
        }
    }
}

impl Display for PieceKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Pawn => "",
                Rook => "R",
                Knight => "N",
                Bishop => "B",
                Queen => "Q",
                King => "K",
            }
        )
    }
}

/// The [`Piece`] contains all the information an actual piece on an actual
/// chessboard would contain as well
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Piece {
    /// The kind only differentiates the kind (Pawn, Queen, Bishop, etc) and not
    /// color
    pub kind: PieceKind,
    /// The number of the piece in a line of pieces with the same kind and color
    pub index: usize,
    /// The current position where the piece is standing
    pub position: Field,
    /// The color of the piece
    pub color: Color,
    /// The amount of moves the piece has made. (This is not resetted by a
    /// promotion)
    pub count_moves: usize,
}

impl Piece {
    pub fn new(kind: PieceKind, position: Field, color: Color, index: usize) -> Self {
        Self {
            kind,
            position,
            index,
            color,
            count_moves: 0,
        }
    }

    /// Checks if the most basic requirements of castling are met. (Is the right
    /// kind and has not moved yet)
    pub fn can_castle(&self) -> bool {
        match self.kind {
            King | Rook => self.count_moves == 0,
            _ => false,
        }
    }

    /// Moves the piece to position and promotes it if a promotion is pressent.
    pub fn move_to(&mut self, position: Field, promotion: Option<PieceKind>) {
        self.count_moves += 1;
        self.position = position;
        if let Some(promotion) = promotion {
            assert_eq!(self.kind, Pawn);
            assert_ne!(promotion, Pawn);
            assert_ne!(promotion, King);
            self.kind = promotion;
        }
    }

    /// Undoes a move with the given arguments.
    pub fn undo_move_back_to(&mut self, position: Field, promotion: bool) {
        self.count_moves -= 1;
        self.position = position;
        if promotion {
            self.kind = Pawn;
        }
    }

    /// Get all moves the piece can do, if it would be alone on the board
    /// (Or in case of the pawn depending if it can capture or not)
    pub fn get_all_moves(&self, captures: bool) -> Vec<Field> {
        match self.kind {
            Pawn => self.get_all_pawn_moves(captures),
            Rook => self.get_all_rook_moves(),
            Knight => self.get_all_knight_moves(),
            Bishop => self.get_all_bishop_moves(),
            Queen => self.get_all_queen_moves(),
            King => self.get_all_king_moves(),
        }
    }

    fn get_all_pawn_moves(&self, captures: bool) -> Vec<Field> {
        let mut result = vec![];
        let y_off = match self.color {
            White => 1,
            Black => -1,
        };
        // The pawn is standing at the border and is not promoted. Something
        // went wrong.
        if !self.position.is_valid_offset(0, y_off) {
            return result;
        }
        let forward = self.position.move_field(0, y_off);
        // If the pawn doesn't capture it can move forward.
        if !captures {
            result.push(forward);
        } else {
            // If it does, it can captures on the fields left and right from the
            // right before it.
            if forward.is_valid_offset(1, 0) {
                result.push(forward.move_field(1, 0));
            }
            if forward.is_valid_offset(-1, 0) {
                result.push(forward.move_field(-1, 0));
            }
        }
        // On its first move a pawn can also move two fields forward.
        if self.count_moves == 0 && !captures {
            result.push(forward.move_field(0, y_off));
        }
        result
    }

    fn get_all_rook_moves(&self) -> Vec<Field> {
        let mut result = vec![];
        // Everything with the same y position
        for x in 0..8 {
            if x == self.position.0 {
                continue;
            }
            result.push(Field(x, self.position.1));
        }
        // or the same x position
        for y in 0..8 {
            if y == self.position.1 {
                continue;
            }
            result.push(Field(self.position.0, y));
        }
        result
    }

    fn get_all_knight_moves(&self) -> Vec<Field> {
        let mut result = vec![];
        // just two for loops for the each two possible L shapes.
        for x_off in &[-2, 2] {
            for y_off in &[-1, 1] {
                if self.position.is_valid_offset(*x_off, *y_off) {
                    result.push(self.position.move_field(*x_off, *y_off))
                }
            }
        }
        for x_off in &[-1, 1] {
            for y_off in &[-2, 2] {
                if self.position.is_valid_offset(*x_off, *y_off) {
                    result.push(self.position.move_field(*x_off, *y_off))
                }
            }
        }
        result
    }

    fn get_all_bishop_moves(&self) -> Vec<Field> {
        let mut result = vec![];
        for i in -7..8 {
            if i == 0 {
                continue;
            }
            // Bishops can move only diagnolly, which means 
            //      abs(deltaX)==abs(deltaY) must be true
            if self.position.is_valid_offset(i, i) {
                result.push(self.position.move_field(i, i));
            }
            if self.position.is_valid_offset(i, -i) {
                result.push(self.position.move_field(i, -i));
            }
        }
        result
    }

    fn get_all_queen_moves(&self) -> Vec<Field> {
        // A queen is really just a bishop and a rook in one person.
        let mut result = self.get_all_rook_moves();
        result.append(&mut self.get_all_bishop_moves());
        result
    }

    fn get_all_king_moves(&self) -> Vec<Field> {
        let mut result = vec![];
        // One field in every direction.
        for x in -1..=1 {
            for y in -1..=1 {
                if x == 0 && y == 0 {
                    continue;
                }
                if self.position.is_valid_offset(x, y) {
                    result.push(self.position.move_field(x, y));
                }
            }
        }
        // And maybe it can castle as well
        if self.can_castle() {
            if self.position.is_valid_offset(2, 0) {
                result.push(self.position.move_field(2, 0));
            }
            if self.position.is_valid_offset(-2, 0) {
                result.push(self.position.move_field(-2, 0));
            }
        }
        result
    }

    /// To display a nice chess board on the terminal for debugging purposes.
    pub fn to_unicode(&self) -> char {
        match self.color {
            White => match self.kind {
                Pawn => '♙',
                Rook => '♖',
                Knight => '♘',
                Bishop => '♗',
                Queen => '♕',
                King => '♔',
            },
            Black => match self.kind {
                Pawn => '♟',
                Rook => '♜',
                Knight => '♞',
                Bishop => '♝',
                Queen => '♛',
                King => '♚',
            },
        }
    }

    pub fn to_pgn(&self, duplicate: Option<&Piece>, target: &Field) -> String {
        let kind = if self.kind != Pawn {
            self.to_fen().to_ascii_uppercase().to_string()
        } else {
            "".to_string()
        };
        // If there is another piece which can go to target, check on which
        // axis those two pieces differentiate.
        let optional_pos = if let Some(duplicate) = duplicate {
            if duplicate.position.0 != self.position.0 {
                self.position
                    .to_string()
                    .chars()
                    .next()
                    .unwrap()
                    .to_string()
            } else {
                self.position
                    .to_string()
                    .chars()
                    .last()
                    .unwrap()
                    .to_string()
            }
        } else {
            "".to_string()
        };
        format!("{}{}{}", kind, optional_pos, target)
    }

    pub fn to_fen(&self) -> char {
        match self.color {
            White => match self.kind {
                Pawn => 'P',
                Rook => 'R',
                Knight => 'N',
                Bishop => 'B',
                Queen => 'Q',
                King => 'K',
            },
            Black => match self.kind {
                Pawn => 'p',
                Rook => 'r',
                Knight => 'n',
                Bishop => 'b',
                Queen => 'q',
                King => 'k',
            },
        }
    }
}

impl Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.to_unicode(), self.position)
    }
}

#[test]
fn test_pawns() {
    // Test that pawns don't capture outside the chessboard
    let piece = Piece::new(Pawn, Field::parse("a2").unwrap(), Color::White, 0);
    let fields = piece.get_all_moves(true);
    assert!(fields.len() == 1);
    assert!(fields[0] == Field::parse("b3").unwrap());

    let piece = Piece::new(Pawn, Field::parse("d2").unwrap(), Color::White, 0);
    let fields = piece.get_all_moves(false);
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0], Field::parse("d3").unwrap());
    assert_eq!(fields[1], Field::parse("d4").unwrap());
}

#[test]
fn test_rooks() {
    let mut color = White;
    for x in 0..8 {
        for y in 0..8 {
            let piece = Piece::new(Rook, Field(x, y), color, 0);
            color = color.invert();
            let fields = piece.get_all_moves(color == White);
            assert_eq!(fields.len(), 14);
        }
    }
}

/// Contains all possible Castles a player could have.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Castling {
    None,
    QueenSide,
    KingSide,
    Both,
}

/// The error a bad move of a player brings.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GameError {
    /// No error at all
    None,
    /// Parsing error. The move was badly formatted at the $0 character
    ParseErrorAt(usize),
    /// Tried to Promote to a Piece which is not valid (e.g. King or Pawn)
    InvalidPromotionPiece(char, PGNMove),
    /// Tried moving the pawn to the end of the chessboard without promotion
    PromotionExpected(Field, PGNMove),
    /// There is no piece on field $0
    NoPiece(Field, PGNMove),
    /// No piece can move to $0, because e.g. it would induce check on oneself.
    NoPieceReaches(Field, Vec<Piece>, PGNMove),
    /// Multiple pieces can move to $0
    AmbiguousTarget(Field, Vec<Piece>, PGNMove),
}

/// Helper function which formatts alternate pgn moves for the player.
fn fmt_pgn_moves(
    pieces: &Vec<Piece>,
    target: &Field,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    match pieces.len() {
        0 => write!(f, ""),
        1 => write!(f, "{}{}", pieces[0], target),
        length => {
            for i in 0..length - 1 {
                let cur = pieces[i];
                let dup = pieces
                    .iter()
                    .find(|p| p.kind == cur.kind && p.position != cur.position);
                write!(f, "{}, ", cur.to_pgn(dup, target))?;
            }
            let cur = pieces[length - 1];
            let dup = pieces
                .iter()
                .find(|p| p.kind == cur.kind && p.position != cur.position);
            write!(f, "or {}, ", cur.to_pgn(dup, target))?;
            Ok(())
        }
    }
}

impl Display for GameError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GameError::None => write!(f, ""),
            GameError::ParseErrorAt(pos) => write!(f, "Could not read PGN at position {}.", pos),
            GameError::InvalidPromotionPiece(piece, _) => write!(
                f,
                "'{}' can't be promoted. Only Q,R,B,N can be promoted from a pawn.",
                piece
            ),
            GameError::PromotionExpected(pos, mov) => write!(
                f,
                "No pawn can be placed at {}. Use {}=<Piece> where Piece is one of Q, R, B, or N.",
                pos, mov,
            ),
            GameError::NoPiece(pos, _) => write!(f, "There is no piece at {}.", pos),
            GameError::NoPieceReaches(pos, pieces, mov) => {
                if pieces.len() > 0 {
                    write!(f, "{} cannot be played. Try ", mov)?;
                    fmt_pgn_moves(pieces, pos, f)?;
                    write!(f, " instead.")
                } else {
                    write!(f, "No piece can reach {}.", pos)?;
                    Ok(())
                }
            }
            GameError::AmbiguousTarget(pos, pieces, _) => {
                write!(f, "Multiple pieces can reach {}.", pos)?;
                let mut pieces_copy = pieces.clone();
                pieces_copy.dedup_by_key(|p| p.kind);
                if pieces_copy.len() == pieces.len() {
                    write!(f, "Try ")?;
                    fmt_pgn_moves(pieces, pos, f)?;
                    write!(f, " instead.")?;
                }
                Ok(())
            }
        }
    }
}

/// The end of a Game
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameResult {
    /// The Game didn't end.
    None,
    /// Black won the game.
    BlackWins,
    /// White won the game.
    WhiteWins,
    /// No moves left for one color.
    Stalemate,
    /// The pieces can not create a check mate anymore..
    PieceDraw,
    /// Nothing happened for too long in the game. This is not implemented, I
    /// believe.
    NoCaptureOrPawnMoveDraw,
}

impl Display for GameResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GameResult::None => write!(f, ""),
            GameResult::BlackWins => write!(f, "Congratulations black, you won!"),
            GameResult::WhiteWins => write!(f, "Congratulations white, you won!"),
            GameResult::Stalemate => write!(f, "There are no moves left. You have a stalemate."),
            GameResult::PieceDraw => write!(
                f,
                "Neither of you has enough pieces to check mate the other. It's a draw."
            ),
            GameResult::NoCaptureOrPawnMoveDraw => write!(
                f,
                "There's been 50 moves without a capture or a pawn move. It's a draw (probably)."
            ),
        }
    }
}

/// This is a PGN Move but in a format which can be easily reverted by the chess
/// engine. Used to check attacks on the king, as well as to supply a undo
/// operation for the user.
pub struct RevertableMove {
    pub from: Field,
    pub to: Field,
    pub captured: Option<Piece>,
    pub promotion: bool,
    pub has_second_move: bool,
}

impl RevertableMove {
    pub fn new(
        from: Field,
        to: Field,
        captured: Option<Piece>,
        promotion: bool,
        has_second_move: bool,
    ) -> Self {
        Self {
            from,
            to,
            captured,
            promotion,
            has_second_move,
        }
    }
}

/// This is zombie version of the chessboard to simulate future moves and check
/// if a move ends the game.
pub struct CrappyChessBoard {
    pub color_to_move: Color,
    pieces: Vec<Piece>,
}

impl CrappyChessBoard {
    pub fn from(color_to_move: Color, pieces: &Vec<Piece>) -> Self {
        Self {
            color_to_move,
            pieces: pieces.clone(),
        }
    }

    /// Returns the index in the pieces vec of the piece which is at position
    pub fn piece_at_index(&self, position: Field) -> Option<usize> {
        for (i, p) in self.pieces.iter().enumerate() {
            if p.position == position {
                return Some(i);
            }
        }
        None
    }

    /// Returns the piece which is at position
    pub fn piece_at(&self, position: Field) -> Option<Piece> {
        self.piece_at_index(position).map(|i| self.pieces[i])
    }

    /// Returns all pieces by a certain PieceKind
    pub fn get_pieces_by_kind(&self, kind: PieceKind) -> Vec<Piece> {
        self.pieces
            .iter()
            .filter(|p| p.kind == kind)
            .map(|p| *p)
            .collect::<Vec<Piece>>()
    }

    /// Returns all pieces of a certain color, which can move to target
    pub fn pieces_that_can_go_to(&self, target: Field, color: Color) -> Vec<Piece> {
        self.pieces
            .iter()
            .filter(|p| p.color == color)
            .map(|p| *p)
            .filter(|p| self.can_move_to(*p, target))
            .collect()
    }

    /// Checks if piece can actually reach [`to`] on the board.
    pub fn can_move_to(&self, piece: Piece, to: Field) -> bool {
        let other_piece = self.piece_at(to);
        // piece could never reach [`to`]
        if !piece.get_all_moves(other_piece.is_some()).contains(&to) {
            return false;
        }
        // piece would capture one of its own team.
        if let Some(other) = other_piece {
            if other.color == piece.color {
                return false;
            }
        } else {
            // if piece.kind == Pawn && other_piece.is_none() && piece.position.0 - to.0 != 0 {
            //     // Hopefully we won't need en passant.
            //     // if let Some(en_passant) = self.en_passant_field {
            //     //     let to_capture = Field(to.0, piece.position.1);
            //     //     if en_passant != to_capture {
            //     //         println!("No en passant");
            //     //         return false;
            //     //     }
            //     // }
            // }
            // Checks castling
            if piece.kind == King && piece.can_castle() {
                if to == piece.position.move_field(2, 0) {
                    let rook: Vec<Piece> = self
                        .get_pieces_by_kind(Rook)
                        .into_iter()
                        .filter(|p| p.color == self.color_to_move && p.index == 1)
                        .collect();
                    // No rooks left
                    if rook.len() == 0 {
                        return false;
                    }
                    let rook = rook[0];
                    if !rook.can_castle() {
                        return false;
                    }
                } else if to == piece.position.move_field(-2, 0) {
                    let rook: Vec<Piece> = self
                        .get_pieces_by_kind(Rook)
                        .into_iter()
                        .filter(|p| p.color == self.color_to_move && p.index == 0)
                        .collect();
                    // No rooks left
                    if rook.len() == 0 {
                        return false;
                    }
                    let rook = rook[0];
                    if !rook.can_castle() {
                        return false;
                    }
                }
            }
        }
        // Knights can always jump. Every other piece has to check if the line
        // between its position and [`to`] is free of other pieces.
        if piece.kind != Knight {
            for field in Field::create_line(piece.position, to).0 {
                if let Some(_) = self.piece_at(field) {
                    return false;
                }
            }
        }
        true
    }

    /// Moves the piece from [`from`] to [`to`]
    pub fn play_move(&mut self, from: Field, to: Field) -> bool {
        // Capture other piece
        if let Some(other) = self.piece_at(to) {
            let index = self.pieces.iter().position(|p| p == &other).unwrap();
            self.pieces.remove(index);
        }
        // FIXME: Super strange and probably buggy code. I think it should just
        // check if there is actually a piece at [`from`]. But it should do so
        // before capturing anything..
        if let Some(piece) = self.piece_at(from) {
            piece
        } else {
            return false;
        };
        let piece = if let Some(it) = self.piece_at_index(from) {
            it
        } else {
            return false;
        };
        // actually move the piece
        let piece = self.pieces.get_mut(piece).unwrap();
        piece.move_to(to, None);
        // the other color has now its turn
        self.color_to_move = self.color_to_move.invert();
        true
    }

    // Is there a check against color_to_move.invert() ?
    pub fn has_check(&self) -> bool {
        for k in self
            .get_pieces_by_kind(King)
            .iter()
            .filter(|p| p.color != self.color_to_move)
        {
            let target = k.position;
            if self.pieces_that_can_go_to(target, k.color.invert()).len() > 0 {
                return true;
            }
        }
        false
    }
}
