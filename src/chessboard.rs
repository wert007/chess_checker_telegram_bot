use std::collections::HashMap;

use crate::{
    chessboard_datatypes::*,
    pgn_parser::{parse_pgn_moves, PGNMove},
};

pub struct ChessBoard {
    pub color_to_move: Color,
    pieces: Vec<Piece>,
    move_count: usize,
    last_capture_or_pawn_advance: usize,
    white_castling: Castling,
    black_castling: Castling,
    en_passant_field: Option<Field>,
    pgn_history: Vec<PGNMove>,
    history: Vec<RevertableMove>,
}

// pub struct ChessMove {
//     from: Field,
//     to: Field,
//     promotion: Option<PieceKind>,
//     extra: PGNMove,
// }

// impl ChessMove {
//     pub fn new(from: Field, to: Field, promotion: Option<PieceKind>, extra: PGNMove) -> Self { Self { from, to, promotion, extra } }
// }

impl ChessBoard {
    pub fn new() -> Self {
        let mut pieces = vec![];
        for i in 0..8 {
            pieces.push(Piece::new(Pawn, Field(i, 1), White, i as usize));
            pieces.push(Piece::new(Pawn, Field(i, 6), Black, i as usize));
        }
        for (i, p) in [Rook, Knight, Bishop].iter().enumerate() {
            let i = i as u8;
            pieces.push(Piece::new(*p, Field(i, 0), White, 0));
            pieces.push(Piece::new(*p, Field(7 - i, 0), White, 1));
            pieces.push(Piece::new(*p, Field(i, 7), Black, 0));
            pieces.push(Piece::new(*p, Field(7 - i, 7), Black, 1));
        }
        pieces.push(Piece::new(Queen, Field(3, 0), White, 0));
        pieces.push(Piece::new(King, Field(4, 0), White, 0));
        pieces.push(Piece::new(Queen, Field(3, 7), Black, 0));
        pieces.push(Piece::new(King, Field(4, 7), Black, 0));
        ChessBoard {
            color_to_move: White,
            pieces,
            move_count: 1,
            last_capture_or_pawn_advance: 1,
            white_castling: Castling::Both,
            black_castling: Castling::Both,
            en_passant_field: None,
            pgn_history: vec![],
            history: vec![],
        }
    }

    pub fn from_pgn(pgn: &str) -> (Self, GameError) {
        let mut result = Self::new();
        let (moves, mut error) = parse_pgn_moves(pgn);
        for mov in moves {
            if let Err(e) = result.play_pgn(mov) {
                error = e;
                break;
            }
        }
        (result, error)
    }

    pub fn from_fen(fen: &str) -> Result<Self, char> {
        let mut fen_chars = fen.chars();
        let mut pieces = vec![];
        let mut x = 0;
        let mut y = 0;
        let mut indices = HashMap::new();
        let mut insert_piece = |cur: char| {
            if !indices.contains_key(&cur) {
                indices.insert(cur, 0);
            }
            let result = 1 + indices[&cur];
            indices.insert(cur, result);
            result
        };
        loop {
            let cur = if let Some(it) = fen_chars.next() {
                it
            } else {
                break;
            };
            match cur {
                '/' => {
                    x = 0;
                    y += 1;
                }
                ' ' => break,
                'Q' => pieces.push(Piece::new(Queen, Field(x, y), White, insert_piece(cur))),
                'q' => pieces.push(Piece::new(Queen, Field(x, y), Black, insert_piece(cur))),
                'K' => pieces.push(Piece::new(King, Field(x, y), White, insert_piece(cur))),
                'k' => pieces.push(Piece::new(King, Field(x, y), Black, insert_piece(cur))),
                'P' => pieces.push(Piece::new(Pawn, Field(x, y), White, insert_piece(cur))),
                'p' => pieces.push(Piece::new(Pawn, Field(x, y), Black, insert_piece(cur))),
                'R' => pieces.push(Piece::new(Rook, Field(x, y), White, insert_piece(cur))),
                'r' => pieces.push(Piece::new(Rook, Field(x, y), Black, insert_piece(cur))),
                'B' => pieces.push(Piece::new(Bishop, Field(x, y), White, insert_piece(cur))),
                'b' => pieces.push(Piece::new(Bishop, Field(x, y), Black, insert_piece(cur))),
                'N' => pieces.push(Piece::new(Knight, Field(x, y), White, insert_piece(cur))),
                'n' => pieces.push(Piece::new(Knight, Field(x, y), Black, insert_piece(cur))),
                digit if digit.is_ascii_digit() => x += digit.to_digit(10).unwrap() as u8,
                err => return Err(err),
            }
        }
        let c = if let Some(it) = fen_chars.next() {
            it
        } else {
            return Err('\0');
        };
        let color_to_move = Color::from_fen(c)?;
        if let Some(space) = fen_chars.next() {
            if space != ' ' {
                return Err(space);
            }
        } else {
            return Err('\0');
        }
        let mut buffer = String::with_capacity(4);
        loop {
            let cur = if let Some(it) = fen_chars.next() {
                it
            } else {
                break;
            };
            match cur {
                ' ' => break,
                '-' | 'K' | 'Q' | 'k' | 'q' => buffer.push(cur),
                err => return Err(err),
            }
        }
        let white_castling = if buffer == "-" {
            Castling::None
        } else if buffer.contains('Q') {
            if buffer.contains('K') {
                Castling::Both
            } else {
                Castling::QueenSide
            }
        } else if buffer.contains('K') {
            Castling::KingSide
        } else {
            Castling::None
        };
        let black_castling = if buffer == "-" {
            Castling::None
        } else if buffer.contains('q') {
            if buffer.contains('k') {
                Castling::Both
            } else {
                Castling::QueenSide
            }
        } else if buffer.contains('k') {
            Castling::KingSide
        } else {
            Castling::None
        };
        buffer.clear();

        loop {
            let cur = if let Some(it) = fen_chars.next() {
                it
            } else {
                break;
            };
            match cur {
                ' ' => break,
                'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | '1' | '2' | '3' | '4' | '5'
                | '6' | '7' | '8' => buffer.push(cur),
                err => return Err(err),
            }
        }
        let en_passant_field = if buffer == "-" {
            None
        } else {
            Some(Field::parse(&buffer).unwrap())
        };

        buffer.clear();
        loop {
            let cur = if let Some(it) = fen_chars.next() {
                it
            } else {
                break;
            };
            match cur {
                ' ' => break,
                digit if digit.is_digit(10) => buffer.push(digit),
                err => return Err(err),
            }
        }
        let last_capture_or_pawn_advance = buffer.parse().unwrap();

        buffer.clear();
        loop {
            let cur = if let Some(it) = fen_chars.next() {
                it
            } else {
                break;
            };
            match cur {
                ' ' => break,
                digit if digit.is_digit(10) => buffer.push(digit),
                err => return Err(err),
            }
        }
        let move_count = 2 * (buffer.parse::<isize>().unwrap() - 1);
        let mut move_count = move_count as usize;
        if color_to_move == White {
            move_count += 1;
        }
        Ok(Self {
            color_to_move,
            pieces,
            move_count,
            last_capture_or_pawn_advance,
            white_castling,
            black_castling,
            en_passant_field,
            pgn_history: vec![],
            history: vec![],
        })
    }

    fn create_copy_from_move(&self, from: Field, to: Field) -> CrappyChessBoard {
        let mut copy = CrappyChessBoard::from(self.color_to_move, &self.pieces);
        assert!(copy.play_move(from, to));
        copy
    }

    pub fn to_reduced_fen(&self) -> String {
        let mut result = String::new();
        let mut count_empty_fields = 0;
        for y in (0..8).rev() {
            for x in 0..8 {
                let piece = self.piece_at(Field(x, y));
                if let Some(piece) = piece {
                    if count_empty_fields != 0 {
                        result.push(count_empty_fields.to_string().chars().next().unwrap());
                        count_empty_fields = 0;
                    }
                    result.push(piece.to_fen());
                } else {
                    count_empty_fields += 1;
                }
            }
            if count_empty_fields != 0 {
                result.push(count_empty_fields.to_string().chars().next().unwrap());
                count_empty_fields = 0;
            }
            if y != 0 {
                result.push('/');
            }
        }
        result.push(' ');
        result.push(self.color_to_move._to_fen());
        result.push(' ');
        if self.white_castling != Castling::None {
            match self.white_castling {
                Castling::QueenSide => {
                    result.push('Q');
                }
                Castling::KingSide => {
                    result.push('K');
                }
                Castling::Both => {
                    result.push_str("KQ");
                }
                Castling::None => panic!(),
            }
        } else if self.black_castling != Castling::None {
            match self.black_castling {
                Castling::QueenSide => {
                    result.push('q');
                }
                Castling::KingSide => {
                    result.push('k');
                }
                Castling::Both => {
                    result.push_str("kq");
                }
                Castling::None => panic!(),
            }
        } else {
            result.push('-');
        }
        result.push(' ');
        if let Some(en_passant_field) = self.en_passant_field {
            result.push_str(&en_passant_field.to_string());
        } else {
            result.push('-');
        }
        result
    }

    pub fn to_fen(&self) -> String {
        let mut result = self.to_reduced_fen();
        result.push(' ');
        result.push_str(&self.halfmove_clock().to_string());
        result.push(' ');
        result.push_str(&self.current_move_no().to_string());

        result
    }

    pub fn pgn_to_string(&self) -> String {
        let mut result = String::new();
        let mut history_iter = self.pgn_history.iter();
        let mut number = 1;
        loop {
            if let Some(entry) = history_iter.next() {
                result.push_str(&number.to_string());
                result.push('.');
                result.push(' ');
                result.push_str(&entry.to_string());
                result.push(' ');
            } else {
                break;
            }
            if let Some(entry) = history_iter.next() {
                result.push_str(&entry.to_string());
                result.push(' ');
            } else {
                break;
            }
            number += 1;
        }

        result
    }

    pub fn check_result(&self) -> GameResult {
        if self.has_check_mate() {
            if self.color_to_move == White {
                GameResult::BlackWins
            } else {
                GameResult::WhiteWins
            }
        } else if self.has_stale_mate() {
            GameResult::Stalemate
        } else if self.halfmove_clock() == 100 {
            GameResult::NoCaptureOrPawnMoveDraw
        } else {
            let bishops = self.get_pieces_by_kind(Bishop);
            if self.pieces.len() == 2
                || self.pieces.len() == 3
                    && self.get_pieces_by_kind(Bishop).len() > 0
                || self.pieces.len() == 3
                    && self.get_pieces_by_kind(Knight).len() > 0
                || self.pieces.len() == 4
                    && bishops.len() == 2
                    && bishops[0].position.color() == bishops[1].position.color()
            {
                GameResult::PieceDraw
            } else {
                GameResult::None
            }
        }
    }

    pub fn get_pieces_by_kind(&self, kind: PieceKind) -> Vec<Piece> {
        self.get_pieces_by(Some(kind), None, None, None, None)
    }

    pub fn get_pieces_by_color(&self, color: Color) -> Vec<Piece> {
        self.get_pieces_by(None, Some(color), None, None, None)
    }

    pub fn get_pieces_by(
        &self,
        piece: Option<PieceKind>,
        color: Option<Color>,
        index: Option<usize>,
        column: Option<u8>,
        row: Option<u8>,
    ) -> Vec<Piece> {
        let mut result = self.pieces.clone();
        for i in (0..result.len()).rev() {
            let cur = result[i];
            if let Some(piece) = piece {
                if cur.kind != piece {
                    result.remove(i);
                    continue;
                }
            }
            if let Some(color) = color {
                if cur.color != color {
                    result.remove(i);
                    continue;
                }
            }
            if let Some(index) = index {
                if cur.index != index {
                    result.remove(i);
                    continue;
                }
            }
            if let Some(column) = column {
                if cur.position.0 != column {
                    result.remove(i);
                    continue;
                }
            }
            if let Some(row) = row {
                if cur.position.1 != row {
                    result.remove(i);
                    continue;
                }
            }
        }
        result
    }

    pub fn set_castling(&mut self, castling: Castling) {
        if self.color_to_move == White {
            if self.white_castling < castling {
                self.white_castling = castling;
            }
        } else {
            if self.black_castling < castling {
                self.black_castling = castling;
            }
        }
    }

    pub fn current_move_no(&self) -> usize {
        (self.move_count) / 2
    }

    pub fn halfmove_clock(&self) -> usize {
        self.move_count - self.last_capture_or_pawn_advance
    }

    pub fn has_check(&self) -> bool {
        for k in self.get_pieces_by_kind(King) {
            let target = k.position;
            if self.pieces_that_can_go_to(target, k.color.invert()).len() > 0 {
                return true;
            }
        }
        false
    }

    pub fn has_stale_mate(&self) -> bool {
        for k in self.get_pieces_by_color(self.color_to_move) {
            if self.get_all_moves_on_board(k).len() > 0 {
                return false;
            }
        }
        true
    }

    pub fn has_check_mate(&self) -> bool {
        self.has_check() && self.has_stale_mate()
    }

    pub fn get_all_moves_on_board(&self, piece: Piece) -> Vec<Field> {
        let mut result = piece.get_all_moves(false);
        for i in (0..result.len()).rev() {
            let cur = result[i];
            if !self.can_move_to(piece, cur) {
                result.remove(i);
            }
        }
        result
    }

    pub fn get_all_possible_moves(&self) -> Vec<PGNMove> {
        let mut result = vec![];
        for piece in self.get_pieces_by_color(self.color_to_move) {
            for mov in self.get_all_moves_on_board(piece) {
                let captures = self.piece_at(mov).is_some();
                result.push(PGNMove::new(
                    piece.to_string().chars().next().unwrap(),
                    mov,
                    captures,
                    false,
                    false,
                    None,
                    None,
                ));
            }
        }
        result
    }

    pub fn pieces_attack_king_at(&self, position: Field, color: Color) -> bool {
        let pieces = self.get_pieces_by_color(color);
        let pieces: Vec<Piece> = pieces
            .into_iter()
            // .filter(|p| p.get_all_moves(mov.captures).contains(&mov.target))
            .filter(|p| p.kind != Pawn && p.kind != King && self.can_move_to(*p, position))
            .collect();
        for p in pieces {
            if p.kind != King && p.kind != Pawn {
                return true;
            }
        }
        for p in self.get_pieces_by(Some(Pawn), Some(color), None, None, None) {
            if p.get_all_moves(true).contains(&position) {
                return true;
            }
        }
        let enemy_king = self.get_pieces_by(Some(King), Some(color), Some(0), None, None)[0];
        if position.distance_to(enemy_king.position) <= 2 {
            return true;
        }
        return false;
    }

    pub fn can_move_to(&self, piece: Piece, to: Field) -> bool {
        let other_piece = self.piece_at(to);
        if !piece.get_all_moves(other_piece.is_some()).contains(&to) {
            return false;
        }
        if let Some(other) = other_piece {
            if other.color == piece.color {
                return false;
            }
        } else {
            if piece.kind == Pawn && other_piece.is_none() && piece.position.0 - to.0 != 0 {
                if let Some(en_passant) = self.en_passant_field {
                    let to_capture = Field(to.0, piece.position.1);
                    if en_passant != to_capture {
                        // println!("No en passant");
                        return false;
                    }
                }
            }
            if piece.kind == King && piece.can_castle() {
                if to == piece.position.move_field(2, 0) {
                    let rook =
                        self.get_pieces_by(Some(Rook), Some(self.color_to_move), Some(1), None, None);
                    if rook.len() == 0 {
                        return false;
                    }
                    let rook = rook[0];
                    if !rook.can_castle() {
                        return false;
                    }
                } else if to == piece.position.move_field(-2, 0) {
                    let rook =
                        self.get_pieces_by(Some(Rook), Some(self.color_to_move), Some(0), None, None);
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
        if piece.kind == King {
            if self.pieces_attack_king_at(to, piece.color.invert()) {
                return false;
            }
        }

        if piece.kind != Knight {
            for field in Field::create_line(piece.position, to).0 {
                if let Some(_) = self.piece_at(field) {
                    return false;
                }
            }
        }
        let board_after_next_move = self.create_copy_from_move(piece.position, to);
        !board_after_next_move.has_check()
    }

    pub fn pieces_that_can_go_to(&self, target: Field, color: Color) -> Vec<Piece> {
        let piece = self.get_pieces_by_color(color);
        piece
            .into_iter()
            .filter(|p| self.can_move_to(*p, target))
            .collect()
    }

    pub fn play_pgn(&mut self, mov: PGNMove) -> Result<GameResult, GameError> {
        if mov.is_castle {
            let result = self.play_castling(mov.is_short_castle);
            // println!(
            //     "Played {} in #{} \n\n {}\n",
            //     mov,
            //     self.current_move_no(),
            //     &self
            // );
            result
        } else {
            let piece_kind = PieceKind::from_char(mov.piece).expect(&format!("{}", mov));
            let optional_column = if let Some(pos) = mov.optional_position {
                if pos.is_ascii_alphabetic() {
                    Some((pos as i16 - 'a' as i16) as u8)
                } else {
                    None
                }
            } else {
                None
            };
            let optional_row = if let Some(pos) = mov.optional_position {
                if pos.is_ascii_digit() {
                    Some((pos as i16 - '1' as i16) as u8)
                } else {
                    None
                }
            } else {
                None
            };
            let piece = self.get_pieces_by(
                Some(piece_kind),
                Some(self.color_to_move),
                None,
                optional_column,
                optional_row,
            );
            let piece: Vec<&Piece> = piece
                .iter()
                // .filter(|p| p.get_all_moves(mov.captures).contains(&mov.target))
                .filter(|p| self.can_move_to(**p, mov.target))
                .collect();
            if piece.len() == 0 {
                // let pieces = self.get_pieces_by(None, Some(self.color_to_move), None, optional_column);
                let pieces = self.pieces_that_can_go_to(mov.target, self.color_to_move);
                return Err(GameError::NoPieceReaches(mov.target, pieces.into_iter().map(|p| p.to_owned()).collect(), mov));
            }
            if piece.len() > 1 {
                return Err(GameError::AmbiguousTarget(mov.target, piece.into_iter().map(|p| p.to_owned()).collect(), mov));
            }
            assert_eq!(
                piece.len(),
                1,
                "Move was {} in #{}",
                mov,
                self.current_move_no()
            );
            let piece = piece.get(0).expect(&format!(
                "Could not find a {:?}, {:?}, {:?} ({}) in #{} on:\n\n\n{}",
                piece_kind,
                self.color_to_move,
                optional_column,
                mov,
                self.current_move_no() + 1,
                &self
            ));
            if let Some(promotion) = mov.optional_promotion {
                if PieceKind::from_char(promotion).is_none() {
                    return Err(GameError::InvalidPromotionPiece(promotion, mov));
                }
            }
            let promotion = mov
                .optional_promotion
                .map(|c| PieceKind::from_char(c).unwrap());

            self.pgn_history.push(mov);
            let result = self.move_from_to(piece.position, mov.target, promotion, mov)?;
            // println!(
            //     "Played {} in #{} \n\n {}\n",
            //     mov,
            //     self.current_move_no(),
            //     &self
            // );
            Ok(result)
        }
    }

    pub fn piece_at_index(&self, position: Field) -> Option<usize> {
        for (i, p) in self.pieces.iter().enumerate() {
            if p.position == position {
                return Some(i);
            }
        }
        None
    }

    pub fn piece_at(&self, position: Field) -> Option<Piece> {
        self.piece_at_index(position).map(|i| self.pieces[i])
    }

    pub fn play_castling(&mut self, is_short_castling: bool) -> Result<GameResult, GameError> {
        self.set_castling(Castling::None);
        let color = self.color_to_move;
        let king = self.get_pieces_by(Some(King), Some(color), None, None,None)[0];
        let target = if is_short_castling {
            king.position.move_field(2, 0)
        } else {
            king.position.move_field(-2, 0)
        };
        let mov = PGNMove::castle(is_short_castling, false, false); 
        self.move_from_to(
            king.position,
            target,
            None,
            mov,
        )?;
        let rook_position = if is_short_castling {
            self.get_pieces_by(Some(Rook), Some(color), Some(1), None, None)[0].position
        } else {
            self.get_pieces_by(Some(Rook), Some(color), Some(0), None, None)[0].position
        };
        let index = if let Some(index) = self.piece_at_index(rook_position) {
            index
        } else {
            return Err(GameError::NoPiece(rook_position, mov));
        };
        let rook = self.pieces.get_mut(index).unwrap();
        let target = if is_short_castling {
            rook.position.move_field(-2, 0)
        } else {
            rook.position.move_field(3, 0)
        };
        rook.move_to(target, None);
        let result = self.check_result();
        let check = self.has_check();
        let mate = self.has_check_mate();
        self.pgn_history
            .push(PGNMove::castle(is_short_castling, check, mate));
        self.history.push(RevertableMove::new(
            rook_position,
            target,
            None,
            false,
            true,
        ));
        Ok(result)
    }

    pub fn undo(&mut self) {
        let to_revert = self.history.pop();
        let to_revert = if let Some(to_revert) = to_revert {
            to_revert
        } else {
            return;
        };
        let piece_index = self.piece_at_index(to_revert.to).unwrap();
        self.pieces[piece_index].undo_move_back_to(to_revert.from, to_revert.promotion);
        if let Some(captured) = to_revert.captured {
            self.pieces.push(captured);
        }
        self.pgn_history.pop();
        if to_revert.has_second_move {
            let to_revert = self.history.pop().unwrap();
            let piece_index = self.piece_at_index(to_revert.to).unwrap();
            self.pieces[piece_index].undo_move_back_to(to_revert.from, to_revert.promotion);
        }
        self.color_to_move = self.color_to_move.invert();
    }

    pub fn move_from_to(
        &mut self,
        from: Field,
        to: Field,
        promotion: Option<PieceKind>,
        extra: PGNMove,
    ) -> Result<GameResult, GameError> {
        let other = self.piece_at(to).clone();
        if let Some(other) = self.piece_at(to) {
            let index = self.pieces.iter().position(|p| p == &other).unwrap();
            self.pieces.remove(index);
            self.last_capture_or_pawn_advance = self.move_count;
        }
        let piece = if let Some(piece) = self.piece_at(from) {
            piece
        } else {
            return Err(GameError::NoPiece(from, extra));
        };
        if piece.kind == Rook {
            self.set_castling(if piece.index == 1 {
                Castling::QueenSide
            } else {
                Castling::KingSide
            });
        } else if piece.kind == King {
            self.set_castling(Castling::None);
        }
        if piece.kind == Pawn {
            self.last_capture_or_pawn_advance = self.move_count;
            if from.distance_to(to) == 4 {
                self.en_passant_field = Some(Field((from.0 + to.0) / 2, (from.1 + to.1) / 2));
            }
            if to.1 == 7 || to.1 == 0 {
                if promotion.is_none() {
                    return Err(GameError::PromotionExpected(to, extra));
                }
            }
        }
        let piece = self
            .piece_at_index(from)
            .ok_or_else(|| GameError::NoPiece(from, extra))?;
        let piece = self.pieces.get_mut(piece).unwrap();
        piece.move_to(to, promotion);
        self.color_to_move = self.color_to_move.invert();
        self.move_count += 1;
        self.history.push(RevertableMove::new(
            from,
            to,
            other,
            promotion.is_some(),
            false,
        ));
        Ok(self.check_result())
    }
}

impl CheckerBoardDisplay for ChessBoard {
    fn get_at(&self, x: u8, y: u8) -> char {
        self.piece_at(Field(x, y)).map_or(' ', |p| p.to_unicode())
    }
}
