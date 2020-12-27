use std::{collections::HashMap, fs::File};

use image::{io::Reader as ImageReader, ImageBuffer, ImageError};
use image::{png::PngEncoder, Rgba};


type Pixel = Rgba<u8>;

fn overlay(top: Pixel, bottom: Pixel) -> Pixel {
    let top_floats: Vec<f64> = top.0.iter().map(|c| *c as f64 / 255.0).collect();
    let bottom_floats: Vec<f64> = bottom.0.iter().map(|c| *c as f64 / 255.0).collect();
    let top_floats: Vec<f64> = top_floats.iter().map(|c| c * top_floats[3]).collect();
    let bottom_floats: Vec<f64> = bottom_floats
        .iter()
        .map(|c| c * (1.0 - top_floats[3]))
        .collect();
    let combined = [
        top_floats[0] + bottom_floats[0],
        top_floats[1] + bottom_floats[1],
        top_floats[2] + bottom_floats[2],
        top_floats[3] + bottom_floats[3],
    ];
    let combined: Vec<u8> = combined.iter().map(|c| (*c * 255.0) as u8).collect();
    let combined = [combined[0], combined[1], combined[2], combined[3]];
    Rgba::<u8>(combined)
}

#[test]
fn test() {
    let chessboard = crate::chessboard::ChessBoard::new();

    generate_view_to_file(chessboard.to_fen());
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Vector(u32, u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct VectorI32(i32, i32);

#[derive(Debug, Clone)]
struct BoardSheet {
    offset: Vector,
    size: Vector,
    tile_size: Vector,
    skew: VectorI32,
    plane: f64,
    file_name: String,
}

impl BoardSheet {
    fn to_position(&self, x: i32, y: i32, piece_size: Vector) -> VectorI32 {
        let x_tiles = x * self.tile_size.0 as i32;
        let x_skew = y * self.skew.0;
        let x_piece_origin = (self.offset.0 + self.tile_size.0)  as i32 - piece_size.0 as i32;
        let x_plane = (self.plane * y as f64 * -(x as f64 / 7.0 - 0.5)).round() as i32;
        let x_new = x_tiles + x_skew + x_piece_origin + x_plane;
        // print!("x = {} x_tiles = {} x_skew ({} * {}) = {} x_p_o = {}", x, x_tiles,y, self.skew.0,  x_skew, x_piece_origin);
        let y_tiles = (7 - y) * self.tile_size.1 as i32;
        let y_skew = x * self.skew.1;
        let y_piece_origin = (self.offset.1 + self.tile_size.1)  as i32 - piece_size.1 as i32;
        let y_new = y_tiles + y_skew + y_piece_origin;
        // print!("y = {} y_tiles = {} y_skew ({} * {}) = {} y_p_o = {}", y, y_tiles, x, self.skew.1,  y_skew, y_piece_origin);
        VectorI32(x_new, y_new)
    }

    fn contains(&self, pos: VectorI32) -> bool {
        pos.0 >= 0 && pos.1 >= 0 && pos.0 < self.size.0 as i32 && pos.1 < self.size.1 as i32
    }
}

#[derive(Debug, Clone)]
struct PieceSheet {
    size: Vector,
    offset: Vector,
    file_name: String,
}

impl PieceSheet {
    fn actual_size(&self) -> Vector {
        Vector(self.size.0 + self.offset.0, self.size.1 + self.offset.1)
    }
}


type Sheet = ChessBoardSheet;
struct ChessBoardSheet {
    pieces: PieceSheet,
    board: BoardSheet,
}

impl ChessBoardSheet {
    fn short(name: &str, offset: u32, size: u32, tile_size: u32, piece_offset_y: u32) -> Self {
        Self {
            pieces: PieceSheet {
                size: Vector(tile_size, tile_size),
                offset: Vector(0, piece_offset_y),
                file_name: name.to_string(),
            },
            board: BoardSheet {
                offset: Vector(offset, offset),
                size: Vector(size, size),
                tile_size: Vector(tile_size, tile_size),
                skew: VectorI32(0, 0),
                plane: 0.,
                file_name: name.to_string(),
            },
        }
    }

    fn short_with_vector(
        name: &str,
        offset: Vector,
        size: Vector,
        tile_size: Vector,
        piece_size: Vector,
        piece_offset: Vector,
        skew: VectorI32,
    ) -> Self {
        Self {
            pieces: PieceSheet {
                size: piece_size,
                offset: piece_offset,
                file_name: name.to_string(),
            },
            board: BoardSheet {
                offset,
                size,
                tile_size,
                skew,
                plane: 0.,
                file_name: name.to_string(),
            },
        }
    }

    fn full(
        name: &str,
        offset: Vector,
        size: Vector,
        tile_size: Vector,
        piece_size: Vector,
        piece_offset: Vector,
        plane: f64,
        skew: VectorI32,
    ) -> Self {
        Self {
            pieces: PieceSheet {
                size: piece_size,
                offset: piece_offset,
                file_name: name.to_string(),
            },
            board: BoardSheet {
                offset,
                size,
                tile_size,
                skew,
                plane,
                file_name: name.to_string(),
            },
        }
    }

    fn to_position(&self, x: i32, y: i32) -> VectorI32 {
        self.board.to_position(x, y, self.pieces.actual_size())
    }

    fn load_pieces_img(&self) -> Result<ImageBuffer<Rgba<u8>, Vec<u8>>, ImageError> {
        let pieces_img =
            ImageReader::open(format!("./gfx/pieces/{}.png", self.pieces.file_name))?.decode()?;
        let result = pieces_img
            .as_rgba8()
            .ok_or(ImageError::IoError(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                 "Could not convert your image into rgba8. make sure it exists as png with 32bit per pixel or 1 byte per color per pixel with alpha channel.",
            )))?;
        Ok(result.clone())
    }

    fn load_board_img(&self) -> Result<ImageBuffer<Rgba<u8>, Vec<u8>>, ImageError> {
        let board_img =
            ImageReader::open(format!("./gfx/boards/{}.png", self.board.file_name))?.decode()?;
        let result = board_img
            .as_rgba8()
            .ok_or(ImageError::IoError(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Could not convert your image into rgba8. make sure it exists as png with 32bit per pixel or 1 byte per color per pixel with alpha channel.",
            )))?;
        Ok(result.clone())
    }
}


pub fn generate_simple_view_to_file(fen: String) -> String {
    let sheet = Sheet::short("lichess", 0, 256, 32, 0);
    let result = "generated/simple_view_chessboard.png".to_string();
    create_from_sheet(fen, &sheet, &result);
    result
}

pub fn generate_view_to_file(fen: String) -> String {
    let sheets = [
        Sheet::short("chessmaster", 0, 256, 32, 3),
        Sheet::short_with_vector(
            "sega",
            Vector(40, 50),
            Vector(324, 334),
            Vector(32, 32),
            Vector(20, 30),
            Vector(0, 2),
            VectorI32(0, 0),
        ),
        Sheet::short_with_vector(
            "3d_chess",
            Vector(1, 38),
            Vector(260, 175),
            Vector(24, 16),
            Vector(16, 40),
            Vector(0, 3),
            VectorI32(8, 0),
        ),
        Sheet::full(
            "chessmaster_snes",
            Vector(9, 39),
            Vector(256, 192),
            Vector(29, 16),
            Vector(22, 36),
            Vector(0, 1),
            4.5,
            VectorI32(0, 0),
        ),
        Sheet::short_with_vector(
            "AtskaHeart",
            Vector(40, 50),
            Vector(460, 280),
            Vector(28, 14),
            Vector(64, 80),
            Vector(0, 0),
            VectorI32(28, 14),
        ),
    ];
    let index = std::time::SystemTime::UNIX_EPOCH
        .elapsed()
        .unwrap()
        .as_millis() as usize;
    let sheet = &sheets[index % sheets.len()];
    
    let result = "generated/view_chessboard.png".to_string();
    create_from_sheet(fen, sheet, &result);
    result
}



fn create_from_sheet(fen: String, sheet: &ChessBoardSheet, output_path: &str) {
    let mut coords = HashMap::new();
    coords.insert('P', (0, 0));
    coords.insert('K', (1, 0));
    coords.insert('N', (2, 0));
    coords.insert('R', (3, 0));
    coords.insert('B', (4, 0));
    coords.insert('Q', (5, 0));
    coords.insert('p', (0, 1));
    coords.insert('k', (1, 1));
    coords.insert('n', (2, 1));
    coords.insert('r', (3, 1));
    coords.insert('b', (4, 1));
    coords.insert('q', (5, 1));
    let pieces_img = sheet.load_pieces_img().unwrap();
    let mut board_img = sheet.load_board_img().unwrap();
    // println!("{}", sheet.board.file_name);
    let mut x = 0;
    let mut y = 7;
    let mut fen = fen.chars();
    loop {
        match fen.next() {
            Some('/') => {
                x = 0;
                y -= 1;
            }
            Some(' ') => {
                break;
            }
            Some(d) if d.is_digit(10) => {
                x += d.to_string().parse::<i32>().unwrap();
            }
            Some(c) => {
                let coord = coords.get(&c);
                let pos = sheet.to_position(x, y);
                if let Some(coord) = coord {
                    for y_board in 0..sheet.pieces.size.1 {
                        for x_board in 0..sheet.pieces.size.0 {
                            let pos = VectorI32(pos.0 + x_board as i32, pos.1 + y_board as i32);
                            if !sheet.board.contains(pos) {
                                continue;
                            }
                            let x = pos.0 as u32;
                            let y = pos.1 as u32;
                            board_img.put_pixel(
                                x,
                                y,
                                overlay(
                                    *pieces_img.get_pixel(
                                        x_board + coord.0 * sheet.pieces.size.0,
                                        y_board + coord.1 * sheet.pieces.size.1,
                                    ),
                                    *board_img.get_pixel(x, y),
                                ),
                            )
                        }
                    }
                    x += 1;
                } else if c == ' ' {
                    break;
                } else {
                    panic!("Unexpected '{}' in fen", c);
                }
            }
            None => break,
        }
    }

    
    let output = File::create(output_path).unwrap();
    let d = PngEncoder::new(&output);
    d.encode(
        board_img.into_raw().as_slice(),
        sheet.board.size.0,
        sheet.board.size.1,
        image::ColorType::Rgba8,
    )
    .unwrap();
}