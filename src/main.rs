// use chessboard::ChessBoard;

mod chessboard;
mod chessboard_datatypes;
mod image_gen;
mod pgn_parser;

use chessboard::ChessBoard;
use chessboard_datatypes::{Color, GameResult};
use futures::StreamExt;
use image_gen::{generate_simple_view_to_file, generate_view_to_file};
use pgn_parser::PGNMove;

use telegram_bot::{
    Api, Error, InputFileUpload, Message, MessageKind, SendMessage, SendPhoto, UpdateKind, User,
};

struct Context {
    chessboard: ChessBoard,
    black_player: Option<User>,
    white_player: Option<User>,
    api: Api,
    message: Option<Message>,
}

impl Context {
    pub fn new(api: Api) -> Self {
        Context {
            chessboard: ChessBoard::new(),
            api,
            black_player: None,
            white_player: None,
            message: None,
        }
    }
    pub async fn send_text(&self, text: String) {
        self.api
            .send(SendMessage::new(&self.message.as_ref().unwrap().chat, text))
            .await
            .unwrap();
    }

    pub async fn send_board(&self, filename: &str, caption: &str) {
        let img = InputFileUpload::with_path(filename);
        let mut img = SendPhoto::new(&self.message.as_ref().unwrap().chat, img);
        img.caption(caption);
        self.api
            .send(img)
            .await
            .unwrap();
    }
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    let token = std::fs::read_to_string("token.txt").unwrap();
    let api = Api::new(token);
    let mut context = Context::new(api);
    let mut stream = context.api.stream();
    while let Some(update) = stream.next().await {
        let update = update?;
        match update.kind {
            UpdateKind::Message(message) => match message.kind {
                MessageKind::Text { ref data, .. } => {
                    context.message = Some(message.clone());

                    if data.starts_with("/start") {
                        start_command(&mut context, &data[6..].trim_start()).await;
                    } else if data.starts_with("/simpleview") {
                        simple_view_command(&mut context, &data[5..].trim_start()).await;
                    } else if data.starts_with("/view") {
                        view_command(&mut context, &data[5..].trim_start()).await;
                    } else if data.starts_with("/pgn") {
                        pgn_command(&mut context, &data[4..].trim_start()).await;
                    } else if data.starts_with("/fen") {
                        fen_command(&mut context, &data[4..].trim_start()).await;
                    } else if data.starts_with("/loadfen") {
                        load_fen_command(&mut context, &data[8..].trim_start()).await;
                    } else if data.starts_with("/load") {
                        load_command(&mut context, &data[5..].trim_start()).await;
                    } else if data.starts_with("/undo") {
                        undo_command(&mut context, &data[4..].trim_start()).await;
                    } else if data.starts_with("/moves") {
                        moves_command(&mut context, &data[6..].trim_start()).await
                    } else if data.starts_with("/github") {
                        context.send_text("https://github.com/wert007/chess_checker_telegram_bot".to_string()).await;
                    } else {
                        play(&mut context, data.as_str()).await;
                    }
                }
                _ => (),
            },
            _ => (),
        }
    }

    Ok(())
}

async fn start_command(context: &mut Context, _: &str) -> () {
    context.chessboard = ChessBoard::new();
    context.white_player = None;
    context.black_player = None;
    context.send_text(format!("New game started!")).await;
}

async fn view_command(context: &mut Context, _: &str) -> () {
    let filename = generate_view_to_file(context.chessboard.to_fen());
    let caption = format!("{} to move.", context.chessboard.color_to_move);
    context.send_board(&filename, &caption).await;
}

async fn simple_view_command(context: &mut Context, _: &str) -> () {
    let filename = generate_simple_view_to_file(context.chessboard.to_fen());
    let caption = format!("{} to move.", context.chessboard.color_to_move);
    context.send_board(&filename, &caption).await;
}

async fn pgn_command(context: &mut Context, _: &str) -> () {
    let mut pgn = context.chessboard.pgn_to_string();
    if pgn.len() == 0 {
        pgn.push_str("First play a game, otherwise there are no moves I could give you here.");
    }
    context.send_text(format!("{}", pgn)).await;
}

async fn fen_command(context: &mut Context, _: &str) -> () {
    let fen = context.chessboard.to_fen();
    context.send_text(format!("{}", fen)).await;
}

async fn load_command(context: &mut Context, data: &str) -> () {
    let (chessboard, error) = ChessBoard::from_pgn(data);
    context.chessboard = chessboard;
    let result = if error == crate::chessboard_datatypes::Error::None {
        context.chessboard.check_result()
    } else {
        println!("{}", data);
        GameResult::None
    };
    context
        .send_text(format!("Cheesboard loaded. {} {}", result, error))
        .await;
    view_command(context, data).await;
}

async fn load_fen_command(context: &mut Context, data: &str) -> () {
    println!("{}", data);
    let chessboard = ChessBoard::from_fen(data);
    match chessboard {
        Ok(chessboard) => {
            context.chessboard = chessboard;
            let result = context.chessboard.check_result();
            context
                .send_text(format!("Cheesboard loaded. {}", result))
                .await;
            view_command(context, data).await;
        }
        Err(err) => {
            context
                .send_text(format!(
                    "Parse Error in FEN on character '{}' (#{}).",
                    err, err as u8
                ))
                .await
        }
    }
}

async fn undo_command(context: &mut Context, _: &str) -> () {
    context.chessboard.undo();
}

async fn moves_command(context: &mut Context, _: &str) -> () {
    let movs: Vec<PGNMove> = context.chessboard.get_all_possible_moves();
    if movs.len() == 0 {
        context
            .send_text(format!(
                "No possible moves, the game is over! {}",
                context.chessboard.check_result()
            ))
            .await;
    } else {
        for chunk in movs.chunks(100) {
            let mut text = String::with_capacity(20 * 5);
            for mov in chunk {
                text.push_str(&mov.to_string());
                text.push(',');
                text.push(' ');
            }
            context.send_text(format!("{}", text)).await;
        }
    }
}

async fn should_abort_because_of_player(context: &mut Context) -> bool {
    let message = context.message.as_ref().unwrap();
    if context.chessboard.color_to_move.invert() == Color::White {
        if let Some(white) = &context.white_player {
            if white.id != message.from.id {
                context
                    .send_text(format!(
                        "It is not your turn. Wait for {}.",
                        white.username.as_ref().unwrap_or(&white.first_name)
                    ))
                    .await;
                return true;
            }
        } else {
            context.white_player = Some(message.from.clone());
        }
    } else {
        if let Some(black) = &context.black_player {
            if black.id != message.from.id {
                context
                    .send_text(format!(
                        "It is not your turn. Wait for {}.",
                        black.username.as_ref().unwrap_or(&black.first_name)
                    )).await;
                return true;
            }
        } else {
            if let Some(white) = &context.white_player {
                if white.id == message.from.id {
                    context.send_text(format!("You are now playing against yourself. If you want do play against others, just use /start to start over.")).await;
                }
            }
            context.black_player = Some(message.from.clone());
        }
    }
    false
}

async fn play(context: &mut Context, data: &str) {
    if context.message.is_none() {
        return;
    }

    let mov = PGNMove::parse(data);
    if let Some(mov) = mov {
        if should_abort_because_of_player(context).await {
            return;
        }
        match context.chessboard.play_pgn(mov.0) {
            Ok(result) if result != GameResult::None => {
                context.send_text(format!("Game is over! {}", result)).await
            }
            Err(error) => {
                context
                    .send_text(format!("{} Use /view to view the board!", error))
                    .await
            }
            _ => {}
        }
    }
}
