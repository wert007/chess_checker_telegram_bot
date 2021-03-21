// use chessboard::ChessBoard;

mod chessboard;
mod chessboard_datatypes;
mod image_gen;
mod pgn_parser;

use chessboard::ChessBoard;
use chessboard_datatypes::{Color, GameError, GameResult};
use futures::StreamExt;
use image_gen::{generate_simple_view_to_file, generate_view_to_file};
use pgn_parser::PGNMove;

use telegram_bot::{
    Api, Error, InputFileUpload, Message, MessageKind, SendMessage, SendPhoto, UpdateKind, User,
};

/// The [`Context`] contains all the data, which is used between messages
struct Context {
    /// The [`ChessBoard`] contains the actual game, which player is to move
    /// and which pieces are still in the game, game results if the game is over
    chessboard: ChessBoard,
    /// The Telegram-[`User`] which represents the black player. If black is to
    /// play only this User can make a move. If another User tries to make a
    /// move, the bot will tell who's turn it is.
    black_player: Option<User>,
    /// The Telegram-[`User`] which represents the white player. If white is to
    /// play only this User can make a move. If another User tries to make a
    /// move, the bot will tell who's turn it is.
    white_player: Option<User>,
    /// The actual Telegram-[`Api`] which is used to communicate with the
    /// Telegram interface.
    api: Api,
    /// The last Text-[`Message`], that the bot received.
    message: Option<Message>,
}

impl Context {
    /// Creates the default [`ChessBoard`] and sets the api for communication
    /// with the telegram bot. The players and the last message are None at the
    /// beginning.
    pub fn new(api: Api) -> Self {
        Context {
            chessboard: ChessBoard::new(),
            api,
            black_player: None,
            white_player: None,
            message: None,
        }
    }

    /// Sends a simple text message to the chat of the last received text
    /// message. Panics if no message has been received yet.
    pub async fn send_text(&self, text: String) {
        self.api
            .send(SendMessage::new(&self.message.as_ref().unwrap().chat, text))
            .await
            .unwrap();
    }

    /// Sends an image with a caption. This is used to show the user how the
    /// board is setup currently.
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
    // The token is saved in a textfile to keep my life easy. You can store your
    // token however you like.
    let token = std::fs::read_to_string("token.txt").unwrap();
    let api = Api::new(token);
    let mut context = Context::new(api);
    let mut stream = context.api.stream();
    // Keeps asking if any new messages for the bot have come.
    while let Some(update) = stream.next().await {
        let update = update?;
        match update.kind {
            UpdateKind::Message(message) => match message.kind {
                // We are only interested in text messages, EVERYTHING else is
                // completely ignored.
                MessageKind::Text { ref data, .. } => {
                    context.message = Some(message.clone());
                    // This is a very naive way to check if a command for the
                    // bot has been sent. What each command does is explained
                    // in the README.md on github.
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
                        // If the text message is no known command, check if
                        // the text starts with a (semi-) valid pgn move. If so
                        // and it is the players turn, play that move.
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
    // Start a new game. Set up a new ChessBoard, reset the players and inform
    // the user, that a new game did indeed start.
    context.chessboard = ChessBoard::new();
    context.white_player = None;
    context.black_player = None;
    context.send_text(format!("New game started!")).await;
}

async fn view_command(context: &mut Context, _: &str) -> () {
    // Uses the fen representation of current chessboard to generate an image
    // and send it to the user. Also tell which colors turn it is.
    let filename = generate_view_to_file(context.chessboard.to_fen());
    let caption = format!("{} to move.", context.chessboard.color_to_move);
    context.send_board(&filename, &caption).await;
}

async fn simple_view_command(context: &mut Context, _: &str) -> () {
    // Uses the fen representation of current chessboard to generate a specific
    // image, which might be simpler on the eyes.
    let filename = generate_simple_view_to_file(context.chessboard.to_fen());
    let caption = format!("{} to move.", context.chessboard.color_to_move);
    context.send_board(&filename, &caption).await;
}

async fn pgn_command(context: &mut Context, _: &str) -> () {
    // Use the chessboards pgn history to give the player the history of moves.
    // This can also be used to parse a game later.
    let mut pgn = context.chessboard.pgn_to_string();
    // If there haven't been made any moves, pgn has a length of zero. In this
    // case give a default message.
    if pgn.len() == 0 {
        pgn.push_str("First play a game, otherwise there are no moves I could give you here.");
    }
    context.send_text(pgn).await;
}

async fn fen_command(context: &mut Context, _: &str) -> () {
    // This might be less usefull, but as well just uses the board capabilities
    // to create a FEN representation.
    let fen = context.chessboard.to_fen();
    context.send_text(fen).await;
}

async fn load_command(context: &mut Context, data: &str) -> () {
    // Load the game from a pgn string. If there was an error in the pgn string
    // it is saved in error. A tuple is used here, because even though there
    // were errors, the game could still have been loaded partly, so that always
    // a chessboard exists.
    let (chessboard, error) = ChessBoard::from_pgn(data);
    // Overwrite the chessboard in anyway, even if there are errors.
    context.chessboard = chessboard;
    // If there were no errors, but a finished game was loaded, tell the user
    // right ahead.
    let result = if error == GameError::None {
        context.chessboard.check_result()
    } else {
        // println!("{}", data);
        GameResult::None
    };
    // result and error will display as empty string, if there is no result/error
    context
        .send_text(format!("Cheesboard loaded. {}{}", result, error))
        .await;
    // Send the current board as well, so that the user can see how the
    // board was loaded.
    view_command(context, data).await;
}

async fn load_fen_command(context: &mut Context, data: &str) -> () {
    // If FEN parsing fails there is no chessboard to load. So it is a Result
    // here.
    let chessboard = ChessBoard::from_fen(data);
    match chessboard {
        Ok(chessboard) => {
            context.chessboard = chessboard;
            // Check if the position can be played or if it is a end position.
            let result = context.chessboard.check_result();
            context
                .send_text(format!("Cheesboard loaded. {}", result))
                .await;
            // Send the current board as well, so that the user can see how the
            // board was loaded.
            view_command(context, data).await;
        }
        Err(err) => {
            // Errors are always unexpected characters in the FEN. They are
            // reported back to the user.
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
    let movs = context.chessboard.get_all_possible_moves();
    if movs.len() == 0 {
        context
            .send_text(format!(
                "No possible moves, the game is over! {}",
                context.chessboard.check_result()
            ))
            .await;
    } else {
        // Chunk moves, to make sure not to exceed the length limit in text
        // messages. You could probably also say, that you just show 20 moves
        // or so.
        for chunk in movs.chunks(100) {
            let mut text = String::with_capacity(100 * 5);
            for mov in chunk {
                text.push_str(&mov.to_string());
                text.push(',');
                text.push(' ');
            }
            context.send_text(format!("{}", text)).await;
        }
    }
}

/// Panics if [`context.message`] is [`None`]. Checks that the sender of the
/// message and the player to move are the same. If there is no user for the
/// color to move set, set the user to the sender of the message.
async fn should_abort_because_of_player(context: &mut Context) -> bool {
    let message = context.message.as_ref().unwrap();
    if context.chessboard.color_to_move == Color::White {
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
                // If its black to move and the sender is the same as the white
                // player, the user is playing against itself. The user should
                // know that, if that was a mistake.
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
    // This should never trigger, since the first thing we do, if we receive a
    // message is to store it in context.message. So this means play was called
    // in a context where it shouldn't have been called.
    if context.message.is_none() {
        return;
    }

    // Check if the message can actually be read as a move. If not just ignore
    // it, so that people still can have normal conversations during the game.
    let mov = PGNMove::parse(data);
    if let Some(mov) = mov {
        // Check if the right user made that move. Otherwise they are warned in
        // [`should_abort_because_of_player`].
        if should_abort_because_of_player(context).await {
            return;
        }
        // Checks if the purposed move can actually played on the board.
        match context.chessboard.play_pgn(mov.0) {
            Ok(result) if result != GameResult::None => {
                // If the game is over congratulate the player.
                context.send_text(format!("Game is over! {}", result)).await
            }
            Err(error) => {
                // Tell the user what error happend.
                context
                    .send_text(format!("{} Use /view to view the board!", error))
                    .await
            }
            _ => {}
        }
    }
}
