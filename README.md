# Chess Checker Telegram bot
This is a bot, which checks the chess moves you write to the bot or in a group
with the bot. It will keep track of the moves played, say if you wrote a wrong/
impossible move and can show you the current state of the game.
If you want to use this bot you have to set it up yourself. Just fork this repo,
supply some graphics in gfx/pieces/ and gfx/boards/ (how the files need to be
and how they are read in you can see in src/image_gen.rs on the Sheet structs
[BoardSheet, PieceSheet, ChessBoardSheet] and generate_... functions), create
your token with BotFather in telegram and put the token in token.txt and 
build it all with rustup.
The code is admittedly really bad commented, since this was just a simple side
project for myself with little intention to be published. Maybe I will add some
over time. If not just create an issue if you don't understand something and 
I'll try to answer asap.

## The commands
### /start
/start will start a new game with the normal position on the chessboard. The 
first player to write a move is white and the one to write the second move will
be black. One player can be both.

### /load
/load will continue a game from a pgn sequence. Your PGN must have move numbers
starting at 1 and its move in algebraic notation. No Comments, no alternative
routes no tags. This is very simple and mostly meant to used with the own /pgn
command.

### /loadfen
/loadfen will start a game with the specified position on the chessboard. The 
first player to write a move is white and the one to write the second move will
be black. One player can be both.

### /pgn
/pgn will convert the played game to PGN. If the game was created with /start or
/loadfen it will only contain the played move, but if it was loaded from PGN
with /load it will contain those PGN moves as well. It will always start the 
move counter at 1 and will contain no comments nor tags.

### /fen
/fen will convert the current position to FEN. This one should be completely to
spec.

### /view
/view will send a small image with the current position. It switches between 
different styles at random. Only the generator (src/image_gen.rs) and not the 
images are included here to avoid licensing problems.

### /simpleview
/simpleview will send a small image with the current position. It has a fixed
style and will not switch. It may be less aesthetic and more clear.

### /undo
/undo will undo the last move played. It doesn't check who called it. It can
also undo moves that you didn't play but load via /load.

### /moves
/moves will display all possible moves from the current position for the color
to move.

### /github
/github will send a link to this repo.

## Dependencies
All dependencies can be found on crates.io
### regex
regex is used to validate and parse PGN moves.
### image
image is used to generate the images seen on /view and /simpleview.
### telegram-bot
telegram-bot is used to communicate with telegrams bot platform. All other
dependencies come from telegram-bot as well.

## Tools
split_pgn_database.py is used to split and convert multiple PGN games in a file
to multiple files with one game each conform to the expectation of 
src/pgn_parser.rs. It will create 120 files at most and convert games at random
so that the testsuit won't bloat up.