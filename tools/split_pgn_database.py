import sys
import re
import random


def main(args):
    if len(args) < 3:
        print("Usage", args[0], "<path to pgn database>",
              "<path to output directory>")
        return
    games = []
    with open(args[1], 'r') as input:
        line = 1
        content = input.read()
        content = content.split('\n\n')
        for potential_game in content:
            if potential_game.startswith('['):
                line += len(re.findall('\n', potential_game))
                line += 2
                continue
            elif potential_game.startswith('1.') or potential_game.startswith('{'):
                if random.random() > 0.04:
                    continue
                new_game = ''
                should_skip = False
                do_not_add = '...' in potential_game
                dollar_tag_found = False
                for c in potential_game:
                    if c == '\n':
                        line += 1
                    if c == '}':
                        assert(should_skip)
                        should_skip = False
                        continue
                    if c == '{':
                        should_skip = True
                    if dollar_tag_found:
                        if c.isdigit():
                            continue
                        else:
                            dollar_tag_found = False
                    if should_skip:
                        continue
                    if c == '$':
                        dollar_tag_found = True
                        continue
                    new_game += c
                line += 2
                if not do_not_add:
                    new_game = new_game.rstrip(
                        '0-1').rstrip('1-0').rstrip('1/2-1/2')
                    games.append(new_game)
                    if len(games) == 120:
                        break
            else:
                print("#{}".format(line), potential_game)
                assert(False)
    for index, game in enumerate(games, start=1):
        with open(args[2] + str(index) + '.pgn', 'w') as output:
            output.write(game)


if __name__ == '__main__':
    main(sys.argv)
