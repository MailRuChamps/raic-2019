from .game import Game
class PlayerView:
    def __init__(self, my_id, game):
        self.my_id = my_id
        self.game = game
    @staticmethod
    def read_from(stream):
        my_id = stream.read_int()
        game = Game.read_from(stream)
        return PlayerView(my_id, game)
    def write_to(self, stream):
        stream.write_int(self.my_id)
        self.game.write_to(stream)
    def __repr__(self):
        return "PlayerView(" + \
            repr(self.my_id) + "," + \
            repr(self.game) + \
            ")"
