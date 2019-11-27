from .player_view import PlayerView
class ServerMessageGame:
    def __init__(self, player_view):
        self.player_view = player_view
    @staticmethod
    def read_from(stream):
        if stream.read_bool():
            player_view = PlayerView.read_from(stream)
        else:
            player_view = None
        return ServerMessageGame(player_view)
    def write_to(self, stream):
        if self.player_view is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.player_view.write_to(stream)
    def __repr__(self):
        return "ServerMessageGame(" + \
            repr(self.player_view) + \
            ")"
