class Player:
    def __init__(self, id, score):
        self.id = id
        self.score = score
    @staticmethod
    def read_from(stream):
        id = stream.read_int()
        score = stream.read_int()
        return Player(id, score)
    def write_to(self, stream):
        stream.write_int(self.id)
        stream.write_int(self.score)
    def __repr__(self):
        return "Player(" + \
            repr(self.id) + "," + \
            repr(self.score) + \
            ")"
