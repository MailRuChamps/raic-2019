import model


class Debug:
    def __init__(self, writer):
        self.writer = writer

    def draw(self, data):
        model.PlayerMessageGame.CustomDataMessage(data).write_to(self.writer)
        self.writer.flush()
