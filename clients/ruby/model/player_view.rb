require_relative 'game'
class PlayerView
    attr_accessor :my_id
    attr_accessor :game
    def initialize(my_id, game)
        @my_id = my_id
        @game = game
    end
    def self.read_from(stream)
        my_id = stream.read_int()
        game = Game.read_from(stream)
        PlayerView.new(my_id, game)
    end
    def write_to(stream)
        stream.write_int(@my_id)
        @game.write_to(stream)
    end
end
