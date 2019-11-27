require_relative 'player_view'
class ServerMessageGame
    attr_accessor :player_view
    def initialize(player_view)
        @player_view = player_view
    end
    def self.read_from(stream)
        if stream.read_bool()
            player_view = PlayerView.read_from(stream)
        else
            player_view = nil
        end
        ServerMessageGame.new(player_view)
    end
    def write_to(stream)
        if @player_view.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            @player_view.write_to(stream)
        end
    end
end
