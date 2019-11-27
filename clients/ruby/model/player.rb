class Player
    attr_accessor :id
    attr_accessor :score
    def initialize(id, score)
        @id = id
        @score = score
    end
    def self.read_from(stream)
        id = stream.read_int()
        score = stream.read_int()
        Player.new(id, score)
    end
    def write_to(stream)
        stream.write_int(@id)
        stream.write_int(@score)
    end
end
