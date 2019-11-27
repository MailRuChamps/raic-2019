require_relative 'model'

class Debug
    def initialize(writer)
        @writer = writer
    end

    def draw(data)
        PlayerMessageGame::CustomDataMessage.new(data).write_to(@writer)
        @writer.flush()
    end
end
